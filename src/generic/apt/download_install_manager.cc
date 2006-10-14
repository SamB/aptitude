// download_install_manager.cc
//
//   Copyright (C) 2005-2006 Daniel Burrows
//
//   This program is free software; you can redistribute it and/or
//   modify it under the terms of the GNU General Public License as
//   published by the Free Software Foundation; either version 2 of
//   the License, or (at your option) any later version.
//
//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//   General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with this program; see the file COPYING.  If not, write to
//   the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//   Boston, MA 02111-1307, USA.

#include "download_install_manager.h"

#include "config_signal.h"
#include "download_signal_log.h"
#include "log.h"

#include <aptitude.h>

#include <apt-pkg/acquire-item.h>
#include <apt-pkg/dpkgpm.h>
#include <apt-pkg/error.h>
#include <apt-pkg/sourcelist.h>

#include <pthread.h>
#include <signal.h>

using namespace std;

download_install_manager::download_install_manager(bool _download_only)
  : log(NULL), download_only(_download_only), pm(new pkgDPkgPM(*apt_cache_file))
{
}

download_install_manager::~download_install_manager()
{
  delete pm;
}

bool download_install_manager::prepare(OpProgress &progress,
				       pkgAcquireStatus &acqlog,
				       download_signal_log *signallog)
{
  log = signallog;

  if(apt_cache_file == NULL)
    {
      _error->Error(_("The package cache is not available; unable to download and install packages."));
      return false;
    }

  if(!(*apt_cache_file)->save_selection_list(progress))
    return false;

  progress.Done();

  // Lock the archive directory..
  FileFd lock;
  if(aptcfg->FindB("Debug::NoLocking", false) == false)
    {
      lock.Fd(GetLock(aptcfg->FindDir("Dir::State::Lists")+"lock"));
      if(_error->PendingError() == true)
	{
	  _error->Error(_("Couldn't lock list directory..are you root?"));
	  return false;
	}
    }

  if(!src_list.ReadMainList())
    {
      _error->Error(_("Couldn't read source list"));

      return false;
    }

  fetcher = new pkgAcquire(&acqlog);

  if(!pm->GetArchives(fetcher, &src_list, apt_package_records) ||
     _error->PendingError())
    {
      _error->Error(_("Internal error: couldn't generate list of packages to download"));

      delete fetcher;
      fetcher = NULL;
      return false;
    }

  return true;
}

download_manager::result download_install_manager::execute_install_run(pkgAcquire::RunResult res,
								       OpProgress &progress)
{
  if(res != pkgAcquire::Continue)
    return failure;
  else if(download_only)
    return success;

  bool failed=false;
  for(pkgAcquire::ItemIterator i = fetcher->ItemsBegin();
      i != fetcher->ItemsEnd(); ++i)
    {
      if((*i)->Status == pkgAcquire::Item::StatDone &&
	 (*i)->Complete)
	continue;

      if((*i)->Status == pkgAcquire::Item::StatIdle)
	continue;

      failed=true;
      _error->Error(_("Failed to fetch %s: %s"), (*i)->DescURI().c_str(), (*i)->ErrorText.c_str());
      break;
    }

  if(failed && !pm->FixMissing())
    {
      _error->Error(_("Unable to correct for unavailable packages"));
      return failure;
    }

  log_changes();

  pre_install_hook();

  // Note that someone could grab the lock before dpkg takes it;
  // without a more complicated synchronization protocol (and I don't
  // control the code at dpkg's end), them's the breaks.
  apt_cache_file->ReleaseLock();

  result rval = success;

  sigset_t allsignals;
  sigset_t oldsignals;
  sigfillset(&allsignals);

  pthread_sigmask(SIG_UNBLOCK, &allsignals, &oldsignals);
  pkgPackageManager::OrderResult pmres = pm->DoInstall(aptcfg->FindI("APT::Status-Fd", -1));
  pthread_sigmask(SIG_SETMASK, &oldsignals, NULL);

  switch(pmres)
    {
    case pkgPackageManager::Failed:
      _error->DumpErrors();
      cerr << _("A package failed to install.  Trying to recover:") << endl;
      system("dpkg --configure -a");
      _error->Discard();
      
      rval = failure;
      break;
    case pkgPackageManager::Completed:
      break;

    case pkgPackageManager::Incomplete:
      rval = do_again;
      break;
    }

  post_install_hook(pmres);

  fetcher->Shutdown();

  if(!pm->GetArchives(fetcher, &src_list, apt_package_records))
    return failure;

  if(!apt_cache_file->GainLock())
    // This really shouldn't happen.
    {
      _error->Error(_("Could not regain the system lock!  (Perhaps another apt or dpkg is running?)"));
      return failure;
    }

  return rval;
}

download_manager::result download_install_manager::finish(pkgAcquire::RunResult res,
							  OpProgress &progress)
{
  result run_res = execute_install_run(res, progress);

  if(run_res != do_again)
    {
      apt_close_cache();

      if(log != NULL)
	log->Complete();

      if(aptcfg->FindB(PACKAGE "::Forget-New-On-Install", false))
	{
	  apt_load_cache(&progress, true);
	  if(apt_cache_file != NULL)
	    {
	      (*apt_cache_file)->forget_new(NULL);
	      post_forget_new_hook();
	    }
	}
    }

  return run_res;
}
