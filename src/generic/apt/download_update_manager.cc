// download_update_manager.cc
//
//   Copyright (C) 2005, 2007 Daniel Burrows
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

#include "download_update_manager.h"

#include "apt.h"
#include "config_signal.h"
#include "download_signal_log.h"

#include <apt-pkg/cachefile.h>
#include <apt-pkg/clean.h>
#include <apt-pkg/error.h>

class my_cleaner:public pkgArchiveCleaner
{
protected:
  virtual void Erase(const char *file,
		     string pkg,
		     string ver,
		     struct stat &stat)
  {
    unlink(file);
  }
};

download_update_manager::download_update_manager()
  : log(NULL)
{
}

download_update_manager::~download_update_manager()
{
}

bool download_update_manager::prepare(OpProgress &progress,
				      pkgAcquireStatus &acqlog,
				      download_signal_log *signallog)
{
  log = signallog;

  if(apt_cache_file != NULL &&
     !(*apt_cache_file)->save_selection_list(progress))
    return false;

  // FIXME: should save_selection_list do this?
  progress.Done();

  if(src_list.ReadMainList() == false)
    {
      _error->Error(_("Couldn't read list of package sources"));
      return false;
    }

  // Abort here so we don't spew random messages below.
  if(_error->PendingError())
    return false;

  // Lock the list directory
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

  fetcher = new pkgAcquire(&acqlog);

  if(!src_list.GetIndexes(fetcher))
    {
      delete fetcher;
      fetcher = NULL;
      return false;
    }
  else
    return true;
}

download_manager::result
download_update_manager::finish(pkgAcquire::RunResult res,
				OpProgress &progress)
{
  if(log != NULL)
    log->Complete();

  apt_close_cache();

  if(res != pkgAcquire::Continue)
    return failure;

  // Clean old stuff out
  if(fetcher->Clean(aptcfg->FindDir("Dir::State::lists")) == false ||
     fetcher->Clean(aptcfg->FindDir("Dir::State::lists")+"partial/") == false)
    {
      _error->Error(_("Couldn't clean out list directories"));
      return failure;
    }

  // Rebuild the apt caches as done in apt-get.  cachefile is scoped
  // so it dies before we possibly-reload the cache.  This will do a
  // little redundant work in visual mode, but avoids lots of
  // redundant work at the command-line.
  {
    pkgCacheFile cachefile;
    if(!cachefile.BuildCaches(progress, true))
      {
	_error->Error(_("Couldn't rebuild package cache"));
	return failure;
      }
  }

  bool need_forget_new = 
    aptcfg->FindB(PACKAGE "::Forget-New-On-Update", false);

  bool need_autoclean =
    aptcfg->FindB(PACKAGE "::AutoClean-After-Update", false);

  if(need_forget_new || need_autoclean)
    apt_load_cache(&progress, true);

  if(apt_cache_file != NULL && need_forget_new)
    {
      (*apt_cache_file)->forget_new(NULL);
      post_forget_new_hook();
    }

  if(apt_cache_file != NULL && need_autoclean)
    {
      pre_autoclean_hook();

      my_cleaner cleaner;
      cleaner.Go(aptcfg->FindDir("Dir::Cache::archives"), *apt_cache_file);
      cleaner.Go(aptcfg->FindDir("Dir::Cache::archives")+"partial/",
		 *apt_cache_file);

      post_autoclean_hook();
    }

  return success;
}

