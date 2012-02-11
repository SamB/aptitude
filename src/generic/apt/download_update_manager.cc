// download_update_manager.cc
//
//   Copyright (C) 2005, 2007-2009, 2011 Daniel Burrows
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

#include <apt-pkg/acquire-item.h>
#include <apt-pkg/cachefile.h>
#include <apt-pkg/clean.h>
#include <apt-pkg/error.h>
#include <apt-pkg/algorithms.h>

#include <cwidget/generic/util/exception.h>
#include <cwidget/generic/util/ssprintf.h>

#include <errno.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/wait.h>

namespace cw = cwidget;

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

  stat = &acqlog;

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

  return true;
}

pkgAcquire::RunResult download_update_manager::do_download()
{
  if(ListUpdate(*stat, src_list) == false)
    return pkgAcquire::Failed;

  return pkgAcquire::Continue;
}

pkgAcquire::RunResult download_update_manager::do_download(int PulseInterval)
{
  if(ListUpdate(*stat, src_list, PulseInterval) == false)
    return pkgAcquire::Failed;

  return pkgAcquire::Continue;
}

// TODO: this should be lifted to generic code.
namespace
{
  std::string parse_quoted_string(char quote_character,
				  const std::string &whole_string,
				  std::string::const_iterator &begin,
				  std::string::const_iterator end)
  {
    std::string rval;

    while(begin != end)
      {
	if(*begin == quote_character)
	  {
	    ++begin;
	    return rval;
	  }
	else if(*begin == '\\')
	  {
	    ++begin;
	    if(begin != end)
	      rval += *begin;
	  }
	else
	  rval += *begin;

	++begin;
      }

    _error->Warning(_("Unterminated quoted string in command: %s"),
		    whole_string.c_str());
    return rval;
  }

  /** \brief Parse whitespace-separated arguments from the given
   *  string, handling both single and double quotes.
   *
   *  Each argument in the string is pushed onto the end of the
   *  given vector.
   */
  void parse_command_arguments(const std::string &command_line,
			       std::vector<std::string> &arguments)
  {
    std::string::const_iterator begin = command_line.begin();;
    const std::string::const_iterator end = command_line.end();

    while(begin != end)
      {
	while(begin != end && isspace(*begin))
	  ++begin;

	std::string arg;

	while(begin != end && !isspace(*begin))
	  {
	    if(*begin == '"' || *begin == '\'')
	      {
		++begin;
		arg += parse_quoted_string(*begin, command_line,
					   begin, end);
	      }
	    else
	      {
		arg.push_back(*begin);
		++begin;
	      }
	  }

	arguments.push_back(arg);
      }
  }

  /** \brief An exception indicating that a sub-process could not be run.
   */
  class SubprocessException : public cwidget::util::Exception
  {
    std::string msg;

  public:
    SubprocessException(const std::string &_msg)
      : msg(_msg)
    {
    }

    std::string errmsg() const { return msg; }
  };

  /** \brief Run a subprocess and redirect its output to /dev/null.
   *
   *  $PATH is not searched, and the arguments are passed directly to
   *  the subprocess without shell intervention.  The return value is
   *  the status of the subcommand as it would be returned from
   *  waitpid().
   */
  int run_in_subprocess_to_devnull(const std::string &command,
				   const std::vector<std::string> &args)
  {
    // Merge any new information in the apt cache into the debtags cache
    // by running 'debtags update'.  Be safe here: don't risk running
    // the wrong thing as root by using system() or scanning the PATH.
    int pid = fork();
    if(pid < 0)
      {
	int errnum = errno;

	throw SubprocessException(cw::util::ssprintf(_("fork() failed: %s"),
						     cw::util::sstrerror(errnum).c_str()));
      }
    else if(pid == 0)
      {
	int fdnullin = open("/dev/null", O_RDONLY);
	if(fdnullin < 0)
	  close(0);
	else
	  {
	    dup2(fdnullin, 0);
	    close(fdnullin);
	  }

	int fdnullout = open("/dev/null", O_WRONLY);
	if(fdnullout < 0)
	  {
	    close(1);
	    close(2);
	  }
	else
	  {
	    dup2(fdnullout, 1);
	    dup2(fdnullout, 2);
	    close(fdnullout);
	  }

	char **argv = new char*[args.size() + 1];
	for(std::vector<std::string>::size_type i = 0;
	    i < args.size(); ++i)
	  {
	    argv[i] = const_cast<char *>(args[i].c_str());
	  }
	argv[args.size()] = NULL;

	exit(execv(command.c_str(), argv));
      }
    else
      {
	while(true)
	  {
	    int status = 0;
	    errno = 0;
	    if(waitpid(pid, &status, 0) == pid)
	      return status;
	    else if(errno != EINTR)
	      {
		int errnum = errno;
		throw SubprocessException(cw::util::ssprintf(_("waitpid() failed: %s"),
							     cw::util::sstrerror(errnum).c_str()));
	      }
	  }
      }
  }
}

void download_update_manager::finish(pkgAcquire::RunResult res,
				     OpProgress *progress,
				     const sigc::slot1<void, result> &k)
{
  if(log != NULL)
    log->Complete();

  apt_close_cache();

  if(res != pkgAcquire::Continue)
    {
      k(failure);
      return;
    }

  // Rebuild the apt caches as done in apt-get.  cachefile is scoped
  // so it dies before we possibly-reload the cache.  This will do a
  // little redundant work in visual mode, but avoids lots of
  // redundant work at the command-line.
  {
    pkgCacheFile cachefile;
    if(!cachefile.BuildCaches(progress, true))
      {
	k(failure);
	return;
      }
  }

  bool need_forget_new = 
    aptcfg->FindB(PACKAGE "::Forget-New-On-Update", false);

  bool need_autoclean =
    aptcfg->FindB(PACKAGE "::AutoClean-After-Update", false);

#ifdef HAVE_EPT
  std::string debtags = aptcfg->Find(PACKAGE "::Debtags-Binary", "/usr/bin/debtags");

  if(debtags.size() == 0)
    _error->Error(_("The debtags command must not be an empty string."));
  // Keep the user from killing themselves without trying: a relative
  // path would open a root exploit.
  else if(debtags[0] != '/')
    _error->Error(_("The debtags command must be an absolute path."));
  // Check up-front if we can execute the command.  This is not ideal
  // since there's a race condition (the command could go away before
  // we try to execute it) but the worst that will happen is that we
  // display a confusing error message (...exited with code 255).
  else if(euidaccess(debtags.c_str(), X_OK) != 0)
    {
      int errnum = errno;
      if(errnum == ENOENT)
	// Fail silently instead of annoying the user over and over.
	;  //_error->Warning(_("The debtags command (%s) does not exist; perhaps you need to install the debtags package?"),
                            //debtags.c_str());
      else
	_error->Error(_("The debtags command (%s) cannot be executed: %s"),
		      debtags.c_str(), cw::util::sstrerror(errnum).c_str());
    }
  else
    {
      progress->OverallProgress(0, 0, 1, _("Updating debtags database"));

      std::string debtags_options = aptcfg->Find(PACKAGE "::Debtags-Update-Options", "--local");

      std::vector<std::string> args;
      args.push_back(debtags);
      args.push_back("update");
      parse_command_arguments(debtags_options, args);

      try
	{
	  int status = run_in_subprocess_to_devnull(debtags, args);
	  if(WIFSIGNALED(status))
	    {
	      std::string coredumpstr;
#ifdef WCOREDUMP
	      if(WCOREDUMP(status))
		coredumpstr = std::string(" ") + _("(core dumped)");
#endif
	      // ForTranslators: "%s update %s" gets replaced by a command line, do not translate it!
	      _error->Warning(_("The debtags update process (%s update %s) was killed by signal %d%s."),
			      debtags.c_str(), debtags_options.c_str(), WTERMSIG(status),
			      coredumpstr.c_str());
	    }
	  else if(WIFEXITED(status))
	    {
	      if(WEXITSTATUS(status) != 0)
	      // ForTranslators: "%s update %s" gets replaced by a command line, do not translate it!
		      _error->Warning(_("The debtags update process (%s update %s) exited abnormally (code %d)."),
				debtags.c_str(), debtags_options.c_str(), WEXITSTATUS(status));
	    }
	  else
	    // This should never happen, but if it does something is
	    // wrong.
	      // ForTranslators: "%s update %s" gets replaced by a command line, do not translate it!
	    _error->Warning(_("The debtags update process (%s update %s) exited in an unexpected way (status %d)."),
			    debtags.c_str(), debtags_options.c_str(), status);
	}
      catch(cw::util::Exception &e)
	{
	      // ForTranslators: "%s update %s" gets replaced by a command line, do not translate it!
	  _error->Warning(_("Updating the debtags database (%s update %s) failed (perhaps debtags is not installed?): %s"),
			  debtags.c_str(), debtags_options.c_str(), e.errmsg().c_str());
	}

      progress->Progress(1);
      progress->Done();
    }
#endif

  if(need_forget_new || need_autoclean)
    apt_load_cache(progress, true);

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

  k(success);
  return;
}

