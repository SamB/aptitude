// dpkg_terminal.cc
//
//  Copyright 2008-2009 Daniel Burrows
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; see the file COPYING.  If not, write to
//  the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//  Boston, MA 02111-1307, USA.

#include "dpkg_terminal.h"

#include <vte/vte.h>
#include <vte/reaper.h>
#include <glib-object.h>

#include <generic/util/temp.h>

#include <cwidget/generic/util/ssprintf.h>

#include <sys/socket.h>
#include <sys/types.h>
#include <sys/un.h> // Fox UNIX-domain sockets.

#include <termios.h>

#include <apt-pkg/error.h>

#include <aptitude.h>
#include <loggers.h>

#include <generic/apt/apt.h>
#include <generic/apt/config_signal.h>

#include "gui.h"

namespace cw = cwidget;

using log4cxx::LoggerPtr;
using aptitude::Loggers;

namespace gui
{
  namespace
  {
    // In some theoretical sense, it might be worthwhile to create a
    // thread to monitor the dpkg socket.  In practical terms, though,
    // I expect that it'll be perfectly acceptable to handle dpkg
    // status messages from the main loop.
    //
    // The main reason for this class is that we need to store the
    // dpkg status fd parser somewhere.  It also makes me feel a
    // little better about copying slots around.
    //
    // This is a self-deleting class: it kills itself when it hits
    // EOF on the socket.
    class dpkg_socket_data_processor : public sigc::trackable
    {
      int fd;
      safe_slot1<void, aptitude::apt::dpkg_status_message> report_message;
      aptitude::apt::dpkg_status_parser parser;
      log4cxx::LoggerPtr logger;

      static void delete_socket_data_processor(log4cxx::LoggerPtr logger,
					       dpkg_socket_data_processor *processor)
      {
	LOG_TRACE(logger, "Deleting dpkg socket data processor " << processor << ".");
	delete processor;
      }

    public:
      dpkg_socket_data_processor(int _fd,
				 const safe_slot1<void, aptitude::apt::dpkg_status_message> &_report_message)
	: fd(_fd), report_message(_report_message),
	  logger(Loggers::getAptitudeDpkgStatusPipe())
      {
      }

      bool process_data_from_dpkg_socket(Glib::IOCondition condition)
      {
	LOG_TRACE(logger, "Got dpkg socket event: condition " << condition);

	bool rval = false;

	if(condition & Glib::IO_IN)
	  {
	    LOG_TRACE(logger, "Reading data from the dpkg socket.");
	    char buf[1024];
	    const int buf_len = sizeof(buf);
	    int amt;
	    bool read_anything = false;
	    do
	      {
		amt = recv(fd, buf, buf_len, MSG_DONTWAIT);

		// TODO: I should escape all the socket data.
		LOG_DEBUG(logger, "Read data from the dpkg socket: \"" << std::string(buf, amt) << "\".");

		parser.process_input(buf, amt);

		if(aptcfg->FindB("Debug::Aptitude::Dpkg-Status-Fd", false))
		  write(1, buf, amt);

		while(parser.has_pending_message())
		  {
		    aptitude::apt::dpkg_status_message
		      msg(parser.pop_message());
		    LOG_TRACE(logger, "Parsed dpkg message: " << msg << ".");
		    report_message.get_slot()(msg);
		  }

		if(amt > 0)
		  read_anything = true;
	      }
	    while(amt > 0);

	    if(read_anything)
	      LOG_TRACE(logger, "No data received from the dpkg socket, assuming the process exited.");
	    else
	      LOG_TRACE(logger, "Done reading data from the dpkg socket.");

	    rval = read_anything;
	  }
	else if( (condition & Glib::IO_NVAL) ||
		 (condition & Glib::IO_ERR) ||
		 (condition & Glib::IO_HUP) )
	  {
	    LOG_TRACE(logger, "The socket seems to have closed, exiting.");
	    rval = false;
	  }
	else
	  {
	    LOG_ERROR(logger, "Unexpected IO condition " << condition);
	    _error->Warning("Unexpected IO condition %d", condition);
	    rval = false;
	  }

	if(!rval)
	  // We could probably just "delete this" right here, but I feel
	  // a little nervous about deleting something that's actively
	  // involved in signal delivery.
	  {
	    LOG_DEBUG(logger, "Scheduling this socket data processor (" << this << ") for deletion.");
	    sigc::slot0<void> delete_this =
	      sigc::bind(sigc::ptr_fun(&dpkg_socket_data_processor::delete_socket_data_processor),
			 logger, this);
	    post_event(make_safe_slot(delete_this));
	  }

	return rval;
      }
    };

    // Closure structure to connect up the child-exited signal.
    struct child_exited_info
    {
      // The PID of the child to monitor.
      pid_t pid;

      // The continuation of the child.
      safe_slot1<void, pkgPackageManager::OrderResult> k;

      LoggerPtr logger;

      gulong handler_id;

      child_exited_info(pid_t _pid,
			const safe_slot1<void, pkgPackageManager::OrderResult> &_k)
	: pid(_pid),
	  k(_k),
	  logger(Loggers::getAptitudeDpkgTerminal())
      {
      }
    };

    void destroy_child_exited_info(gpointer data,
				   GClosure *closure)
    {
      LOG_TRACE(((child_exited_info *)data)->logger,
		"Destroying the child exited signal associated with " << data << ".");
      delete (child_exited_info *)data;
    }

    void handle_dpkg_result(VteReaper *vtereaper,
			    gint pid,
			    gint status,
			    gpointer user_data)
    {
      child_exited_info *info = (child_exited_info *)user_data;
      LOG_DEBUG(info->logger,
		"PID " << pid << " exited, status " << status << " (" << user_data << ").");

      if(info->pid != pid)
	{
	  LOG_TRACE(info->logger,
		    "Wrong PID, not announcing that dpkg finished.");
	  return;
	}

      pkgPackageManager::OrderResult result = pkgPackageManager::Failed;
      if(WIFEXITED(status))
	result = (pkgPackageManager::OrderResult) WEXITSTATUS(status);


      LOG_TRACE(info->logger,
		"dpkg result is " << result << ".");

      info->k.get_slot()(pkgPackageManager::Failed);

      g_signal_handler_disconnect(vte_reaper_get(),
				  info->handler_id);
    }

    // Connect to the child-exited signal so we know when dpkg exits.
    // We can't use a sigc++ connection because there's no C++ wrapper
    // for VTE, and just using an old-school GTK+ binding is easier
    // than writing a C++ wrapper.
    void connect_dpkg_result(pid_t pid,
			     safe_slot1<void, pkgPackageManager::OrderResult> k)
    {
      child_exited_info *info = new child_exited_info(pid, k);

      LOG_TRACE(info->logger, "Setting up callback to listen for the exit status of process "
		<< pid << " (" << info << ")");

      // We use implicit locking here (plus the fact that we are
      // running in the foreground thread) to know that the signal
      // won't be triggered before handler_id is set.
      info->handler_id =
	g_signal_connect_data(vte_reaper_get(),
			      "child-exited",
			      G_CALLBACK(&handle_dpkg_result),
			      info,
			      &destroy_child_exited_info,
			      (GConnectFlags)0);
    }
  }

  DpkgTerminal::DpkgTerminal()
    : sent_finished_signal(false),
      logger(Loggers::getAptitudeDpkgTerminal()),
      logger_backgrounding(Loggers::getAptitudeDpkgTerminalBackgrounding())
  {
    LOG_TRACE(logger, "Creating the dpkg terminal manager.");

    GtkWidget *vte = vte_terminal_new();
    terminal = (Glib::wrap(vte, false));
  }

  DpkgTerminal::~DpkgTerminal()
  {
    LOG_TRACE(logger, "Destroying the dpkg terminal manager (" << this << ").");
    delete terminal;
  }

  bool DpkgTerminal::handle_suspend_resume_event(Glib::IOCondition condition)
  {
    LOG_TRACE(logger_backgrounding,
	      "Received IO condition " << condition << " on the control socket.");

    bool rval = false;

    if(condition & Glib::IO_IN)
      {
	bool state;

	int amt;
	bool read_anything = false;
	do
	  {
	    char buf;
	    amt = recv(subprocess_map_signal_fd, &buf, 1, MSG_DONTWAIT);
	    if(amt > 0)
	      {
		state = (buf != 0);
		read_anything = true;

		LOG_DEBUG(logger_backgrounding,
			  "Read status from control socket: dpkg is "
			  << (state ? "running" : "suspended"));
	      }
	  }
	while(amt > 0);

	if(read_anything)
	  subprocess_running_changed(state);

	if(!read_anything)
	  LOG_TRACE(logger_backgrounding,
		    "No data on the control socket, shutting down the listener.");

	rval = read_anything;
      }
    else if( (condition & Glib::IO_NVAL) ||
	     (condition & Glib::IO_ERR) ||
	     (condition & Glib::IO_HUP) )
      {
	LOG_TRACE(logger_backgrounding,
		  "Control socket shut down, shutting down the listener.");
	rval = false;
      }
    else
      {
	LOG_TRACE(logger_backgrounding,
		  "Unexpected IO condition " << condition << " on control socket, shutting down the listener.");
	_error->Warning("Unexpected IO condition %d", condition);
	rval = false;
      }

    return rval;
  }

  /** \brief Exception related to the temporary socket class. */
  class TemporarySocketFail : public cwidget::util::Exception
  {
    const std::string msg;
  public:
    TemporarySocketFail(const std::string &_msg) : msg(_msg)
    {
    }

    std::string errmsg() const
    {
      return msg;
    }
  };

  int open_unix_socket()
  {
    int sock = socket(AF_UNIX, SOCK_STREAM, 0);
    if(sock == -1)
      {
	int errnum = errno;
	std::string err = cw::util::sstrerror(errnum);
	std::string msg = cw::util::ssprintf(_("%s: Unable to create a Unix-domain socket: %s"),
					     "open_unix_socket",
					     err.c_str());
	LOG_FATAL(Loggers::getAptitudeDpkgTerminal(), msg);
	throw TemporarySocketFail(msg);
      }

    return sock;
  }

  /** \brief Represents the read end of a pair of Unix-domain sockets. */
  class temporary_listen_socket
  {
    temp::name name;

    std::auto_ptr<FileFd> listen_sock;
    struct sockaddr_un addr;

  public:
    temporary_listen_socket(const temp::name &_name,
			    int backlog)
      : name(_name)
    {
      using namespace cwidget::util;
      int fd = open_unix_socket();

      LOG_TRACE(Loggers::getAptitudeDpkgTerminal(),
		"Listening on the temporary socket \"" << name.get_name() << "\".");

      listen_sock = std::auto_ptr<FileFd>(new FileFd(fd));

      const size_t max_socket_name = sizeof(addr.sun_path);

      if(name.get_name().size() > max_socket_name)
	{
	  LOG_FATAL(Loggers::getAptitudeDpkgTerminal(), "Internal error: the temporary socket name \"" << name.get_name() << " is too long!");
	  throw TemporarySocketFail(ssprintf(_("Internal error: the temporary socket name \"%s\" is too long!"),
					     name.get_name().c_str()));
	}

      addr.sun_family = AF_UNIX;
      strncpy(addr.sun_path, name.get_name().c_str(), max_socket_name);

      if(bind(listen_sock->Fd(), (struct sockaddr *)&addr, sizeof(addr)) != 0)
	{
	  int errnum = errno;
	  std::string err = cw::util::sstrerror(errnum);
	  LOG_FATAL(Loggers::getAptitudeDpkgTerminal(),
		    __PRETTY_FUNCTION__ << ": unable to bind to the temporary socket: " << err);
	  throw TemporarySocketFail(ssprintf("%s: Unable to bind to the temporary socket: %s",
					     __PRETTY_FUNCTION__,
					     err.c_str()));
	}

      if(listen(listen_sock->Fd(), backlog) != 0)
	{
	  int errnum = errno;
	  std::string err = cw::util::sstrerror(errnum);
	  LOG_FATAL(Loggers::getAptitudeDpkgTerminal(),
		    __PRETTY_FUNCTION__ << ": unable to listen on the temporary socket: " << err);
	  throw TemporarySocketFail(ssprintf("%s: Unable to listen on the temporary socket: %s",
					     __PRETTY_FUNCTION__,
					     err.c_str()));
	}
    }

    int accept() const
    {
      LOG_TRACE(Loggers::getAptitudeDpkgTerminal(),
		"Accepting a connection to \"" << name.get_name() << "\"");
      int result = ::accept(listen_sock->Fd(), NULL, NULL);
      if(result == -1)
	{
	  int errnum = errno;
	  std::string errmsg = cw::util::sstrerror(errnum);
	  LOG_FATAL(Loggers::getAptitudeDpkgTerminal(),
		    __PRETTY_FUNCTION__ << ": unable to accept a connection: " << errmsg);
	  throw TemporarySocketFail(cw::util::ssprintf(_("%s: Unable to accept a connection: %s"),
						       __PRETTY_FUNCTION__,
						       errmsg.c_str()));
	}

      LOG_DEBUG(Loggers::getAptitudeDpkgTerminal(),
		"Accepted connection to the temporary socket \"" << name.get_name() << "\" as fd " << result << ".");

      return result;
    }

    const sockaddr_un &get_addr() const { return addr; }
  };


  class temporary_client_socket
  {
    temp::name name;

    struct sockaddr_un addr;

    std::auto_ptr<FileFd> fd;

  public:
    temporary_client_socket(const temp::name &_name)
      : name(_name)
    {
      LOG_TRACE(Loggers::getAptitudeDpkgTerminal(),
		"Connecting to the temporary socket \"" << name.get_name() << "\".");

      const size_t max_socket_name = sizeof(addr.sun_path);

      if(name.get_name().size() > max_socket_name)
	{
	  LOG_FATAL(Loggers::getAptitudeDpkgTerminal(),
		    "Internal error: the temporary socket name \""
		    << name.get_name() << "\" is too long!");
	  throw TemporarySocketFail(cw::util::ssprintf(_("Internal error: the temporary socket name \"%s\" is too long!"),
						       name.get_name().c_str()));
	}

      addr.sun_family = AF_UNIX;
      strncpy(addr.sun_path, name.get_name().c_str(), max_socket_name);

      fd = std::auto_ptr<FileFd>(new FileFd(open_unix_socket()));


      if(connect(fd->Fd(), (struct sockaddr *)&addr, sizeof(addr)) != 0)
	{
	  int errnum = errno;
	  std::string err = cw::util::sstrerror(errnum);
	  LOG_FATAL(Loggers::getAptitudeDpkgTerminal(),
		    __PRETTY_FUNCTION__ << ": Unable to bind to the temporary socket: " << err);
	  throw TemporarySocketFail(cw::util::ssprintf("%s: Unable to bind to the temporary socket: %s",
						       __PRETTY_FUNCTION__,
						       err.c_str()));
	}

      LOG_DEBUG(Loggers::getAptitudeDpkgTerminal(),
		"Connected to the temporary socket \"" << name.get_name() << "\" as fd " << fd->Fd() << ".");
    }

    int get_fd() const { return fd->Fd(); }
  };

  namespace
  {
    // Global variable storing the file descriptor used for control
    // input from / output to the parent.  This is safe because it's
    // only used in the child process, when there's just one thread
    // running; we need a global variable because it's accessed by a
    // signal handler.
    int child_process_to_parent_control_fd = -1;
    // Same thing, storing the pid of the child that will be suspended
    // and continued.
    pid_t child_process_pid = -1;

    // Self-pipe, used to indicate that the child process finished.
    // We write to it only once, when the child exits, and we write
    // the exit status to it.
    //
    // This is used to break out of the main wait loop in the shell
    // process.
    int child_process_to_self_control_fd_write = -1;

    void handle_child_process_sigchld(int signal,
				      siginfo_t *info,
				      void *ucontext)
    {
      if(signal != SIGCHLD)
	return;

      if(info->si_pid != child_process_pid)
	return;

      int status;
      int result = waitpid(child_process_pid, &status, WUNTRACED | WNOHANG);

      if(result == -1)
	// ... Can't safely log b/c I don't know if log4cxx is
	// reentrant (probably not), so just do nothing.  (anyway,
	// why did I get a SIGCHLD if there are no children?)
	;
      else if(result == 0)
	// ... no child has a status change, but I got a SIGCHLD?
	;
      else if(result == child_process_pid)
	{
	  if(WIFEXITED(status) || WIFSIGNALED(status))
	    {
	      child_process_pid = -1;
	      if(write(child_process_to_self_control_fd_write, &status, sizeof(status)) < (int)sizeof(status))
		{
		  // Something is very very wrong.  Make a last ditch
		  // effort to tell the world about our troubles.
		  const char *msg =
		    "aptitude: error writing completion status to control fd, aborting.\n";
		  const int msglen = strlen(msg);
		  if(write(2, msg, msglen) < msglen)
		    write(1, msg, msglen);
		  _exit(-1);
		}
	    }
	  else if(WIFSTOPPED(status))
	    {
	      unsigned char c = 0;
	      if(write(child_process_to_parent_control_fd, &c, 1) < 1)
		{
		  // Something is very very wrong.
		  const char *msg = "aptitude: error informing parent that the child has stopped.\n";
		  const int msglen = strlen(msg);

		  if(write(2, msg, msglen) < msglen)
		    write(1, msg, msglen);
		}
	    }
	}
    }

    // SIGHUP is only sent to foreground processes, so we might need
    // to kill dpkg&co off with SIGHUP.
    void handle_shell_process_sighup(int signal)
    {
      if(signal != SIGHUP)
	return;

      if(child_process_pid != -1)
	{
	  if(kill(-child_process_pid, SIGHUP) != 0)
	    {
	      const char *msg = "aptitude: unable to kill the dpkg process.\n";
	      const int msglen = strlen(msg);

	      if(write(2, msg, msglen) < msglen)
		write(1, msg, msglen);
	    }
	}

      // Kill ourselves with SIGHUP too.
      struct sigaction act;
      act.sa_handler = SIG_DFL;
      sigemptyset(&act.sa_mask);
      act.sa_flags = 0;

      sigaction(SIGHUP, &act, NULL);
      raise(SIGHUP);
      // And just in case that didn't work,
      _exit(0);
    }

    /** \brief Handles running the "shell" that manages the
     *  suspended/unsuspended state and foreground/background of the
     *  dpkg process.
     *
     *  \return   The return value of the dpkg process.
     */
    pkgPackageManager::OrderResult shell_process()
    {
      LoggerPtr logger(Loggers::getAptitudeDpkgTerminal());
      LoggerPtr loggerBackgrounding(Loggers::getAptitudeDpkgTerminalBackgrounding());

      // Create a pipe to ourselves, used to pass back the result of
      // the dpkg invocation when it exits.

      int selfpipe[2];
      if(pipe(selfpipe) != 0)
	{
	  int errnum = errno;
	  const std::string err(cw::util::sstrerror(errnum));
	  LOG_FATAL(logger, "Can't create result notification: pipe() failed: " << err << ".");
	  _error->Error(_("aptitude: can't create result notification: pipe() failed: %s"),
			err.c_str());
	  _error->DumpErrors();

	  if(child_process_pid != -1)
	    {
	      LOG_TRACE(logger, "Killing the process group of the child process " << child_process_pid << ".");
	      kill(-child_process_pid, SIGINT);
	    }

	  return pkgPackageManager::Failed;
	}

      int child_process_to_self_control_fd_read = selfpipe[0];
      child_process_to_self_control_fd_write = selfpipe[1];

      LOG_DEBUG(logger, "Opened a self-pipe to send child states (read end " << child_process_to_self_control_fd_read << ", write end " << child_process_to_self_control_fd_write << ").");

      // Set up the SIGCHLD signal handler, which handles informing
      // the parent process when the child is suspended and this
      // process when it dies.
      {
	struct sigaction act;
	act.sa_sigaction = &handle_child_process_sigchld;
	sigemptyset(&act.sa_mask);
	act.sa_flags = SA_SIGINFO;
	sigaction(SIGCHLD, &act, NULL);
      }

      // Set up a SIGHUP handler that kills the subprocess.
      {
	struct sigaction act;
	act.sa_handler = &handle_shell_process_sighup;
	sigemptyset(&act.sa_mask);
	act.sa_flags = SA_SIGINFO;
	sigaction(SIGHUP, &act, NULL);
      }

      // Block terminal control signals.
      {
	struct sigaction act;
	act.sa_handler = SIG_IGN;
	sigemptyset(&act.sa_mask);
	act.sa_flags = 0;
	sigaction(SIGTSTP, &act, NULL);
	sigaction(SIGTTIN, &act, NULL);
	sigaction(SIGTTOU, &act, NULL);
      }

      // Wait around for input events on the two control FDs, until
      // the child process terminates.
      while(true)
	{
	  fd_set fds;
	  FD_ZERO(&fds);
	  FD_SET(child_process_to_parent_control_fd, &fds);
	  FD_SET(child_process_to_self_control_fd_read, &fds);

	  const int nfds = std::max(child_process_to_parent_control_fd,
				    child_process_to_self_control_fd_read) + 1;

	  int result = select(nfds, &fds, NULL, NULL, NULL);

	  if(result < 0)
	    {
	      int errnum = errno;
	      if(errnum != EINTR)
		{
		  std::string err(cw::util::sstrerror(errnum));
		  LOG_FATAL(logger, "Can't wait for the dpkg process: select() failed: " << err);
		  _error->Error(_("aptitude: can't wait for the dpkg process: select() failed: %s"),
				err.c_str());
		  return pkgPackageManager::Failed;
		}
	      else
		LOG_TRACE(logger, "select(): interrupted system call (looping).");
	    }
	  else if(result > 0)
	    {
	      if(FD_ISSET(child_process_to_self_control_fd_read, &fds))
		{
		  LOG_TRACE(logger, "Data available on the self-pipe.");

		  int state;
		  int amt;

		  amt = read(child_process_to_self_control_fd_read,
			     &state, sizeof(state));
		  // If something went wrong, assume that dpkg is done
		  // and abort.
		  if(amt < 0)
		    {
		      int errnum = errno;
		      std::string err(cw::util::sstrerror(errnum));
		      LOG_FATAL(logger, "Can't check the dpkg process state: read() failed: " << err);
		      _error->Error(_("aptitude: can't check the dpkg process state: read() failed: %s"),
				    err.c_str());
		      return pkgPackageManager::Failed;
		    }
		  else if(amt != sizeof(state))
		    {
		      // This should never happen.
		      LOG_FATAL(logger, "Can't check the dpkg process state: read() returned too few bytes.");
		      _error->Error(_("aptitude: can't check the dpkg process state: read() returned too few bytes"));
		      return pkgPackageManager::Failed;
		    }
		  else
		    {
		      if(WIFEXITED(state))
			{
			  LOG_DEBUG(logger, "Child exited with status " << WEXITSTATUS(state));
			  return (pkgPackageManager::OrderResult)WEXITSTATUS(state);
			}
		      else if(!WIFSIGNALED(state))
			{
			  LOG_FATAL(logger, "Unknown child state " << state);
			  // This should never happen.
			  _error->Error(_("aptitude: unexpected dpkg process exit status %d"),
					state);
			  return pkgPackageManager::Failed;
			}
		      else
			{
			  LOG_FATAL(logger, "Child terminated by signal " << WTERMSIG(state));
			  _error->Error(_("aptitude: dpkg process terminated by signal %d"),
					WTERMSIG(state));
			  return pkgPackageManager::Failed;
			}
		    }
		}
	      // Done handling input from the dpkg process status fd.

	      if(FD_ISSET(child_process_to_parent_control_fd, &fds))
		{
		  LOG_TRACE(loggerBackgrounding, "Data available on the parent's control socket.");
		  bool read_anything = false;
		  bool foreground;
		  int amt;
		  // Read the current state.
		  do
		    {
		      char buf;
		      amt = recv(child_process_to_parent_control_fd, &buf, 1, MSG_DONTWAIT);
		      if(amt > 0)
			{
			  foreground = (buf != 0);
			  LOG_DEBUG(loggerBackgrounding, "Request: place dpkg in the " << (foreground ? "foreground" : "background"));
			  read_anything = true;
			}
		    }
		  while(amt > 0);

		  if(read_anything)
		    {
		      sigset_t sigchld;
		      sigemptyset(&sigchld);
		      sigaddset(&sigchld, SIGCHLD);

		      // Block SIGCHLD while we're fiddling with the
		      // child's state, just to be safe.
		      sigprocmask(SIG_BLOCK, &sigchld, NULL);

		      if(foreground)
			{
			  if(child_process_pid != -1)
			    {
			      LOG_TRACE(loggerBackgrounding, "Bringing dpkg into the foreground.");
			      // Give the dpkg process access to the
			      // terminal and wake it up.
			      int result = tcsetpgrp(0, child_process_pid);
			      if(result != 0)
				{
				  int errnum = errno;
				  std::string err(cw::util::sstrerror(errnum));
				  LOG_FATAL(loggerBackgrounding, "Can't put the dpkg process in the foreground: tcsetpgrp() failed: " << err);
				  _error->Error(_("aptitude: can't put the dpkg process in the foreground: tcsetpgrp() failed: %s"),
						err.c_str());
				  _error->DumpErrors();
				}
			      // Tell the parent that we are going to
			      // continue the process.  This needs to
			      // be sent before we actually send
			      // SIGCONT, or if the subprocess
			      // resuspends, its state change might
			      // get sent before this state change.
			      unsigned char c = 1;
			      write(child_process_to_parent_control_fd, &c, 1);
			      result = kill(child_process_pid, SIGCONT);
			      if(result != 0)
				{
				  int errnum = errno;
				  std::string err(cw::util::sstrerror(errnum));
				  LOG_FATAL(loggerBackgrounding, "Can't wake up the dpkg process: " << err);
				  _error->Error(_("aptitude: can't wake up the dpkg process: %s"),
						cw::util::sstrerror(errnum).c_str());
				  _error->DumpErrors();

				  c = 0;
				  write(child_process_to_parent_control_fd, &c, 1);
				}
			    }
			  else
			    LOG_WARN(loggerBackgrounding, "Can't bring dpkg into the foreground (no active process).");
			}
		      else
			{
			  // Stuff the dpkg process into the background.
			  if(child_process_pid != -1)
			    {
			      LOG_TRACE(loggerBackgrounding, "Placing dpkg into the background.");
			      // Take control of the terminal ourselves.
			      int result = tcsetpgrp(0, getpid());
			      if(result != 0)
				{
				  int errnum = errno;
				  std::string err(cw::util::sstrerror(errnum));
				  LOG_FATAL(loggerBackgrounding, "Can't place dpkg into the background: tcsetpgrp() failed: " << err);
				  _error->Error(_("aptitude: can't put the dpkg process in the background: tcsetpgrp() failed: %s"),
						err.c_str());
				  _error->DumpErrors();
				}
			    }
			  else
			    LOG_WARN(loggerBackgrounding, "Can't place dpkg into the background (no active process).");
			}

		      sigprocmask(SIG_UNBLOCK, &sigchld, NULL);
		    }
		}
	      // Done handling input from the parent process
	      // suspend/resume fd.
	    }
	}
    }
  }

  void DpkgTerminal::child_process(const temp::name &dpkg_socket_name,
				   const safe_slot1<pkgPackageManager::OrderResult, int> &f)
  {
    // The child process.  It passes status information to the
    // parent process and uses its *return code* to indicate the
    // success / failure state.

    LOG_INFO(logger, "Child process starting.");


    // We have to do this before tcsetattr(), because tcsetattr()
    // requires the terminal (that's the parent process) to respond,
    // but that process is blocked in accept() until we open the side
    // channels.
    //
    // This is all somewhat fragile; I wish Gnome bug #320128 was
    // fixed so that I could just make some internal pipes. :-(
    LOG_TRACE(logger, "Opening sockets to parent process.");

    std::auto_ptr<temporary_client_socket> dpkg_sock;
    std::auto_ptr<temporary_client_socket> control_sock;
    try
      {
	// Open two sockets, one for dpkg status messages and one for
	// intra-program control messages.
	dpkg_sock = std::auto_ptr<temporary_client_socket>(new temporary_client_socket(dpkg_socket_name));
	control_sock = std::auto_ptr<temporary_client_socket>(new temporary_client_socket(dpkg_socket_name));
      }
    catch(TemporarySocketFail &ex)
      {
	LOG_FATAL(logger, "Unable to open sockets: " << ex.errmsg());
	_error->Error("%s", ex.errmsg().c_str());
	_error->DumpErrors();
	_exit(pkgPackageManager::Failed);
      }

    child_process_to_parent_control_fd = control_sock->get_fd();


    LOG_TRACE(logger, "Disabling TOSTOP.");
    // Try to disable TOSTOP (so that output never suspends a
    // backgrounded process).
    {
      struct termios current_settings;
      if(tcgetattr(0, &current_settings) < 0)
	{
	  int errnum = errno;
	  std::string err(cw::util::sstrerror(errnum));
	  LOG_ERROR(logger_backgrounding, "Can't read the current terminal settings: tcgetattr() failed: " << err);
	  _error->Error(_("aptitude: can't read the current terminal settings: tcgetattr() failed: %s"),
			err.c_str());
	}
      else
	{
	  current_settings.c_lflag &= ~(TOSTOP);
	  if(tcsetattr(0, TCSADRAIN, &current_settings) < 0)
	    {
	      int errnum = errno;
	      std::string err(cw::util::sstrerror(errnum));
	      LOG_ERROR(logger_backgrounding, "Can't modify the terminal settings: tcsetattr() failed: " << err);
	      _error->Error(_("aptitude: can't modify the terminal settings: tcsetattr() failed: %s"),
			    err.c_str());
	    }
	  else
	    {
	      if(tcgetattr(0, &current_settings) < 0)
		{
		  int errnum = errno;
		  std::string err(cw::util::sstrerror(errnum));
		  LOG_ERROR(logger_backgrounding, "Can't read back the terminal settings: tcgetattr() failed: " << err);
		  _error->Error(_("aptitude: can't read back the terminal settings: tcgetattr() failed: %s"),
				cw::util::sstrerror(errnum).c_str());
		}
	      else if((current_settings.c_lflag & (TOSTOP)) == (TOSTOP))
		{
		  LOG_ERROR(logger_backgrounding, "TOSTOP is set even though I just cleared it!");
		  _error->Error("%s",
				_("aptitude: TOSTOP is set even though I just cleared it!"));
		}
	    }
	}
    }

    char timebuf[512] = "";

    const time_t start_time = time(0);
    struct tm start_local_time;
    localtime_r(&start_time, &start_local_time);
    if(strftime(timebuf, sizeof(timebuf), "%c", &start_local_time) == 0)
      strcpy(timebuf, "ERR");
    printf(_("[%s] dpkg process starting...\n"), timebuf);

    _error->DumpErrors();

    // To handle job control (so that we can detect when the child
    // needs terminal access), we fork into a "shell process" and a
    // "dpkg process".  The "dpkg process" actually runs dpkg; its
    // exit status indicates whether it was successful.  Both
    // processes set the dpkg process up as the head of a new process
    // group (the reason for this is to ensure that it ends up in that
    // group before either process starts to do stuff with it).
    //
    // The dpkg process is started in the background; it is moved into
    // the foreground or the background based on the commands received
    // from the parent (GUI) process.

    pkgPackageManager::OrderResult result;

    child_process_pid = fork();
    switch(child_process_pid)
      {
      case -1:
	{
	  int errnum = errno;
	  std::string err(cw::util::sstrerror(errnum));
	  LOG_FATAL(logger, "failed to invoke dpkg: fork() failed: " << err);
	  _error->Error(_("aptitude: failed to invoke dpkg: fork() failed: %s"),
			err.c_str());
	  result = pkgPackageManager::Failed;
	  break;
	}

      case 0:
	{
	  struct termios current_settings;
	  if(tcgetattr(0, &current_settings) < 0)
	    {
	      int errnum = errno;
	      std::string err(cw::util::sstrerror(errnum));
	      LOG_ERROR(logger, "Can't read back the terminal settings: tcgetattr() failed: " << err);
	      _error->Error(_("aptitude: can't read back the terminal settings: tcgetattr() failed: %s"),
			    err.c_str());
	    }
	  else if((current_settings.c_lflag & (TOSTOP)) == (TOSTOP))
	    {
	      LOG_ERROR(logger, "TOSTOP is set even though I just cleared it!");
	      _error->Error("%s",
			    _("aptitude: TOSTOP is set even though I just cleared it!"));
	    }

	  // The dpkg process: set up the process group and run dpkg,
	  // then exit with its return value.
	  if(setpgid(getpid(), getpid()) < 0)
	    {
	      int errnum = errno;
	      std::string err(cw::util::sstrerror(errnum));
	      LOG_ERROR(logger, "Failed to set the process group for dpkg: setpgid() failed: " << err);
	      _error->Error(_("aptitude: failed to set the process group for dpkg: setpgid() failed: %s"),
			    err.c_str());
	      _error->DumpErrors();
	      // Continue anyway.
	    }

	  LOG4CXX_TRACE(logger, "Running dpkg.");

	  pkgPackageManager::OrderResult result =
	    f.get_slot()(dpkg_sock->get_fd());

	  LOG4CXX_TRACE(logger, "Finished running dpkg, result is " << result << ".");

	  _exit(result);
	}

      default:
	LOG_INFO(logger, "The dpkg process is PID " << child_process_pid << ".");
	setpgid(child_process_pid, child_process_pid);
	result = shell_process();
      }

    LOG4CXX_TRACE(logger, "Subprocess finished, result is " << result << ".");

    // Make sure errors appear somewhere (we really ought to push
    // them down the FIFO).
    _error->DumpErrors();

    const time_t end_time = time(0);
    struct tm end_local_time;
    localtime_r(&end_time, &end_local_time);
    if(strftime(timebuf, sizeof(timebuf), "%c", &end_local_time) == 0)
      strcpy(timebuf, "ERR");

    switch(result)
      {
      case pkgPackageManager::Completed:
	printf(_("[%s] dpkg process complete.\n"), timebuf);
	break;
      case pkgPackageManager::Failed:
	printf(_("[%s] dpkg process failed.\n"), timebuf);
	break;
      case pkgPackageManager::Incomplete:
	printf(_("[%s] dpkg process complete; there are more packages left to process.\n"),
	       timebuf);
	break;
      default:
	printf("[%s] dpkg process complete; internal error: bad result code (%d).\n",
	       timebuf, result);
	break;
      }

    _exit(result);
  }

  void DpkgTerminal::run(const safe_slot1<pkgPackageManager::OrderResult, int> &f)
  {
    // Create a temporary UNIX-domain socket to pass status
    // information to the parent.
    temp::dir tempdir("aptitude");
    temp::name dpkg_socket_name(tempdir, "commsocket");

    // To avoid races, we bind the receive end of the socket first and
    // start accepting connections.
    std::auto_ptr<temporary_listen_socket> listen_sock;

    try
      {
	listen_sock = std::auto_ptr<temporary_listen_socket>(new temporary_listen_socket(dpkg_socket_name, 1));
      }
    catch(TemporarySocketFail &ex)
      {
	LOG_FATAL(logger, "Failed to create a listen socket for the temporary control socket: " << ex.errmsg());
	_error->Error("%s", ex.errmsg().c_str());
	finished(pkgPackageManager::Failed);
	return;
      }

    GtkWidget *vte = terminal->gobj();
    pid_t pid = vte_terminal_forkpty(VTE_TERMINAL(vte),
				     NULL, NULL,
				     FALSE, FALSE, FALSE);

    if(pid == 0)
      child_process(dpkg_socket_name, f);
    else
      {
	LOG_INFO(logger, "The shell process is PID " << pid << ".");

	int dpkg_sock = -1;
	subprocess_map_signal_fd = -1;

	try
	  {
	    dpkg_sock = listen_sock->accept();
	    subprocess_map_signal_fd = listen_sock->accept();
	  }
	catch(TemporarySocketFail &ex)
	  {
	    LOG_ERROR(logger, "Failed to establish control sockets: " << ex.errmsg());
	    _error->Error("%s", ex.errmsg().c_str());
	    // Not a fatal error -- try to install anyway.
	  }

	// Catch status output from the install process.
	sigc::slot1<void, aptitude::apt::dpkg_status_message>
	  report_message_slot(status_message.make_slot());
	dpkg_socket_data_processor *data_processor = new dpkg_socket_data_processor(dpkg_sock, make_safe_slot(report_message_slot));
	if(dpkg_sock != -1)
	  Glib::signal_io().connect(sigc::mem_fun(*data_processor, &dpkg_socket_data_processor::process_data_from_dpkg_socket),
				    dpkg_sock,
				    Glib::IO_IN | Glib::IO_ERR | Glib::IO_HUP | Glib::IO_NVAL);
	if(subprocess_map_signal_fd != -1)
	  Glib::signal_io().connect(sigc::mem_fun(*this, &DpkgTerminal::handle_suspend_resume_event),
				    subprocess_map_signal_fd,
				    Glib::IO_IN | Glib::IO_ERR | Glib::IO_HUP | Glib::IO_NVAL);

	// The parent process.  Here we just wait for the reaper to
	// tell us that the child finished, then return the result.
	// We use implicit locking here to avoid a race condition that
	// could occur: we know that the reaper won't fire before we
	// can connect to it because its signal executions go through
	// the main loop, and this function call is blocking the main
	// loop.
	sigc::slot1<void, pkgPackageManager::OrderResult> finished_slot =
	  finished.make_slot();
	connect_dpkg_result(pid, make_safe_slot(finished_slot));

	vte_reaper_add_child(pid);
      }
  }

  void DpkgTerminal::set_foreground(bool foreground)
  {
    LOG_DEBUG(logger_backgrounding,
	      "Placing the dpkg process in the " << (foreground ? "foreground" : "background") << ".");

    unsigned char byte = foreground ? 1 : 0;
    int result = write(subprocess_map_signal_fd, &byte, 1);
    if(result < 1)
      {
	int errnum = errno;

	if(result < 0)
	  {
	    std::string err(cw::util::sstrerror(errnum));
	    LOG4CXX_ERROR(logger_backgrounding,
			  "Unable to send foreground state to subprocess: " << err);
	    _error->Error(_("Unable to send foreground state to subprocess: %s"),
			  cw::util::sstrerror(errnum).c_str());
	  }
	else
	  {
	    LOG4CXX_ERROR(logger_backgrounding,
			  "Unable to send foreground state to subprocess: no bytes written");
	    _error->Error(_("Unable to send foreground state to subprocess (no bytes written)."));
	  }
      }
  }

  void DpkgTerminal::inject_yes()
  {
    LOG4CXX_TRACE(Loggers::getAptitudeDpkgTerminal(), "Sending 'y' to the dpkg process.");

    VteTerminal *vte = VTE_TERMINAL(terminal->gobj());

    vte_terminal_feed_child(vte, "y\n", 2);
  }

  void DpkgTerminal::inject_no()
  {
    LOG4CXX_TRACE(Loggers::getAptitudeDpkgTerminal(), "Sending 'n' to the dpkg process.");

    VteTerminal *vte = VTE_TERMINAL(terminal->gobj());

    vte_terminal_feed_child(vte, "n\n", 2);
  }
}
