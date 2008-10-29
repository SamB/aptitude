// dpkg_terminal.cc
//
//  Copyright 2008 Daniel Burrows
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

#include <apt-pkg/error.h>

#include <aptitude.h>

#include "gui.h"

namespace cw = cwidget;

namespace gui
{
  namespace
  {
    // In some theoretical sense, it might be worthwhile to create a
    // thread to monitor the dpkg socket.  In practical terms, though,
    // I expect that it'll be perfectly acceptable to handle dpkg
    // status messages from the main loop.
    //
    // \todo actually parse the messages and emit some sort of signal
    // for them.
    bool process_data_from_dpkg_socket(Glib::IOCondition condition,
				       int fd)
    {
      if(condition & Glib::IO_IN)
	{
	  char c;
	  // If we can't read anything, assume we got EOF and stop
	  // listening to this FD.
	  bool read_anything = false;
	  while(recv(fd, &c, 1, MSG_DONTWAIT) > 0)
	    read_anything = true; // Just snarf all the data in a lame way for now.
	  return read_anything;
	}

      else if( (condition & Glib::IO_NVAL) ||
	       (condition & Glib::IO_ERR) ||
	       (condition & Glib::IO_HUP) )
	return false;

      _error->Warning("Unexpected IO condition %d", condition);
      return false;
    }

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
	  _error->Error("%s", msg.c_str());
	}

      return sock;
    }

    // Closure structure to connect up the child-exited signal.
    struct child_exited_info
    {
      // The PID of the child to monitor.
      pid_t pid;

      // The continuation of the child.
      sigc::slot1<void, pkgPackageManager::OrderResult> k;

      gulong handler_id;

      child_exited_info(pid_t _pid,
			const sigc::slot1<void, pkgPackageManager::OrderResult> &_k)
	: pid(_pid),
	  k(_k)
      {
      }
    };

    void destroy_child_exited_info(gpointer data,
				   GClosure *closure)
    {
      delete (child_exited_info *)data;
    }

    void handle_dpkg_result(VteReaper *vtereaper,
			    gint pid,
			    gint status,
			    gpointer user_data)
    {
      child_exited_info *info = (child_exited_info *)user_data;

      if(info->pid != pid)
	return;

      pkgPackageManager::OrderResult result = pkgPackageManager::Failed;
      if(WIFEXITED(status))
	result = (pkgPackageManager::OrderResult) WEXITSTATUS(status);

      info->k(pkgPackageManager::Failed);

      g_signal_handler_disconnect(vte_reaper_get(),
				  info->handler_id);
    }

    void connect_dpkg_result(pid_t pid,
			     sigc::slot1<void, pkgPackageManager::OrderResult> k)
    {
      child_exited_info *info = new child_exited_info(pid, k);
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

    // This should always run from the foreground thread.
    void do_run_dpkg(const sigc::slot1<pkgPackageManager::OrderResult, int> f,
		     const sigc::slot1<void, Gtk::Widget *> register_terminal,
		     const sigc::slot1<void, pkgPackageManager::OrderResult> k)
    {
      GtkWidget *vte = vte_terminal_new();

      Gtk::Widget *w(Glib::wrap(vte, false));

      register_terminal(w);

      // Create a temporary UNIX-domain socket to pass status
      // information to the parent.
      temp::dir tempdir("aptitude");
      temp::name socketname(tempdir, "commsocket");

      // To avoid races, we bind the receive end of the socket first and
      // start accepting connections.
      int listen_sock = open_unix_socket();
      if(listen_sock == -1)
	{
	  k(pkgPackageManager::Failed);
	  return;
	}

      // Ensure that the socket is always closed when this routine
      // exits.
      FileFd sock_fd(listen_sock);

      struct sockaddr_un sa;

      const size_t max_socket_name = sizeof(sa.sun_path);

      if(socketname.get_name().size() > max_socket_name)
	{
	  _error->Error("Internal error: the temporary socket name is too long!");
	  k(pkgPackageManager::Failed);
	  return;
	}

      sa.sun_family = AF_UNIX;
      strncpy(sa.sun_path, socketname.get_name().c_str(), max_socket_name);
      if(bind(listen_sock, (struct sockaddr *)&sa, sizeof(sa)) != 0)
	{
	  int errnum = errno;
	  std::string err = cw::util::sstrerror(errnum);
	  _error->Error("%s: Unable to bind to the temporary socket: %s",
			__PRETTY_FUNCTION__,
			err.c_str());
	  k(pkgPackageManager::Failed);
	  return;
	}

      if(listen(listen_sock, 1) != 0)
	{
	  int errnum = errno;
	  std::string err = cw::util::sstrerror(errnum);
	  _error->Error("%s: Unable to listen on the temporary socket: %s",
			__PRETTY_FUNCTION__,
			err.c_str());
	  k(pkgPackageManager::Failed);
	  return;
	}

      pid_t pid = vte_terminal_forkpty(VTE_TERMINAL(vte),
				       NULL, NULL,
				       FALSE, FALSE, FALSE);

      if(pid == 0)
	{
	  // The child process.  It passes status information to the
	  // parent process and uses its *return code* to indicate the
	  // success / failure state.

	  // NB: we should close the listen side here, but I don't
	  // because of my magic knowledge that vte will.

	  int write_sock = open_unix_socket();
	  if(write_sock == -1)
	    {
	      _error->DumpErrors();
	      _exit(pkgPackageManager::Failed);
	    }

	  if(connect(write_sock, (struct sockaddr *)&sa, sizeof(sa)) != 0)
	    {
	      int errnum = errno;
	      std::string err = cw::util::sstrerror(errnum);
	      _error->Error("%s: Unable to bind to the temporary socket: %s",
			    __PRETTY_FUNCTION__,
			    err.c_str());
	      _error->DumpErrors();
	      _exit(pkgPackageManager::Failed);
	    }

	  pkgPackageManager::OrderResult result = f(write_sock);

	  // Make sure errors appear somewhere (we really ought to push
	  // them down the FIFO).
	  _error->DumpErrors();

	  _exit(result);
	}
      else
	{
	  int read_sock = accept(listen_sock, NULL, NULL);
	  if(read_sock == -1)
	    {
	      int errnum = errno;
	      std::string errmsg = cw::util::sstrerror(errnum);
	      _error->Error(_("%s: Unable to accept a connection from the subprocess: %s"),
			    __PRETTY_FUNCTION__,
			    errmsg.c_str());
	    }

	  // Catch status output from the install process.
	  Glib::signal_io().connect(sigc::bind(sigc::ptr_fun(&process_data_from_dpkg_socket),
					       read_sock),
				    read_sock,
				    Glib::IO_IN | Glib::IO_ERR | Glib::IO_HUP | Glib::IO_NVAL);

	  // The parent process.  Here we just wait for the reaper to
	  // tell us that the child finished, then return the result.
	  // We use implicit locking here to avoid a race condition that
	  // could occur: we know that the reaper won't fire before we
	  // can connect to it because its signal executions go through
	  // the main loop, and this function call is blocking the main
	  // loop.
	  connect_dpkg_result(pid, k);

	  vte_reaper_add_child(pid);
	}
    }
  }

  void run_dpkg_in_terminal(const sigc::slot1<pkgPackageManager::OrderResult, int> &f,
			    const sigc::slot1<void, Gtk::Widget *> &register_terminal,
			    const sigc::slot1<void, pkgPackageManager::OrderResult> &k)
  {
    // Ask the main thread to start the dpkg run and to invoke the
    // continuation.
    post_event(sigc::bind(sigc::ptr_fun(&do_run_dpkg),
			  f,
			  register_terminal,
			  k));
  }
}
