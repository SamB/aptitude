// download_install_manager.h                          -*-c++-*-
//
//   Copyright (C) 2005, 2008 Daniel Burrows
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

#ifndef DOWNLOAD_INSTALL_MANAGER_H
#define DOWNLOAD_INSTALL_MANAGER_H

#include "download_manager.h"

#include "apt.h"

#include <apt-pkg/packagemanager.h> // For OrderResult
#include <apt-pkg/pkgcache.h>       // For logging
#include <apt-pkg/sourcelist.h>

#include <sigc++/signal.h>

#include <utility>
#include <vector>

/** \file download_install_manager.h
 */

// This complex slot-based mechanism is used so that we can still have
// a generic downloading framework (e.g., ui_download_manager).

// The type of a function that runs dpkg in a terminal.  It's passed a
// function that invokes dpkg as currently appropriate, given the
// status file descriptor on which to invoke it (or -1 to discard
// status messages).  The slot that's passed in can be invoked in a
// background thread or a sub-process.
typedef sigc::slot1<pkgPackageManager::OrderResult, sigc::slot1<pkgPackageManager::OrderResult, int> > run_dpkg_in_terminal_func;

/** Manages downloading and installing packages. */
class download_install_manager : public download_manager
{
  /** A signal log upon which Complete() should be called. */
  download_signal_log *log;

  /** If \b true, don't actually invoke the package manager. */
  bool download_only;

  /** The package manager object used when installing packages */
  pkgPackageManager *pm;

  /** The list of sources from which to download. */
  pkgSourceList src_list;

  /** How to run the actual install process. */
  run_dpkg_in_terminal_func run_dpkg_in_terminal;

  /** Actually run dpkg; this is the part of the installation
   *  that might run in a sub-process.
   */
  pkgPackageManager::OrderResult run_dpkg(int status_fd);

  /** Actually perform the installation/removal of packages and tell
   *  the caller what happened.
   */
  result execute_install_run(pkgAcquire::RunResult res,
			     OpProgress &load_progress);
public:
  /** \param _download_only if \b true, this download process will
   *  stop after downloading files (i.e., it won't run the package
   *  manager).
   *
   *  \param _run_dpkg_in_terminal  how to set up the terminal for dpkg.
   */
  download_install_manager(bool _download_only,
			   const run_dpkg_in_terminal_func &_run_dpkg_in_terminal);
  ~download_install_manager();

  /** Set up an install run.  Does not take ownership of any of the
   *  arguments to the method.
   *
   *  \param progress the progress object to use when
   *                  saving the current cache state.
   *
   *  \param _download_only if \b true, don't actually install packages.
   *
   *  \param acqlog the status object to receive direct messages as
   *                the download proceeds.
   *
   *  \param signallog an object upon which Complete() should be
   *                called once the entire install process is finished.
   *
   *  \return \b true iff the preparation was successful.
   */
  bool prepare(OpProgress &progress,
	       pkgAcquireStatus &acqlog,
	       download_signal_log *signallog);

  /** If download_only is false, call the package manager to install
   *  or remove packages.
   *
   *  The status file descriptor will be set 
   */
  result finish(pkgAcquire::RunResult result,
		OpProgress &progress);

  /** \brief Invoke the part of finish() that runs before dpkg.
   *
   *  This function must be invoked in the foreground thread.
   *
   *  \param result   The return value of do_download().
   *
   *  \return success or failure; if success is returned, you may
   *  proceed to invoke finish_run_dpkg().  Otherwise,
   *  finish_post_dpkg() should be invoked with
   *  pkgPackageManager::Failed.
   */
  result finish_pre_dpkg(pkgAcquire::RunResult result);

  /** \brief Actually run dpkg.
   *
   *  This function may be invoked in a background thread or even in a
   *  sub-process; it will use the callback passed to the constructor
   *  to wrap the actual invocation of dpkg.  Its return value should
   *  be passed to finish_post_dpkg().
   */
  pkgPackageManager::OrderResult finish_run_dpkg();

  /** \brief Invoke the part of finish() that runs after dpkg.
   *
   *  This function must be invoked in the foreground thread.
   *
   *  \param dpkg_result   The return value of finish_run_dpkg().
   *  \param progress      An object to use in displaying the
   *                       progress when reloading.
   *
   *  \return the result of the install process; if do_again, then the
   *  caller should repeat the process beginning with do_download().
   */
  result finish_post_dpkg(pkgPackageManager::OrderResult dpkg_result,
			  OpProgress &progress);

  /** Invoked after an automatic 'forget new' operation. */
  sigc::signal0<void> post_forget_new_hook;
};

#endif
