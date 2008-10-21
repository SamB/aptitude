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

// The type of a function that runs dpkg in a terminal.  It's passed a
// function that invokes dpkg as currently appropriate, given the
// status file descriptor on which to invoke it (or -1 to discard
// status messages).  This may be invoked in a sub-process.
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
   *
   *  \param _status_fd  the file descriptor on which to print
   *                     status information, or -1 to not print
   *                     status information.
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
   *  or remove packages. */
  result finish(pkgAcquire::RunResult result,
		OpProgress &progress);

  /** Invoked after an automatic 'forget new' operation. */
  sigc::signal0<void> post_forget_new_hook;
};

#endif
