// download_install_manager.h                          -*-c++-*-
//
//   Copyright (C) 2005 Daniel Burrows
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

#include <sigc++/signal.h>

#include <utility>
#include <vector>

/** Manages downloading and installing packages. */
class download_install_manager : public download_manager
{
  /** A signal log upon which Complete() should be called. */
  download_signal_log *log;

  /** If \b true, don't actually invoke the package manager. */
  bool download_only;

  /** The package manager object used when installing packages */
  pkgPackageManager *pm;

  /** Actually perform the installation/removal of packages and tell
   *  the caller what happened.
   */
  result execute_install_run(pkgAcquire::RunResult res,
			     OpProgress &load_progress);
public:
  /** \param _download_only if \b true, this download process will
   *  stop after downloading files (i.e., it won't run the package
   *  manager).
   */
  download_install_manager(bool _download_only);
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

  /** Invoked prior to actually performing the install run. */
  sigc::signal0<void> pre_install_hook;

  /** Invoked right after performing the install run.  Takes the
   *  result of the run as an argument.
   */
  sigc::signal1<void, pkgPackageManager::OrderResult> post_install_hook;

  /** Invoked after an automatic 'forget new' operation. */
  sigc::signal0<void> post_forget_new_hook;
};

#endif
