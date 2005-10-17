// ui_download_manager.h                           -*-c++-*-
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
//
// Glue code to go between the UI and the download manager/thread
// stuff.

#ifndef UI_DOWNLOAD_MANAGER_H
#define UI_DOWNLOAD_MANAGER_H

#include "download_thread.h"

#include <apt-pkg/acquire.h>

#include <vscreen/ref_ptr.h>

#include <sigc++/trackable.h>

class download_manager;
class download_signal_log;
class download_thread;
class vscreen_widget;

/** Represents the UI end of a download process.  This object
 *  completely handles its own memory management -- you don't have to
 *  delete it and you shouldn't try.
 */
class ui_download_manager : public sigc::trackable
{
  /** Used to indicate that a download was cancelled. */
  class aborter : public sigc::trackable
  {
    bool aborted;
  public:
    aborter() : aborted(false)
    {
    }

    void abort()
    {
      aborted = true;
    }

    bool get_aborted()
    {
      return aborted;
    }
  };

  download_manager *manager;

  download_thread *t;

  aborter abort_state;

  download_signal_log *log;

  background_status *st;

  /** Used to keep the download status widget alive until the download
   *  completes.
   */
  ref_ptr<vscreen_widget> download_status;

  void done(download_thread *, pkgAcquire::RunResult res);
public:
  ui_download_manager(download_manager *_manager,
		      bool force_noninvasive,
		      bool list_update,
		      bool hide_preview,
		      const std::string &title,
		      const std::string &longtitle,
		      const std::string &tablabel);
  ~ui_download_manager();

  void start();
};


#endif
