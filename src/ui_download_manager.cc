// ui_download_manager.cc
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

#include "ui_download_manager.h"

#include "aptitude.h"
#include "ui.h"
#include "vs_progress.h"

#include <generic/apt/apt.h>
#include <generic/apt/download_manager.h>
#include <generic/apt/download_signal_log.h>

#include <sigc++/functors/mem_fun.h>

#include <vscreen/vscreen_widget.h> // For vs_widget_ref

ui_download_manager::ui_download_manager(download_manager *_manager,
					 bool force_noninvasive,
					 bool list_update,
					 bool hide_preview,
					 const std::string &title,
					 const std::string &longtitle,
					 const std::string &tablabel)
  : manager(_manager),
    t(NULL)
{
  std::pair<download_signal_log *, vs_widget_ref> progpair =
    gen_download_progress(force_noninvasive, list_update,
			  title, longtitle, tablabel,
			  arg(sigc::mem_fun(abort_state,
					    &aborter::abort)));

  log             = progpair.first;
  download_status = progpair.second;
  st              = new background_status(log);

  ui_start_download(hide_preview);
}

ui_download_manager::~ui_download_manager()
{
  log->Complete();

  delete manager;

  ui_stop_download();

  abort_state.abort();

  delete t;
  delete log;
  delete st;
}

void ui_download_manager::done(download_thread *, pkgAcquire::RunResult res)
{
  vs_progress_ref p = gen_progress_bar();

  download_manager::result run_res = download_manager::failure;

  if(!abort_state.get_aborted())
    run_res = manager->finish(res, *p.unsafe_get_ref());

  apt_load_cache(p.unsafe_get_ref(), true);

  p->destroy();

  delete t;
  t = NULL;

  if(run_res == download_manager::do_again && !abort_state.get_aborted())
    (new download_thread(manager,
			 sigc::mem_fun(this,
				       &ui_download_manager::done)))->start();
  else
    delete this;
}

void ui_download_manager::start()
{
  vs_progress_ref p = gen_progress_bar();

  bool ok = manager->prepare(*p.unsafe_get_ref(), *st, log);

  p->destroy();

  if(ok)
    (new download_thread(manager,
			 sigc::mem_fun(this,
				       &ui_download_manager::done)))->start();
  else
    delete this;
}
