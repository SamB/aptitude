/** \file cmdline_progress.cc */    // -*-c++-*-


// Copyright (C) 2010 Daniel Burrows
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; see the file COPYING.  If not, write to
// the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.


// Local includes:
#include "cmdline_progress.h"

#include "terminal.h"

#include <generic/apt/acqprogress.h>
#include <generic/apt/apt.h>
#include <generic/apt/config_signal.h>
#include <generic/apt/download_signal_log.h>


// System includes:
#include <sigc++/adaptors/bind.h>
#include <sigc++/functors/mem_fun.h>
#include <sigc++/functors/ptr_fun.h>

using aptitude::cmdline::terminal_metrics;
using boost::shared_ptr;

static void dl_complete(download_signal_log &manager,
			AcqTextStatus *acqprogress)
{
  delete acqprogress;
}

namespace
{
  void do_update_screen_width(pkgAcquire *,
                              download_signal_log &,
                              const sigc::slot1<void, bool> &,
                              unsigned int &screen_width,
                              const shared_ptr<terminal_metrics> &term_metrics)
  {
    screen_width = term_metrics->get_screen_width();
    // Note that we don't call the continuation; we assume that
    // another slot invoked by the same signal will do that.
  }
}

download_signal_log *gen_cmdline_download_progress(const shared_ptr<terminal_metrics> &term_metrics)
{
  // The terminal expects a reference to a variable that will be
  // updated in-place to contain the current screen width.
  //
  // \todo Instead of doing this, rewrite the progress display to use
  // the terminal object directly -- will require either moving it
  // into cmdline/ (preferred?) or moving the terminal into generic/.
  static unsigned int screen_width;
  screen_width = term_metrics->get_screen_width();

  download_signal_log *m=new download_signal_log;

  AcqTextStatus *acqprogress=new AcqTextStatus(screen_width, aptcfg->FindI("Quiet", 0));

  m->MediaChange_sig.connect(sigc::mem_fun(*acqprogress, &AcqTextStatus::MediaChange));
  m->IMSHit_sig.connect(sigc::mem_fun(*acqprogress, &AcqTextStatus::IMSHit));
  m->Fetch_sig.connect(sigc::mem_fun(*acqprogress, &AcqTextStatus::Fetch));
  m->Done_sig.connect(sigc::mem_fun(*acqprogress, &AcqTextStatus::Done));
  m->Fail_sig.connect(sigc::mem_fun(*acqprogress, &AcqTextStatus::Fail));
  m->Pulse_sig.connect(sigc::bind(sigc::ptr_fun(&do_update_screen_width),
                                  sigc::ref(screen_width),
                                  term_metrics));
  m->Pulse_sig.connect(sigc::mem_fun(*acqprogress, &AcqTextStatus::Pulse));
  m->Start_sig.connect(sigc::mem_fun(*acqprogress, &AcqTextStatus::Start));
  m->Stop_sig.connect(sigc::mem_fun(*acqprogress, &AcqTextStatus::Stop));
  m->Complete_sig.connect(sigc::bind(sigc::ptr_fun(dl_complete),
				     acqprogress));

  return m;
}

