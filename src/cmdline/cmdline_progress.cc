#include "cmdline_progress.h"

#include <sys/ioctl.h>

#include <generic/apt/acqprogress.h>
#include <generic/apt/apt.h>
#include <generic/apt/config_signal.h>
#include <generic/apt/download_signal_log.h>

#include <sigc++/adaptors/bind.h>
#include <sigc++/functors/mem_fun.h>
#include <sigc++/functors/ptr_fun.h>

unsigned int screen_width=80;

void update_screen_width(int foo=0)
{
  // Ripped from apt-get, which ripped it from GNU ls
  winsize ws;

  if (ioctl(1, TIOCGWINSZ, &ws) != -1 && ws.ws_col >= 5)

    // NB: originally this set things to ws_col-1, which (a) seems
    // wrong, and (b) produced incorrect results in everything except
    // the progress bar.  I suspect the progress bar has off-by-one
    // errors...
    screen_width = ws.ws_col;
}

static void dl_complete(download_signal_log &manager,
			AcqTextStatus *acqprogress)
{
  delete acqprogress;
}

download_signal_log *gen_cmdline_download_progress()
{
  download_signal_log *m=new download_signal_log;

  // FIXME: LEAK!!!!  (since the command-line stuff will only use this once
  // and then terminate, and killing this leak is a major pain, I'm
  // letting it by for now, but this MUST BE FIXED EVENTUALLY!)
  AcqTextStatus *acqprogress=new AcqTextStatus(screen_width, aptcfg->FindI("Quiet", 0));

  m->MediaChange_sig.connect(sigc::mem_fun(*acqprogress, &AcqTextStatus::MediaChange));
  m->IMSHit_sig.connect(sigc::mem_fun(*acqprogress, &AcqTextStatus::IMSHit));
  m->Fetch_sig.connect(sigc::mem_fun(*acqprogress, &AcqTextStatus::Fetch));
  m->Done_sig.connect(sigc::mem_fun(*acqprogress, &AcqTextStatus::Done));
  m->Fail_sig.connect(sigc::mem_fun(*acqprogress, &AcqTextStatus::Fail));
  m->Pulse_sig.connect(sigc::mem_fun(*acqprogress, &AcqTextStatus::Pulse));
  m->Start_sig.connect(sigc::mem_fun(*acqprogress, &AcqTextStatus::Start));
  m->Stop_sig.connect(sigc::mem_fun(*acqprogress, &AcqTextStatus::Stop));
  m->Complete_sig.connect(sigc::bind(sigc::ptr_fun(dl_complete),
				     acqprogress));

  // FIXME: Maybe the AcqTextStatus should be deleted on Complete?
  //m->Complete_sig.connect(sigc::mem_fun(acqprogress, &AcqTextStatus::Complete));

  return m;
}

