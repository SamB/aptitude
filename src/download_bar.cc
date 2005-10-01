// download_bar.cc
//
//  Copyright 1999-2000, 2004-2005 Daniel Burrows
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

#include "aptitude.h"

#include "download_bar.h"
#include "ui.h"

#include <vscreen/config/keybindings.h>
#include <vscreen/config/colors.h>
#include <vscreen/fragment.h>
#include <vscreen/vs_minibuf_win.h>
#include <vscreen/vs_util.h>
#include <vscreen/transcode.h>

#include <apt-pkg/strutl.h>
#include <apt-pkg/acquire-item.h>
#include <apt-pkg/acquire-worker.h>

#include <signal.h>

#include <sigc++/functors/ptr_fun.h>


download_status_bar::download_status_bar()
{
}

bool download_status_bar::MediaChange(string media, string drive)
{
  vs_widget_ref tmpref(this);

  fragment *f=wrapbox(fragf(_("Please insert the disc labeled \"%s\" into the drive \"%s\""),
			      media.c_str(), drive.c_str()));

  vs_widget_ref w = vs_dialog_ok(f, arg(sigc::ptr_fun(vscreen_exitmain)),
				 get_style("MediaChange"));
  w->show_all();
  popup_widget(w);

  vscreen_mainloop();  // Eeeeeek!  Recursive mainloop!  I'm afraid..

  return true;
}

void download_status_bar::IMSHit(pkgAcquire::ItemDesc &itm)
{
  vs_widget_ref tmpref(this);

  last_msg=_("Hit ")+itm.Description;
  last_switchtime=time(0);

  vscreen_update();
  vscreen_tryupdate();
}

void download_status_bar::Fetch(pkgAcquire::ItemDesc &itm)
{
  vs_widget_ref tmpref(this);

  last_msg=_("Downloading ")+itm.ShortDesc;
  last_switchtime=time(0);

  vscreen_update();
  vscreen_tryupdate();
}

void download_status_bar::Done(pkgAcquire::ItemDesc &itm)
{
  vs_widget_ref tmpref(this);

  last_msg=_("Got ")+itm.Description;
  last_switchtime=time(0);

  vscreen_update();
  vscreen_tryupdate();
}

void download_status_bar::Fail(pkgAcquire::ItemDesc &itm)
{
  vs_widget_ref tmpref(this);

  last_msg=itm.Description+": "+itm.Owner->ErrorText;
  last_switchtime=time(0);

  vscreen_update();
  vscreen_tryupdate();
}

bool download_status_bar::Pulse(pkgAcquire *Owner)
{
  vs_widget_ref tmpref(this);

  pkgAcquireStatus::Pulse(Owner);

  if(difftime(time(0), last_switchtime)>1)
    {
      pkgAcquire::Worker *test=Owner->WorkersBegin();
      bool found=false;
      while(test)
	{
	  if(test==last_worker)
	    {
	      found=true;
	      break;
	    }
	  test=Owner->WorkerStep(test);
	}

      if(!found)
	last_worker=Owner->WorkersBegin();
      else if(last_worker!=NULL)
	last_worker=Owner->WorkerStep(last_worker);

      if(last_worker==NULL)
	last_worker=Owner->WorkersBegin();

      while(last_worker!=NULL && !(last_worker->CurrentItem || !last_worker->Status.empty()))
	last_worker=Owner->WorkerStep(last_worker);

      if(last_worker==NULL)
	last_msg=_("Downloading...");
      else
	{
	  if(!last_worker->CurrentItem)
	    last_msg=last_worker->Status;
	  else
	    {
	      last_msg=last_worker->CurrentItem->ShortDesc;
	      if(last_worker->CurrentItem->Owner->Mode)
		last_msg+=string("[")+last_worker->CurrentItem->Owner->Mode+"]";

	      if(last_worker->TotalSize>0)
		last_msg+=": "+SizeToStr(last_worker->CurrentSize)+"B/"+SizeToStr(last_worker->TotalSize)+"B";
	      else if(last_worker->CurrentSize>0)
		last_msg+=": "+SizeToStr(last_worker->CurrentSize)+"B";
	    }
	}

      vscreen_poll();

      vscreen_update();
      vscreen_tryupdate();
    }

  return !cancelled;
}

void download_status_bar::paint(const style &st)
{
  vs_widget_ref tmpref(this);

  int width=getmaxx();
  string todisp=last_msg,totalprogress;

  int barsize=0;
  if((TotalBytes+TotalItems)>0)
    {
      char progress_string[50]; // More'n enough chars.
      unsigned long ETA=(unsigned long)((TotalBytes-CurrentBytes)/CurrentCPS);
      // Straight from acqprogress.cc, that is..
      
      barsize=int((double(width*(CurrentBytes+CurrentItems)))/(TotalBytes+TotalItems));
      if(barsize>width)
	barsize=width;

      snprintf(progress_string, 50, " [ %i%% ] (%sB/s %s )", int(double((100.0*(CurrentBytes+CurrentItems)))/(TotalBytes+TotalItems)), SizeToStr(CurrentCPS).c_str(), TimeToStr(ETA).c_str());
      totalprogress=progress_string;
    }

  if(totalprogress.size()>(unsigned) width)
    todisp="";
  else if(todisp.size()+totalprogress.size()>(unsigned) width)
    todisp=last_msg.substr(0, width-totalprogress.size());
  else
    while(todisp.size()<(width-totalprogress.size()))
      todisp+=" ";

  todisp+=totalprogress;

  show_string_as_progbar(0,
			 0,
			 transcode(todisp),
			 st+get_style("Progress"),
			 st+get_style("Status"),
			 barsize,
			 width);
}

void download_status_bar::Start()
{
  vs_widget_ref tmpref(this);

  pkgAcquireStatus::Start();
}

void download_status_bar::Stop()
{
  vs_widget_ref tmpref(this);

  pkgAcquireStatus::Stop();

  destroy();
}

bool download_status_bar::handle_key(const key &k)
{
  vs_widget_ref tmpref(this);

  if(global_bindings.key_matches(k, "Quit"))
    {
      cancelled=true;
      return true;
    }
  else
    return false;
}

int download_status_bar::width_request()
{
  return 0;
}

int download_status_bar::height_request(int)
{
  return 0;
}
