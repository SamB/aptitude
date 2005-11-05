// download_screen.cc
//
//  Copyright 1999-2001, 2004-2005 Daniel Burrows
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
//
//  Handles a download by acting as a progress meter.

#include "aptitude.h"

#include "download_screen.h"
#include "download_item.h"
#include "ui.h"

#include <generic/util/eassert.h>
#include <signal.h>

#include <apt-pkg/strutl.h>
#include <apt-pkg/acquire-worker.h>

#include <vscreen/transcode.h>
#include <vscreen/vscreen.h>
#include <vscreen/vs_util.h>

#include <sigc++/adaptors/bind.h>
#include <sigc++/functors/ptr_fun.h>

#include <generic/util/mut_fun.h>

static void set_and_exit(bool &target, bool val)
{
  target=val;
  vscreen_exitmain();
}

bool download_screen::MediaChange(string Media, string Drive)
{
  vs_widget_ref tmpref(this);

  char buf[512];

  snprintf(buf, 512,
	   _("Please insert the disc labeled \"%s\" into the drive \"%s\""),
	   Media.c_str(), Drive.c_str());

  bool rval=true;

  popup_widget(vs_dialog_yesno(transcode(buf),
			       arg(sigc::bind(sigc::ptr_fun(set_and_exit),
					      rval, true)),
			       transcode(_("Continue")),
			       arg(sigc::bind(sigc::ptr_fun(set_and_exit),
					      rval, false)),
			       transcode(_("Abort")),
			       get_style("MediaChange")));

  vscreen_mainloop();  // Eeeeeek!  Recursive mainloop!  I'm afraid..

  return rval;
}

void download_screen::IMSHit(pkgAcquire::ItemDesc &itmdesc)
{
  vs_widget_ref tmpref(this);

  downloadmap::iterator found=active_items.find(itmdesc.Owner);

  if(found==active_items.end())
    {
      download_item *newitm=new download_item(itmdesc);
      newitm->download_done(true);
      contents->add_child(newitm);
      sync_bounds();
    }
  else
    found->second->download_done(true);
  vscreen_update();
  vscreen_tryupdate();
}

void download_screen::Fetch(pkgAcquire::ItemDesc &itmdesc)
{
  vs_widget_ref tmpref(this);

  downloadmap::iterator found=active_items.find(itmdesc.Owner);

  if(found==active_items.end())
    {
      download_item *newitm=new download_item(itmdesc);
      active_items[itmdesc.Owner]=newitm;
      contents->add_child(newitm);
      sync_bounds();
    }

  vscreen_update();
  vscreen_tryupdate();
}

void download_screen::Done(pkgAcquire::ItemDesc &itmdesc)
{
  vs_widget_ref tmpref(this);

  downloadmap::iterator found=active_items.find(itmdesc.Owner);
  if(found==active_items.end())
    {
      download_item *newitm=new download_item(itmdesc);
      newitm->download_done(false);
      contents->add_child(newitm);
      sync_bounds();
    }
  else
    {
      found->second->download_done(false);
      found->second->set_worker(NULL);
    }

  vscreen_update();
  vscreen_tryupdate();
}

void download_screen::Fail(pkgAcquire::ItemDesc &itmdesc)
{
  vs_widget_ref tmpref(this);

  downloadmap::iterator found=active_items.find(itmdesc.Owner);
  if(found!=active_items.end())
    found->second->set_worker(NULL);

  // Nothing really to do??
  vscreen_update();
  vscreen_tryupdate();
}

bool download_screen::Pulse(pkgAcquire *Owner)
{
  vs_widget_ref tmpref(this);

  pkgAcquireStatus::Pulse(Owner);

  for(pkgAcquire::Worker *i=Owner->WorkersBegin(); i; i=Owner->WorkerStep(i))
    {
      if(i->CurrentItem)
	get_itm(*i->CurrentItem)->set_worker(i);
    }

  vscreen_poll();

  vscreen_update();
  vscreen_tryupdate();

  return !cancelled;
}

void download_screen::Start()
{
  vs_widget_ref tmpref(this);

  pkgAcquireStatus::Start();
}

void download_screen::Stop()
{
  vs_widget_ref tmpref(this);

  char buf[256];

  pkgAcquireStatus::Stop();

  snprintf(buf, 256, _("Downloaded %sB in %ss (%sB/s)."), SizeToStr(FetchedBytes).c_str(), TimeToStr(ElapsedTime).c_str(), SizeToStr(CurrentCPS).c_str());

  popup_widget(vs_dialog_ok(transcode(buf),
			    arg(sigc::ptr_fun(vscreen_exitmain))));

  vscreen_mainloop();

  destroy();
}

#if 0
void download_screen::paint_status()
{
  if(finished)
    vs_tree::paint_status();
  else
    {
      int width,height;
      getmaxyx(height,width);
      string output="";

      output+=_("Total Progress: ");

      int barsize=0;

      if((TotalBytes+TotalItems)>0)
	{
	  char progress_string[50]; // More'n enough chars.
	  unsigned long ETA=(unsigned long)((TotalBytes-CurrentBytes)/CurrentCPS);
	  // Straight from acqprogress.cc, that is..

	  barsize=int((double(width*(CurrentBytes+CurrentItems)))/(TotalBytes+TotalItems));
	  if(barsize>width)
	    barsize=width;

	  attrset(get_status_color());
	  snprintf(progress_string, 50, _(" [ %i%% ] (%sB/s, %s remaining)"), int(double((100.0*(CurrentBytes+CurrentItems)))/(TotalBytes+TotalItems)), SizeToStr(CurrentCPS).c_str(), TimeToStr(ETA).c_str());
	  output+=progress_string;
	}

      show_string_as_progbar(0,
			     height-1,
			     output,
			     get_color("Progress"),
			     get_status_color(),
			     barsize,
			     width);
    }
}
#endif

bool download_screen::handle_key(const key &k)
{
  vs_widget_ref tmpref(this);

  if(global_bindings.key_matches(k, "Quit"))
    cancelled=true;
  else
    return vs_tree::handle_key(k);

  return true;
}

download_screen::~download_screen()
{
}
