// download_bar.h    -*-c++-*-
//
//  Copyright 1999-2001, 2005 Daniel Burrows
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
//  Like download_screen, but much much much simpler.  In essence, we do a
// Netscape-style display of what's going on with the download, in the minibuf
// of a window.  It would be best if the user could carry on doing stuff while
// the download progressed, but libapt makes this dangerously difficult to
// impossible.

#ifndef DOWNLOAD_BAR_H
#define DOWNLOAD_BAR_H

#include <apt-pkg/acquire.h>
#include "vscreen/vscreen_widget.h"

class download_status_bar:public pkgAcquireStatus, public vscreen_widget
{
  std::string last_msg;
  // Stores the last thing we displayed

  pkgAcquire::Worker *last_worker;
  // The workers are stepped around in a circular fashion; this stores where
  // we just were.
  time_t last_switchtime;
  // Stores the last time that we stepped through the list (so we don't just
  // flicker around)

  bool cancelled;
  // True if the user cancelled the download

protected:
  download_status_bar();
public:
  static ref_ptr<download_status_bar> create()
  {
    ref_ptr<download_status_bar> rval(new download_status_bar);
    rval->decref();
    return rval;
  }

  bool MediaChange(std::string Media, std::string Drive);

  void IMSHit(pkgAcquire::ItemDesc &itm);
  void Fetch(pkgAcquire::ItemDesc &itm);
  void Done(pkgAcquire::ItemDesc &itm);
  void Fail(pkgAcquire::ItemDesc &itm);
  bool Pulse(pkgAcquire *Owner);
  void Start();
  void Stop();

  void paint(const style &st);
  bool handle_key(const key &k);

  int width_request();
  int height_request(int);
  bool get_cursorvisible() {return false;}
  point get_cursorloc() {return point(0,0);}
};

typedef ref_ptr<download_status_bar> download_status_bar_ref;

#endif
