// vs_progress.cc
//
//  Copyright 2000, 2004-2005 Daniel Burrows
//
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License as
//  published by the Free Software Foundation; either version 2 of the
//  License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//  General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; see the file COPYING.  If not, write to
//  the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//  Boston, MA 02111-1307, USA.
//
//  A vs_minibuf that also acts as a progress bar.

#include "aptitude.h"

#include "vs_progress.h"

#include <vscreen/transcode.h>

vs_progress::vs_progress()
{
}

void vs_progress::paint(const style &st)
{
  int barsize;
  char percent_string[50];
  int width=getmaxx();

  if(!Op.empty())
    {
      eassert(Percent>=0 && Percent<=100);
      barsize=int(Percent*width/100.0);

      snprintf(percent_string, 50, ": %i%%", int(Percent));

      show_string_as_progbar(0,
			     0,
			     transcode(Op+percent_string),
			     st+get_style("Progress"),
			     st,
			     barsize,
			     width);
    }
  else
    erase();
}

bool vs_progress::get_cursorvisible()
{
  return false;
}

point vs_progress::get_cursorloc()
{
  return point(int(Percent*getmaxx()/100.0), 0);
}

void vs_progress::Update()
{
  show();

  if(CheckChange(0.25))
    {
      vscreen_update();
      vscreen_updatecursor();
      vscreen_tryupdate();
    }
}

void vs_progress::Done()
{
  vscreen_update();
  vscreen_updatecursor();
  vscreen_tryupdate();
  hide();
}

int vs_progress::width_request()
{
  return 40;
}

int vs_progress::height_request(int w)
{
  return 1;
}
