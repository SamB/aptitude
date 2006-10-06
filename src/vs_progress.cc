// vs_progress.cc
//
//  Copyright 2000, 2004-2006 Daniel Burrows
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

#include "aptitude.h"

#include "vs_progress.h"

#include <sstream>

#include <vscreen/transcode.h>

vs_progress::vs_progress()
{
}

void vs_progress::paint(const style &st)
{
  int width=getmaxx();

  if(!Op.empty())
    {
      eassert(Percent>=0 && Percent<=100);

      std::ostringstream percentstream;
      percentstream << " " << ((int) Percent) << "%";
      std::string percentstr = percentstream.str();

      mvaddstr(0, 0, transcode(Op));
      mvaddstr(0, width - percentstr.size(), transcode(percentstr));
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
