// colors.cc   -*-c++-*-
//
//  Copyright 1999-2001, 2003-2006 Daniel Burrows
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

#include "colors.h"

#include <vscreen/curses++.h>

#include <math.h>

static bool colors_avail=false;
static bool default_colors_avail = false;
static int colors = 0;

// Simplistic allocation scheme for colors: (fg,bg) => fg*COLORS+bg

void init_colors()
{
  if(COLOR_PAIRS < COLORS * COLORS)
    colors = (int) floor(sqrt(COLOR_PAIRS));
  else
    colors = COLORS;

  if(colors < 8)
    return;
  else
    colors = std::min(colors, 8);

  colors_avail=true;
  default_colors_avail = (use_default_colors() != ERR);

  for(short fg = 0; fg < colors; ++fg)
    for(short bg = 0; bg < colors; ++bg)
      {
	eassert(fg * colors + bg < COLOR_PAIRS);

	if(default_colors_avail && fg == bg)
	  init_pair(fg * colors + bg, fg, -1);
	else if(fg == 0 && bg == 0)
	  // do nothing; on some terminals, doing this causes the
	  // cursor to become INVISIBLE, and black-on-black text is a
	  // bad idea anyway..
	  ;
	  /*assume_default_colors(0, 0);*/
	else
	  init_pair(fg * colors + bg, fg, bg);
      }
}

int get_color_pair(short fg, short bg)
{
  if(!colors_avail)
    return 0;
  else
    {
      // Fall back to the default color if the user asked for too
      // much.
      if(fg >= colors)
	fg = 0;

      if(bg >= colors)
	bg = 0;

      eassert(fg >= 0 && bg >= -1 && fg < colors && bg < colors);

      if(bg == -1)
	return fg * colors + fg;
      else if(fg == bg && default_colors_avail)
	// Pick an arbitrary distinct foreground color to match with
	// the background.
	{
	  if(bg == COLOR_WHITE)
	    return COLOR_BLACK * colors + COLOR_WHITE;
	  else
	    return COLOR_WHITE * colors + bg;
	}
      else
	return fg * colors + bg;
    }
}

int mix_color(short color, short fg, short bg)
{
  if(!colors_avail)
    return 0;
  else if(fg == -1 && bg == -2)
    return color & A_COLOR;
  else
    {
      short old_fg = PAIR_NUMBER(color) / colors;
      short old_bg = PAIR_NUMBER(color) % colors;

      if(old_fg == old_bg && default_colors_avail)
	old_bg = -1;

      if(bg == -1 && !default_colors_avail)
	return 0;
      else if(fg == -1)
	return COLOR_PAIR(get_color_pair(old_fg, bg));
      else if(bg == -2)
	return COLOR_PAIR(get_color_pair(fg, old_bg));
      else
	return COLOR_PAIR(get_color_pair(fg, bg));
    }
}
