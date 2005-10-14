// vs_staticitem.cc
//
//  Copyright 2000, 2005 Daniel Burrows
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
//  Displays static information

#include "vs_staticitem.h"
#include "vs_tree.h"

void vs_staticitem::paint(vs_tree *win, int y, bool hierarchical, const style &st)
{
  int width, height;
  int basex = hierarchical ? 2*get_depth() : 0;
  win->getmaxyx(height,width);

  win->move(y,0);
  int x = 0;

  while(x < basex && x < width)
    {
      win->add_wch(L' ');
      x += wcwidth(L' ');
    }

  if(x >= width)
    return;

  win->apply_style(st + style_attrs_on(A_BOLD));
  size_t i = 0;
  while(i < name.size() && x < width)
    {
      wchar_t ch = name[i];

      win->add_wch(ch);
      x += wcwidth(ch);
      ++i;
    }

  i = 0;
  while(i < value.size() && x < width)
    {
      wchar_t ch = name[i];

      win->add_wch(ch);
      x += wcwidth(ch);
      ++i;
    }
  win->apply_style(st);

  while(x<width)
    {
      win->add_wch(L' ');
      x += wcwidth(L' ');
    }
}
