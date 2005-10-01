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
  int basex=hierarchical?2*get_depth():0;
  int width,height;
  win->getmaxyx(height,width);

  win->attron(A_BOLD);
  win->move(y,0);
  for(int i=0; i<basex && i<width; i++)
    win->addch(' ');
  if(basex >= width)
    return;

  win->addnstr(name.c_str(), width - basex);
  if((basex + name.size()) >= (unsigned) (width - basex))
    return;

  win->attroff(A_BOLD);
  win->addnstr(value.c_str(), width - basex - name.size());
  for(int newx = basex + name.size() + value.size(); newx < width; newx++)
    win->addch(' ');
}
