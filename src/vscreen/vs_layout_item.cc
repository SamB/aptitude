// vs_layout_item.cc
//
//   Copyright (C) 2004-2005, 2007 Daniel Burrows
//
//   This program is free software; you can redistribute it and/or
//   modify it under the terms of the GNU General Public License as
//   published by the Free Software Foundation; either version 2 of
//   the License, or (at your option) any later version.
//
//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//   General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with this program; see the file COPYING.  If not, write to
//   the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//   Boston, MA 02111-1307, USA.


#include "vs_layout_item.h"

#include "fragment.h"
#include "vs_tree.h"

vs_layout_item::vs_layout_line::vs_layout_line(int _n, vs_layout_item &_parent)
  :vs_treeitem(false),n(_n), parent(_parent)
{
  set_depth(parent.get_depth());
}

const wchar_t *vs_layout_item::vs_layout_line::tag() {return L"";}
const wchar_t *vs_layout_item::vs_layout_line::label() {return L"";}


vs_layout_item::levelref::levelref(const levelref &x)
  :vs_tree_levelref(x), item_num(x.item_num), lines(x.lines) {}

vs_layout_item::levelref::levelref(size_t n, const child_list &_lines)
  :item_num(n), lines(_lines)
{
}

vs_treeitem *vs_layout_item::levelref::get_item()
{
  return item_num<lines.size()?lines[item_num]:lines.back();
}

void vs_layout_item::levelref::advance_next() {++item_num;}
void vs_layout_item::levelref::return_prev() {--item_num;}
bool vs_layout_item::levelref::is_begin() {return item_num==0;}
bool vs_layout_item::levelref::is_end() {return item_num>=lines.size();}

vs_layout_item::levelref *vs_layout_item::levelref::clone() const
{
  return new levelref(*this);
}



vs_layout_item::vs_layout_item(fragment *_f)
  :vs_treeitem(false), f(_f), lastw(0), lastbasex(-1)
{
}

// These don't need to be defined (?)
const wchar_t *vs_layout_item::tag() {return L"";}
const wchar_t *vs_layout_item::label() {return L"";}

int vs_layout_item::get_normal_attr()
{
  return A_BOLD;
}

vs_layout_item::levelref *vs_layout_item::begin() {
  return (!children.empty())?new levelref(0, children):NULL;
}

vs_layout_item::levelref *vs_layout_item::end()
{
  return (!children.empty())?new levelref(children.size(), children):NULL;
}

bool vs_layout_item::has_visible_children()
{
  return !children.empty();
}

vs_layout_item::~vs_layout_item()
{
  for(child_list::iterator i=children.begin(); i!=children.end(); ++i)
    delete *i;
  delete f;
}

const fragment_line &vs_layout_item::get_line(vs_tree *win, size_t n,
					      int basex, const style &st)
{
  if(win->getmaxx()!=lastw || basex != lastbasex)
    {
      fragment_contents tmplines=f->layout(win->getmaxx()-basex,
					   win->getmaxx()-basex,
					   st);

      lines=fragment_contents();
      attr_t attr=st.get_attrs();
      for(fragment_contents::const_iterator i=tmplines.begin();
	  i!=tmplines.end(); ++i)
	lines.push_back(fragment_line(basex, wchtype(L' ', attr))+*i);

      for(child_list::iterator i=children.begin(); i!=children.end(); ++i)
	delete *i;

      children.clear();

      for(size_t i=1; i<lines.size(); ++i)
	children.push_back(new vs_layout_line(i, *this));

      lastw=win->getmaxx();
      lastbasex=basex;
    }

  if(n>=lines.size())
    return lines.back();
  else
    return lines[n];
}

void vs_layout_item::paint_line(int n, vs_tree *win, int y, bool hierarchical, const style &st)
{
  int basex=hierarchical?2*get_depth():0;

  const fragment_line &s=get_line(win, n, basex, st);

  win->mvaddnstr(y, 0, s, s.size());
}

void vs_layout_item::vs_layout_line::paint(vs_tree *win, int y,
					   bool hierarchical, const style &st)
{
  parent.paint_line(n, win, y, hierarchical, st);
}

void vs_layout_item::paint(vs_tree *win, int y, bool hierarchical,
			   const style &st)
{
  paint_line(0, win, y, hierarchical, st);
}

