// vs_tree.cc
//
//  Copyright 1999-2002, 2004-2005 Daniel Burrows
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
//  Implementation of stuff in vs_tree.h

#include "vs_tree.h"
#include "vs_editline.h"
#include "config/keybindings.h"
#include "config/colors.h"
#include "transcode.h"

#include "../aptitude.h" // For _()

#include <sigc++/functors/ptr_fun.h>

using namespace std;

keybindings *vs_tree::bindings=NULL;

bool vs_tree_search_string::operator()(const vs_treeitem &item)
{
  return item.matches(s);
}

vs_tree::vs_tree()
  :vscreen_widget(),
   root(NULL),
   begin(new vs_tree_root_iterator(NULL)),
   end(begin),
   top(begin),
   selected(top),
   hierarchical(true),
   prev_level(NULL)
{
  focussed.connect(sigc::ptr_fun(vscreen_update));
  unfocussed.connect(sigc::ptr_fun(vscreen_update));
}

vs_tree::vs_tree(vs_treeitem *_root, bool showroot)
  :vscreen_widget(),
   root(NULL),
   begin(new vs_tree_root_iterator(NULL)),
   end(begin),
   top(begin),
   selected(top),
   hierarchical(true),
   prev_level(NULL)
{
  set_root(_root, showroot);

  focussed.connect(sigc::ptr_fun(vscreen_update));
  unfocussed.connect(sigc::ptr_fun(vscreen_update));
}

vs_tree::~vs_tree()
{
   while(prev_level)
    {
      flat_frame *next=prev_level->next;
      delete prev_level;
      prev_level=next;
    }

  delete root; root=NULL;
}

void vs_tree::do_shown()
{
  if(selected!=end)
    selected->highlighted(this);
}

int vs_tree::width_request()
{
  return 1;
}

int vs_tree::height_request(int w)
{
  return 1;
}

void vs_tree::set_root(vs_treeitem *_root, bool showroot)
{
  // Clear out the "history list"
  while(prev_level)
    {
      flat_frame *next=prev_level->next;
      delete prev_level;
      prev_level=next;
    }

  if(selected!=end)
    selected->unhighlighted(this);

  if(root)
    delete root;

  root=_root;

  if(root)
    {
      if(showroot)
	{
	  vs_tree_root_iterator *realbegin=new vs_tree_root_iterator(_root);
	  // NOTE: realbegin will be DELETED when it is assigned to begin,
	  // because the temporary that wraps it will be destroyed.
	  // This is Just Plain Evil (probably conversion from vs_levelrefs
	  // to vs_treeiterators shouldn't be allowed) but the workaround is
	  // here: reference its end() routine *before* we assign it.

	  end=realbegin->end();
	  begin=realbegin; // now realbegin is INVALID!!
	}
      else
	{
	  begin=_root->begin();
	  end=_root->end();
	}

      top=begin;
    }
  else
    {
      top=begin=end=new vs_tree_root_iterator(NULL);
    }

  selected=top;
  while(selected!=end && !selected->get_selectable())
    selected++;
  if(selected!=end)
    selected->highlighted(this);
}

void vs_tree::sync_bounds()
  // As I said: yuck!
{
  begin=root->begin();
  if(top==end)
    top=begin;
  if(selected==end)
    selected=begin;
  end=root->end();
}

int vs_tree::line_of(vs_treeiterator item)
  // Returns the Y coordinate of the given item.  (so we have to count
  // from 1)
{
  int j;
  vs_treeiterator i=top;
  if(item==top)
    return 1;

  j=1;
  do {
    if(hierarchical)
      ++i;
    else
      i.move_forward_level();

    ++j;

    if(i==item)
      return j;
  } while(i!=end);

  i=top;
  j=1;
  do {
    if(hierarchical)
      --i;
    else
      i.move_backward_level();

    --j;

    if(i==item)
      return j;
  } while(i!=begin);

  // Only happens if the iterator isn't in the visible range at all.
  abort();
}

bool vs_tree::item_visible(vs_treeiterator pkg)
{
  int width,height;
  vs_treeiterator i=top;

  getmaxyx(height,width);

  if(!hierarchical)
    --height;

  while(height>0 && i!=pkg && i!=end)
    {
      --height;
      ++i;
    }

  return height>0 && i!=end;
}

void vs_tree::set_selection(vs_treeiterator to)
{
  // Expand all its parents so that it's possible to make it visible.
  vs_treeiterator curr = to;
  while(!curr.is_root())
    {
      curr = curr.get_up();
      curr.expand();
    }

  // Expand the root as well if necessary.
  if(curr != to)
    curr.expand();

  if(item_visible(to))
    {
      if(selected!=end)
	selected->unhighlighted(this);
      selected=to;
      if(selected!=end)
	selected->highlighted(this);

      vscreen_update();
    }
  else
    {
      int height = getmaxy();
      if(height == 0)
	{
	  selected = top = to;
	  return;
	}

      // Give up and just directly determine the line of 'to'.
      int l = line_of(to);

      while(l < 1)
	{
	  assert(top != end);

	  if(hierarchical)
	    --top;
	  else
	    top.move_backward_level();

	  ++l;
	}

      while(l > height)
	{
	  assert(top != end);

	  if(hierarchical)
	    ++top;
	  else
	    top.move_forward_level();

	  --l;
	}

      if(selected != to)
	{
	  if(selected != end)
	    selected->unhighlighted(this);

	  if(to != end)
	    to->highlighted(this);
	}

      selected = to;

      vscreen_update();
    }
}

bool vs_tree::get_cursorvisible()
{
  return (root != NULL && selected != end && selected->get_selectable());
}

point vs_tree::get_cursorloc()
{
  if(root == NULL)
    return point(0, 0);
  else if(selected==end || !selected->get_selectable())
    return point(0,0);
  else
    return point(0, hierarchical?line_of(selected)-1:line_of(selected));
}

void vs_tree::line_down()
{
  if(root == NULL)
    return;

  int width,height;
  getmaxyx(height,width);

  if(!hierarchical)
    --height;

  vs_treeiterator orig = selected, prevtop = top;

  int newline = line_of(selected);
  int scrollcount = 0;
  bool moved = false;

  while(selected != end &&
	scrollcount < 1 &&
	(!moved || !selected->get_selectable()))
    {
      if(hierarchical)
	++selected;
      else
	selected.move_forward_level();

      ++newline;
      moved = true;

      // If we fell off the end of the screen and not off the end of
      // the list, scroll the screen forward.
      if(newline > height && selected != end)
	{
	  if(hierarchical)
	    ++top;
	  else
	    top.move_forward_level();

	  --newline;
	  ++scrollcount;
	}
    }

  if(selected == end)
    {
      if(hierarchical)
	--selected;
      else
	selected.move_backward_level();

      --newline;
    }

  if(orig != selected)
    {
      if(orig != end)
	orig->unhighlighted(this);

      if(selected != end)
	selected->highlighted(this);
    }

  vscreen_update();
}

void vs_tree::set_hierarchical(bool _hierarchical)
{
  if(_hierarchical!=hierarchical)
    {
      hierarchical=_hierarchical;

      if(_hierarchical)
	{
	  while(prev_level && prev_level->next)
	    {
	      flat_frame *next=prev_level->next;
	      delete prev_level;
	      prev_level=next;
	    }

	  if(prev_level)
	    {
	      top=prev_level->top;
	      begin=prev_level->begin;
	      end=prev_level->end;
	      selected=prev_level->selected;

	      delete prev_level;
	      prev_level=NULL;
	    }
	}

      vscreen_update();
    }
}

void vs_tree::highlight_current()
{
  if(root != NULL && selected != end)
    selected->highlighted(this);
}

void vs_tree::unhighlight_current()
{
  if(root != NULL && selected != end)
    selected->unhighlighted(this);
}

void vs_tree::line_up()
{
  if(root == NULL)
    return;

  int width,height;
  getmaxyx(height,width);

  if(!hierarchical)
    --height;

  vs_treeiterator orig=selected;

  bool moved = false;
  int scrollcount = 0;

  while(selected != begin &&
	scrollcount < 1 &&
	(!moved || !selected->get_selectable()))
    {
      if(selected == top)
	{
	  if(hierarchical)
	    --top;
	  else
	    top.move_backward_level();

	  ++scrollcount;
	}

      if(hierarchical)
	--selected;
      else
	selected.move_backward_level();
      moved = true;
    }

  // Handle the special case where the first element of the tree is
  // non-selectable.
  if(selected == begin && !selected->get_selectable())
    {
      while(selected != end && !selected->get_selectable())
	++selected;

      if(line_of(selected) >= height)
	selected = begin;
    }

  if(orig != selected)
    {
      if(orig != end)
	orig->unhighlighted(this);

      if(selected != end)
	selected->highlighted(this);
    }

  vscreen_update();
}

void vs_tree::page_down()
{
  if(root == NULL)
    return;

  int width,height;
  getmaxyx(height,width);

  if(!hierarchical)
    --height;

  int count=height;
  vs_treeiterator newtop=top;
  while(count>0 && newtop!=end)
    {
      if(hierarchical)
	++newtop;
      else
	newtop.move_forward_level();
      count--;
    }

  if(count==0 && newtop!=end)
    {
      int l=0;
      (*selected).unhighlighted(this);
      selected=top=newtop;
      while(l<height && selected!=end && !selected->get_selectable())
	if(hierarchical)
	  ++selected;
	else
	  selected.move_forward_level();
      if(l==height || selected==end)
	selected=top;
      (*selected).highlighted(this);
      vscreen_update();
    }
}

void vs_tree::page_up()
{
  if(root == NULL)
    return;

  int width,height;
  getmaxyx(height,width);

  if(!hierarchical)
    --height;

  int count=height;
  vs_treeiterator newtop=top;
  while(count>0 && newtop!=begin)
    {
      if(hierarchical)
	--newtop;
      else
	newtop.move_backward_level();
      count--;
    }

  if(newtop!=top)
    {
      int l=0;
      if(selected!=end)
	(*selected).unhighlighted(this);
      selected=top=newtop;
      while(l<height && selected!=end && !selected->get_selectable())
	if(hierarchical)
	  ++selected;
	else
	  selected.move_forward_level();
      if(l==height || selected==end)
	selected=top;

      if(selected!=end)
	(*selected).highlighted(this);
      vscreen_update();
    }
}

void vs_tree::jump_to_begin()
{
  if(root == NULL)
    return;

  int width,height;
  getmaxyx(height,width);

  if(!hierarchical)
    --height;

  int l=0;
  vs_treeiterator prev=selected;

  if(selected!=end)
    selected->unhighlighted(this);

  selected=begin;
  while(l<height && selected!=end && !selected->get_selectable())
    if(hierarchical)
      ++selected;
    else
      selected.move_forward_level();
  if(l==height || selected==end)
    selected=begin;

  if(selected!=end)
    selected->highlighted(this);

  if(top!=begin)
    top=begin;

  vscreen_update();
}

void vs_tree::jump_to_end()
{
  if(root == NULL)
    return;

  int width,height;
  getmaxyx(height,width);

  if(!hierarchical)
    --height;

  int l=-1;
  vs_treeiterator last=end,newtop=end,prev=selected;
  if(hierarchical)
    --last;
  else
    last.move_backward_level();
  while(newtop!=begin && newtop!=top && height>0)
    {
      if(hierarchical)
	--newtop;
      else
	newtop.move_backward_level();
      --height;
      l++;
    }

  if(selected!=end)
    selected->unhighlighted(this);

  selected=last;
  while(l>=0 && selected!=end && !selected->get_selectable())
    {
      if(hierarchical)
	--selected;
      else
	selected.move_backward_level();
      l--;
    }
  if(selected==end && l<0)
    selected=last;

  if(selected!=end)
    selected->highlighted(this);

  if(newtop!=top)
    top=newtop;

  vscreen_update();
}

void vs_tree::level_line_up()
{
  if(root == NULL)
    return;

  vs_treeiterator tmp=selected;
  tmp.move_backward_level();
  if(tmp!=end)
    set_selection(tmp);
}

void vs_tree::level_line_down()
{
  if(root == NULL)
    return;

  vs_treeiterator tmp=selected;
  tmp.move_forward_level();
  if(tmp!=end)
    set_selection(tmp);
}

bool vs_tree::handle_key(const key &k)
{
  // umm...
  //width++;
  //height++;

  if(selected!=vs_treeiterator(NULL))
    {
      if(root != NULL && hierarchical && bindings->key_matches(k, "Parent"))
	{
	  if(!selected.is_root())
	    set_selection(selected.get_up());
	}
      else if(root != NULL && !hierarchical && prev_level && bindings->key_matches(k, "Left"))
	{
	  selected->unhighlighted(this);

	  top=prev_level->top;
	  begin=prev_level->begin;
	  end=prev_level->end;
	  selected=prev_level->selected;

	  flat_frame *next=prev_level->next;
	  delete prev_level;
	  prev_level=next;

	  selected->highlighted(this);

	  vscreen_update();
	}
      else if(root != NULL && !hierarchical &&
	      selected!=end && selected->get_selectable() &&
	      selected->begin()!=selected->end() &&
	      (bindings->key_matches(k, "Right") ||
	       bindings->key_matches(k, "Confirm")))
	{
	  selected->unhighlighted(this);
	  prev_level=new flat_frame(begin, end, top, selected, prev_level);

	  begin=selected->begin();
	  end=selected->end();
	  top=begin;
	  selected=begin;

	  selected->highlighted(this);

	  vscreen_update();
	}
      else if(bindings->key_matches(k, "Down"))
	line_down();
      else if(bindings->key_matches(k, "Up"))
	line_up();
      else if(bindings->key_matches(k, "NextPage"))
	page_down();
      else if(bindings->key_matches(k, "PrevPage"))
	page_up();
      else if(bindings->key_matches(k, "Begin"))
	jump_to_begin();
      else if(bindings->key_matches(k, "End"))
	jump_to_end();
      else if(bindings->key_matches(k, "LevelUp"))
	level_line_up();
      else if(bindings->key_matches(k, "LevelDown"))
	level_line_down();
      /*else if(bindings->key_matches(ch, "Search"))
	{
	  vs_statusedit *ed=new vs_statusedit("Search for: ");
	  ed->entered.connect(sigc::mem_fun(this, &vs_tree::search_for));
	  add_widget(ed);
	  vscreen_update();
	}
      else if(bindings->key_matches(ch, "ReSearch"))
      search_for("");*/
      else
	{
	  if(root != NULL && selected!=end && selected->get_selectable() &&
	     selected->dispatch_key(k, this))
	    vscreen_update();
	  else
	    return vscreen_widget::handle_key(k);
	}
      return true;
    }
  return false;
}

void vs_tree::search_for(vs_tree_search_func &matches)
{
  if(root == NULL)
    return;

  vs_treeiterator curr((selected==vs_treeiterator(NULL))?begin:selected, hierarchical),
    start(curr);
  // Make an iterator that ignores all the rules >=)

  if(curr!=end)
    {
      if(hierarchical)
	++curr;
      else
	curr.move_forward_level();

      // Don't forget this case!
      if(curr==end)
	curr=begin;
    }

  while(curr!=start && !matches(*curr))
    {
      if(hierarchical)
	++curr;
      else
	curr.move_forward_level();

      if(curr==end)
	curr=begin;
    }

  if(curr==start)
    beep();
  else
    {
      set_selection(curr);
      vscreen_update();
    }
}

void vs_tree::search_back_for(vs_tree_search_func &matches)
{
  if(root == NULL)
    return;

  vs_treeiterator curr((selected == vs_treeiterator(NULL))
		       ? begin : selected, hierarchical),
    start(curr);

  // Skip the starting location, cycling to the end.
  if(curr != begin)
    {
      if(hierarchical)
	--curr;
      else
	curr.move_backward_level();
    }
  else
    {
      if(hierarchical)
	{
	  curr = end;
	  --curr;
	}
      else
	{
	  vs_treeiterator curr2 = curr;
	  curr2.move_forward_level();

	  while(curr2 != curr)
	    {
	      curr = curr2;
	      curr2.move_forward_level();
	    }
	}
    }

  while(curr != start && !matches(*curr))
    {
      // Code duplication alert
      if(curr != begin)
	{
	  if(hierarchical)
	    --curr;
	  else
	    curr.move_backward_level();
	}
      else
	{
	  if(hierarchical)
	    {
	      curr = end;
	      --curr;
	    }
	  else
	    {
	      vs_treeiterator curr2 = curr;
	      curr2.move_forward_level();

	      while(curr2 != curr)
		{
		  curr = curr2;
		  curr2.move_forward_level();
		}
	    }
	}
    }

  if(curr == start)
    beep();
  else
    {
      set_selection(curr);
      vscreen_update();
    }
}

void vs_tree::paint(const style &st)
{
  if(root == NULL)
    return;

  int width,height;
  int selectedln=line_of(selected);

  getmaxyx(height,width);

  if(selectedln>height)
    {
      while(selected!=top && selectedln>height)
	{
	  if(hierarchical)
	    ++top;
	  else
	    top.move_forward_level();
	  selectedln--;
	}
    }
  else
    {
      while(selected!=top && selectedln<0)
	{
	  if(hierarchical)
	    --top;
	  else
	    top.move_backward_level();
	  selectedln++;
	}
    }

  //if(selected!=end && selected->get_selectable())
  //selected->highlighted(this);
  // Some classes need this to update display stuff properly.  For instance,
  // when a new pkg_tree is created, its 'update the status line' signal
  // won't be properly called without this.

  vs_treeiterator i=top;
  int y=0;

  if(!hierarchical && y<height)
    {
      wstring todisp;

      // Uh...I'd rather use the iterators to do this..
      flat_frame *curr=prev_level;
      while(curr)
	{
	  if(todisp.empty())
	    todisp=curr->selected->label()+todisp;
	  else
	    todisp=curr->selected->label()+(L"::"+todisp);
	  curr=curr->next;
	}

      if(todisp.empty())
	todisp=transcode(_("TOP LEVEL"));

      while(todisp.size()<(unsigned) width)
	todisp+=L" ";

      apply_style(st+get_style("Header"));
      mvaddnstr(y, 0, todisp.c_str(), width);

      ++y;
    }

  // FIXME: this is a hack around nasty edge cases.  All the tree code needs
  //       a rewrite.
  vs_treeiterator prev=i;
  while(y<height && i!=end)
    {
      vs_treeitem *curr=&*i;

      style curr_st;

      if(get_isfocussed() && i==selected && i->get_selectable())
	curr_st = st+curr->get_highlight_style();
      else
	curr_st = st+curr->get_normal_style();

      apply_style(curr_st);
      curr->paint(this, y, hierarchical, curr_st);

      if(hierarchical)
	++i;
      else
	i.move_forward_level();
      y++;

      // FIXME: this is a hack.
      if(i==prev) // If we hit the end, it will refuse to advance.
	break;
      prev=i;
    }
}

void vs_tree::dispatch_mouse(short id, int x, int y, int z, mmask_t bstate)
{
  if(root == NULL)
    return;

  if(!hierarchical)
    --y;

  vs_treeiterator i=top;
  while(y>0 && i!=end)
    {
      if(hierarchical)
	++i;
      else
	i.move_forward_level();

      --y;
    }

  if(y==0 && i!=end)
    {
      set_selection(i);

      i->dispatch_mouse(id, x, bstate, this);
    }
}

void vs_tree::init_bindings()
{
  bindings=new keybindings(&global_bindings);
}
