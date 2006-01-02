// vs_multiplex.cc
//
//  Copyright 1999-2006 Daniel Burrows
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

#include "vscreen.h"
#include "vs_multiplex.h"

#include "config/colors.h"

#include <sigc++/adaptors/bind.h>
#include <sigc++/functors/mem_fun.h>

#include <generic/util/eassert.h>

using namespace std;

vs_multiplex::vs_multiplex(bool _show_tabs)
  :vs_passthrough(), visible_child(children.end()), show_tabs(_show_tabs)
{
  do_layout.connect(sigc::mem_fun(*this, &vs_multiplex::layout_me));

  focussed.connect(sigc::mem_fun(*this, &vs_multiplex::got_focus));
  unfocussed.connect(sigc::mem_fun(*this, &vs_multiplex::lost_focus));
}

vs_multiplex::~vs_multiplex()
{
  eassert(children.empty());
}

void vs_multiplex::destroy()
{
  vs_widget_ref tmpref(this);

  while(!children.empty())
    children.front().w->destroy();

  vs_passthrough::destroy();
}

bool vs_multiplex::tabs_visible() const
{
  if(!show_tabs)
    return false;

  bool one_visible=false;

  for(list<child_info>::const_iterator i=children.begin();
      i!=children.end();
      i++)
    if(i->w->get_visible())
      {
	if(!one_visible)
	  one_visible=true;
	else
	  return true;
      }

  return false;
}

void vs_multiplex::set_show_tabs(bool shown)
{
  show_tabs = shown;
  vscreen_queuelayout();
}

int vs_multiplex::width_request()
{
  vs_widget_ref tmpref(this);

  int rval=0;

  for(list<child_info>::iterator i=children.begin();
      i!=children.end(); ++i)
    if(i->w->get_visible())
      rval=max(rval, i->w->width_request());

  return rval;
}

int vs_multiplex::height_request(int width)
{
  vs_widget_ref tmpref(this);

  int rval=0;

  for(list<child_info>::iterator i=children.begin();
      i!=children.end(); ++i)
    if(i->w->get_visible())
      rval=max(rval, i->w->height_request(width));

  if(tabs_visible())
    return rval+1;
  else
    return rval;
}


void vs_multiplex::layout_me()
{
  vs_widget_ref tmpref(this);

  if(visible_child!=children.end())
    {
      if(tabs_visible())
	visible_child->w->alloc_size(0, 1, getmaxx(), getmaxy()-1);
      else
	visible_child->w->alloc_size(0, 0, getmaxx(), getmaxy());
    }
}

void vs_multiplex::paint(const style &st)
{
  vs_widget_ref tmpref(this);

  if(tabs_visible())
    {
      int visible_children=0;

      for(list<child_info>::iterator i=children.begin();
	  i!=children.end(); ++i)
	if(i->w->get_visible())
	  ++visible_children;

      eassert(visible_children>0);

      int remaining_w=getmaxx();
      move(0, 0);

      const style tab_style=get_style("MultiplexTab");
      const style tabhighlighted_style=get_style("MultiplexTabHighlighted");

      for(list<child_info>::iterator i=children.begin();
	  i!=children.end(); ++i)
	if(i->w->get_visible()) // draw one
	  {
	    if(i == visible_child)
	      apply_style(tabhighlighted_style);
	    else
	      apply_style(tab_style);

	    int thisw=remaining_w/visible_children;
	    --visible_children;
	    remaining_w-=thisw;

	    const wstring &title = i->title;
	    int titlew = wcswidth(title.c_str(), title.size());
	    unsigned int leftpadding = (titlew<=thisw) ? (thisw-titlew)/2 : 0;

	    while(leftpadding>0)
	      {
		add_wch(L' ');
		int chw=wcwidth(L' ');

		leftpadding-=chw;
		thisw-=chw;
	      }

	    size_t loc=0;
	    while(thisw>0 && loc<title.size())
	      {
		wchar_t ch=title[loc];
		add_wch(ch);
		thisw-=wcwidth(ch);
		++loc;
	      }

	    while(thisw>0)
	      {
		add_wch(L' ');
		thisw-=wcwidth(L' ');
	      }
	  }
      eassert(visible_children == 0);
    }

  if(visible_child!=children.end())
    visible_child->w->display(st);
}

void vs_multiplex::dispatch_mouse(short id, int x, int y, int z,
				  mmask_t bstate)
{
  vs_widget_ref tmpref(this);

  if(tabs_visible() && y == 0)
    {
      // FIXME: duplicated code from above, is there a good way to
      // unduplicate it? (eg, an "iterator" for child locations)
      int visible_children=0;

      for(list<child_info>::iterator i=children.begin();
	  i!=children.end(); ++i)
	if(i->w->get_visible())
	  ++visible_children;

      eassert(visible_children>0);

      int startx=0;
      int remaining_w=getmaxx();

      for(list<child_info>::iterator i=children.begin();
	  i!=children.end(); ++i)
	if(i->w->get_visible())
	  {
	    int thisw=remaining_w/visible_children;
	    --visible_children;
	    remaining_w-=thisw;

	    if(x>=startx && x<startx+thisw)
	      {
		visible_child=i;
		vscreen_queuelayout();
		return;
	      }

	    startx+=thisw;
	  }

      eassert(x<0 || x>=getmaxx());
      eassert(visible_children == 0);
    }
  else if(visible_child!=children.end())
    visible_child->w->dispatch_mouse(id,
				     x-visible_child->w->get_startx(),
				     y-visible_child->w->get_starty(),
				     z, bstate);
}

void vs_multiplex::got_focus()
{
  vs_widget_ref tmpref(this);

  if(visible_child!=children.end())
    visible_child->w->focussed();
}

void vs_multiplex::lost_focus()
{
  vs_widget_ref tmpref(this);

  if(visible_child!=children.end())
    visible_child->w->unfocussed();
}

void vs_multiplex::show_all()
{
  vs_widget_ref tmpref(this);

  show();

  if(visible_child!=children.end())
    visible_child->w->show_all();
}

void vs_multiplex::show_widget(const vs_widget_ref &w)
{
  vs_widget_ref tmpref(this);

  eassert(!children.empty());

  list<child_info>::iterator new_visible=visible_child;

  if(new_visible!=children.end())
    ++new_visible;
  else
    new_visible=children.begin();

  while(new_visible!=visible_child)
    {
      if(new_visible==children.end())
	new_visible=children.begin();
      else if(new_visible->w==w)
	break;
      else
	++new_visible;
    }

  if(visible_child!=children.end() && get_isfocussed())
    visible_child->w->unfocussed();

  list<child_info>::iterator old_visible = visible_child;
  visible_child=new_visible;

  if(visible_child!=children.end() && get_isfocussed())
    visible_child->w->focussed();

  if(visible_child != old_visible)
    {
      cycled();
      vscreen_queuelayout();
      vscreen_update();
    }
}

void vs_multiplex::hide_widget(const vs_widget_ref &w)
{
  vs_widget_ref tmpref(this);

  eassert(!children.empty());

  if(visible_child!=children.end() && visible_child->w==w)
    {
      list<child_info>::iterator new_visible=visible_child;

      if(new_visible!=children.begin())
	--new_visible;
      else
	{
	  new_visible=children.end();
	  --new_visible;
	}

      while(new_visible!=visible_child)
	{
	  if(new_visible->w->get_visible())
	    break;
	  else if(new_visible==children.begin())
	    {
	      new_visible=children.end();
	      --new_visible;
	    }
	  else
	    --new_visible;
	}

      if(visible_child!=children.end() && get_isfocussed())
	visible_child->w->unfocussed();

      list<child_info>::iterator old_visible = visible_child;

      if(new_visible==visible_child)
	visible_child=children.end();
      else
	visible_child=new_visible;

      if(visible_child!=children.end() && get_isfocussed())
	visible_child->w->focussed();


      // Since we just hid the previously-visible child, this MUST be
      // the case.
      eassert(visible_child != old_visible);

      cycled();
      vscreen_queuelayout();
      vscreen_update();
    }
}

void vs_multiplex::show_widget_bare(vscreen_widget &w)
{
  show_widget(vs_widget_ref(&w));
}

void vs_multiplex::hide_widget_bare(vscreen_widget &w)
{
  hide_widget(vs_widget_ref(&w));
}

vs_widget_ref vs_multiplex::get_focus()
{
  return visible_child==children.end()?NULL:visible_child->w;
}

void vs_multiplex::rem_widget(const vs_widget_ref &w)
{
  vs_widget_ref tmpref(this);

  hide_widget(w);

  for(list<child_info>::iterator i=children.begin(); i!=children.end(); i++)
    {
      if(i->w==w)
	{
	  eassert(w->get_owner().unsafe_get_ref() == this);

	  w->set_owner(NULL);

	  break;
	}
    }

  list<child_info>::iterator i=children.begin(),j;
  while(i!=children.end())
    {
      j=i;
      ++j;

      if(i->w==w)
	children.erase(i);

      i=j;
    }

  vscreen_queuelayout();
  vscreen_update();
}

void vs_multiplex::add_widget(const vs_widget_ref &w,
			      const wstring &title)
{
  vs_widget_ref tmpref(this);

  w->shown_sig.connect(sigc::bind(sigc::mem_fun(*this, &vs_multiplex::show_widget_bare), w.weak_ref()));
  w->hidden_sig.connect(sigc::bind(sigc::mem_fun(*this, &vs_multiplex::hide_widget_bare), w.weak_ref()));

  children.push_back(child_info(w, title));
  w->set_owner(this);

  if(w->get_visible())
    show_widget(w);
}

void vs_multiplex::add_widget(const vs_widget_ref &w)
{
  add_widget(w, L"Untitled");
}

void vs_multiplex::add_widget_after(const vs_widget_ref &w,
				    const vs_widget_ref &after,
				    const wstring &title)
{
  vs_widget_ref tmpref(this);

  for(list<child_info>::iterator i=children.begin();
      i!=children.end();
      i++)
    {
      if(i->w==after)
	{
	  ++i;

	  w->shown_sig.connect(sigc::bind(sigc::mem_fun(*this, &vs_multiplex::show_widget_bare), w.weak_ref()));
	  w->hidden_sig.connect(sigc::bind(sigc::mem_fun(*this, &vs_multiplex::hide_widget_bare), w.weak_ref()));

	  children.insert(i, child_info(w, title));
	  w->set_owner(this);

	  if(w->get_visible())
	    show_widget(w);

	  return;
	}
    }

  // Fallback to something reasonable.  (or just abort? dunno)
  add_widget(w);
}

void vs_multiplex::add_widget_after(const vs_widget_ref &w,
				    const vs_widget_ref &after)
{
  add_widget_after(w, after, L"Untitled");
}

void vs_multiplex::cycle_forward()
{
  vs_widget_ref tmpref(this);

  if(!children.empty())
    {
      list<child_info>::iterator new_visible=visible_child;

      if(new_visible!=children.end())
	++new_visible;
      else
	new_visible=children.begin();

      while(new_visible!=visible_child)
	{
	  if(new_visible==children.end())
	    new_visible=children.begin();
	  else if(new_visible->w->get_visible())
	    break;
	  else
	    ++new_visible;
	}

      list<child_info>::iterator old_visible = visible_child;

      if(visible_child!=children.end() && get_isfocussed())
	visible_child->w->unfocussed();

      visible_child=new_visible;

      if(visible_child!=children.end() && get_isfocussed())
	visible_child->w->focussed();


      if(visible_child != old_visible)
	{
	  cycled();
	  vscreen_queuelayout();
	  vscreen_update();
	}
    }
}

void vs_multiplex::cycle_backward()
{
  vs_widget_ref tmpref(this);

  if(!children.empty())
    {
      list<child_info>::iterator new_visible=visible_child;

      if(new_visible!=children.begin())
	--new_visible;
      else
	{
	  new_visible=children.end();
	  --new_visible;
	}

      while(new_visible!=visible_child)
	{
	  if(new_visible->w->get_visible())
	    break;
	  else if(new_visible==children.begin())
	    {
	      new_visible=children.end();
	      --new_visible;
	    }
	  else
	    --new_visible;
	}

      list<child_info>::iterator old_visible = visible_child;

      if(visible_child!=children.end() && get_isfocussed())
	visible_child->w->unfocussed();

      visible_child=new_visible;

      if(visible_child!=children.end() && get_isfocussed())
	visible_child->w->focussed();

      if(visible_child != old_visible)
	{
	  cycled();

	  vscreen_queuelayout();
	  vscreen_update();
	}
    }
}

vs_widget_ref vs_multiplex::visible_widget()
{
  if(visible_child!=children.end())
    return visible_child->w;
  else
    return NULL;
}

unsigned int vs_multiplex::num_children()
{
  return children.size();
}

unsigned int vs_multiplex::num_visible()
{
  unsigned int n=0;

  for(list<child_info>::iterator i=children.begin();
      i!=children.end(); ++i)
    if(i->w->get_visible())
      ++n;

  return n;
}
