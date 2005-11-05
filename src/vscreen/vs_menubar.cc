// vs_menubar.cc
//
//   Copyright (C) 2000-2005 Daniel Burrows
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

#include "vs_container.h"
#include "vs_menubar.h"
#include "vs_menu.h"
#include "vscreen.h"

#include "config/colors.h"

#include <sigc++/adaptors/bind.h>
#include <sigc++/functors/mem_fun.h>

using namespace std;

keybindings *vs_menubar::bindings=NULL;

vs_menubar::vs_menubar(bool _always_visible)
  :vs_container(), startloc(0), active(false),
   always_visible(_always_visible), curloc(0), subwidget(NULL)
{
  do_layout.connect(sigc::mem_fun(*this, &vs_menubar::layout_me));

  focussed.connect(sigc::mem_fun(*this, &vs_menubar::got_focus));
  unfocussed.connect(sigc::mem_fun(*this, &vs_menubar::lost_focus));
}

vs_menubar::~vs_menubar()
{
  eassert(!subwidget.valid());
  eassert(items.empty());
  eassert(active_menus.empty());
}

vs_widget_ref vs_menubar::get_active_widget()
{
  return subwidget;
}

void vs_menubar::destroy()
{
  vs_widget_ref tmpref(this);

  if(subwidget.valid())
    subwidget->destroy();
  eassert(!subwidget.valid());

  // ew.  You see, we need to individually destroy the subwidgets, but
  // doing so will cause them to be removed, so we can't iterate over
  // the main list...
  vector<item> curr_items(items);

  for(vector<item>::const_iterator i = curr_items.begin();
      i != curr_items.end(); ++i)
    i->menu->destroy();

  eassert(items.empty());
  eassert(active_menus.empty());

  vs_container::destroy();
}

void vs_menubar::got_focus()
{
  vs_widget_ref tmpref(this);

  vs_widget_ref w=get_focus();
  if(w.valid())
    w->focussed();
}

void vs_menubar::lost_focus()
{
  vs_widget_ref tmpref(this);

  vs_widget_ref w=get_focus();
  if(w.valid())
    w->unfocussed();
}

bool vs_menubar::focus_me()
{
  if(active)
    return true;
  else if(subwidget.valid() && subwidget->focus_me())
    return true;
  else
    return vscreen_widget::focus_me();
}

vs_widget_ref vs_menubar::get_focus()
{
  if(active)
    {
      if(!active_menus.empty())
	return active_menus.front();
      else
	return NULL;
    }
  else if(subwidget.valid())
    return subwidget;
  else
    return NULL;
  // NB: could just end with 'return subwidget' but this makes the
  // order more explicit.
}

bool vs_menubar::get_cursorvisible()
{
  vs_widget_ref w = get_focus();
  return (w.valid() && w->get_cursorvisible()) ||
    (!w.valid() && active);
}

point vs_menubar::get_cursorloc()
{
  vs_widget_ref w = get_focus();

  if(w.valid())
    {
      point p=w->get_cursorloc();
      p.x+=w->get_startx();
      p.y+=w->get_starty();
      return p;
    }
  else if(active)
    return point(get_menustart(curloc), 0);
  else
    return point(0, 0);
}

int vs_menubar::get_menustart(itemlist::size_type idx) const
{
  int rval = 0;

  if(idx >= startloc)
    {
      for(itemlist::size_type i = startloc; i < idx; ++i)
	{
	  const wstring &title = items[i].title;
	  rval += wcswidth(title.c_str(), title.size());
	}
    }
  else
    {
      for(itemlist::size_type i = idx; i < startloc; ++i)
	{
	  const wstring &title = items[i].title;
	  rval -= wcswidth(title.c_str(), title.size());
	}
    }

  return rval;
}

void vs_menubar::update_x_start()
{
  if(!active)
    startloc = 0;
  else if(curloc < startloc)
    startloc = curloc;
  else
    {
      int width = get_width();
      if(width == 0)
	return;


      int start_x = get_menustart(startloc);

      const wstring &curr_title = items[curloc].title;
      int curr_x = get_menustart(curloc);
      int curr_width = wcswidth(curr_title.c_str(),
				curr_title.size());

      if(width < curr_width)
	while(curr_x >= start_x + width)
	  {
	    const wstring &title = items[startloc].title;
	    start_x += wcswidth(title.c_str(), title.size());
	    ++startloc;
	  }
      else
	while(curr_x + curr_width > start_x + width)
	  {
	    const wstring &title = items[startloc].title;
	    start_x += wcswidth(title.c_str(), title.size());
	    ++startloc;
	  }
    }
}

void vs_menubar::append_item(const wstring &title,
			     const vs_menu_ref &menu)
{
  vs_widget_ref tmpref(this);

  items.push_back(item(L' '+title+L' ', menu));

  menu->shown_sig.connect(sigc::bind(sigc::mem_fun(*this, &vs_menubar::show_menu_bare), menu.weak_ref()));
  menu->hidden_sig.connect(sigc::bind(sigc::mem_fun(*this, &vs_menubar::hide_menu_bare), menu.weak_ref()));
  menu->menus_goaway.connect(sigc::mem_fun(*this, &vs_menubar::disappear));
  menu->set_owner(this);

  vscreen_update();
}

void vs_menubar::set_subwidget(const vs_widget_ref &w)
{
  vs_widget_ref tmpref(this);

  if(subwidget.valid())
    {
      subwidget->set_owner(NULL);
      subwidget->unfocussed();
    }

  subwidget=w;

  if(subwidget.valid())
    {
      subwidget->set_owner(this);
      subwidget->focussed();
    }

  vscreen_queuelayout();
}

void vs_menubar::show_all()
{
  vs_widget_ref tmpref(this);

  if(subwidget.valid())
    subwidget->show_all();
}

void vs_menubar::add_widget(const vs_widget_ref &w)
{
  vs_widget_ref tmpref(this);

  eassert(!subwidget.valid());

  set_subwidget(w);
}

void vs_menubar::rem_widget(const vs_widget_ref &w)
{
  vs_widget_ref tmpref(this);

  if(w == subwidget)
    set_subwidget(NULL);
  else
    {
      eassert(w->get_owner().unsafe_get_ref() == this);

      bool found = false;

      // make a new strong reference
      vs_widget_ref w2 = w;

      // hrm.
      for(vector<item>::iterator i = items.begin();
	  i != items.end(); ++i)
	{
	  if(i->menu == w2)
	    {
	      found = true;
	      items.erase(i);
	      break;
	    }
	}

      eassert(found);

      active_menus.remove(w2);

      w2->set_owner(NULL);
    }
}

int vs_menubar::width_request()
{
  vs_widget_ref tmpref(this);

  int w=0;

  // Calculate the size of the bar itself.
  for(itemlist::size_type i=0; i<items.size(); i++)
    {
      const wstring &title=items[i].title;

      w+=wcswidth(title.c_str(), title.size());
    }

  // Expand the width as needed to account for active menus.
  for(activemenulist::iterator i=active_menus.begin(), num=0;
      i!=active_menus.end();
      i++, num++)
    {
      int menux=0;

      // Calculate the starting X location of this menu.
      for(itemlist::size_type j=0; j<items.size(); j++)
	{
	  if(items[j].menu==*i)
	    break;

	  const wstring &title=items[j].title;

	  menux+=wcswidth(title.c_str(), title.size());
	}

      // Now expand our width request.
      w=max(w, menux+(*i)->width_request());
    }

  // Expand the width to account for the subwidget.
  if(subwidget.valid())
    w=max(w, subwidget->width_request());

  return w;
}

// TODO: What if the width is insufficient: should I scroll left/right
// or wrap to the next line?
int vs_menubar::height_request(int w)
{
  vs_widget_ref tmpref(this);

  int h=always_visible?1:0;

  for(activemenulist::iterator i=active_menus.begin(), num=0;
      i!=active_menus.end();
      i++, num++)
    h=max(h, 1+(*i)->height_request(w));

  if(subwidget.valid())
    {
      int subwidget_h=subwidget->height_request(w);

      if(always_visible)
	subwidget_h+=1;

      h=max(h, subwidget_h);
    }

  return h;
}

void vs_menubar::layout_me()
{
  vs_widget_ref tmpref(this);

  update_x_start();

  // Find the starting X location of each active menu.
  for(activemenulist::iterator i=active_menus.begin();
      i!=active_menus.end();
      i++)
    {
      int menuloc = -1;

      for(itemlist::size_type j = 0; j < items.size(); j++)
	{
	  if(items[j].menu == *i)
	    {
	      menuloc = j;
	      break;
	    }
	}

      int menux = get_menustart(menuloc);

      int req_w=(*i)->width_request();

      if(menux < 0)
	menux = 0;
      else if(getmaxx() < menux + req_w)
	{
	  if(getmaxx() >= req_w)
	    menux = getmaxx() - req_w;
	  else
	    {
	      menux = 0;
	      req_w = getmaxx();
	    }
	}

      int req_h = (*i)->height_request(req_w);

      if(getmaxy() < 1 + req_h)
	req_h = getmaxy()-1;

      (*i)->alloc_size(menux,
		       1,
		       req_w,
		       req_h);
    }

  if(subwidget.valid())
    subwidget->alloc_size(0,
			  always_visible?1:0,
			  getmaxx(),
			  always_visible?getmaxy()-1:getmaxy());
}

void vs_menubar::show_menu(const vs_menu_ref &w)
{
  vs_widget_ref tmpref(this);

  if(active)
    {
      vs_widget_ref old_focus=get_focus();

      for(activemenulist::iterator i=active_menus.begin();
	  i!=active_menus.end();
	  i++)
	eassert(w != *i);

      if(old_focus.valid())
	old_focus->unfocussed();

      active_menus.push_front(w);

      w->focussed();

      vscreen_queuelayout();
      vscreen_update();
    }
}

void vs_menubar::hide_menu(const vs_menu_ref &w)
{
  vs_widget_ref tmpref(this);

  if(active)
    {
      for(activemenulist::iterator i=active_menus.begin();
	  i!=active_menus.end();
	  i++)
	{
	  if(*i==w)
	    {
	      w->unfocussed();
	      active_menus.remove(w);

	      vs_widget_ref new_focus=get_focus();
	      if(new_focus.valid())
		new_focus->focussed();

	      vscreen_queuelayout();
	      vscreen_update();
	      return;
	    }
	}

      abort();
    }
}

void vs_menubar::hide_menu_bare(vs_menu &w)
{
  hide_menu(vs_menu_ref(&w));
}

void vs_menubar::show_menu_bare(vs_menu &w)
{
  show_menu(vs_menu_ref(&w));
}

void vs_menubar::appear()
{
  vs_widget_ref tmpref(this);

  if(!active)
    {
      active=true;
      if(subwidget.valid())
	subwidget->unfocussed();

      // Added to eliminate the weird "titles are selected but contents aren't"
      // state that was normal with the previous settings:
      if(items.size()>0)
	items[curloc].menu->show();

      update_x_start();
      vscreen_update();
    }
}

void vs_menubar::disappear()
{
  vs_widget_ref tmpref(this);

  if(active)
    {
      while(!active_menus.empty())
	active_menus.front()->hide();

      active=false;
      if(subwidget.valid())
	subwidget->focussed();

      curloc=0;

      vscreen_update();
    }
}

bool vs_menubar::handle_key(const key &k)
{
  vs_widget_ref tmpref(this);

  if(bindings->key_matches(k, "ToggleMenuActive"))
    {
      if(active)
	disappear();
      else
	appear();
    }
  else if(active)
    {
      if(bindings->key_matches(k, "Cancel"))
	{
	  disappear();

	  //if(!active_menus.empty())
	  //  active_menus.front()->hide();
	  //else
	  //  disappear();

	  vscreen_update();
	}
      else if(!active_menus.empty())
	{
	  if(bindings->key_matches(k, "Right"))
	    {
	      if(items.size()>0)
		{
		  while(!active_menus.empty())
		    active_menus.front()->hide();

		  active_menus.clear();

		  if(curloc<items.size()-1)
		    curloc++;
		  else
		    curloc=0;

		  items[curloc].menu->show();

		  update_x_start();
		  vscreen_update();
		}
	    }
	  else if(bindings->key_matches(k, "Left"))
	    {
	      if(items.size()>0)
		{
		  while(!active_menus.empty())
		    active_menus.front()->hide();

		  active_menus.clear();

		  if(curloc>0)
		    curloc--;
		  else
		    curloc=items.size()-1;

		  items[curloc].menu->show();

		  update_x_start();
		  vscreen_update();
		}
	    }
	  else if(active_menus.front()->dispatch_key(k))
	    return true;
	  else
	    return vscreen_widget::handle_key(k);
	}
      else if(bindings->key_matches(k, "Right"))
	{
	  if(items.size()>0)
	    {
	      if(curloc<items.size()-1)
		curloc++;
	      else
		curloc=0;

	      update_x_start();
	      vscreen_update();
	    }
	}
      else if(bindings->key_matches(k, "Left"))
	{
	  if(items.size()>0)
	    {
	      if(curloc>0)
		curloc--;
	      else
		curloc=items.size()-1;

	      update_x_start();
	      vscreen_update();
	    }
	}
      else if(bindings->key_matches(k, "Down") ||
	      bindings->key_matches(k, "Confirm"))
	{
	  if(items.size()>0)
	    items[curloc].menu->show();
	}
      else
	return vscreen_widget::handle_key(k);

      return true;
    }
  else if(subwidget.valid() && subwidget->dispatch_key(k))
    return true;
  else
    return vscreen_widget::handle_key(k);

  return true;
}

void vs_menubar::paint(const style &st)
{
  vs_widget_ref tmpref(this);

  if(subwidget.valid())
    subwidget->display(st);

  if(active || always_visible)
    {
      const style menubar_style=get_style("MenuBar");
      const style highlightedmenubar_style=get_style("HighlightedMenuBar");

      if(active)
	for(activemenulist::reverse_iterator i=active_menus.rbegin();
	    i!=active_menus.rend();
	    i++)
	  (*i)->display(st);

      int pos=0, maxx=getmaxx();

      apply_style(menubar_style);
      move(0, 0);
      for(int i=0; i<maxx; i+=wcwidth(L' '))
	add_wch(L' ');

      move(0, 0);

      itemlist::size_type i;
      for(i = startloc; i < items.size() && pos < maxx; ++i)
	{
	  if(active && i==curloc)
	    apply_style(highlightedmenubar_style);
	  else
	    apply_style(menubar_style);

	  wstring &title = items[i].title;
	  size_t titleloc = 0;

	  while(titleloc < title.size() && pos < maxx)
	    {
	      wchar_t wch=title[titleloc];

	      add_wch(wch);
	      pos+=wcwidth(wch);
	      ++titleloc;
	    }
	}

      apply_style(menubar_style);

      if(startloc > 0)
	mvadd_wch(0, 0, WACS_LARROW);
      if(i < items.size() || pos > maxx)
	mvadd_wch(0, maxx-1, WACS_RARROW);
    }
}

void vs_menubar::dispatch_mouse(short id, int x, int y, int z, mmask_t bmask)
{
  vs_widget_ref tmpref(this);

  if(y==0)
    {
      if(bmask & (BUTTON1_CLICKED | BUTTON2_CLICKED |
		  BUTTON3_CLICKED | BUTTON4_CLICKED |
		  BUTTON1_RELEASED | BUTTON2_RELEASED |
		  BUTTON3_RELEASED | BUTTON4_RELEASED |
		  BUTTON1_PRESSED | BUTTON2_PRESSED |
		  BUTTON3_PRESSED | BUTTON4_PRESSED))
	{
	  if(!active)
	    appear();

	  int loc=0;
	  activemenulist::size_type i=0;

	  if(items.size()>0)
	    {
	      loc += wcswidth(items[0].title.c_str(), items[0].title.size());

	      while(i<items.size()-1 && loc<=x)
		{
		  loc += wcswidth(items[i+1].title.c_str(), items[i+1].title.size());

		  ++i;
		}
	    }

	  if(i<items.size())
	    {
	      while(!active_menus.empty())
		active_menus.front()->hide();

	      active_menus.clear();

	      curloc=i;

	      items[curloc].menu->show();

	      vscreen_update();
	    }
	}
    }
  else if(active)
    {
      for(activemenulist::iterator i=active_menus.begin();
	  i!=active_menus.end();
	  i++)
	if((*i)->enclose(y, x))
	  {
	    (*i)->dispatch_mouse(id,
				 x-(*i)->get_startx(), y-(*i)->get_starty(), z,
				 bmask);
	    return;
	  }
    }

  if(subwidget.valid())
    subwidget->dispatch_mouse(id,
			      x-subwidget->get_startx(),
			      y-subwidget->get_starty(), z, bmask);
}

void vs_menubar::init_bindings()
{
  bindings=new keybindings(&global_bindings);
}

void vs_menubar::set_always_visible(bool _always_visible)
{
  if(_always_visible!=always_visible)
    {
      always_visible=_always_visible;
      vscreen_update();
      vscreen_queuelayout();
    }
}
