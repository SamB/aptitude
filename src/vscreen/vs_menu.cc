// vs_menu.cc
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


#include "vs_menu.h"
#include "vscreen.h"
#include "config/colors.h"

#include "transcode.h"

#include <sigc++/functors/mem_fun.h>

#include <algorithm>
#include <ctype.h>

using namespace std;

keybindings *vs_menu::bindings=NULL;

vs_menu_info::vs_menu_info(item_types type, const char *name, const char *binding,
			   const char *description, sigc::slot0<void> slot)
  :item_type(type), item_name(name), item_binding(binding),
   item_description(description), item_slot(slot), item_enabled(NULL)
{
}

vs_menu_info::vs_menu_info(item_types type, const char *name, const char *binding,
			   const char *description, sigc::slot0<void> *slot)
  :item_type(type), item_name(name), item_binding(binding),
   item_description(description), item_slot(slot), item_enabled(NULL)
{
}

vs_menu_info::vs_menu_info(item_types type, const char *name, const char *binding,
			   const char *description, sigc::slot0<void> slot,
			   sigc::slot0<bool> enabled)
  :item_type(type), item_name(name), item_binding(binding),
   item_description(description), item_slot(slot), item_enabled(enabled)
{
}

vs_menu_info::vs_menu_info(item_types type, const char *name, const char *binding,
			   const char *description, sigc::slot0<void> *slot,
			   sigc::slot0<bool> enabled)
  :item_type(type), item_name(name), item_binding(binding),
   item_description(description), item_slot(slot), item_enabled(enabled)
{
}

vs_menu_info::vs_menu_info(item_types type)
  :item_type(type), item_name(NULL), item_binding(NULL),
   item_description(NULL), item_slot(NULL), item_enabled(NULL)
{
}

vs_menu_item::vs_menu_item(const wstring &_title, const string &_binding,
			   const wstring &_description)
  :title(_title), description(_description), binding(_binding),
   hotkey((chtype) ERR)
{
  for(wstring::size_type i=0; i<title.size(); i++)
    if(title[i]==L'^' && i+1<title.size())
      {
	hotkey=title[i+1];
	break;
      }
}

bool vs_menu_item::is_enabled() const
{
  if(enabled.empty())
    return !selected.empty();
  else
    return enabled();
}

vs_menu::vs_menu()
  :vscreen_widget(), cursorloc(0), min_width(2)
{
  shown_sig.connect(sigc::mem_fun(*this, &vs_menu::appear));
  hidden_sig.connect(sigc::mem_fun(*this, &vs_menu::disappear));
  do_layout.connect(sigc::mem_fun(*this, &vs_menu::update_startloc));
}

vs_menu::~vs_menu()
{
  for(itemlist::iterator i=items.begin(); i!=items.end(); i++)
    delete *i;
}

vs_menu::vs_menu(int x, int y, int w, vs_menu_info *inf)
  :vscreen_widget(), cursorloc(0), startloc(0), min_width(w)
{
  while(inf->item_type!=vs_menu_info::VS_MENU_END)
    {
      switch(inf->item_type)
	{
	case vs_menu_info::VS_MENU_ITEM:
	  eassert(inf->item_name!=NULL);

	  {
	    vs_menu_item *newitem=new vs_menu_item(transcode(inf->item_name),
						   inf->item_binding?inf->item_binding:"",
						   transcode(inf->item_description?inf->item_description:""));

	    if(inf->item_slot)
	      newitem->selected.connect(*inf->item_slot);

	    if(inf->item_enabled)
	      newitem->enabled.connect(*inf->item_enabled);

	    append_item(newitem);
	  }
	  break;
	case vs_menu_info::VS_MENU_SEPARATOR:
	  eassert(inf->item_name==NULL);

	  append_item(NULL);
	  break;
	default:
	  fprintf(stderr, "ERROR: unknown item type code %i\n", inf->item_type);
	  abort();
	}

      inf++;
    }

  shown_sig.connect(sigc::mem_fun(*this, &vs_menu::appear));
  hidden_sig.connect(sigc::mem_fun(*this, &vs_menu::disappear));
  do_layout.connect(sigc::mem_fun(*this, &vs_menu::update_startloc));
}

void vs_menu::append_item(vs_menu_item *newitem)
{
  vs_widget_ref tmpref(this);

  items.push_back(newitem);

  if(get_visible())
    {
      vscreen_queuelayout();
      vscreen_update();
    }
}

void vs_menu::remove_item(vs_menu_item *item)
{
  vs_widget_ref tmpref(this);

  itemlist::size_type idx=0;

  while(idx<items.size() && items[idx]!=item)
    ++idx;

  eassert(idx<items.size());

  for(itemlist::size_type newidx=idx; newidx<items.size()-1; ++newidx)
    items[newidx]=items[newidx+1];

  items.pop_back();

  if(items.size()==0)
    set_cursor(0);
  else if(idx==cursorloc)
    set_cursor(prev_selectable(next_selectable(items.size()-1)));

  while(startloc >= items.size())
    --startloc;

  if(get_visible())
    vscreen_queuelayout();
}

int vs_menu::width_request()
{
  int req_width=min_width;

  vs_widget_ref tmpref(this);

  for(itemlist::const_iterator item=items.begin();
      item!=items.end(); ++item)
    if(*item)
      {
	int titlewidth=0, shortcutwidth=0;
	const wstring &title=(*item)->get_title();
	const string &binding=(*item)->get_binding();

	for(wstring::size_type i=0; i<title.size(); ++i)
	  if(title[i]!=L'^')
	    titlewidth+=wcwidth(title[i]);

	if(binding.empty())
	  shortcutwidth=0;
	else
	  {
	    wstring keyname=global_bindings.readable_keyname(binding);
	    shortcutwidth=wcswidth(keyname.c_str(), keyname.size())+1;
	  }

	req_width=max<int>(titlewidth+2+shortcutwidth,req_width);
      }

  return req_width;
}

int vs_menu::height_request(int w)
{
  return items.size()+2;
}

void vs_menu::highlight_current()
{
  vs_widget_ref tmpref(this);

  if(cursorloc>=0 && cursorloc<items.size())
    item_highlighted(items[cursorloc]);
  else
    item_highlighted(NULL);
}

void vs_menu::update_startloc()
{
  unsigned int h = get_height();

  if(h <= 2)
    return;
  else if(h - 2 >= items.size())
    {
      startloc = 0;
      return;
    }
  else if(cursorloc >= items.size() || cursorloc < items.size())
    ; // Do nothing, but also do the final sanitization below.
  else if(cursorloc < startloc)
    startloc = cursorloc;
  else if(startloc + h - 2 <= cursorloc)
    startloc = cursorloc - (h - 2 - 1);

  if(startloc + (h - 2) > items.size())
    startloc = items.size() - (h - 2);
}

void vs_menu::set_cursor(itemlist::size_type pos)
{
  vs_widget_ref tmpref(this);

  if(cursorloc!=pos)
    {
      cursorloc = pos;

      update_startloc();

      highlight_current();

      if(get_visible())
	vscreen_update();
    }
}

void vs_menu::appear()
{
  vs_widget_ref tmpref(this);

  cursorloc = next_selectable(0);
  startloc = 0;

  update_startloc();

  highlight_current();
}

void vs_menu::disappear()
{
  vs_widget_ref tmpref(this);

  set_cursor(items.size());
}

bool vs_menu::focus_me()
{
  return true;
}

bool vs_menu::selectable(itemlist::size_type pos)
{
  vs_widget_ref tmpref(this);

  return pos>=0 && pos<items.size() && items[pos] && items[pos]->is_enabled();
}

vs_menu::itemlist::size_type vs_menu::next_selectable(itemlist::size_type pos)
{
  vs_widget_ref tmpref(this);

  if(pos<0 || pos>=items.size())
    pos=0;

  while(pos<items.size() && (!items[pos] ||
			     !items[pos]->is_enabled()))
    ++pos;

  return pos;
}

vs_menu::itemlist::size_type vs_menu::prev_selectable(itemlist::size_type pos)
{
  vs_widget_ref tmpref(this);

  if(pos<0 || pos>=items.size())
    pos=items.size()-1;

  while(pos>=0 && pos<items.size() && (!items[pos] ||
				       !items[pos]->is_enabled()))
    --pos;

  if(pos<0 || pos>=items.size())
    pos=items.size();

  return pos;
}

void vs_menu::sanitize_cursor(bool forward)
{
  vs_widget_ref tmpref(this);

  if(forward)
    // Double-invoking next_selectable will search from the
    // start of the menu if searching forwards fails.
    cursorloc=next_selectable(next_selectable(cursorloc));
  else
    cursorloc=prev_selectable(prev_selectable(cursorloc));

  update_startloc();

  highlight_current();
}

void vs_menu::move_selection_up()
{
  if(cursorloc > 0)
    {
      itemlist::size_type newloc = prev_selectable(cursorloc-1);

      if(newloc >= 0 && newloc < items.size())
	{
	  if(newloc < startloc)
	    --startloc;

	  if(newloc >= startloc)
	    set_cursor(newloc);
	}
      else if(startloc > 0)
	--startloc;

      update_startloc();

      vscreen_update();
    }
  else if(startloc > 0)
    {
      --startloc;
      vscreen_update();
    }
}

void vs_menu::move_selection_down()
{
  int h = get_height();

  if(cursorloc < items.size() - 1)
    {
      itemlist::size_type newloc = next_selectable(cursorloc+1);

      if(newloc >= 0 && newloc < items.size())
	{
	  if(newloc >= startloc + h - 2)
	    ++startloc;

	  if(newloc < startloc + h - 2)
	    set_cursor(newloc);
	}
      else if(startloc + h < items.size())
	++startloc;

      vscreen_update();
    }
  else if(startloc + h - 2 < items.size())
    {
      ++startloc;
      vscreen_update();
    }
}

void vs_menu::move_selection_top()
{
  startloc = 0;
  set_cursor(next_selectable(startloc));
  vscreen_update();
}

void vs_menu::move_selection_bottom()
{
  startloc = items.size() - 1;
  set_cursor(prev_selectable(startloc));
  vscreen_update();
}

bool vs_menu::handle_key(const key &k)
{
  vs_widget_ref tmpref(this);

  // This will ensure that the cursor is in bounds if possible, and that
  // if it is in bounds, a "real" item is selected.
  sanitize_cursor(true);

  if(bindings->key_matches(k, "Up"))
    move_selection_up();
  else if(bindings->key_matches(k, "Down"))
    move_selection_down();
  else if(bindings->key_matches(k, "Begin"))
    move_selection_top();
  else if(bindings->key_matches(k, "End"))
    move_selection_bottom();
  else if(bindings->key_matches(k, "Confirm"))
    {
      itemlist::size_type selected=cursorloc;

      menus_goaway();
      item_highlighted(NULL);

      if(selectable(selected))
	items[selected]->selected();
    }
  else
    {
      for(itemlist::iterator i=items.begin(); i!=items.end(); i++)
	if(*i && (*i)->is_enabled() &&
	   towupper(k.ch)==towupper((*i)->get_hotkey()))
	  {
	    menus_goaway();
	    item_highlighted(NULL);

	    (*i)->selected();

	    return true;
	  }
      return vscreen_widget::handle_key(k);
    }

  return true;
}

void vs_menu::dispatch_mouse(short id, int x, int y, int z, mmask_t bmask)
{
  vs_widget_ref tmpref(this);

  if(bmask & (BUTTON1_RELEASED | BUTTON2_RELEASED |
	      BUTTON3_RELEASED | BUTTON4_RELEASED |
	      BUTTON1_CLICKED | BUTTON2_CLICKED |
	      BUTTON3_CLICKED | BUTTON4_CLICKED))
    {
      itemlist::size_type num=y-1;

      if(selectable(num))
	{
	  menus_goaway();
	  item_highlighted(NULL);
	  items[num]->selected();
	}
    }
  else if(bmask & (BUTTON1_PRESSED | BUTTON2_PRESSED |
		   BUTTON3_PRESSED | BUTTON4_PRESSED))
    {
      itemlist::size_type num=y-1;

      if(selectable(num))
	set_cursor(num);
    }
}

// FIXME: handle truncated menus (eg, from menus appearing that are
// larger than the screen).  REALLY FIX THIS!  It's incredibly annoying in, eg,
// GTK+, and text screens have even less space to play with..
void vs_menu::paint(const style &st)
{
  vs_widget_ref tmpref(this);

  int width, height;
  const style border_style=st+get_style("MenuBorder");
  const style highlighted_style=st+get_style("HighlightedMenuEntry");
  const style entry_style=st+get_style("MenuEntry");
  const style disabled_style=st+get_style("DisabledMenuEntry");

  getmaxyx(height, width);

  apply_style(border_style);
  mvadd_wch(0, 0, WACS_ULCORNER);

  const bool up_arrows_visible = (startloc != 0);
  for(int i = 1; i < width-1; i++)
    {
      add_wch((up_arrows_visible && i % 3 == 0) ? WACS_UARROW : WACS_HLINE);
    }
  add_wch(WACS_URCORNER);

  // Make sure that whatever is selected is really selectable.
  sanitize_cursor(true);

  for(itemlist::size_type i = startloc; i < items.size(); ++i)
    if(items[i])
      {
	int y = i - startloc + 1;
	bool boldthis=false;

	apply_style(border_style);
	mvadd_wch(y, 0, WACS_VLINE);
	mvadd_wch(y, width-1, WACS_VLINE);

	wstring title=items[i]->get_title();
	wstring righttext=items[i]->get_binding().empty()?L"":global_bindings.readable_keyname(items[i]->get_binding());

	bool enabled=items[i]->is_enabled();
	style textst;

	if(i==cursorloc)
	  textst=highlighted_style;
	else if(enabled)
	  textst=entry_style;
	else
	  textst=disabled_style;

	apply_style(textst);

	move(y, 1);

	wstring::size_type titleloc=0, rightloc=0;
	int rightwidth=wcswidth(righttext.c_str(), righttext.size());
	int curw=1;

	while(curw<width-1)
	  {
	    while(titleloc<title.size() && title[titleloc]=='^')
	      {
		boldthis=enabled;
		++titleloc;
	      }

	    if(titleloc==title.size())
	      {
		add_wch(L' ');
		++titleloc;
		curw+=wcwidth(L' ');;
	      }
	    else if(titleloc>title.size())
	      {
		if(curw<width-1-rightwidth)
		  {
		    add_wch(L' ');
		    curw+=wcwidth(L' ');
		  }
		else
		  {
		    wchar_t wch=righttext[rightloc];

		    add_wch(wch);
		    curw+=wcwidth(wch);
		    ++rightloc;
		  }
	      }
	    else if(boldthis)
	      {
		wchar_t wch=title[titleloc];

		apply_style(textst+style_attrs_on(A_BOLD));
		add_wch(wch);
		apply_style(textst);
		boldthis=false;

		curw+=wcwidth(wch);
		++titleloc;
	      }
	    else
	      {
		wchar_t wch=title[titleloc];

		add_wch(wch);
		curw+=wcwidth(wch);
		++titleloc;
	      }
	  }
      }
    else
      {
	int y = i - startloc + 1;

	apply_style(border_style);
	mvadd_wch(y, 0, WACS_LTEE);
	for(int j=1; j<width-1; j++)
	  add_wch(WACS_HLINE);
	add_wch(WACS_RTEE);
      }

  apply_style(border_style);

  for(int i=items.size()+1; i<height-1; i++)
    {
      move(i, 0);
      add_wch(WACS_VLINE);

      apply_style(entry_style);
      for(int j=0; j<width-2; j++)
	add_wch(L' ');
      apply_style(border_style);

      add_wch(WACS_VLINE);
    }

  mvadd_wch(height-1, 0, WACS_LLCORNER);

  const bool down_arrows_visible = startloc + height - 2 < items.size();

  for(int i = 1; i < width-1; ++i)
    add_wch((down_arrows_visible && i % 3 == 0) ? WACS_DARROW : WACS_HLINE);

  add_wch(WACS_LRCORNER);
}

bool vs_menu::get_cursorvisible()
{
  vs_widget_ref tmpref(this);

  sanitize_cursor(true);

  return cursorloc>=0 && cursorloc<items.size();
}

point vs_menu::get_cursorloc()
{
  vs_widget_ref tmpref(this);

  sanitize_cursor(true);

  return point(0, 1 + cursorloc - startloc);
}

void vs_menu::init_bindings()
{
  bindings=new keybindings(&global_bindings);
}
