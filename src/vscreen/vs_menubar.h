// vs_menubar.h   -*-c++-*-
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
//
//  Provides a horizontal menubar and a space for submenus.  This widget and
// its menus are superimposed on top of another widget.

#ifndef VS_MENUBAR_H
#define VS_MENUBAR_H

#include "vscreen_widget.h"
#include "vs_container.h"
#include "config/keybindings.h"

#include <string>
#include <vector>

class vs_menu;

typedef ref_ptr<vs_menu> vs_menu_ref;

class vs_menubar:public vs_container
{
  struct item
  {
    std::wstring title;
    ref_ptr<vs_menu> menu;

    item(std::wstring _title, ref_ptr<vs_menu> _menu)
      :title(_title), menu(_menu)
    {
    }
  };

  typedef std::vector<item> itemlist;
  typedef std::list<vs_widget_ref> activemenulist;

  // A list of the items in the menubar itself
  itemlist items;
  // A list of active menus
  activemenulist active_menus;

  /** The index of the leftmost visible menu item. */
  itemlist::size_type startloc;

  // True if the menu-bar is visible and/or being used
  bool active;

  // True if the menu-bar should always be visible
  bool always_visible;

  /** The index of the currently selected item. */
  itemlist::size_type curloc;

  // the widget underneath this one.
  vs_widget_ref subwidget;

  // Returns the starting X location of the given item in the menu
  int get_menustart(itemlist::size_type idx) const;

  /** Re-calculates the starting X location, moving it left or right
   *  if needed.
   */
  void update_x_start();

  // Show/hide menus
  void show_menu(const vs_menu_ref &w);
  void show_menu_bare(vs_menu &w);

  void hide_menu(const vs_menu_ref &w);
  void hide_menu_bare(vs_menu &w);

  void appear();
  void disappear();

  // Similar to the passthrough widget's routine (there's not enough
  // similarity, though, to justify making this a passthrough widget)
  vs_widget_ref get_focus();

  void got_focus();
  void lost_focus();
protected:
  virtual bool handle_key(const key &k);

  vs_menubar(bool _always_visible);
public:
  static ref_ptr<vs_menubar> create(bool always_visible = true)
  {
    ref_ptr<vs_menubar> rval(new vs_menubar(always_visible));
    rval->decref();
    return rval;
  }

  ~vs_menubar();

  /** The 'active' widget of a menubar is always its subwidget. */
  vs_widget_ref get_active_widget();

  void destroy();

  int width_request();
  int height_request(int w);
  void layout_me();

  void set_subwidget(const vs_widget_ref &w);

  void append_item(const std::wstring &title, const vs_menu_ref &menu);
  void append_item(const std::wstring &title, vs_menu &menu)
  {
    append_item(title, vs_menu_ref(&menu));
  }

  void show_all();

  /** Add a widget as the new subwidget, like a bin. */
  void add_widget(const vs_widget_ref &w);
  /** Remove the subwidget OR a menu. */
  void rem_widget(const vs_widget_ref &w);

  virtual void paint(const style &st);
  virtual bool focus_me();
  virtual void dispatch_mouse(short id, int x, int y, int z,
			      mmask_t bmask);

  bool get_cursorvisible();
  point get_cursorloc();

  bool get_always_visible() {return always_visible;}
  void set_always_visible(bool _always_visible);

  static keybindings *bindings;
  static void init_bindings();
};

typedef ref_ptr<vs_menubar> vs_menubar_ref;

#endif
