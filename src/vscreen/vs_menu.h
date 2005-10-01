// vs_menu.h  -*-c++-*-
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
//  A nice menu with selectable entries.  (fairly basic atm)

#ifndef VS_MENU_H
#define VS_MENU_H

#include "vscreen_widget.h"
#include "config/keybindings.h"

#include <generic/util/bool_accumulate.h>
#include <generic/util/slotarg.h>

#include <vector>

// Currently, menu-items aren't full widgets--it's simply too much
// baggage (lots of signals and a Curses window) for something that's
// quite simple and special-purpose.
class vs_menu_item
{
  // The text displayed in the menu entry
  std::wstring title;

  // A string describing the item's function
  std::wstring description;

  // The keybinding whose definition (in the GLOBAL bindings list) is
  // displayed to the right of the menu entry.
  std::string binding;

  // The key that directly activates this item *while the menu is open*
  chtype hotkey;
public:
  // Infers the hotkey from the title
  vs_menu_item(const std::wstring &_title, const std::string &_binding,
	       const std::wstring &_description);

  std::wstring get_title() const {return title;}
  std::string get_binding() const {return binding;}
  std::wstring get_description() const {return description;}
  chtype get_hotkey() const {return hotkey;}

  /** The canonical way to test whether an item is really enabled. */
  bool is_enabled() const;

  /** Emitted when the menu item is selected. */
  sigc::signal0<void> selected;

  /** Emitted to test whether the menu item should be displayed as "active".
   *
   *  If this signal is empty, the item will be displayed as "active" iff
   *  "selected" is non-empty.  Otherwise, the return value of the signal
   *  is used.
   */
  sigc::signal0<bool, accumulate_or> enabled;
};

#define VS_MENU_NOP NULL

// Info for easy static generation of menus

struct vs_menu_info
{
public:
  // VS_MENU_ITEM: a "real" menu-item
  // VS_MENU_END: the last item in this information block
  enum item_types {VS_MENU_ITEM, VS_MENU_SEPARATOR, VS_MENU_END} item_type;

  /** item_name and item_description are multibyte representations. */
  const char *item_name, *item_binding, *item_description;

  // How to communicate with the outside world..
  slot0arg item_slot;

  // For activation
  slotarg<sigc::slot0<bool> >  item_enabled;

  vs_menu_info(item_types type, const char *name, const char *binding,
	       const char *description, sigc::slot0<void> slot);

  vs_menu_info(item_types type, const char *name, const char *binding,
	       const char *description, sigc::slot0<void> *slot);

  vs_menu_info(item_types type, const char *name, const char *binding,
	       const char *description, sigc::slot0<void> slot,
	       sigc::slot0<bool> enabled);

  vs_menu_info(item_types type, const char *name, const char *binding,
	       const char *description, sigc::slot0<void> *slot,
	       sigc::slot0<bool> enabled);

  vs_menu_info(item_types type);
};

const vs_menu_info VS_MENU_SEPARATOR(vs_menu_info::VS_MENU_SEPARATOR);
const vs_menu_info VS_MENU_END(vs_menu_info::VS_MENU_END);

class vs_menu:public vscreen_widget
{
  typedef std::vector<vs_menu_item *> itemlist;

  // A set of menu items.  NULL indicates a separator.
  // These items are deleted with the menu.
  itemlist items;

  /** The location of the cursor, or items.size() if no item is selected. */
  itemlist::size_type cursorloc;

  /** The first visible item in the menu. */
  itemlist::size_type startloc;

  /** The minimum width of this menu. */
  int min_width;

  // connected to "shown"
  void appear();

  // connected to "hidden"
  void disappear();

  /** Update the starting location from the current height and cursor
   *  location.
   */
  void update_startloc();

  /** Returns \b true iff the given item is selectable. */
  bool selectable(itemlist::size_type pos);

  /** Select the given location. */
  void set_cursor(itemlist::size_type pos);

  /** Highlight the currently selected item (highlight NULL if no
   *  item is selected)
   */
  void highlight_current();

  /** Search for the first selectable item from the given location; if
   *  the search runs off the end of the list, return items.size().
   *
   *  \param pos the starting location of the search; if it is out of
   *  bounds, the search starts with the first menu item.
   */
  itemlist::size_type next_selectable(itemlist::size_type pos);

  /** Search backwards for the first selectable item from the given
   *  location; if the search runs off the beginning of the list,
   *  return items.size().
   *
   *  \param pos the starting location of the search; if it is out of
   *  bounds, the search starts with the last menu item.
   */
  itemlist::size_type prev_selectable(itemlist::size_type pos);

  /** If the cursor is "out of bounds" or on a disabled item, attempt
   *  to select the first enabled non-separator item.
   *
   *  \param forward if \b true, search forward in the list for an
   *  enabled item; otherwise, search backward.
   */
  void sanitize_cursor(bool forward);

protected:
  virtual bool handle_key(const key &k);

  /** Create a blank menu. */
  vs_menu();

  // Initialize a menu from a block of information.  If there is no
  // VS_MENU_END in the block, RANDOM ERRORS WILL OCCUR!!
  vs_menu(int x, int y, int w, vs_menu_info *inf);
public:
  static ref_ptr<vs_menu> create()
  {
    ref_ptr<vs_menu> rval(new vs_menu);
    rval->decref();
    return rval;
  }

  static ref_ptr<vs_menu> create(int x, int y, int w, vs_menu_info *inf)
  {
    ref_ptr<vs_menu> rval(new vs_menu(x, y, w, inf));
    rval->decref();
    return rval;
  }

  // Deletes the items it holds!
  ~vs_menu();

  bool get_cursorvisible();
  point get_cursorloc();

  int width_request();
  int height_request(int width);

  void append_item(vs_menu_item *newitem);
  void remove_item(vs_menu_item *item);

  /** Move the selection up, as if Up had been pressed. */
  void move_selection_up();

  /** Move the selection down, as if Down had been pressed. */
  void move_selection_down();

  /** Move the selection to the top of the menu, as if Home had been pressed. */
  void move_selection_top();

  /** Move the selection to the bottom of the menu, as if End had been pressed. */
  void move_selection_bottom();

  virtual bool focus_me();
  virtual void paint(const style &st);
  virtual void dispatch_mouse(short id, int x, int y, int z, mmask_t bstate);

  // Emitted when an item is highlighted or when the selection "goes away".
  // In the latter case, the argument is NULL.  (happens only when
  // the menu is hidden -- FIXME?)
  sigc::signal1<void, vs_menu_item *> item_highlighted;

  // FIXME: there should be a less hacky way..
  sigc::signal0<void> menus_goaway;

  static keybindings *bindings;
  static void init_bindings();
};

typedef ref_ptr<vs_menu> vs_menu_ref;

#endif
