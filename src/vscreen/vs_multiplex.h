// vs_multiplex.h                       (This is -*-c++-*-)
// Copyright 1999-2005 Daniel Burrows
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
//  This file defines a widget with the behavior of the old vscreen class.
//  In essence, this widget can multiplex multiple sub-widgets.  When a
// subwidget emits the "hide" signal, the vscreen does what vscreen_hide()
// used to do.  "shown" is actually emitted by a vscreen when a particular
// subwidget becomes visible.  (strange but true)
// (you can no longer show/hide at a particular point in the stack, at least
//  not right now.  I don't think there's much point)

#ifndef VSMULTIPLEX_H
#define VSMULTIPLEX_H

#include "curses++.h"
#include "vs_passthrough.h"

#include <generic/util/eassert.h>

#include <list>
#include <string>

/** This widget displays exactly one of its children at once.  It is
 *  the same size as its currently active child (plus an optional "tab
 *  bar" displaying all its children).
 */
class vs_multiplex:public vs_passthrough
{
  struct child_info
  {
    vs_widget_ref w;
    std::wstring title;

    child_info(const vs_widget_ref &_w, const std::wstring &_title)
      :w(_w), title(_title)
    {
    }
  };

  std::list<child_info> children;

  std::list<child_info>::iterator visible_child;

  /** If \b true and more than one child is visible, "tabs" are
   *  displayed at the top of the widget, corresponding to the visible
   *  children.
   */
  bool show_tabs;

  /** \return \b true if show_tabs is \b true and more than one
   *  child is visible.
   */
  bool tabs_visible() const;

  void show_widget(const vs_widget_ref &widget);
  // Used to bring a widget to the front
  void hide_widget(const vs_widget_ref &widget);
  // Used to hide a widget

  void show_widget_bare(vscreen_widget &widget);
  void hide_widget_bare(vscreen_widget &widget);

  void got_focus();
  void lost_focus();
protected:
  bool winavail() {return get_win();}

  vs_multiplex(bool _show_tabs);
public:
  static ref_ptr<vs_multiplex> create(bool show_tabs = false)
  {
    ref_ptr<vs_multiplex> rval(new vs_multiplex(show_tabs));
    rval->decref();
    return rval;
  }

  virtual ~vs_multiplex();

  /** Returns the maximum width requested by any child. */
  int width_request();

  /** Returns the maximum height requested by any child. */
  int height_request(int width);

  void destroy();

  void layout_me();

  virtual vs_widget_ref get_focus();
  vs_widget_ref visible_widget();
  unsigned int num_children();
  // Returns the number of widgets in the multiplexer.
  unsigned int num_visible();

  virtual void paint(const style &st);
  void dispatch_mouse(short id, int x, int y, int z, mmask_t bstate);

  void show_all();

  /** Add a title-less widget.  Provided to implement a required
   *  function and for backwards compatibility; use of this routine is
   *  deprecated.
   */
  void add_widget(const vs_widget_ref &widget);
  void add_widget(const vs_widget_ref &widget, const std::wstring &title);
  void add_widget_bare(vscreen_widget &widget, const std::wstring &title)
  {
    add_widget(vs_widget_ref(&widget), title);
  }

  void add_widget_after(const vs_widget_ref &widget,
			const vs_widget_ref &after);

  void add_widget_after_bare(vscreen_widget &widget,
			     vscreen_widget &after)
  {
    add_widget_after(vs_widget_ref(&widget), vs_widget_ref(&after));
  }


  void add_widget_after(const vs_widget_ref &widget,
			const vs_widget_ref &after,
			const std::wstring &title);


  void add_widget_after_bare(vscreen_widget &widget,
			     vscreen_widget &after,
			     const std::wstring &title)
  {
    add_widget_after(vs_widget_ref(&widget), vs_widget_ref(&after), title);
  }


  void rem_widget(const vs_widget_ref &widget);

  // These cycle forward and backwards through the list of visible items.
  void cycle_forward();
  void cycle_backward();

  /** Emitted when the currently visible widget changes. */
  sigc::signal0<void> cycled;
};

typedef ref_ptr<vs_multiplex> vs_multiplex_ref;

#endif
