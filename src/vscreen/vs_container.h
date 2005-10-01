// vs_container.h              -*-c++-*-
//
//
//   Copyright (C) 2000, 2005 Daniel Burrows
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
//  A generic interface for a vscreen_widget that can hold other
//  widgets.

#ifndef VS_CONTAINER_H
#define VS_CONTAINER_H

#include "vscreen_widget.h"

class vs_container:public vscreen_widget
{
public:
  vs_container():vscreen_widget() {}
  ~vs_container();

  virtual void add_widget(const vs_widget_ref &)=0;
  void add_visible_widget(const vs_widget_ref &, bool visible);
  virtual void rem_widget(const vs_widget_ref &)=0;

  // Variants of the above that take a bare reference; used for weak
  // slot connections.
  void add_widget_bare(vscreen_widget &w)
  {
    add_widget(vs_widget_ref(&w));
  }

  void add_visible_widget_bare(vscreen_widget &w, bool visible)
  {
    add_visible_widget(vs_widget_ref(&w), visible);
  }

  void rem_widget_bare(vscreen_widget &w)
  {
    rem_widget(vs_widget_ref(&w));
  }

  /** Return the currently "active" child of this container, or \b NULL. */
  virtual vs_widget_ref get_active_widget() = 0;

  /** Display this widget and all its subwidgets. */
  virtual void show_all()=0;
};

#endif
