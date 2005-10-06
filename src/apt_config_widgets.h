// apt_config_widgets.h     -*-c++-*-
//
//   Copyright 2000, 2005 Daniel Burrows
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
//  Classes that mediate between the configuration layer and the user
//  interface layer.  Past versions of this file used multiple
//  inheritance, but in order to handle memory management in an
//  obvious and safe way, I modified this to explicitly make the
//  UI-layer objects public members of the config-layer objects.  This
//  is arguably cleaner anyway.

#ifndef APT_CONFIG_WIDGETS_H
#define APT_CONFIG_WIDGETS_H

#include <vscreen/vs_togglebutton.h>
#include <vscreen/vs_editline.h>
#include <vscreen/vs_radiogroup.h>

#include <sigc++/object.h>

#include <string>
#include <vector>

// This basically just provides a generic interface to commit changes
// to apt options.
class apt_config_widget : public sigc::trackable
{
public:
  virtual ~apt_config_widget() {}

  virtual void commit()=0;
};

class apt_bool_widget : public apt_config_widget
{
  std::string item; // the config-item we're associated with.

  bool my_default;
public:
  apt_bool_widget(const std::wstring &_label,
		  const std::string &_item, bool _default);
  apt_bool_widget(const std::string &_label,
		  const std::string &_item, bool _default);

  /** The actual underlying object. */
  vs_checkbutton_ref cb;

  void commit();
};

class apt_string_widget : public apt_config_widget
{
  std::string item; // the config-item we're associated with.

  std::string my_default;

  /** The underlying object. */
  vs_editline_ref el;
public:
  apt_string_widget(const std::string &_item,
		    const std::string &_default);

  /** The enclosing size box. */
  vs_widget_ref w;

  void commit();
};

/** Manages a multi-choice option by providing radio buttons.  The
 *  creator of this widget must give each choice an id corresponding
 *  to its index in the "choices" array.
 */
class apt_radio_widget : public apt_config_widget
{
  std::string item;

  std::vector<std::string> choices;

  std::string my_default;
public:
  apt_radio_widget(const std::string &_item,
		   const std::vector<std::string> &choices,
		   const std::string &_default);

  /** The underlying interface object. */
  vs_radiogroup rg;

  void commit();
};

#endif
