// style.h   -*-c++-*-
//
//   Copyright (C) 2005 Daniel Burrows
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

#ifndef STYLE_H
#define STYLE_H

#include <ncursesw/curses.h>

#include <vscreen/curses++.h>

#include <string>

#include "colors.h"

#include <generic/util/eassert.h>

/** A "style" is a setting to be applied to a display element (widget,
 *  text, etc).  This means color (foreground and background) and
 *  attributes.  A style may contain settings for any or all of these;
 *  settings which are unspecified are taken from the context in which
 *  the display element occurs.  For instance, the "button
 *  highlighted" style might simply toggle the REVERSE flag.
 *
 *  The interface of styles is high-level: each style is viewed as a
 *  function that applies to the current text attributes; you can also
 *  extract the style's "current" state in order to merge it with a
 *  cchar (this is the style applied to the "null" or default style).
 *  However, as there are only a relatively limited number of things
 *  each style can change, the representation doesn't rely on virtual
 *  functions or even on keeping an array of operations to be applied;
 *  instead, it just keeps a summary of the operations it represents.
 */
class style
{
  /** The foreground color to be used. (if negative, no change) */
  short fg;
  /** The background color to be used. (if -2, no change; if -1, the
   *  'default' color)
   */
  short bg;

  /** Attributes to set. */
  attr_t set_attrs;
  /** Attributes to clear. */
  attr_t clear_attrs;
  /** Attributes to flip.  These are always applied after set_attrs
   *  and clear_attrs.
   */
  attr_t flip_attrs;

  // Note: it is assumed that set_attrs and clear_attrs are pairwise
  // disjoint.

public:
  /** Initialize an "empty" style. */
  style():fg(-1), bg(-2), set_attrs(0), clear_attrs(0), flip_attrs(0)
  {
  }

  /** Set the foreground color.  There is no change if the
   *  new foreground color is "empty".
   */
  void set_fg(short _fg) {if(_fg >= 0) fg=_fg;}

  /** Set the background color.  There is no change if the
   *  new background color is "empty".
   */
  void set_bg(short _bg) {if(_bg >= -1) bg = _bg;}

  /** Set the given attribute(s). */
  void attrs_on(attr_t attrs)
  {
    set_attrs|=attrs;
    clear_attrs&=~attrs;
    flip_attrs&=~attrs;
  }

  /** Clear the given attribute(s). */
  void attrs_off(attr_t attrs)
  {
    clear_attrs|=attrs;
    set_attrs&=~attrs;
    flip_attrs&=~attrs;
  }

  /** Flip the given attribute(s). */
  void attrs_flip(attr_t attrs)
  {
    flip_attrs^=attrs;
  }

  /** Update this style by applying the settings in the other
   *  style.
   */
  void apply_style(const style &other)
  {
    set_fg(other.fg);
    set_bg(other.bg);
    attrs_on(other.set_attrs);
    attrs_off(other.clear_attrs);
    attrs_flip(other.flip_attrs);
  }

  /** \return a new style formed by composing this style with other.
   *  Note that this is a noncommutative operator!
   */
  style operator+(const style &other) const
  {
    style rval(*this);
    rval+=other;
    return rval;
  }

  /** Shorthand for apply_style. */
  style &operator+=(const style &other)
  {
    apply_style(other);
    return *this;
  }

  bool operator==(const style &other) const
  {
    return fg == other.fg && bg == other.bg &&
      set_attrs == other.set_attrs && clear_attrs == other.clear_attrs &&
      flip_attrs == other.flip_attrs;
  }

  bool operator!=(const style &other) const
  {
    return fg != other.fg || bg != other.bg ||
      set_attrs != other.set_attrs || clear_attrs != other.clear_attrs ||
      flip_attrs != other.flip_attrs;
  }

  /** \return the foreground color. */
  short get_fg() const {return fg<0?0:fg;}
  /** \return the background color. */
  short get_bg() const {return bg<0?0:bg;}
  /** \return the current attributes. */
  attr_t get_attrs() const
  {
    attr_t rval=0;
    rval|=set_attrs;
    rval&=~clear_attrs;
    rval^=flip_attrs;
    rval|=mix_color(0, fg, bg);
    if(fg == bg)
      rval |= A_INVIS;
    return rval;
  }

  /** \return the given character with its attributes updated with ours. */
  chtype apply_to(chtype ch) const
  {
    // Relies somewhat on the bitwise representation of attributes;
    // the multicharacter-capable stuff needed for utf8 will make this
    // go away (for better or for worse..)
    return (ch & A_CHARTEXT) |
      mix_color(ch, fg, bg) |
      ((((ch & ~ (A_CHARTEXT | A_COLOR)) | set_attrs) & ~clear_attrs) ^ flip_attrs);
  }

  /** \return the given character with its attributes updated with ours. */
  wchtype apply_to(wchtype ch) const
  {
    // Relies somewhat on the bitwise representation of attributes;
    // the multicharacter-capable stuff needed for utf8 will make this
    // go away (for better or for worse..)
    return wchtype(ch.ch,
		   mix_color(ch.attrs, fg, bg) |
		   ((((ch.attrs & ~ A_COLOR) | set_attrs) & ~clear_attrs) ^ flip_attrs));
  }
};

// To allow styles to be built functionally, the following
// 'constructors' are provided.  The main idea here is to make
// default-setting more compact and less obscure.

/** \return a style that just sets the foreground color to the
 *          given value.
 */
inline style style_fg(short fg)
{
  style rval;
  rval.set_fg(fg);
  return rval;
}

/** \return a style that just sets the background color to the
 *          given value.
 */
inline style style_bg(short bg)
{
  style rval;
  rval.set_bg(bg);
  return rval;
}

/** \return a style that just sets the given attributes.
 */
inline style style_attrs_on(attr_t attrs)
{
  style rval;
  rval.attrs_on(attrs);
  return rval;
}

/** \return a style that just clears the given attributes. */
inline style style_attrs_off(attr_t attrs)
{
  style rval;
  rval.attrs_off(attrs);
  return rval;
}

/** \return a style that just flips the given attributes. */
inline style style_attrs_flip(attr_t attrs)
{
  style rval;
  rval.attrs_flip(attrs);
  return rval;
}

/** Look up a style in the global registry.
 *
 *  \throws NoSuchStyle if the style does not exist.
 */
const style &get_style(const std::string &name);

/** Place a style in the global registry. */
void set_style(const std::string &name, const style &style);

#endif // STYLE_H
