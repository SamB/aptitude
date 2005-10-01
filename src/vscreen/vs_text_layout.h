// vs_text_layout.h                     -*-c++-*-
//
//   Copyright (C) 2004-2005 Daniel Burrows
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


#ifndef VS_TEXT_LAYOUT_H
#define VS_TEXT_LAYOUT_H

#include "vscreen_widget.h"
#include "fragment_contents.h"

class fragment;

/** Code to display formatted text.
 *
 *  The text to display is composed of a tree of "fragments".  A
 *  fragment stores some amount of text; at any time, it can be
 *  formatted to a particular width.  The main layout mechanisms
 *  provided are flowboxes and clipboxes: flowboxes word-wrap their
 *  contents to a particular width, while clipboxes clip their
 *  contents to a particular width.  These boxes can be nested, if the
 *  user feels like it for some reason or other, although some
 *  nestings are non-sensical (for instance, placing a flowbox inside
 *  a smaller flowbox is likely to lead to really ugly text).
 *
 *  This provides some primitive layout mechanisms; higher-level
 *  layouts can be expressed in terms of these.
 */
class vs_text_layout:public vscreen_widget
{
protected:
  vs_text_layout();
  vs_text_layout(fragment *f);
public:
  /** Create an empty vs_text_layout. */
  static ref_ptr<vs_text_layout> create()
  {
    ref_ptr<vs_text_layout> rval(new vs_text_layout);
    rval->decref();
    return rval;
  }

  /** Create a vs_text_layout with the given root fragment.
   *
   *  All fragments are implicitly placed within a clipbox of width
   *  equal to the width of this widget.
   */
  static ref_ptr<vs_text_layout> create(fragment *f)
  {
    ref_ptr<vs_text_layout> rval(new vs_text_layout(f));
    rval->decref();
    return rval;
  }

  /** Handle the given keypress.  Returns \b true if the keystroke
   *  was "consumed" by this widget.
   */
  bool handle_key(const key &k);

  /** Change the fragment being displayed in this layout widget. */
  void set_fragment(fragment *f);

  /** Append the given fragment to the current fragment.
   *
   *  \note this is slightly less efficient than placing the two
   *  fragments into the text_layout up-front via a single
   *  sequence_fragment.  Normally this isn't a problem, but if you
   *  want to append hundreds of fragments this way, it might be.
   *
   *  \todo this is only needed for memory-management reasons
   *  (otherwise I could safely extract the current fragment and
   *  create my own sequence).  Would refcounting help?
   *
   *  \todo if this becomes very useful, it would be better to just
   *  explicitly store a sequence of fragments in the layout.
   */
  void append_fragment(fragment *f);

  /** Return the requested width of this widget.  The requested width
   *  will be the largest possible width of any line.
   */
  int width_request();

  /** Return the requested height of this widget given its width, by
   *  running the fragment-layout algorithm.
   */
  int height_request(int w);

  /** Return \b true iff the cursor is visible in this widget. */
  bool get_cursorvisible();

  /** Return the location of the cursor in this widget. */
  point get_cursorloc();

  /** Return \b true iff this widget should be given focus. */
  bool focus_me();

  /** Paint this widget. */
  void paint(const style &st);

  /** Move the view one line down. */
  void line_down();

  /** Move the view one line up. */
  void line_up();

  /** Move the view to the top of the widget. */
  void move_to_top();

  /** Move the view to the bottom of the widget. */
  void move_to_bottom();

  /** Move a page forward. */
  void page_down();

  /** Move a page back. */
  void page_up();

  /** Search either forwards or backwards for the string s.  The
   *  search will start on either the next or the previous line
   *  from the top of the screen.
   */
  void search_for(const std::wstring &s,
		  bool search_forwards);

  /** Page based on a scrollbar signal.
   *
   *  \param dir the direction to page: if \b true, call page_up();
   *       else call page_down().
   */
  void scroll(bool dir);

  /** Delete the root fragment. */
  ~vs_text_layout();

  /** A signal that is called whenever the "location" of the view
   *  within the text changes.
   */
  sigc::signal2<void, int, int> location_changed;

  static keybindings *bindings;

  static void init_bindings();
private:
  /** Move the starting location of the widget. */
  void set_start(unsigned int new_start);

  /** Update the cached contents of the widget, if necessary. */
  void freshen_contents(const style &st);

  /** Called when this needs layout. */
  void layout_me();

  /** Emits the above signal based on the present location of the display. */
  void do_signal();

  /** The line which is currently at the top of the widget. */
  size_t start;

  /** The root fragment of this layout.  This is always a clipbox. */
  fragment *f;

  /** Cache the current contents of the widget. */
  fragment_contents contents;

  /** If \b true, the current cached contents need to be updated. */
  bool stale;

  /** The width of the widget the last time we updated the cached contents. */
  int lastw;

  /** The enclosing display style the last time we updated the cached contents. */
  style lastst;
};

typedef ref_ptr<vs_text_layout> vs_text_layout_ref;

#endif
