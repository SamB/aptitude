// vs_tree.h  (this is -*-c++-*-)
//
//  Copyright 1999-2001, 2004-2006 Daniel Burrows
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
//  A simple tree displayer that is a vscreen by itself.  Note that
// you can't modify a tree's contents once it has been created (although you
// can change the display order), and that everything put into the tree until
// the first subtree will display as a flat list.
//
//  Displaying the tree-ness is up to the derived classes (a generic
// indentation policy probably won't work)
//
//  One more point: after much consideration, I have decided that
// *HETEROGENOUS TREES ARE NOT ALLOWED*.  There's just no good way to flexibly
// sort such a beast.  I don't anticipate this being a major problem, though.
//  (note that while not allowed in general, they can sort of be done -- just
// sort with the key item.  A default class that does just this is provided)

#ifndef VS_TREE_H
#define VS_TREE_H

#include "vscreen_widget.h"
#include "vs_treeitem.h"

#include <list>
#include <generic/util/eassert.h>

class keybindings;

// A predicate on vs_treeitems:
class vs_tree_search_func
{
public:
  virtual bool operator()(const vs_treeitem &item)=0;
  virtual ~vs_tree_search_func() {}
};

class vs_tree_search_string:public vs_tree_search_func
{
  std::wstring s;
public:
  vs_tree_search_string(const std::wstring &_s):s(_s) {}

  virtual bool operator()(const vs_treeitem &item);
};

class vs_tree:public vscreen_widget
{
  vs_treeitem *root;
  vs_treeiterator begin, end;

  vs_treeiterator top;
  vs_treeiterator selected;
  // The top item on the current page and the currently selected item.
  // NOTE: it's implicitly assumed in many places in the code that the 
  // currently selected item is visible (ie, on the screen).

  bool hierarchical;
  // If not true, display the tree as a series of "flat thingies".
  // Must be seen to be described :)

  // This structure is used to easily retrace our steps in flat-mode.
  // (it could probably be done without this, but this makes it MUCH simpler)
  // Note that we don't even bother with an STL list here; it's
  // just not worth it.
  struct flat_frame
  {
    vs_treeiterator begin, end, top, selected;

    flat_frame *next;
    flat_frame(vs_treeiterator _begin,
	       vs_treeiterator _end,
	       vs_treeiterator _top,
	       vs_treeiterator _selected,
	       flat_frame *_next)
      :begin(_begin), end(_end), top(_top), selected(_selected), next(_next) {}
  };
  flat_frame *prev_level;

  int line_of(vs_treeiterator item);
  bool item_visible(vs_treeiterator item);

  void do_shown();
protected:
  vs_treeiterator get_selected() {return selected;}
  vs_treeiterator get_begin() {return begin;}
  vs_treeiterator get_end() {return end;}

  void sync_bounds();
  // This is an awful hack; I've been thinking about an alternate design of
  // the tree code for a while, and this just confirms it.  Yuck! :)
  //  It'll be the first thing to be removed in the next version..
  //  -- well, it wasn't.

  virtual bool handle_key(const key &k);

protected:
  vs_tree();
  vs_tree(vs_treeitem *_root, bool showroot);

public:
  static ref_ptr<vs_tree>
  create()
  {
    ref_ptr<vs_tree> rval(new vs_tree);
    rval->decref();
    return rval;
  }

  static ref_ptr<vs_tree>
  create(vs_treeitem *root, bool showroot = false)
  {
    ref_ptr<vs_tree> rval(new vs_tree(root, showroot));
    rval->decref();
    return rval;
  }

  void set_root(vs_treeitem *_root, bool showroot=false);

  /** \return the desired width of the widget. */
  int width_request();

  /** \param w the width of the widget.
   *
   *  \return the desired height of the widget for the given width.
   */
  int height_request(int w);

  bool get_cursorvisible();
  point get_cursorloc();
  virtual bool focus_me() {return true;}
  virtual void paint(const style &st);
  virtual void dispatch_mouse(short id, int x, int y, int z, mmask_t bstate);

  void set_selection(vs_treeiterator to);
  // Directly sets the selection to a given item.  [ the item must be
  // visible -- ie, all its parents must be expanded ]
  // Causes a redraw.
  virtual ~vs_tree();

  void search_for(vs_tree_search_func &matches);
  void search_for(const std::wstring &s)
  {
    vs_tree_search_string matches(s);
    search_for(matches);
  }

  void search_back_for(vs_tree_search_func &matches);
  void search_back_for(const std::wstring &s)
  {
    vs_tree_search_string matches(s);
    search_back_for(matches);
  }

  void set_hierarchical(bool _hierarchical);
  bool get_hierarchical() {return hierarchical;}

  /** Send a 'highlighted' message to the currently selected item. */
  void highlight_current();

  /** Send an 'unhighlighted' message to the currently selected item. */
  void unhighlight_current();


  // Execute the given command
  void line_up();
  void line_down();
  void page_up();
  void page_down();
  void jump_to_begin();
  void jump_to_end();
  void level_line_up();
  void level_line_down();

  static keybindings *bindings;
  static void init_bindings();
  // Sets up the bindings..
};

typedef ref_ptr<vs_tree> vs_tree_ref;

#endif
