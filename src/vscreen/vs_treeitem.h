// vs_treeitem.h    (this is -*-c++-*-)
//
//  Copyright 1999-2001, 2004-2005 Daniel Burrows
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
//  The basic tree item declaration (moved here to reduce the size of
// vs_tree.h)
//
//  I think this is *TOO* general.  A simplified version will probably be
// introduced at some point -- but the API should be fairly stable and nothing
// outside this file is likely to change significantly.  Yeah, right.

#ifndef VS_TREEITEM_H
#define VS_TREEITEM_H

#include <stdlib.h>
#include "curses++.h"
#include "config/style.h"

#include "vs_minibuf_win.h"

class vs_tree;
class vs_treeitem;
class sortpolicy;

class vs_tree_levelref
// A generic way to iterate over a *single level* of a tree.  Not really an
// "iterator" since it doesn't do the operators and so on.
{
  vs_tree_levelref *parent;
public:
  vs_tree_levelref():parent(NULL) {}
  vs_tree_levelref(const vs_tree_levelref &x)
    :parent(x.parent?x.parent->clone():NULL) {}
  virtual ~vs_tree_levelref() {}

  virtual vs_treeitem *get_item()=0;
  virtual void advance_next()=0;
  virtual void return_prev()=0;
  // Like ++ and --, sort of..

  virtual bool is_begin()=0;
  // Should return true if this iterator is at the front of a list (so going
  // back will do Really Bad Things) -- many STL containers need this..
  virtual bool is_end()=0;
  // Should return true if this iterator no longer refers to something valid.

  virtual vs_tree_levelref *clone() const=0;

  friend class vs_treeiterator;
};

class vs_tree_root_iterator:public vs_tree_levelref
// A dummy iterator for use on parent-less trees (all other iterators
// assume that they have a valid parent -- er?)
{
  vs_treeitem *val;
  vs_treeitem *prevval;

  vs_treeitem *get_item() {return val;}
 public:
  vs_tree_root_iterator(vs_treeitem *_val):val(_val), prevval(NULL) {}
  vs_tree_root_iterator(const vs_tree_root_iterator &x):vs_tree_levelref(x), val(x.val), prevval(x.prevval) {}

  void advance_next() {if(val) { prevval=val; val=NULL;} }
  void return_prev() {if(prevval) {val=prevval; prevval=NULL;} }

  vs_tree_root_iterator *clone() const {return new vs_tree_root_iterator(*this);}

  bool is_end() {return !val;}
  bool is_begin() {return prevval==NULL;}

  // Returns an "end iterator" for this tree
  vs_tree_root_iterator *end()
  {
    vs_tree_root_iterator *rval=new vs_tree_root_iterator(*this);
    rval->advance_next();
    return rval;
  }
};

class vs_treeitem
// An abstracted item (could be a leaf node or a subtree) -- this isn't really
// meant to be that useful in general, derive from the specialized children
// instead.
{
  int depth;
  bool selectable;
 protected:
  virtual void set_depth(int _depth) {depth=_depth;}
  virtual void set_selectable(bool _selectable) {selectable=_selectable;}
 public:
  vs_treeitem(bool _selectable=true):depth(0),selectable(_selectable) {}

  /** Display this item and this item only (does not descend to the
   *  children of the item, if any).  The current style of the
   *  corresponding tree widget will be initialized using
   *  get_normal_style() and/or get_highlight_style() prior to the
   *  invocation of this method.
   *
   *  \param win the tree in which to paint this item
   *  \param y the y location at which to paint this item
   *  \param hierarchical if \b true, paint this item as an
   *         entry in a 'hierarchical' tree.
   *  \param st the style with which this item is being displayed.
   */
  virtual void paint(vs_tree *win, int y, bool hierarchical,
		     const style &st)=0;

  /** Display the given text as the label of this item at the given
   *  shifted depth.
   *
   *  \param win the tree in which to paint this item
   *  \param y the y location at which to paint this item
   *  \param hierarchical if \b true, paint this item as an
   *                      entry in a 'hierarchical' tree
   *  \param st the style with which this item is to be displayed.
   */
  void paint(vs_tree *win, int y, bool hierarchical,
	     const std::wstring &str, int depth_shift=2);

  virtual const wchar_t *tag()=0;
  // The tag that this item should be sorted by [for the trivial version of
  // the subtree object]
  virtual const wchar_t *label()=0;
  // The label to display when this item is "active" in non-hierarchical mode.

  int get_depth() {return depth;}
  bool get_selectable() {return selectable;}

  virtual style get_normal_style() {return style();}
  virtual style get_highlight_style() {return get_normal_style()+style_attrs_flip(A_REVERSE);}

  virtual void sort(sortpolicy &sort_method) {}
  // Sorts an item's subtree using the given method.

  virtual void sort() {}
  // Sorts an item's subtree (NOP for most items) -- provided to make it easy
  // to recursively sort the list.

  virtual void highlighted(vs_tree *win);
  // Called when an item is highlighted
  virtual void unhighlighted(vs_tree *win) {}
  // Called when the highlight leaves an item.
  virtual bool dispatch_key(const key &k, vs_tree *owner) {return false;}
  // Called when a key is pressed while the item is highlighted.  The return
  // value indicates whether a redraw of the screen is needed.
  // (not any more; now it indicates whether the item consumed the keystroke
  //  -- this also triggers a screen repainting, but is a more useful semantic)
  virtual void dispatch_mouse(short id, int x, mmask_t bstate, vs_tree *owner)
  {
  }
  // Called when a mouse event occurs at the y location assigned to this
  // item.

  virtual vs_tree_levelref *begin() {return NULL;}
  virtual vs_tree_levelref *end() {return NULL;}
  // These can act like containers; these routines return a NEWLY ALLOCATED
  // POINTER, though.

  virtual bool has_visible_children() {return false;}
  virtual bool has_children() {return false;}

  virtual bool matches(const std::wstring &s) const {return false;}
  // Returns true if this item matches the given string.

  // More hackery: do what's needed to expand the children of this item.
  virtual void expand() {}
  virtual void expand_all() {}
  virtual void collapse_all() {}

  template<class childtype, class sorter>
    friend class vs_subtree;

  virtual ~vs_treeitem() {}
};

class vs_treeiterator
{
  vs_tree_levelref *curr;
  bool ignore_collapsed;

public:
  vs_treeiterator(vs_tree_levelref *_curr, bool _ignore_collapsed=false):curr(_curr),ignore_collapsed(_ignore_collapsed) {}
  vs_treeiterator(const vs_treeiterator &x):curr(x.curr?x.curr->clone():NULL),ignore_collapsed(x.ignore_collapsed) {}
  vs_treeiterator(const vs_treeiterator &x, bool _ignore_collapsed):curr(x.curr?x.curr->clone():NULL),ignore_collapsed(_ignore_collapsed) {}

  bool is_root() {return curr->parent==NULL;}
  // Ugh, a really nasty hack.
  vs_treeiterator get_up()
    {
      return vs_treeiterator(curr->parent->clone());
    }

  void expand()
  {
    if(curr && curr->get_item())
      curr->get_item()->expand();
  }

  vs_treeitem &operator*() {return *curr->get_item();}
  const vs_treeitem &operator*() const {return *curr->get_item();}
  vs_treeitem *operator->() {return curr->get_item();}
  const vs_treeitem *operator->() const {return curr->get_item();}

  bool operator==(const vs_treeiterator &x)
    {
      if(!curr)
	return !x.curr;
      else if(!x.curr)
	return false;
      else if(curr->is_end())
	return x.curr->is_end() && curr->parent == x.curr->parent;
      else if(x.curr->is_end())
	return false;
      else
	return curr->get_item()==x.curr->get_item();
    }
  bool operator!=(const vs_treeiterator &x)
    {return !(*this==x);}

  vs_treeiterator &operator=(const vs_treeiterator &x)
    {
      while(curr)
	{
	  vs_tree_levelref *old=curr;
	  curr=curr->parent;
	  delete old;
	}

      curr=x.curr?x.curr->clone():NULL;

      return *this;
    }

  vs_treeiterator &operator++()
    {
      if(curr->get_item() &&
	 (ignore_collapsed?curr->get_item()->has_children():curr->get_item()->has_visible_children()))
	{
	  vs_tree_levelref *newref=curr->get_item()->begin();
	  newref->parent=curr;
	  curr=newref;
	}
      else
	{
	  curr->advance_next();

	  while(curr->is_end() && curr->parent)
	    {
	      vs_tree_levelref *old=curr;
	      curr=curr->parent;
	      curr->advance_next();
	      delete old;
	    }
	}

      return *this;
    }
  vs_treeiterator operator++(int)
    {
      vs_treeiterator oldval(curr?curr->clone():NULL);

      ++*this;

      return oldval;
    }

  vs_treeiterator &operator--()
    {
      if(curr->is_begin())
	{
	  if(curr->parent)
	    {
	      vs_tree_levelref *old=curr;
	      curr=curr->parent;
	      delete old;
	    }
	}
      else
	{
	  curr->return_prev();
	  while(curr->get_item() &&
		(ignore_collapsed?curr->get_item()->has_children():curr->get_item()->has_visible_children()))
	    {
	      vs_tree_levelref *newref=curr->get_item()->end();

	      newref->parent=curr;
	      newref->return_prev();
	      curr=newref;
	    }
	}
      return *this;
    }

  vs_treeiterator operator--(int)
    {
      vs_treeiterator oldval(curr?curr->clone():NULL);

      --*this;

      return oldval;
    }

  void move_forward_level()
  {
    if(!curr->is_end())
      {
	vs_tree_levelref *old=curr->clone();
	curr->advance_next();

	if(curr->is_end())
	  {
	    delete curr;
	    curr=old;
	  }
	else
	  delete old;
      }
  }

  void move_backward_level()
  {
    if(!curr->is_begin())
      curr->return_prev();
  }

  ~vs_treeiterator()
    {
      while(curr)
	{
	  vs_tree_levelref *old=curr;
	  curr=curr->parent;
	  delete old;
	}
    }
};

// A generic sorter.
//
// Making operator() virtual is a bit icky.
class sortpolicy
{
public:
  sortpolicy() {}

  virtual bool operator()(vs_treeitem *item1,
			  vs_treeitem *item2)=0;

  virtual ~sortpolicy() {}
};

// ack!  How dare this be templated?
//template<class childtype>
class tag_sort_policy:public sortpolicy
{
public:
  bool operator()(vs_treeitem *item1, vs_treeitem *item2)
  {
    return (wcscmp(item1->tag(), item2->tag())<0);
  }
};

// Hack? hmm..
//
// Here's the situation: STL sort passes the sort method by value, probably
// to keep people from being horrendously inefficient like I'm about to.  But
// we can get around that with an inline wrapper:
class sortpolicy_wrapper
{
  sortpolicy &real_policy;
public:
  sortpolicy_wrapper(sortpolicy &_real_policy):real_policy(_real_policy)
  {
  }

  inline bool operator()(vs_treeitem *item1,
			 vs_treeitem *item2) const
  {
    return real_policy(item1, item2);
  }
};

#endif
