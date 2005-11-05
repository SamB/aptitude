// vs_subtree.h  (this is -*-c++-*-)
//
//  Copyright 1999-2003, 2005 Daniel Burrows
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
//  Subtrees for vs_trees.

#ifndef VS_SUBTREE_H
#define VS_SUBTREE_H

#include <list>
#include "vs_treeitem.h"
#include "config/keybindings.h"
// Hmm, this seems to be necessary for the bindings..
#include "vs_tree.h"

class vs_tree;

template<class childtype, class default_sorter=tag_sort_policy>
class vs_subtree:virtual public vs_treeitem
// A tree-item which can contain other tree items.  Still abstract -- the
// display routines have to be filled in, and you may want to add more behavior
// on keypresses -- but we're getting there :)
{
protected:

  typedef std::list<childtype *> child_list;
  typedef typename std::list<childtype *>::iterator child_iterator;

  class levelref:public vs_tree_levelref
  {
    child_iterator realitem;

    child_list *parent_list;
  public:
    levelref(const levelref &x)
      :vs_tree_levelref(x), realitem(x.realitem), parent_list(x.parent_list) {}
    levelref(const child_iterator &_realitem, child_list *_parent_list)
      :realitem(_realitem), parent_list(_parent_list)
    {
    }

    vs_treeitem *get_item() {eassert(realitem!=parent_list->end()); return *realitem;}
    virtual void advance_next() {++realitem;}
    virtual void return_prev() {--realitem;}
    bool is_begin() {return realitem==parent_list->begin();}
    bool is_end() {return realitem==parent_list->end();}
    levelref *clone() const {return new levelref(*this);}
  };

private:
  bool expanded;

  child_list children;

protected:
  child_iterator get_children_begin() {return children.begin();}
  child_iterator get_children_end() {return children.end();}

public:
  typedef vs_treeiterator iterator;
  typedef default_sorter default_sort;

  vs_subtree(bool _expanded):vs_treeitem(),expanded(_expanded) {}

  bool get_expanded() {return expanded;}

  void expand() {expanded=true;}

  void expand_all()
  {
    expanded=true;
    for(child_iterator i=children.begin(); i!=children.end(); i++)
      (*i)->expand_all();
  }

  void collapse_all()
  {
    expanded=false;
    for(child_iterator i=children.begin(); i!=children.end(); i++)
      (*i)->collapse_all();
  }

  void paint(vs_tree *win, int y, bool hierarchical,
	     const std::wstring &str, int depth_shift=2)
  {
    int width, height;
    int basex=hierarchical?depth_shift*get_depth():0;
    win->getmaxyx(height,width);

    win->move(y,0);

    int x=0;
    while(x<basex && x<width)
      {
	win->add_wch(L' ');
	x+=wcwidth(L' ');
      }

    if(basex>width)
      return;

    const wchar_t *ws;
    if(hierarchical)
      ws=get_expanded()?L"--\\ ":L"--- ";
    else
      ws=L"-> ";

    while(*ws!=0 && x<width)
      {
	win->add_wch(*ws);
	x+=wcwidth(*ws);
	++ws;
      }

    if(x>=width)
      return;

    size_t i=0;
    while(i<str.size() && x<width)
      {
	wchar_t ch=str[i];

	win->add_wch(ch);
	x+=wcwidth(ch);
	++i;
      }

    while(x<width)
      {
	win->add_wch(L' ');
	x+=wcwidth(L' ');
      }
  }

  void set_depth(int _depth)
  {
    for(child_iterator i=children.begin(); i!=children.end(); i++)
      (*i)->set_depth(_depth+1);

    vs_treeitem::set_depth(_depth);
  }

  void add_child(childtype *newchild)
  {
    newchild->set_depth(get_depth()+1);

    children.push_back(newchild);
  }

  // Adds a new child item at an unspecified location -- you should call sort()
  // after adding children or the tree will have an undetermined order.  (yes,
  // you can deduce the real order.  Don't.)
  void sort(sortpolicy &sort_method)
  {
    for(child_iterator i=children.begin(); i!=children.end(); i++)
      (*i)->sort(sort_method);

    children.sort(sortpolicy_wrapper(sort_method));
  }

  void sort()
  {
    default_sort sorter;
    sort(sorter);
  }

  virtual bool dispatch_key(const key &k, vs_tree *owner)
  {
    if(vs_tree::bindings->key_matches(k, "ToggleExpanded"))
      {
	expanded=!expanded;
	return true;
      }
    else if(vs_tree::bindings->key_matches(k, "ExpandTree"))
      {
        if(!expanded)
	  {
	    expanded=true;
	    return true;
	  }
	else
	  return false;
      }
    else if(vs_tree::bindings->key_matches(k, "CollapseTree"))
      {
	if(expanded)
	{
	  expanded=false;
	  return true;
	} else
	  return false;
      }
    else if(vs_tree::bindings->key_matches(k, "ExpandAll"))
      {
	expand_all();
	return true;
      }
    else if(vs_tree::bindings->key_matches(k, "CollapseAll"))
      {
	collapse_all();
	return true;
      }
    return false;
  }
  // The default action is to expand or shrink the tree when Enter is pressed.
  // FIXME: should I use '+' and '-' here?  That would appear to conflict with
  // the other keybindings I need.  Hm.

  virtual void dispatch_mouse(short id, int x, mmask_t bstate, vs_tree *owner)
  {
    if(bstate & (BUTTON1_DOUBLE_CLICKED | BUTTON2_DOUBLE_CLICKED |
		 BUTTON3_DOUBLE_CLICKED | BUTTON4_DOUBLE_CLICKED |
		 BUTTON1_TRIPLE_CLICKED | BUTTON2_TRIPLE_CLICKED |
		 BUTTON3_TRIPLE_CLICKED | BUTTON4_TRIPLE_CLICKED))
      expanded=!expanded;
  }

  virtual levelref *begin() {return new levelref(children.begin(), &children);}
  virtual levelref *end() {return new levelref(children.end(), &children);}

  bool has_visible_children() {return expanded && children.size()>0;}
  bool has_children() {return children.size()>0;}

  virtual ~vs_subtree()
  {
    child_iterator i,j;
    for(i=children.begin(); i!=children.end(); i=j)
      {
	j=i;
	j++;
	delete *i;
      }
  }
};

class vs_subtree_generic:public vs_subtree<vs_treeitem>
{
public:
  vs_subtree_generic(int _expanded):vs_subtree<vs_treeitem>(_expanded) {}
};

#endif
