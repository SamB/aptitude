// vs_layout_item.h                                       -*-c++-*-
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
//
// A bridge from the layout code to the tree code.

#ifndef VS_LAYOUT_ITEM_H
#define VS_LAYOUT_ITEM_H

#include "vs_text_layout.h"
#include "vs_treeitem.h"

class vs_layout_item:public vs_treeitem
{
protected:
  class vs_layout_line;

private:
  typedef std::vector<vs_layout_line *> child_list;

  child_list children;
  fragment *f;
  fragment_contents lines;

  int lastw;
  int lastbasex;

protected:
  class vs_layout_line:public vs_treeitem
  {
    int n;
    vs_layout_item &parent;
  public:
    vs_layout_line(int _n, vs_layout_item &_parent);

    void paint(vs_tree *win, int y, bool hierarchical,
	       const style &st);

    const wchar_t *tag();
    const wchar_t *label();
  };

  // Assumes that children.size()>0
  class levelref:public vs_tree_levelref
  {
    size_t item_num;
    const child_list &lines;

  public:
    levelref(const levelref &x);
    levelref(size_t n, const child_list &_lines);

    vs_treeitem *get_item();
    virtual void advance_next();
    virtual void return_prev();
    bool is_begin();
    bool is_end();
    levelref *clone() const;
  };

public:
  vs_layout_item(fragment *f);

  const wchar_t *tag();
  const wchar_t *label();

  /** Paints the nth line of this item at the given location in 'win'. */
  void paint_line(int n,
		  vs_tree *win, int y, bool hierarchical,
		  const style &st);
  void paint(vs_tree *win, int y, bool hierarchical,
	     const style &st);

  int get_normal_attr();

  levelref *begin();
  levelref *end();
  bool has_visible_children();

  const fragment_line &get_line(vs_tree *win, size_t n, int basex,
				const style &st);

  ~vs_layout_item();
};

#endif
