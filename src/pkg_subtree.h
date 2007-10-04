// pkg_subtree.h (this is -*-c++-*-)
//
//  Copyright 1999-2002, 2004-2005, 2007 Daniel Burrows
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
//  A subtree which contains packages (and other subtrees)

#ifndef PKG_SUBTREE_H
#define PKG_SUBTREE_H

#include "vscreen/vs_subtree.h"

#include "pkg_node.h"

class pkg_subtree:public vs_subtree<pkg_tree_node>,
		  public pkg_tree_node
{
  std::wstring name;
  std::wstring description; // This is like a Description: field.

  sigc::signal1<void, std::wstring> *info_signal;

  void do_highlighted_changed(bool highlighted);
protected:
  void set_label(const std::wstring &_name) {name=_name;}
public:
  pkg_subtree(std::wstring _name, std::wstring _description=L"",
	      sigc::signal1<void, std::wstring> *_info_signal=NULL,
	      bool _expanded=false):
    vs_subtree<pkg_tree_node>(_expanded), name(_name),
    description(_description), info_signal(_info_signal)
  {
    highlighted_changed.connect(sigc::mem_fun(this, &pkg_subtree::do_highlighted_changed));
  }

  pkg_subtree(std::wstring _name, bool _expanded):
    vs_subtree<pkg_tree_node>(_expanded), name(_name),
    description(L""), info_signal(NULL)
  {
    highlighted_changed.connect(sigc::mem_fun(this, &pkg_subtree::do_highlighted_changed));
  }

  virtual void paint(vs_tree *win, int y, bool hierarchical,
		     const style &st);
  virtual const wchar_t *tag();
  virtual const wchar_t *label();

  virtual void select(undo_group *undo);
  virtual void hold(undo_group *undo);
  virtual void keep(undo_group *undo);
  virtual void remove(undo_group *undo);
  virtual void purge(undo_group *undo);
  virtual void reinstall(undo_group *undo);
  virtual void set_auto(bool isauto, undo_group *undo);

  std::wstring get_name() {return name;}
  std::wstring get_description() {return description;}

  bool dispatch_key(const key &k, vs_tree *owner);
};

#endif
