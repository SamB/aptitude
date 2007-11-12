// pkg_item_with_subtree.h   (this is -*-c++-*-)
//
//  Copyright 1999-2002, 2005 Daniel Burrows
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
//  A package item with an associated subtree.  More useful than you might
// think ;-)

#ifndef PKG_ITEM_WITH_SUBTREE_H
#define PKG_ITEM_WITH_SUBTREE_H

#include "pkg_item.h"
#include "pkg_columnizer.h"

#include <vscreen/columnify.h>
#include <vscreen/config/column_definition.h>
#include <vscreen/transcode.h>
#include <cwidget/widgets/subtree.h>

template<class child, class sorter=tag_sort_policy>
class pkg_item_with_subtree:public vs_subtree<child, sorter>, public pkg_item
{
  class pkg_with_subtree_columnizer:public pkg_columnizer
  {
    bool expanded, hierarchical;
  protected:
    column_disposition setup_column(int type)
      {
	if(type==name)
	  {
	    std::wstring name=transcode(get_pkg().Name());

	    // Interesting question: would it be reasonable to use graphical
	    // characters here?  What about Unicode graphical characters?
	    if(hierarchical)
	      return column_disposition(std::wstring(expanded?L"--\\ ":L"--- ")+name, get_basex());
	    else
	      {
		return column_disposition(L"-> "+name, get_basex());
	      }
	  }
	else
	  return pkg_columnizer::setup_column(type);
      }

  public:
    pkg_with_subtree_columnizer(bool _expanded, bool _hierarchical,
				const pkgCache::PkgIterator &_pkg,
				const pkgCache::VerIterator &_visible_ver,
				const column_definition_list &_columns, int _basex)
      :pkg_columnizer(_pkg, _visible_ver, _columns, _basex),
       expanded(_expanded), hierarchical(_hierarchical)
    {}
  };

public:
  pkg_item_with_subtree<child,sorter>(const pkgCache::PkgIterator &_package,
				      sigc::signal2<void, const pkgCache::PkgIterator &, const pkgCache::VerIterator &> *_sig,
				      bool _expanded=false):vs_subtree<child, sorter>(_expanded), pkg_item(_package, _sig) {}

  virtual void paint(vs_tree *win, int y, bool hierarchical,
		     const style &st)
  {
    int basex=2*get_depth();
    int width, height;

    win->getmaxyx(height, width);
    pkg_columnizer::setup_columns();

    empty_column_parameters p;

    std::wstring disp=pkg_with_subtree_columnizer(vs_subtree<child, sorter>::get_expanded(),
						  hierarchical,
						  get_package(),
						  visible_version(),
						  pkg_columnizer::get_columns(),
						  basex).layout_columns(width, p);
    win->mvaddnstr(y, 0, disp, width);
  }

  bool dispatch_key(const key &k, vs_tree *owner)
  {
    if(vs_subtree<child, sorter>::dispatch_key(k, owner))
      return true;
    else
      return pkg_item::dispatch_key(k, owner);
  }

  void dispatch_mouse(short id, int x, mmask_t bstate, vs_tree *owner)
  {
    pkg_item::dispatch_mouse(id, x, bstate, owner);
  }
};

typedef pkg_item_with_subtree<vs_treeitem> pkg_item_with_generic_subtree;
typedef pkg_item_with_subtree<pkg_tree_node> pkg_item_with_pkgnode_subtree;

#endif
