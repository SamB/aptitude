// -*-c++-*-

// pkgview.h
//
//  Copyright 1999-2008 Daniel Burrows
//  Copyright 2008 Obey Arthur Liu
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

#ifndef PKGVIEW_H_
#define PKGVIEW_H_

#undef OK
#include <gtkmm.h>

#include <generic/apt/apt.h>

#include <gtk/entityview.h>

namespace gui
{
  class PkgEntity : public Entity
  {
    private:
      string current_state_string();
      string selected_package_state_string();
      string selected_package_state_color();
    public:
      pkgCache::PkgIterator pkg;
      /** \brief Fill in the contents of a tree-model row for the given
       *  package/version pair.
       *
       *  \param row                 The row to fill in; any existing values
       *                             will be overwritten.
       *  \param pkg                 The package to display in this row.
       *  \param ver                 The version to display in this row.
       *  \param version_specific    The row is version specific (influences
       *                             coloring and selected status display)
       */
      void fill_row(Gtk::TreeModel::Row &row);
      pkgCache::PkgIterator get_pkg();
      pkgCache::VerIterator get_ver();
      void add_actions(std::set<PackagesAction> &actions);
      void dispatch_action(PackagesAction action);
      PkgEntity(EntityColumns * cols, pkgCache::PkgIterator pkg);
  };
}

#endif /* PKGVIEW_H_ */
