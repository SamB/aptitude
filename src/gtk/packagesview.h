// -*-c++-*-

// packagesview.h
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

#ifndef PACKAGESVIEW_H_
#define PACKAGESVIEW_H_

#undef OK
#include <gtkmm.h>
#include <libglademm/xml.h>

#include <apt-pkg/error.h>

#include <generic/apt/apt.h>

#include <gtk/entityview.h>
#include <gtk/progress.h>

namespace gui
{

  /** \brief Interface for generating tree-views.
   *
   *  A tree-view generator takes each package that appears in the
   *  current package view and places it into an encapsulated
   *  Gtk::TreeModel.
   */
  class PackagesViewGenerator : public EntityTreeModelGenerator
  {
    private:
      guiOpProgress * p;
      int num;
      int total;
      bool finished;
      EntityColumns * cols;
      Glib::RefPtr<Gtk::ListStore> store;
      std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator> * revstore;
      void add(pkgCache::PkgIterator pkg);
      bool add_reverse(const Gtk::TreeModel::iterator iter);
      void prefinish();
      void finish();
    public:
      /** \brief Build the tree model using the given generator.
       *
       *  This adds all packages that pass the current limit to the
       *  generator, one at a time.
       *
       *  \param  generatorK         A function that constructs a generator
       *                             to use in building the new store.
       *  \param  packages_columns   The columns of the new store.
       *  \param  reverse_packages_store  A multimap to be filled with the
       *                                  location of each package iterator
       *                                  in the generated store.
       *  \param  limit             The limit pattern for the current view.
       */
      void build_store(Glib::ustring limit);

      PackagesViewGenerator(EntityColumns *cols);
  };

}

#endif /* PACKAGESVIEW_H_ */
