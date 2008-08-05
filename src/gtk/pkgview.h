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

#include <cwidget/generic/util/ref_ptr.h>

namespace gui
{
  class PkgEntity : public Entity
  {
    private:
      string current_state_string();
      string selected_package_state_string();
      string selected_package_state_color();
      pkgCache::PkgIterator pkg;

    public:
      PkgEntity(const pkgCache::PkgIterator &_pkg) : pkg(_pkg) { }

      void activated(const Gtk::TreeModel::Path &path,
		     const Gtk::TreeViewColumn *column,
		     const EntityView *view);

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
      void fill_row(const EntityColumns *columns, Gtk::TreeModel::Row &row);
      void add_packages(std::set<pkgCache::PkgIterator> &packages);
      void add_actions(std::set<PackagesAction> &actions);
      void dispatch_action(PackagesAction action);

      pkgCache::PkgIterator get_pkg() { return pkg; }
      /** \brief Get the version, if any, that should be displayed. */
      pkgCache::VerIterator get_ver() const;
  };

  /** \brief Interface for generating tree-views.
   *
   *  A tree-view generator takes each package that appears in the
   *  current package view and places it into an encapsulated
   *  Gtk::TreeModel.
   */
  class PkgTreeModelGenerator
  {
  public:
    // FIXME: Hack while finding a nonblocking thread join.
    bool finished;
    PkgTreeModelGenerator()
    {
      finished = false;
    }
    virtual ~PkgTreeModelGenerator();

    /** \brief Add the given package and version to this tree view.
     *
     *  \param pkg  The package to add.
     */
    virtual void add(const pkgCache::PkgIterator &pkg) = 0;

    /** \brief Perform actions that need to be taken after adding all
     *  the packages.
     *
     *  For instance, this typically sorts the entire view.
     */
    virtual void finish() = 0;

    /** \brief Retrieve the model associated with this generator.
     *
     *  The model will be filled in as add_package() is invoked.
     *  Normally you should only use the model once it is entirely
     *  filled in (to avoid unnecessary screen updates).
     *
     *  \return  The model built by this generator.
     */
    virtual Glib::RefPtr<Gtk::TreeModel> get_model() = 0;
  };

  /** \brief Base class for views that display a subset of the packages
   *  found in the cache.
   *
   *  \todo The name is lame.
   */
  class PkgViewBase : public EntityView
  {
    Glib::ustring limit;
    PkgTreeModelGenerator *generator;

  public:
    PkgViewBase(const sigc::slot1<PkgTreeModelGenerator *, const EntityColumns *> generatorK,
		const Glib::RefPtr<Gnome::Glade::Xml> &refGlade,
		const Glib::ustring &gladename,
		const Glib::ustring &limit = "");

    virtual ~PkgViewBase();

    /** \brief Rebuild the store using the currently set limit. */
    virtual void rebuild_store();

    /** \brief Change this view's limit to the given string. */
    void set_limit(const Glib::ustring &limit);
  };

  /** \brief A view that displays an unorganized list of packages. */
  class PkgView : public PkgViewBase
  {
  public:
    class Generator : public PkgTreeModelGenerator
    {
      Glib::RefPtr<Gtk::ListStore> store;
      // We need this to set up the sorting at the end.
      const EntityColumns *columns;
    public:
      Generator(const EntityColumns *columns);
      static Generator *create(const EntityColumns *columns);

      void add(const pkgCache::PkgIterator &pkg);
      void finish();
      Glib::RefPtr<Gtk::TreeModel> get_model();
    };

    PkgView(const Glib::RefPtr<Gnome::Glade::Xml> &refGlade,
	    const Glib::ustring &gladename,
	    const Glib::ustring &limit = "");
  };
}

#endif /* PKGVIEW_H_ */
