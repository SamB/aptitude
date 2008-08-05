// -*-c++-*-

// entityview.h
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

#ifndef ENTITYVIEW_H_
#define ENTITYVIEW_H_

#undef OK
#include <gtkmm.h>
#include <libglademm/xml.h>

#include <generic/apt/apt.h>

namespace gui
{
  extern undo_group * undo;

  enum PackagesAction
  {
    /** \brief A synonym for Install.
     *
     *  This is used when building menus to decide whether to label
     *  the Install menu item "Install", "Upgrade", or
     *  "Install/Upgrade".
     */
    Upgrade, Downgrade, Install, Remove, Purge, Keep, Hold
  };

  class EntityView;
  class EntityColumns;

  class Entity
  {
    protected:
      EntityColumns * cols;
    public:
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
      virtual void fill_row(Gtk::TreeModel::Row &row) = 0;
      virtual pkgCache::PkgIterator get_pkg() = 0;
      virtual pkgCache::VerIterator get_ver() = 0;
      virtual void add_actions(std::set<PackagesAction> &actions) = 0;
      virtual void dispatch_action(PackagesAction action) = 0;
  };

  // This is a base class. Views can add model columns by deriving it.
  class EntityColumns : public Gtk::TreeModel::ColumnRecord
  {
    public:
      Gtk::TreeModelColumn<Entity*> EntObject;
      Gtk::TreeModelColumn<bool> BgSet;
      Gtk::TreeModelColumn<Glib::ustring> BgColor;
      Gtk::TreeModelColumn<Glib::ustring> Status;
      Gtk::TreeModelColumn<Glib::ustring> Name;
      Gtk::TreeModelColumn<Glib::ustring> Version;

      EntityColumns();
  };

  class EntityTreeView : public Gtk::TreeView
  {
    public:
      EntityTreeView(BaseObjectType* cobject, const Glib::RefPtr<Gnome::Glade::Xml>& refGlade);
      bool on_button_press_event(GdkEventButton* event);
      sigc::signal<void, GdkEventButton*> signal_context_menu;
      sigc::signal<void> signal_selection;
  };

  /** \brief Interface for generating tree-views.
   *
   *  A tree-view generator takes each package that appears in the
   *  current package view and places it into an encapsulated
   *  Gtk::TreeModel.
   */
  class EntityTreeModelGenerator
  {
    private:
      // FIXME: Hack while finding a nonblocking thread join.
      Glib::RefPtr<Gtk::TreeModel> store;
      std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator> * revstore;
    public:
      //~EntityTreeModelGenerator();

      /** \brief Retrieve the model associated with this generator.
       *
       *  The model will be filled in as add_package() is invoked.
       *  Normally you should only use the model once it is entirely
       *  filled in (to avoid unnecessary screen updates).
       *
       *  \return  The model built by this generator.
       */
      Glib::RefPtr<Gtk::TreeModel> get_store() const { return store; };

      std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator> * get_reverse_store() { return revstore; };

      void dump_stores(EntityView * entview);

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
      virtual void build_store(Glib::ustring limit) = 0;
  };

  class EntityView
  {
    private:
      EntityTreeView * tree;
      EntityColumns * cols;
      EntityTreeModelGenerator * generator;
      Glib::RefPtr<Gtk::TreeModel> store;
      std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator> * revstore;
      void init(EntityTreeModelGenerator * generator,
                Glib::RefPtr<Gnome::Glade::Xml> refGlade,
                Glib::ustring gladename);

      void on_cache_closed();
      void on_cache_reloaded();

      Gtk::TreeViewColumn * Status;
      Gtk::TreeViewColumn * Name;
      Gtk::TreeViewColumn * Version;

      /** \brief Sets up generic column properties that don't have to do
       *  with creating the renderer.
       */
      void setup_column_properties(Gtk::TreeViewColumn *treeview_column, int size);

      /** \brief Creates a column with a default renderer. */
      template<class ColumnType>
        int append_column(const Glib::ustring &title, Gtk::TreeViewColumn *treeview_column,
            Gtk::TreeModelColumn<ColumnType> &model_column, int size);

      /** \brief Creates a column that uses the given model column as
       *  Pango markup.
       */
      int append_markup_column(const Glib::ustring &title, Gtk::TreeViewColumn *treeview_column,
          Gtk::TreeModelColumn<Glib::ustring> &model_column, int size);

      /** \brief Build a menu of package actions. */
      Gtk::Menu * get_menu(const std::set<PackagesAction> &actions, sigc::slot1<void, PackagesAction> callback);

      /** \brief Apply the given action to all the currently selected packages. */
      void apply_action_to_selected(PackagesAction action);

    public:
      /** \brief Fill in the contents of a tree-model row for a header.
       *
       *  \param row                 The row to fill in; any existing values
       *                             will be overwritten.
       *  \param text                The text content of the header.
       */
      void fill_header(Gtk::TreeModel::Row &row, Glib::ustring text);

      /** \brief Construct a new packages view.
       *
       *  The store will not be initialized if _limit is not set.
       *
       *  \param _generatorK A constructor for the callback
       *                     object used to build the model.
       *  \param refGlade    The XML tree containing
       *                     the widgets for this view.
       *  \param gladename   The Glade name of the widget.
       *  \param  limit      The limit pattern for the current view.
       */
      EntityView(EntityTreeModelGenerator * generator,
                 Glib::RefPtr<Gnome::Glade::Xml> refGlade,
                 Glib::ustring gladename,
                 Glib::ustring limit = "");
      ~EntityView();
      void context_menu_handler(GdkEventButton * event);
      void row_activated_handler(const Gtk::TreeModel::Path &, Gtk::TreeViewColumn*);
      void refresh_view(const std::set<pkgCache::PkgIterator> *changed_packages);
      void relimit_view(EntityTreeModelGenerator * generator, Glib::ustring limit);
      EntityTreeView * get_treeview() const { return tree; };
      EntityColumns * get_columns() const { return cols; };
      Glib::RefPtr<Gtk::TreeModel> get_store() const { return store; };
      std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator> * get_reverse_store() const { return revstore; };
      void swap_stores(std::pair<Glib::RefPtr<Gtk::TreeModel>,
          std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator> *> stores);
  };

}

#endif /* ENTITYVIEW_H_ */
