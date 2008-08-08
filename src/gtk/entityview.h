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

#include <cwidget/generic/util/ref_ptr.h>

namespace gui
{
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

  /** \brief A class meant to be wrapped in cwidget::ref_ptr objects. */
  class refcounted_base : public sigc::trackable
  {
    mutable int refcount;

  public:
    refcounted_base() : refcount(0) { }
    virtual ~refcounted_base();

    void incref() { ++refcount; }
    void decref()
    {
      --refcount;
      if(refcount == 0)
	delete this;
    }
  };

  class Entity : public refcounted_base
  {
    public:
      virtual ~Entity();

      /** \brief Fill in the contents of a tree-model row for this entity.
       *
       *  The entity should be placed in the 
       *
       *  \param row                 The row to fill in; any existing values
       *                             will be overwritten.
       *  \param pkg                 The package to display in this row.
       *  \param ver                 The version to display in this row.
       *  \param version_specific    The row is version specific (influences
       *                             coloring and selected status display)
       */
      virtual void fill_row(const EntityColumns *columns, Gtk::TreeModel::Row &row) = 0;

      /** \brief Invoked when the row is double-clicked. */
      virtual void activated(const Gtk::TreeModel::Path &path,
			     const Gtk::TreeViewColumn *column,
			     const EntityView *view) = 0;

      /** \brief Retrieve the set of packages upon which this row depends.
       *
       *  This is the set of packages that should trigger a redraw of this
       *  row when they change.
       *
       *  If this becomes a bottleneck, consider switching to vector or to
       *  returning a single package.
       */
      virtual void add_packages(std::set<pkgCache::PkgIterator> &packages) = 0;

      virtual void add_actions(std::set<PackagesAction> &actions) = 0;

      virtual void dispatch_action(PackagesAction action) = 0;
  };
  typedef cwidget::util::ref_ptr<Entity> EntityRef;

  /** \brief An entity that is responsible for a header row. */
  class HeaderEntity : public Entity
  {
    Glib::ustring text;
  public:
    HeaderEntity(const Glib::ustring &_text) : text(_text) { }

    void fill_row(const EntityColumns *columns, Gtk::TreeModel::Row &row);
    void add_packages(std::set<pkgCache::PkgIterator> &packages);
    void activated(const Gtk::TreeModel::Path &path,
		   const Gtk::TreeViewColumn *column,
		   const EntityView *view);
    void add_actions(std::set<PackagesAction> &actions);
    void dispatch_action(PackagesAction action);

    void set_text(const Glib::ustring &_text) { text = _text; }
  };

  // This is a base class. Views can add model columns by deriving it.
  class EntityColumns : public Gtk::TreeModel::ColumnRecord
  {
    public:
      Gtk::TreeModelColumn<EntityRef> EntObject;
      Gtk::TreeModelColumn<bool> BgSet;
      Gtk::TreeModelColumn<Glib::ustring> BgColor;
      Gtk::TreeModelColumn<Glib::ustring> Status;
      /** \brief The markup that's displayed in the "name" column. */
      Gtk::TreeModelColumn<Glib::ustring> NameMarkup;
      /** \brief The markup that's displayed in the "version" column. */
      Gtk::TreeModelColumn<Glib::ustring> VersionMarkup;
      /** \brief The string that the "name" column should be sorted on. */
      Gtk::TreeModelColumn<Glib::ustring> Name;
      /** \brief The string that the "version" column should be sorted on. */
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

  class EntityView : public refcounted_base
  {
    private:
      EntityTreeView * tree;
      EntityColumns cols;

      std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator> revstore;
      void init(Glib::RefPtr<Gnome::Glade::Xml> refGlade,
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
      int append_column(const Glib::ustring &title, Gtk::TreeViewColumn *&treeview_column,
			Gtk::TreeModelColumn<ColumnType> &model_column, int size);

      /** \brief Creates a column that uses the given model column as
       *  Pango markup.
       */
      int append_markup_column(const Glib::ustring &title, Gtk::TreeViewColumn *&treeview_column,
			       Gtk::TreeModelColumn<Glib::ustring> &model_column, int size);

      /** \brief Build a menu of package actions. */
      Gtk::Menu * get_menu(const std::set<PackagesAction> &actions, const sigc::slot1<void, PackagesAction> &callback) const;

      /** \brief Apply the given action to all the currently selected packages. */
      void apply_action_to_selected(PackagesAction action);

      void context_menu_handler(GdkEventButton * event);
      void row_activated_handler(const Gtk::TreeModel::Path &, Gtk::TreeViewColumn*);
    public:
      /** \brief Construct a new packages view.
       *
       *  \param refGlade    The XML tree containing
       *                     the widgets for this view.
       *  \param gladename   The Glade name of the widget.
       */
      EntityView(Glib::RefPtr<Gnome::Glade::Xml> refGlade,
                 Glib::ustring gladename);
      virtual ~EntityView();

      // TODO: perhaps rebuild_store() should be a virtual function
      // invoked when the global cache is reopened.  This would amount
      // to a design decision that EntityViews are always dependent on
      // the cache state, which I think will always be the case, but
      // I'd like to wait on doing this.  Also, handling reloads
      // properly could involve tab-level coordination anyway.  --
      // dburrows 2008-08-05

      void refresh_view(const std::set<pkgCache::PkgIterator> *changed_packages);
      EntityTreeView * get_treeview() const { return tree; };
      const EntityColumns * get_columns() const { return &cols; };
      Gtk::TreeViewColumn *get_status_column() const { return Status; }
      Gtk::TreeViewColumn *get_name_column() const { return Name; }
      Gtk::TreeViewColumn *get_version_column() const { return Version; }
      Glib::RefPtr<Gtk::TreeModel> get_model() const { return get_treeview()->get_model(); };
      const std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator> * get_reverse_store() const { return &revstore; };
      std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator> * get_reverse_store() { return &revstore; };
      void set_model(const Glib::RefPtr<Gtk::TreeModel> &model);
  };

}

#endif /* ENTITYVIEW_H_ */
