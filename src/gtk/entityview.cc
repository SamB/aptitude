// entityview.cc
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

#include "entityview.h"
#include "aptitude.h"

#undef OK
#include <gtkmm.h>

#include <generic/apt/apt_undo_group.h>

#include <gtk/info.h>

namespace gui
{
  Entity::~Entity()
  {
  }

  refcounted_base::~refcounted_base()
  {
  }

  void HeaderEntity::fill_row(const EntityColumns *cols, Gtk::TreeModel::Row &row)
  {
    row[cols->EntObject] = this;

    row[cols->BgColor] = "light yellow"; // Say we want blue header..
    row[cols->BgSet] = true; // We do want to put color in there, yes.

    row[cols->Status] = ""; // dummy

    // This is the content of the header
    // TODO: Maybe we should delegate the markup to the caller
    row[cols->Name] = "<span size=\"large\">" + Glib::Markup::escape_text(text) + "</span>";

    row[cols->Version] = ""; // dummy
  }

  void HeaderEntity::activated(const Gtk::TreeModel::Path &path,
			       const Gtk::TreeViewColumn *column,
			       const EntityView *view)
  {
  }

  void HeaderEntity::add_packages(std::set<pkgCache::PkgIterator> &packages)
  {
  }

  void HeaderEntity::add_actions(std::set<PackagesAction> &actions)
  {
  }

  void HeaderEntity::dispatch_action(PackagesAction action)
  {
  }

  EntityColumns::EntityColumns()
  {
    add(EntObject);
    add(BgSet);
    add(BgColor);
    add(Status);
    add(Name);
    add(Version);
  }

  EntityTreeView::EntityTreeView(BaseObjectType* cobject, const Glib::RefPtr<Gnome::Glade::Xml>& refGlade)
  : Gtk::TreeView(cobject)
  {
    ;;
  }

  bool EntityTreeView::on_button_press_event(GdkEventButton* event)
  {
    bool return_value = true;

    if ((event->type == GDK_BUTTON_PRESS) && (event->button == 3))
    {
      Gtk::TreeModel::Path path;
      Gtk::TreeViewColumn *column;
      int cell_x;
      int cell_y;

      if(get_path_at_pos(round(event->x), round(event->y),
                         path, column,
                         cell_x, cell_y))
        {
          // If this row isn't already selected, change the selection
          // to just it.
          Glib::RefPtr<Gtk::TreeView::Selection> selection = get_selection();

          // We could try letting the user expand the selection by
          // holding a shift key down.  I decided not to because I
          // couldn't figure out an obviously right semantics for it,
          // so I figured behaving the same way all the time was the
          // best shot at following the Principle of Least Surprise.
          if(!selection->is_selected(path))
            {
              selection->unselect_all();
              selection->select(path);
            }

          signal_context_menu(event);
        }
    }
    else if ((event->type == GDK_BUTTON_PRESS) && (event->button == 1))
    {
      //Call base class, to allow normal handling,
      //such as allowing the row to be selected by the right-click:
      return_value = Gtk::TreeView::on_button_press_event(event);
      signal_selection();
    }
    else if ((event->type == GDK_2BUTTON_PRESS) && (event->button == 1))
    {
      //Call base class, to allow normal handling,
      //such as allowing the regular signals to be emitted:
      return_value = Gtk::TreeView::on_button_press_event(event);
    }
    return return_value;
  }

  void EntityView::init(Glib::RefPtr<Gnome::Glade::Xml> refGlade,
                        Glib::ustring gladename)
  {
    refGlade->get_widget_derived(gladename, tree);

    tree->signal_context_menu.connect(sigc::mem_fun(*this, &EntityView::context_menu_handler));
    tree->signal_row_activated().connect(sigc::mem_fun(*this, &EntityView::row_activated_handler));
    if(apt_cache_file != NULL)
      (*apt_cache_file)->package_states_changed.connect(sigc::mem_fun(*this, &EntityView::refresh_view));
    cache_closed.connect(sigc::mem_fun(*this, &EntityView::on_cache_closed));
    cache_reloaded.connect(sigc::mem_fun(*this, &EntityView::on_cache_reloaded));

    append_column(Glib::ustring(_("Status")), Status, cols.Status, 32);
    append_markup_column(Glib::ustring(_("Name")), Name, cols.Name, 350);
    {
      Gtk::CellRenderer *renderer = tree->get_column_cell_renderer(tree->get_columns().size() - 1);
      if(renderer == NULL)
        std::cerr << "Why don't I have a renderer when I just added one?" << std::endl;
      else
        {
          Gtk::CellRendererText *renderer_text = dynamic_cast<Gtk::CellRendererText *>(renderer);
          if(renderer_text == NULL)
            std::cerr << "Why don't I have a text renderer when I just added one?" << std::endl;
          else
            renderer_text->property_ellipsize() = Pango::ELLIPSIZE_END;
        }
    }
    append_markup_column(Glib::ustring(_("Version")), Version, cols.Version, 80);
    {
      Gtk::CellRenderer *renderer = tree->get_column_cell_renderer(tree->get_columns().size() - 1);
      if(renderer == NULL)
        std::cerr << "Why don't I have a renderer when I just added one?" << std::endl;
      else
        {
          Gtk::CellRendererText *renderer_text = dynamic_cast<Gtk::CellRendererText *>(renderer);
          if(renderer_text == NULL)
            std::cerr << "Why don't I have a text renderer when I just added one?" << std::endl;
          else
            renderer_text->property_ellipsize() = Pango::ELLIPSIZE_END;
        }
    }

    tree->set_search_column(cols.Name);

    // TODO: There should be a way to do this in Glade maybe.
    tree->get_selection()->set_mode(Gtk::SELECTION_MULTIPLE);
  }

  void EntityView::on_cache_closed()
  {
    // TODO: throw away all the model rows here?
  }

  void EntityView::on_cache_reloaded()
  {
    if(apt_cache_file != NULL)
      (*apt_cache_file)->package_states_changed.connect(sigc::mem_fun(*this, &EntityView::refresh_view));
    // TODO: we should rebuild the display, but we can't do that
    // without more information about what we were displaying.  Maybe
    // we should just rely on the tab to trigger a rebuild.
  }

  void EntityView::setup_column_properties(Gtk::TreeViewColumn *treeview_column,
                                             int size)
  {
    Gtk::CellRenderer* treeview_cellrenderer = treeview_column->get_first_cell_renderer();
    treeview_column->add_attribute(treeview_cellrenderer->property_cell_background_set(), cols.BgSet);
    treeview_column->add_attribute(treeview_cellrenderer->property_cell_background(), cols.BgColor);
    treeview_column->set_sizing(Gtk::TREE_VIEW_COLUMN_FIXED);
    treeview_column->set_fixed_width(size);
    treeview_column->set_resizable(true);
    treeview_column->set_reorderable(true);
  }

  template <class ColumnType>
  int EntityView::append_column(const Glib::ustring &title,
                                  Gtk::TreeViewColumn *&treeview_column,
                                  Gtk::TreeModelColumn<ColumnType> &model_column,
                                  int size)
  {
    treeview_column = manage(new Gtk::TreeViewColumn(title, model_column));
    setup_column_properties(treeview_column, size);
    treeview_column->set_sort_column(model_column);
    return tree->append_column(*treeview_column);
  }

  int EntityView::append_markup_column(const Glib::ustring &title,
				       Gtk::TreeViewColumn *&treeview_column,
				       Gtk::TreeModelColumn<Glib::ustring> &model_column,
				       int size)
  {
    Gtk::CellRendererText *renderer = manage(new Gtk::CellRendererText);
    treeview_column = manage(new Gtk::TreeViewColumn(title, *renderer));
    treeview_column->add_attribute(renderer->property_markup(),
                                   model_column);
    // TODO: this will work for now, but ideally we would have a
    // second, un-marked-up column that we use to sort.
    treeview_column->set_sort_column(model_column);
    setup_column_properties(treeview_column, size);
    return tree->append_column(*treeview_column);
  }

  void EntityView::refresh_view(const std::set<pkgCache::PkgIterator> *changed_packages)
  {
    for(std::set<pkgCache::PkgIterator>::iterator pkg = changed_packages->begin(); pkg != changed_packages->end(); pkg++)
      {
        std::pair<std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator>::iterator,
        std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator>::iterator> reverse_range =
	  get_reverse_store()->equal_range(*pkg);

        for (std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator>::iterator reverse_iter =
          reverse_range.first;
        reverse_iter != reverse_range.second; reverse_iter++)
          {
            Gtk::TreeModel::iterator iter = reverse_iter->second;
            Gtk::TreeModel::Row row = *iter;
	    cwidget::util::ref_ptr<Entity> ent = row[get_columns()->EntObject];
	    ent->fill_row(get_columns(), row);
          }
      }
  }

  void EntityView::apply_action_to_selected(PackagesAction action)
  {
    Glib::RefPtr<Gtk::TreeModel> model = get_model();
    Glib::RefPtr<Gtk::TreeView::Selection> refSelection = get_treeview()->get_selection();
    if(refSelection)
    {
      Gtk::TreeSelection::ListHandle_Path path_list = refSelection->get_selected_rows();
      std::list<Gtk::TreeModel::iterator> iter_list;
      for (Gtk::TreeSelection::ListHandle_Path::iterator path = path_list.begin();
        path != path_list.end(); path++)
      {
        iter_list.push_back(model->get_iter(*path));
      }
      aptitudeDepCache::action_group group(*apt_cache_file, NULL);
      while (!iter_list.empty())
        {
          Gtk::TreeModel::iterator iter = iter_list.front();
	  cwidget::util::ref_ptr<Entity> ent = (*iter)[cols.EntObject];
          ent->dispatch_action(action);

          iter_list.pop_front();
        }
    }
  }

  EntityView::EntityView(Glib::RefPtr<Gnome::Glade::Xml> refGlade,
			 Glib::ustring gladename)
  {
    init(refGlade, gladename);
  }

  namespace
  {
    void add_menu_item(Gtk::Menu *menu,
                       Glib::ustring label,
                       Gtk::StockID icon,
                       sigc::slot0<void> callback,
                       bool sensitive = true)
    {
      Gtk::Image *image = manage(new Gtk::Image(icon, Gtk::ICON_SIZE_MENU));
      Gtk::MenuItem *item = manage(new Gtk::ImageMenuItem(*image, label));
      menu->append(*item);

      if (sensitive)
        item->signal_activate().connect(callback);
      else
        item->set_sensitive(false);

      item->show_all();
    }

    // for convenience
    void add_menu_item(Gtk::Menu *menu,
                       Glib::ustring label,
                       Gtk::StockID icon)
    {
      add_menu_item(menu, label, icon, sigc::slot0<void>(), false);
    }
  }

  Gtk::Menu *
  EntityView::get_menu(const std::set<PackagesAction> &actions,
		       const sigc::slot1<void, PackagesAction> &callback) const
  {
    Gtk::Menu *rval(manage(new Gtk::Menu));

    if(actions.find(Upgrade) != actions.end())
      {
	if(actions.find(Install) != actions.end())
	  add_menu_item(rval, "Install/Upgrade", Gtk::Stock::ADD,
			sigc::bind(callback, Install));
	else
	  add_menu_item(rval, "Upgrade", Gtk::Stock::GO_UP,
			sigc::bind(callback, Install));
      }
    else if(actions.find(Downgrade) != actions.end())
      add_menu_item(rval, "Downgrade", Gtk::Stock::GO_DOWN,
                    sigc::bind(callback, Install));
    else if(actions.find(Install) != actions.end())
      add_menu_item(rval, "Install", Gtk::Stock::ADD,
                    sigc::bind(callback, Install));
    else
      add_menu_item(rval, "Install/Upgrade", Gtk::Stock::ADD); // Insensitive

    add_menu_item(rval, "Remove", Gtk::Stock::REMOVE,
                  sigc::bind(callback, Remove),
                  actions.find(Remove) != actions.end());

    add_menu_item(rval, "Purge", Gtk::Stock::CLEAR,
                  sigc::bind(callback, Purge),
                  actions.find(Purge) != actions.end());

    add_menu_item(rval, "Keep", Gtk::Stock::MEDIA_REWIND,
                  sigc::bind(callback, Keep),
                  actions.find(Keep) != actions.end());

    add_menu_item(rval, "Hold", Gtk::Stock::MEDIA_PAUSE,
                  sigc::bind(callback, Hold),
                  actions.find(Hold) != actions.end());

    return rval;
  }

  void EntityView::context_menu_handler(GdkEventButton * event)
  {
    Glib::RefPtr<Gtk::TreeModel> model = get_model();
    Glib::RefPtr<Gtk::TreeView::Selection> selected = tree->get_selection();
    if(selected)
      {
        std::set<PackagesAction> actions;

        Gtk::TreeSelection::ListHandle_Path selected_rows = selected->get_selected_rows();
        for (Gtk::TreeSelection::ListHandle_Path::iterator path = selected_rows.begin();
             path != selected_rows.end(); ++path)
          {
            Gtk::TreeModel::iterator iter = model->get_iter(*path);
	    cwidget::util::ref_ptr<Entity> ent = (*iter)[cols.EntObject];
            ent->add_actions(actions);
          }

        if(!actions.empty())
          {
            get_menu(actions, sigc::mem_fun(this, &EntityView::apply_action_to_selected))
              ->popup(event->button, event->time);
          }
      }
  }

  EntityView::~EntityView()
  {
  }

  void EntityView::row_activated_handler(const Gtk::TreeModel::Path & path, Gtk::TreeViewColumn* column)
  {
      Gtk::TreeModel::iterator iter = get_model()->get_iter(path);
      cwidget::util::ref_ptr<Entity> ent = (*iter)[cols.EntObject];
      ent->activated(path, column, this);
  }

  namespace
  {
    bool add_entity_to_revstore(const Gtk::TreeModel::iterator &iter,
				const Glib::RefPtr<Gtk::TreeModel> &model,
				const EntityColumns *columns,
				std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator> *revstore)
    {
      std::set<pkgCache::PkgIterator> packages;

      const Gtk::TreeModel::Row &row = *iter;

      cwidget::util::ref_ptr<Entity> ent = row[columns->EntObject];
      ent->add_packages(packages);

      for(std::set<pkgCache::PkgIterator>::const_iterator it = packages.begin();
	  it != packages.end(); ++it)
	revstore->insert(std::pair<pkgCache::PkgIterator, Gtk::TreeModel::iterator>(*it, iter));

      return false;
    }
  }

  void EntityView::set_model(const Glib::RefPtr<Gtk::TreeModel> &model)
  {
    revstore.clear();
    model->foreach_iter(sigc::bind(sigc::ptr_fun(add_entity_to_revstore),
				   model,
				   get_columns(),
				   get_reverse_store()));
    get_treeview()->set_model(model);
  }
}
