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

#include <apt-pkg/error.h>

#include <generic/apt/apt.h>
#include <generic/apt/apt_undo_group.h>
#include <generic/apt/matchers.h>

#include <gtk/gui.h>
#include <gtk/info.h>
#include <gtk/progress.h>

#include <cwidget/generic/util/ssprintf.h>
#include <cwidget/generic/util/transcode.h>

namespace gui
{
  template <class EntIter>
  EntityColumns<EntIter>::EntityColumns()
  {
    add(EntIterator);
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


  template <class EntIter>
  void EntityView<EntIter>::init(const sigc::slot1<EntityTreeModelGenerator<EntIter> *, EntityColumns<EntIter> *> &_generatorK,
                          Glib::RefPtr<Gnome::Glade::Xml> refGlade,
                          Glib::ustring gladename)
  {
    refGlade->get_widget_derived(gladename, tree);

    generatorK = _generatorK;

    cols = new EntityColumns<EntIter>();

    tree->signal_context_menu.connect(sigc::mem_fun(*this, &EntityView<EntIter>::context_menu_handler));
    tree->signal_row_activated().connect(sigc::mem_fun(*this, &EntityView<EntIter>::row_activated_handler));
    if(apt_cache_file != NULL)
      (*apt_cache_file)->package_states_changed.connect(sigc::mem_fun(*this, &EntityView<EntIter>::refresh_view));
    cache_closed.connect(sigc::mem_fun(*this, &EntityView<EntIter>::on_cache_closed));
    cache_reloaded.connect(sigc::mem_fun(*this, &EntityView<EntIter>::on_cache_reloaded));

    append_column(Glib::ustring(_("Status")), Status, cols->Status, 32);
    append_markup_column(Glib::ustring(_("Name")), Name, cols->Name, 350);
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
    append_markup_column(Glib::ustring(_("Version")), Version, cols->Version, 80);
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


    tree->set_search_column(cols->Name);

    // TODO: There should be a way to do this in Glade maybe.
    tree->get_selection()->set_mode(Gtk::SELECTION_MULTIPLE);
  }

  template <class EntIter>
  void EntityView<EntIter>::on_cache_closed()
  {
    // TODO: throw away all the model rows here?
  }

  template <class EntIter>
  void EntityView<EntIter>::on_cache_reloaded()
  {
    if(apt_cache_file != NULL)
      (*apt_cache_file)->package_states_changed.connect(sigc::mem_fun(*this, &EntityView<EntIter>::refresh_view));
    // TODO: we should rebuild the display, but we can't do that
    // without more information about what we were displaying.  Maybe
    // we should just rely on the tab to trigger a rebuild.
  }

  template <class EntIter> std::pair<Glib::RefPtr<Gtk::TreeModel>, std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator> *>
  EntityView<EntIter>::build_store(const sigc::slot1<EntityTreeModelGenerator<EntIter> *, EntityColumns<EntIter> *> & generatorK,
      EntityColumns<EntIter> *cols, Glib::ustring limit)
  {
    std::auto_ptr<EntityTreeModelGenerator<EntIter> >
      generator(generatorK(cols));

    guiOpProgress * p = new guiOpProgress;

    int num = 0;
    int total = generator->count();
    bool limited = false;
    aptitude::matching::pkg_matcher * limiter = NULL;
    if (limit != "")
    {
      limiter = aptitude::matching::parse_pattern(limit);
      limited = (limiter != NULL);
    }

    for(EntIter ent=generator->iterator(); !ent.end(); ent++)
      {
        p->OverallProgress(num, total, 1, _("Building view"));

        ++num;

        if (generator->prefilter(ent)
            || !limited
            || aptitude::matching::apply_matcher(limiter, get_packages(ent), *apt_cache_file, *apt_package_records))
        {
          generator->add(ent);
        }
      }

    p->OverallProgress(total, total, 1,  _("Prefinalizing view"));

    Glib::Thread * work_thread;

    work_thread = Glib::Thread::create(sigc::mem_fun(*generator, &EntityTreeModelGenerator<EntIter>::prefinish), true);
    while(!generator->finished)
    {
      pMainWindow->get_progress_bar()->pulse();
      gtk_update();
      Glib::usleep(100000);
    }
    work_thread->join();

    generator->finished = false;

    p->OverallProgress(total, total, 1,  _("Finalizing view"));

    work_thread = Glib::Thread::create(sigc::mem_fun(*generator, &EntityTreeModelGenerator<EntIter>::finish), true);
    while(!generator->finished)
    {
      pMainWindow->get_progress_bar()->pulse();
      gtk_update();
      Glib::usleep(100000);
    }
    work_thread->join();

    delete p;

    return std::make_pair(generator->get_model(), generator->get_reverse_model());
  }

  template <class EntIter>
  void EntityView<EntIter>::setup_column_properties(Gtk::TreeViewColumn *treeview_column,
                                             int size)
  {
    Gtk::CellRenderer* treeview_cellrenderer = treeview_column->get_first_cell_renderer();
    treeview_column->add_attribute(treeview_cellrenderer->property_cell_background_set(), cols->BgSet);
    treeview_column->add_attribute(treeview_cellrenderer->property_cell_background(), cols->BgColor);
    treeview_column->set_sizing(Gtk::TREE_VIEW_COLUMN_FIXED);
    treeview_column->set_fixed_width(size);
    treeview_column->set_resizable(true);
    treeview_column->set_reorderable(true);
  }

  template <class EntIter>
  template <class ColumnType>
  int EntityView<EntIter>::append_column(const Glib::ustring &title,
                                  Gtk::TreeViewColumn *treeview_column,
                                  Gtk::TreeModelColumn<ColumnType> &model_column,
                                  int size)
  {
    treeview_column = manage(new Gtk::TreeViewColumn(title, model_column));
    setup_column_properties(treeview_column, size);
    treeview_column->set_sort_column(model_column);
    return tree->append_column(*treeview_column);
  }

  template <class EntIter>
  int EntityView<EntIter>::append_markup_column(const Glib::ustring &title,
                                         Gtk::TreeViewColumn *treeview_column,
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

  template <class EntIter>
  void EntityView<EntIter>::fill_header(Gtk::TreeModel::Row &row,
                                    Glib::ustring text)
  {
    row[cols->EntIter] = EntIter();

    row[cols->BgColor] = "light yellow"; // Say we want blue header..
    row[cols->BgSet] = true; // We do want to put color in there, yes.

    row[cols->Status] = ""; // dummy

    // This is the content of the header
    // TODO: Maybe we should delegate the markup to the caller
    row[cols->Name] = "<span size=\"large\">" + Glib::Markup::escape_text(text) + "</span>";

    row[cols->Version] = ""; // dummy
  }

  template <class EntIter>
  EntityView<EntIter>::EntityView(const sigc::slot1<EntityTreeModelGenerator<EntIter> *, EntityColumns<EntIter> *> &_generatorK,
                             Glib::RefPtr<Gnome::Glade::Xml> refGlade,
                             Glib::ustring gladename,
                             Glib::ustring limit)
  {
    init(_generatorK, refGlade, gladename);
    revstore = new std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator>;
    if(!limit.empty())
      {
      std::pair<Glib::RefPtr<Gtk::TreeModel>, std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator> *> stores =
        build_store(generatorK, cols, limit);
      store = stores.first;
      revstore = stores.second;
      }
  }

  template <class EntIter>
  EntityView<EntIter>::~EntityView()
  {
    // TODO: Auto-generated destructor stub
  }

  template <class EntIter>
  void EntityView<EntIter>::context_menu_handler(GdkEventButton * event)
  {
    Glib::RefPtr<Gtk::TreeView::Selection> selected = tree->get_selection();
    if(selected)
      {
        std::set<PackagesAction> actions;

        Gtk::TreeSelection::ListHandle_Path selected_rows = selected->get_selected_rows();
        for (Gtk::TreeSelection::ListHandle_Path::iterator path = selected_rows.begin();
             path != selected_rows.end(); ++path)
          {
            Gtk::TreeModel::iterator iter = store->get_iter(*path);
            add_actions((*iter)[cols->EntIterator], actions);
          }

        if(!actions.empty())
          {
            get_menu(actions, sigc::mem_fun(this,
                                            &EntityView<EntIter>::apply_action_to_selected))->popup(event->button, event->time);
          }
      }
  }

  template <class EntIter>
  void EntityView<EntIter>::row_activated_handler(const Gtk::TreeModel::Path & path, Gtk::TreeViewColumn* column)
  {
      Gtk::TreeModel::iterator iter = store->get_iter(path);
      pkgCache::VerIterator ver = get_version((*iter)[cols->EntIterator]);
      pkgCache::PkgIterator pkg = get_package((*iter)[cols->EntIterator]);

      if (!pkg.end() && !ver.end())
        InfoTab::show_tab(pkg, ver);
  }

  template <class EntIter>
  void EntityView<EntIter>::relimit_packages_view(Glib::ustring limit)
  {
    std::pair<Glib::RefPtr<Gtk::TreeModel>, std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator> *> stores =
      build_store(generatorK, cols, limit);
    store = stores.first;
    revstore = stores.second;
    tree->set_model(store);
  }

}
