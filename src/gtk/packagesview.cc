// packagesview.cc
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

#include "packagesview.h"
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

namespace gui
{
  undo_group * undo;

  string current_state_string(pkgCache::PkgIterator pkg, pkgCache::VerIterator ver)
  {
    if(!ver.end() && ver != pkg.CurrentVer())
      return "p";

    switch(pkg->CurrentState)
      {
      case pkgCache::State::NotInstalled:
        return "p";
      case pkgCache::State::UnPacked:
        return "u";
      case pkgCache::State::HalfConfigured:
        return "C";
      case pkgCache::State::HalfInstalled:
        return "H";
      case pkgCache::State::ConfigFiles:
        return "c";
  #ifdef APT_HAS_TRIGGERS
      case pkgCache::State::TriggersAwaited:
        return "W";
      case pkgCache::State::TriggersPending:
        return "T";
  #endif
      case pkgCache::State::Installed:
        return "i";
      default:
        return "E";
      }
  }

  string selected_state_string(pkgCache::PkgIterator pkg, pkgCache::VerIterator ver)
  {
    aptitudeDepCache::StateCache &state=(*apt_cache_file)[pkg];
    aptitudeDepCache::aptitude_state &estate=(*apt_cache_file)->get_ext_state(pkg);
    pkgCache::VerIterator candver=state.CandidateVerIter(*apt_cache_file);

    string selected_state = string();
    if (state.Status != 2
        && (*apt_cache_file)->get_ext_state(pkg).selection_state
            == pkgCache::State::Hold && !state.InstBroken())
      selected_state += "h";
    if (state.Upgradable() && !pkg.CurrentVer().end() && !candver.end()
        && candver.VerStr() == estate.forbidver)
      selected_state += "F";
    if (state.Delete())
      selected_state += ((state.iFlags & pkgDepCache::Purge) ? "p" : "d");
    if (state.InstBroken())
      selected_state += "B";
    if (state.NewInstall())
      selected_state += "i";
    if (state.iFlags & pkgDepCache::ReInstall)
      selected_state += "r";
    if (state.Upgrade())
      selected_state += "u";
    return selected_state;
  }

  PackagesMarker::PackagesMarker(PackagesView * view)
  {
    this->view = view;
  }

  void PackagesMarker::dispatch(pkgCache::PkgIterator pkg, pkgCache::VerIterator ver, PackagesAction action)
  {
    if (!ver.end())
    {
      switch(action)
      {
      case Install:
        std::cout << "selected for install : " << pkg.Name() << " (" << ver.VerStr() << ") , status from " << selected_state_string(pkg, pkg.VersionList());
        (*apt_cache_file)->set_candidate_version(ver, undo);
        (*apt_cache_file)->mark_install(pkg, true, false, undo);
        std::cout << " to " << selected_state_string(pkg, ver) << std::endl;
        break;
      case Remove:
        std::cout << "selected for remove : " << pkg.Name() << " (" << ver.VerStr() << ") , status from " << selected_state_string(pkg, pkg.VersionList());
        (*apt_cache_file)->mark_delete(pkg, false, false, undo);
        std::cout << " to " << selected_state_string(pkg, ver) << std::endl;
        break;
      case Purge:
        std::cout << "selected for purge : " << pkg.Name() << " (" << ver.VerStr() << ") , status from " << selected_state_string(pkg, pkg.VersionList());
        (*apt_cache_file)->mark_delete(pkg, true, false, undo);
        std::cout << " to " << selected_state_string(pkg, ver) << std::endl;
        break;
      case Keep:
        std::cout << "selected for keep : " << pkg.Name() << " (" << ver.VerStr() << ") , status from " << selected_state_string(pkg, pkg.VersionList());
        (*apt_cache_file)->mark_keep(pkg, false, false, undo);
        std::cout << " to " << selected_state_string(pkg, ver) << std::endl;
        break;
      case Hold:
        std::cout << "selected for hold : " << pkg.Name() << " (" << ver.VerStr() << ") , status from " << selected_state_string(pkg, pkg.VersionList());
        (*apt_cache_file)->mark_delete(pkg, false, true, undo);
        std::cout << " to " << selected_state_string(pkg, ver) << std::endl;
        break;
      default:
        break;
      }
    }
  }

  void PackagesMarker::callback(const Gtk::TreeModel::iterator& iter, PackagesAction action)
  {
    pkgCache::PkgIterator pkg = (*iter)[view->get_packages_columns()->PkgIterator];
    pkgCache::VerIterator ver = (*iter)[view->get_packages_columns()->VerIterator];
    dispatch(pkg, ver, action);
  }

  // TODO: This should maybe rather take a general functor than going through an exhaustive enum
  void PackagesMarker::select(PackagesAction action)
  {
    Glib::RefPtr<Gtk::TreeView::Selection> refSelection = view->get_treeview()->get_selection();
    if(refSelection)
    {
      Gtk::TreeSelection::ListHandle_Path path_list = refSelection->get_selected_rows();
      std::list<Gtk::TreeModel::iterator> iter_list;
      for (Gtk::TreeSelection::ListHandle_Path::iterator path = path_list.begin();
        path != path_list.end(); path++)
      {
        iter_list.push_back(view->get_packages_store()->get_iter(*path));
      }
      std::set<pkgCache::PkgIterator> changed_packages;
      {
        aptitudeDepCache::action_group group(*apt_cache_file, NULL, &changed_packages);
        while (!iter_list.empty())
        {
          callback(iter_list.front(), action);
          iter_list.pop_front();
        }
      }
      signal_on_changed_packages(changed_packages);
    }
  }

  PackagesContextMenu::PackagesContextMenu(PackagesView * view)
  {
    PackagesMarker * marker = view->get_marker();
    Glib::RefPtr<Gnome::Glade::Xml> refGlade = Gnome::Glade::Xml::create(glade_main_file, "main_packages_context");
    refGlade->get_widget("main_packages_context", pMenu);
    refGlade->get_widget("main_packages_context_install", pMenuInstall);
    pMenuInstall->signal_activate().connect(sigc::bind(sigc::mem_fun(*marker, &PackagesMarker::select), Install));
    refGlade->get_widget("main_packages_context_remove", pMenuRemove);
    pMenuRemove->signal_activate().connect(sigc::bind(sigc::mem_fun(*marker, &PackagesMarker::select), Remove));
    refGlade->get_widget("main_packages_context_purge", pMenuPurge);
    pMenuPurge->signal_activate().connect(sigc::bind(sigc::mem_fun(*marker, &PackagesMarker::select), Purge));
    refGlade->get_widget("main_packages_context_keep", pMenuKeep);
    pMenuKeep->signal_activate().connect(sigc::bind(sigc::mem_fun(*marker, &PackagesMarker::select), Keep));
    refGlade->get_widget("main_packages_context_hold", pMenuHold);
    pMenuHold->signal_activate().connect(sigc::bind(sigc::mem_fun(*marker, &PackagesMarker::select), Hold));
  }

  PackagesColumns::PackagesColumns()
  {
    add(PkgIterator);
    add(VerIterator);
    add(CurrentStatus);
    add(SelectedStatus);
    add(Name);
    add(Section);
    add(Version);
  }

  PackagesTreeView::PackagesTreeView(BaseObjectType* cobject, const Glib::RefPtr<Gnome::Glade::Xml>& refGlade) : Gtk::TreeView(cobject)
  {
    ;;
  }

  bool PackagesTreeView::on_button_press_event(GdkEventButton* event)
  {
    bool return_value = true;

    if ((event->type == GDK_BUTTON_PRESS) && (event->button == 3))
    {
      //Base class not called because we don't want to deselect...
      //TODO: This has the side effect that the select+context_menu action
      //      with one right-click won't work, which should.
      //      We need information about the selection.
      //context->get_menu()->popup(event->button, event->time);
      signal_context_menu(event);
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

  PackagesTreeModelGenerator::~PackagesTreeModelGenerator()
  {
  }


  Glib::RefPtr<Gtk::TreeModel>
  PackagesView::build_store(const GeneratorK & generatorK,
                            PackagesColumns *packages_columns,
                            std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator> * reverse_packages_store,
                            Glib::ustring limit)
  {
    std::auto_ptr<PackagesTreeModelGenerator>
      generator(generatorK(packages_columns));

    guiOpProgress * p = gen_progress_bar();

    int num=0;
    int total=(*apt_cache_file)->Head().PackageCount;
    bool limited = false;
    aptitude::matching::pkg_matcher * limiter = NULL;
    if (limit != "")
    {
      limiter = aptitude::matching::parse_pattern(limit);
      limited = (limiter != NULL);
    }

    for(pkgCache::PkgIterator pkg=(*apt_cache_file)->PkgBegin(); !pkg.end(); pkg++)
      {
        p->OverallProgress(num, total, 1, _("Building view"));

        ++num;

        // Filter useless packages up-front.
        if(pkg.VersionList().end() && pkg.ProvidesList().end())
          continue;
        if (!limited || aptitude::matching::apply_matcher(limiter, pkg, *apt_cache_file, *apt_package_records))
        {
          pkgCache::VerIterator ver;
          if(!pkg.CurrentVer().end())
          {
            ver = pkg.CurrentVer();
            generator->add(pkg, ver, reverse_packages_store);
          }
          else
          {
            ver=(*apt_cache_file)[pkg].CandidateVerIter(*apt_cache_file);
            if (!ver.end())
              generator->add(pkg, ver, reverse_packages_store);
          }
        }
      }

    p->OverallProgress(total, total, 1,  _("Finalizing view"));

    Glib::Thread * sort_thread = Glib::Thread::create(sigc::mem_fun(*generator, &PackagesTreeModelGenerator::finish), true);
    while(!generator->finished)
    {
      pMainWindow->get_progress_bar()->pulse();
      gtk_update();
      Glib::usleep(100000);
    }
    sort_thread->join();

    //generator->finish();

    delete p;

    return generator->get_model();
  }

  template <class ColumnType>
  int PackagesView::append_column(Glib::ustring title,
      Gtk::TreeViewColumn * treeview_column,
      Gtk::TreeModelColumn<ColumnType>& model_column,
      int size)
  {
    treeview_column = new Gtk::TreeViewColumn(title, model_column);
    treeview_column->set_sizing(Gtk::TREE_VIEW_COLUMN_FIXED);
    treeview_column->set_fixed_width(size);
    treeview_column->set_resizable(true);
    treeview_column->set_reorderable(true);
    treeview_column->set_sort_column(model_column);
    return treeview->append_column(*treeview_column);
  }

  PackagesView::PackagesView(const GeneratorK &_generatorK,
                             Glib::RefPtr<Gnome::Glade::Xml> refGlade,
                             Glib::ustring limit)
  {
    refGlade->get_widget_derived("main_packages_treeview", treeview);

    generatorK = _generatorK;

    packages_columns = new PackagesColumns();
    marker = new PackagesMarker(this);
    context = new PackagesContextMenu(this);

    treeview->signal_context_menu.connect(sigc::mem_fun(*this, &PackagesView::context_menu_handler));
    treeview->signal_selection.connect(sigc::bind(sigc::mem_fun(*marker, &PackagesMarker::select), Description));
    treeview->signal_row_activated().connect(sigc::mem_fun(*this, &PackagesView::row_activated_package_handler));
    signal_on_changed_packages.connect(sigc::mem_fun(*this, &PackagesView::refresh_packages_view));

    append_column(Glib::ustring(_("C")), CurrentStatus, packages_columns->CurrentStatus, 32);
    append_column(Glib::ustring(_("S")), SelectedStatus, packages_columns->SelectedStatus, 32);
    append_column(Glib::ustring(_("Name")), Name, packages_columns->Name, 200);
    append_column(Glib::ustring(_("Section")), Section, packages_columns->Section, 80);
    append_column(Glib::ustring(_("Version")), Version, packages_columns->Version, 80);

    treeview->set_search_column(packages_columns->Name);

    reverse_packages_store = new std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator>;
    packages_store = build_store(generatorK,
                                 packages_columns,
                                 reverse_packages_store,
                                 limit);

    treeview->set_model(packages_store);

    // TODO: There should be a way to do this in Glade maybe.
    treeview->get_selection()->set_mode(Gtk::SELECTION_MULTIPLE);
  }

  PackagesView::~PackagesView()
  {
  }

  void PackagesView::context_menu_handler(GdkEventButton * event)
  {
    context->get_menu()->popup(event->button, event->time);
  }

  void PackagesView::row_activated_package_handler(const Gtk::TreeModel::Path & path, Gtk::TreeViewColumn* column)
  {
      Gtk::TreeModel::iterator iter = packages_store->get_iter(path);
      pkgCache::PkgIterator pkg = (*iter)[packages_columns->PkgIterator];
      pkgCache::VerIterator ver = (*iter)[packages_columns->VerIterator];
      if (pkg && ver)
      {
        InfoTab * infotab = tab_add<InfoTab>(_("Info:"));
        infotab->disp_package(pkg, ver);
      }
  }

  void PackagesView::relimit_packages_view(Glib::ustring limit)
  {
    reverse_packages_store = new std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator>;
    packages_store = build_store(generatorK, packages_columns, reverse_packages_store, limit);
    treeview->set_model(packages_store);
  }

  void PackagesView::refresh_packages_view(std::set<pkgCache::PkgIterator> changed_packages)
  {
    guiOpProgress * p = gen_progress_bar();
    int num=0;
    int total=changed_packages.size();

    for(std::set<pkgCache::PkgIterator>::iterator pkg = changed_packages.begin(); pkg != changed_packages.end(); pkg++)
      {
        p->OverallProgress(num, total, 1, _("Building view"));

        ++num;

        std::pair<std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator>::iterator,
        std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator>::iterator> reverse_range =
                  reverse_packages_store->equal_range(*pkg);

        for (std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator>::iterator reverse_iter =
          reverse_range.first;
        reverse_iter != reverse_range.second; reverse_iter++)
          {
            Gtk::TreeModel::iterator iter = reverse_iter->second;
            Gtk::TreeModel::Row row = *iter;
            pkgCache::PkgIterator pkg = row[packages_columns->PkgIterator];
            pkgCache::VerIterator ver = row[packages_columns->VerIterator];

            row[packages_columns->CurrentStatus] = current_state_string(pkg, ver);
            row[packages_columns->SelectedStatus] = selected_state_string(pkg, ver);
            row[packages_columns->Name] = pkg.Name()?pkg.Name():"";
            row[packages_columns->Section] = pkg.Section()?pkg.Section():"";
            row[packages_columns->Version] = ver.VerStr();

            if (want_to_quit)
              return;
          }
      }
    gtk_update();
    p->OverallProgress(total, total, 1,  _("Building view"));
    delete p;
  }

}
