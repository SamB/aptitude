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

#include <cwidget/generic/util/ssprintf.h>
#include <cwidget/generic/util/transcode.h>

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

  string selected_package_state_string(pkgCache::PkgIterator pkg)
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

  // TODO: This maybe should be merged with the previous function as an overload
  string selected_version_state_string(pkgCache::PkgIterator pkg, pkgCache::VerIterator ver)
  {
    aptitudeDepCache::StateCache &state=(*apt_cache_file)[pkg];
    aptitudeDepCache::aptitude_state &estate=(*apt_cache_file)->get_ext_state(pkg);
    pkgCache::VerIterator candver=state.CandidateVerIter(*apt_cache_file);

    string selected_state = string();

    if(ver.end())
      return selected_state;
    // Rest of the show :

    if(state.Status!=2 && estate.selection_state==pkgCache::State::Hold && !state.NowBroken())
      selected_state += "h";
    else if(ver.VerStr() == estate.forbidver)
      selected_state += "F";
    else if(state.Delete())
      selected_state += (state.iFlags&pkgDepCache::Purge)?"p":"d";
    else if(state.InstBroken() && state.InstVerIter(*apt_cache_file)==ver)
      selected_state += "B";
    else if(state.NewInstall())
    {
      if(candver==ver)
        selected_state += "i";
    }
    else if(state.iFlags&pkgDepCache::ReInstall)
    {
      if(ver.ParentPkg().CurrentVer()==ver)
        selected_state += "i";
    }
    else if(state.Upgrade())
    {
      if(ver.ParentPkg().CurrentVer()==ver)
        selected_state += "d";
      else if(candver==ver)
        selected_state += "i";
      selected_state += "u";
    }
    return selected_state;
  }

  string selected_package_state_color(pkgCache::PkgIterator pkg)
  {
    aptitudeDepCache::StateCache &state=(*apt_cache_file)[pkg];
    aptitudeDepCache::aptitude_state &estate=(*apt_cache_file)->get_ext_state(pkg);
    pkgCache::VerIterator candver=state.CandidateVerIter(*apt_cache_file);

    if (state.Status != 2
        && (*apt_cache_file)->get_ext_state(pkg).selection_state
            == pkgCache::State::Hold && !state.InstBroken())
      return "#FFCCCC";
    if (state.Upgradable() && !pkg.CurrentVer().end() && !candver.end()
        && candver.VerStr() == estate.forbidver)
      // FIXME: does this really deserve its own color?
      return "dark red";
    if (state.Delete())
      return ((state.iFlags & pkgDepCache::Purge) ? "#FFBBFF" : "#FFEEFF");
    if (state.InstBroken())
      return "#FFCCCC";
    if (state.NewInstall())
      return "#DDFFDD";
    if (state.Install() && (state.iFlags & pkgDepCache::ReInstall))
      return "#BBFFBB";
    if (state.Upgrade())
      return "#DDDDFF";
    return "white";
  }

  // TODO: This maybe should be merged with the previous function as an overload
  string selected_version_state_color(pkgCache::PkgIterator pkg, pkgCache::VerIterator ver)
  {
    aptitudeDepCache::StateCache &state=(*apt_cache_file)[pkg];
    aptitudeDepCache::aptitude_state &estate=(*apt_cache_file)->get_ext_state(pkg);
    pkgCache::VerIterator candver=state.CandidateVerIter(*apt_cache_file);

    if(ver.end())
      return "white";

    if(state.Status!=2 && estate.selection_state==pkgCache::State::Hold && !state.NowBroken())
      return "#FFCCCC"; // hold
    else if(ver.VerStr() == estate.forbidver)
      // FIXME: does this really deserve its own color?
      return "dark red"; // forbid
    else if(state.Delete())
      return ((state.iFlags&pkgDepCache::Purge) ? "#FFBBFF" /* purge */ : "#FFEEFF" /* remove */);
    else if(state.InstBroken() && state.InstVerIter(*apt_cache_file)==ver)
      return "#FFCCCC"; // broken
    else if(state.NewInstall())
    {
      if(candver==ver)
        return "#DDFFDD"; // install
    }
    else if(state.iFlags&pkgDepCache::ReInstall)
    {
      if(ver.ParentPkg().CurrentVer()==ver)
        return "#DDFFDD"; // install
    }
    else if(state.Upgrade())
    {
      if(ver.ParentPkg().CurrentVer()==ver)
        return "#FFEEFF"; // remove
      else if(candver==ver)
        return "#DDFFDD"; // install
    }

    // Make sure we return something
    return "white";
  }

  void dispatch_action(pkgCache::PkgIterator pkg, pkgCache::VerIterator ver, PackagesAction action)
  {
    if (!ver.end())
    {
      switch(action)
      {
      case Install:
      case Upgrade:
      case Downgrade:
        (*apt_cache_file)->mark_install(pkg, true, false, undo);
        break;
      case Remove:
        (*apt_cache_file)->mark_delete(pkg, false, false, undo);
        break;
      case Purge:
        (*apt_cache_file)->mark_delete(pkg, true, false, undo);
        break;
      case Keep:
        (*apt_cache_file)->mark_keep(pkg, false, false, undo);
        break;
      case Hold:
        (*apt_cache_file)->mark_delete(pkg, false, true, undo);
        break;
      default:
        break;
      }
    }
  }

  // TODO: This should maybe rather take a general functor than going through an exhaustive enum
  void PackagesView::apply_action_to_selected(PackagesAction action) const
  {
    Glib::RefPtr<Gtk::TreeView::Selection> refSelection = get_treeview()->get_selection();
    if(refSelection)
    {
      Gtk::TreeSelection::ListHandle_Path path_list = refSelection->get_selected_rows();
      std::list<Gtk::TreeModel::iterator> iter_list;
      for (Gtk::TreeSelection::ListHandle_Path::iterator path = path_list.begin();
        path != path_list.end(); path++)
      {
        iter_list.push_back(get_packages_store()->get_iter(*path));
      }
      aptitudeDepCache::action_group group(*apt_cache_file, NULL);
      while (!iter_list.empty())
        {
	  Gtk::TreeModel::iterator iter = iter_list.front();
	  const pkgCache::PkgIterator pkg = (*iter)[get_packages_columns()->PkgIterator];
	  const pkgCache::VerIterator ver = (*iter)[get_packages_columns()->VerIterator];
	  dispatch_action(pkg, ver, action);

          iter_list.pop_front();
        }
    }
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
  PackagesView::get_menu(const std::set<PackagesAction> &actions,
			 sigc::slot1<void, PackagesAction> callback) const
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

  void add_actions(const pkgCache::PkgIterator &pkg,
		   std::set<PackagesAction> &actions)
  {
    // Defensiveness.
    if(pkg.end())
      return;

    pkgDepCache::StateCache state = (*apt_cache_file)[pkg];

    if(state.Status == 2 && !state.Install())
      actions.insert(Install);

    if(state.Status == 1 && !state.Install())
      actions.insert(Upgrade);

    if(state.Status == -1 && !state.Install())
      actions.insert(Downgrade);

    if(state.Status != 2 && !(state.Delete() &&
			      ((state.iFlags & pkgDepCache::Purge) == 0)))
      actions.insert(Remove);

    if((state.Status != 2 ||
	(state.Status == 2 && pkg->CurrentState == pkgCache::State::ConfigFiles)) &&
       !(state.Delete() &&
	 ((state.iFlags & pkgDepCache::Purge) != 0)))
      actions.insert(Purge);

    if(!state.Keep())
      actions.insert(Keep);

    if((*apt_cache_file)->get_ext_state(pkg).selection_state == pkgCache::State::Hold)
      actions.insert(Keep);
    else
      actions.insert(Hold);
  }

  PackagesColumns::PackagesColumns()
  {
    add(PkgIterator);
    add(VerIterator);
    add(BgSet);
    add(BgColor);
    add(Status);
    add(Name);
    add(Section);
    add(Version);
  }

  void PackagesColumns::fill_row(Gtk::TreeModel::Row &row,
                                 const pkgCache::PkgIterator &pkg,
                                 const pkgCache::VerIterator &ver,
                                 bool version_specific) const
  {
    using cwidget::util::ssprintf;
    using cwidget::util::transcode;

    row[PkgIterator] = pkg;
    row[VerIterator] = ver;

    if (!pkg.end() && !ver.end())
      {
      if (version_specific)
        row[BgColor] = selected_version_state_color(pkg, ver);
      else
        row[BgColor] = selected_package_state_color(pkg);
      }
    else
      row[BgColor] = "white";
    row[BgSet] = (row[BgColor] != "white");

      if (!pkg.end() && !ver.end())
        {
        if (version_specific)
          row[Status] = current_state_string(pkg, ver) + selected_version_state_string(pkg, ver);
        else
          row[Status] = current_state_string(pkg, ver) + selected_package_state_string(pkg);
        }
      else
        row[Status] = "";

    if(pkg.end())
      row[Name] = "";
    else
      {
        Glib::ustring safe_name = Glib::Markup::escape_text(pkg.Name());
        if(ver.end())
          row[Name] = ssprintf("<b>%s</b>", safe_name.c_str());
        else
          {
            Glib::ustring safe_description =
              Glib::Markup::escape_text(transcode(get_short_description(ver,
                                                                        apt_package_records),
                                                  "UTF-8"));
            row[Name] =
              ssprintf("<b>%s</b>\n<span size=\"smaller\">%s</span>",
                       safe_name.c_str(), safe_description.c_str());
          }
      }
    row[Section] = (!pkg.end() && pkg.Section()) ? pkg.Section() : "";

    if (!ver.end())
    {
      row[Version] = Glib::Markup::escape_text(ver.VerStr());
      if (!version_specific)
      {
        aptitudeDepCache::StateCache &state=(*apt_cache_file)[pkg];
        pkgCache::VerIterator candver=state.CandidateVerIter(*apt_cache_file);
        if (state.Upgrade() || state.Downgrade())
        {
          row[Version] = row[Version] + "\n<i>" + Glib::Markup::escape_text(candver.VerStr()) + "</i>";
        }
      }
    }
    else
      row[Version] = "";
  }

  void PackagesColumns::fill_header(Gtk::TreeModel::Row &row,
                                    Glib::ustring text) const
  {
    row[PkgIterator] = pkgCache::PkgIterator();
    row[VerIterator] = pkgCache::VerIterator();

    row[BgColor] = "light yellow"; // Say we want blue header..
    row[BgSet] = true; // We do want to put color in there, yes.

    row[Status] = ""; // dummy

    // This is the content of the header
    row[Name] = "<span size=\"large\">" + Glib::Markup::escape_text(text) + "</span>";

    row[Section] = ""; // dummy
    row[Version] = ""; // dummy
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

  Glib::RefPtr<Gtk::TreeModel>
  PackagesView::build_store_single(const GeneratorK & generatorK,
                                   PackagesColumns *packages_columns,
                                   std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator> * reverse_packages_store,
                                   pkgCache::PkgIterator pkg, pkgCache::VerIterator ver)
  {
    std::auto_ptr<PackagesTreeModelGenerator>
      generator(generatorK(packages_columns));

    generator->add(pkg, ver, reverse_packages_store);

    generator->finish();

    return generator->get_model();
  }

  void PackagesView::setup_column_properties(Gtk::TreeViewColumn *treeview_column,
					     int size)
  {
    Gtk::CellRenderer* treeview_cellrenderer = treeview_column->get_first_cell_renderer();
    treeview_column->add_attribute(treeview_cellrenderer->property_cell_background_set(), packages_columns->BgSet);
    treeview_column->add_attribute(treeview_cellrenderer->property_cell_background(), packages_columns->BgColor);
    treeview_column->set_sizing(Gtk::TREE_VIEW_COLUMN_FIXED);
    treeview_column->set_fixed_width(size);
    treeview_column->set_resizable(true);
    treeview_column->set_reorderable(true);
  }

  template <class ColumnType>
  int PackagesView::append_column(const Glib::ustring &title,
				  Gtk::TreeViewColumn *treeview_column,
				  Gtk::TreeModelColumn<ColumnType> &model_column,
				  int size)
  {
    treeview_column = manage(new Gtk::TreeViewColumn(title, model_column));
    setup_column_properties(treeview_column, size);
    treeview_column->set_sort_column(model_column);
    return treeview->append_column(*treeview_column);
  }

  int PackagesView::append_markup_column(const Glib::ustring &title,
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
    return treeview->append_column(*treeview_column);
  }

  void PackagesView::on_cache_closed()
  {
    // TODO: throw away all the model rows here?
  }

  void PackagesView::on_cache_reloaded()
  {
    if(apt_cache_file != NULL)
      (*apt_cache_file)->package_states_changed.connect(sigc::mem_fun(*this, &PackagesView::refresh_packages_view));
    // TODO: we should rebuild the display, but we can't do that
    // without more information about what we were displaying.  Maybe
    // we should just rely on the tab to trigger a rebuild.
  }

  void PackagesView::init(const GeneratorK &_generatorK,
                          Glib::RefPtr<Gnome::Glade::Xml> refGlade,
                          Glib::ustring gladename)
  {
    refGlade->get_widget_derived(gladename, treeview);

    generatorK = _generatorK;

    packages_columns = new PackagesColumns();

    treeview->signal_context_menu.connect(sigc::mem_fun(*this, &PackagesView::context_menu_handler));
    treeview->signal_row_activated().connect(sigc::mem_fun(*this, &PackagesView::row_activated_package_handler));
    if(apt_cache_file != NULL)
      (*apt_cache_file)->package_states_changed.connect(sigc::mem_fun(*this, &PackagesView::refresh_packages_view));
    cache_closed.connect(sigc::mem_fun(*this, &PackagesView::on_cache_closed));
    cache_reloaded.connect(sigc::mem_fun(*this, &PackagesView::on_cache_reloaded));

    append_column(Glib::ustring(_("Status")), Status, packages_columns->Status, 32);
    append_markup_column(Glib::ustring(_("Name")), Name, packages_columns->Name, 350);
    {
      Gtk::CellRenderer *renderer = treeview->get_column_cell_renderer(treeview->get_columns().size() - 1);
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
    append_markup_column(Glib::ustring(_("Version")), Version, packages_columns->Version, 80);
    {
      Gtk::CellRenderer *renderer = treeview->get_column_cell_renderer(treeview->get_columns().size() - 1);
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


    treeview->set_search_column(packages_columns->Name);

    // TODO: There should be a way to do this in Glade maybe.
    treeview->get_selection()->set_mode(Gtk::SELECTION_MULTIPLE);
  }

  PackagesView::PackagesView(const GeneratorK &_generatorK,
                             Glib::RefPtr<Gnome::Glade::Xml> refGlade,
                             Glib::ustring gladename,
                             Glib::ustring limit)
  {
    init(_generatorK, refGlade, gladename);
    reverse_packages_store = new std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator>;
    if(!limit.empty())
      {
	Glib::RefPtr<Gtk::TreeModel> packages_store =
	  build_store(generatorK,
		      packages_columns,
		      reverse_packages_store,
		      limit);
	treeview->set_model(packages_store);
      }
  }

  PackagesView::PackagesView(const GeneratorK &_generatorK,
                             Glib::RefPtr<Gnome::Glade::Xml> refGlade,
                             Glib::ustring gladename,
                             pkgCache::PkgIterator pkg, pkgCache::VerIterator ver)
  {
    init(_generatorK, refGlade, gladename);
    reverse_packages_store = new std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator>;
    Glib::RefPtr<Gtk::TreeModel> packages_store =
      build_store_single(generatorK,
			 packages_columns,
			 reverse_packages_store,
			 pkg, ver);
    treeview->set_model(packages_store);
  }

  PackagesView::~PackagesView()
  {
  }

  void PackagesView::context_menu_handler(GdkEventButton * event)
  {
    Glib::RefPtr<Gtk::TreeView::Selection> selected = get_treeview()->get_selection();
    if(selected)
      {
	std::set<PackagesAction> actions;

	Gtk::TreeSelection::ListHandle_Path selected_rows = selected->get_selected_rows();
	for (Gtk::TreeSelection::ListHandle_Path::iterator path = selected_rows.begin();
	     path != selected_rows.end(); ++path)
	  {
	    Gtk::TreeModel::iterator iter = get_packages_store()->get_iter(*path);
	    add_actions((*iter)[get_packages_columns()->PkgIterator],
			actions);
	  }

	if(!actions.empty())
	  {
	    get_menu(actions, sigc::mem_fun(this,
					    &PackagesView::apply_action_to_selected))->popup(event->button, event->time);
	  }
      }
  }

  void PackagesView::row_activated_package_handler(const Gtk::TreeModel::Path & path, Gtk::TreeViewColumn* column)
  {
      Gtk::TreeModel::iterator iter = get_packages_store()->get_iter(path);
      pkgCache::PkgIterator pkg = (*iter)[packages_columns->PkgIterator];
      pkgCache::VerIterator ver = (*iter)[packages_columns->VerIterator];
      if (!pkg.end() && !ver.end())
	InfoTab::show_tab(pkg, ver);
  }

  void PackagesView::relimit_packages_view(Glib::ustring limit)
  {
    reverse_packages_store = new std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator>;
    Glib::RefPtr<Gtk::TreeModel> packages_store =
      build_store(generatorK, packages_columns, reverse_packages_store, limit);
    treeview->set_model(packages_store);
  }

  void PackagesView::refresh_packages_view(const std::set<pkgCache::PkgIterator> *changed_packages)
  {
    guiOpProgress * p = gen_progress_bar();
    int num=0;
    int total = changed_packages->size();

    for(std::set<pkgCache::PkgIterator>::iterator pkg = changed_packages->begin(); pkg != changed_packages->end(); pkg++)
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

	    packages_columns->fill_row(row, pkg, ver);

            if (want_to_quit)
              return;
          }
      }
    gtk_update();
    p->OverallProgress(total, total, 1,  _("Building view"));
    delete p;
  }

}
