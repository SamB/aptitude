#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "gui.h"

#include <vector>

#include "aptitude.h"

#undef OK
#include <gtkmm.h>
#include <libglademm/xml.h>

#include <apt-pkg/error.h>

#include <../generic/apt/apt.h>
#include <../generic/apt/apt_undo_group.h>
#include <../generic/apt/matchers.h>
#include <../generic/apt/download_install_manager.h>
#include <../generic/apt/download_update_manager.h>
#include <../generic/apt/aptitude_resolver_universe.h>
#include <../generic/apt/resolver_manager.h>
#include <../generic/problemresolver/exceptions.h>
#include <../generic/problemresolver/solution.h>
//#include <../main.h>
#include <../progress.h>
#include <../generic/util/util.h>

#include <sigc++/signal.h>

#include <cwidget/generic/util/transcode.h>

typedef generic_solution<aptitude_universe> aptitude_solution;

namespace gui
{
  //This is a list of global and unique base widgets and other related stuff
  Glib::RefPtr<Gnome::Glade::Xml> refXml;
  AptitudeWindow * pMainWindow;
  std::string glade_main_file;
  undo_group * undo;

  // True if a download or package-list update is proceeding.  This hopefully will
  // avoid the nasty possibility of collisions between them.
  // FIXME: uses implicit locking -- if multithreading happens, should use a mutex
  //       instead.
  static bool active_download;
  static bool want_to_quit = false;

  void gtk_update()
  {
    while (Gtk::Main::events_pending())
      Gtk::Main::iteration();
  }

  class guiOpProgress : public OpProgress
  { // must derive to read protected member..
    private:
      float sanitizePercentFraction(float percent)
      {
        float rval = percent / 100;
        if (percent < 0)
          rval = 0;
        if (percent > 1)
          rval = 1;
        return rval;
      }
    public:
      ~guiOpProgress()
      {
        pMainWindow->get_progress_bar()->set_text("");
        pMainWindow->get_progress_bar()->set_fraction(0);
      }
      void Update()
      {
        if (CheckChange(0.25))
        {
          pMainWindow->get_progress_bar()->set_text(Op);
          pMainWindow->get_progress_bar()->set_fraction(sanitizePercentFraction(Percent));
          gtk_update();
        }
      }
  };

  guiOpProgress * gen_progress_bar()
  {
    return new guiOpProgress;
  }

  Tab::Tab(TabType _type, const Glib::ustring &_label,
	   const Glib::RefPtr<Gnome::Glade::Xml> &_xml, const std::string &widgetName)
    : type(_type), label(_label),
      xml(_xml), widget(NULL)
  {
    xml->get_widget(widgetName, widget);
  }

  class DashboardTab : public Tab
  {
    public:
      DashboardTab(Glib::ustring label)
	: Tab(Dashboard, label,
	      Gnome::Glade::Xml::create(glade_main_file, "label1"),
	      "label1")
      {
        get_widget()->show();
      }
  };

  class DownloadColumns : public Gtk::TreeModel::ColumnRecord
  {
    public:
      Gtk::TreeModelColumn<Glib::ustring> URI;
      Gtk::TreeModelColumn<Glib::ustring> Description;
      Gtk::TreeModelColumn<Glib::ustring> ShortDesc;

      DownloadColumns()
      {
        add(URI);
        add(ShortDesc);
        add(Description);
      }
  };

  class DownloadTab : public Tab
  {
    public:
      Glib::RefPtr<Gtk::ListStore> download_store;
      DownloadColumns download_columns;
      Gtk::TreeView * pDownloadTreeView;

      DownloadTab(const Glib::ustring &label)
        : Tab(Download, label,
              Gnome::Glade::Xml::create(glade_main_file, "main_download_scrolledwindow"),
              "main_download_scrolledwindow")
      {
        get_xml()->get_widget("main_download_treeview", pDownloadTreeView);
        get_widget()->show();
        createstore();
        pDownloadTreeView->append_column(_("URI"), download_columns.URI);
        pDownloadTreeView->get_column(0)->set_sort_column(download_columns.URI);
        pDownloadTreeView->append_column(_("Description"), download_columns.Description);
        pDownloadTreeView->get_column(1)->set_sort_column(download_columns.Description);
        pDownloadTreeView->append_column(_("Short Description"), download_columns.ShortDesc);
        pDownloadTreeView->get_column(2)->set_sort_column(download_columns.ShortDesc);
      }
      void createstore()
      {
        download_store = Gtk::ListStore::create(download_columns);
        pDownloadTreeView->set_model(download_store);
      }
  };

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

  // FIXME: Shouldn't this take a tab and a package rather than duplicating code to read the selection?
  //        Maybe this should be called by PackagesMarker::select.
  void display_desc(pkgCache::PkgIterator pkg, pkgCache::VerIterator ver, PackagesTab * tab)
  {
    if (ver)
    {
      pkgRecords::Parser &rec=apt_package_records->Lookup(ver.FileList());
      string misc = ssprintf("%s%s\n"
          "%s%s\n"
          "%s%s\n"
          "%s%s\n"
          "%s%s\n"
          "%s%s\n"
          "%s%s\n",
          _("Name: "), pkg.Name(),
          _("Priority: "),pkgCache::VerIterator(ver).PriorityType()?pkgCache::VerIterator(ver).PriorityType():_("Unknown"),
              _("Section: "),pkg.Section()?pkg.Section():_("Unknown"),
                  _("Maintainer: "),rec.Maintainer().c_str(),
                  _("Compressed size: "), SizeToStr(ver->Size).c_str(),
                  _("Uncompressed size: "), SizeToStr(ver->InstalledSize).c_str(),
                  _("Source Package: "),
                  rec.SourcePkg().empty()?pkg.Name():rec.SourcePkg().c_str());
      string desc = cwidget::util::transcode(get_long_description(ver, apt_package_records), "UTF-8");
      tab->pPackagesTextView->get_buffer()->set_text(misc + _("Description: ") + desc);
    }
    else
    {
      tab->pPackagesTextView->get_buffer()->set_text(ssprintf("%s%s\n", _("Name: "), pkg.Name()));
    }
  }

  PackagesMarker::PackagesMarker(PackagesTab * tab)
  {
    this->tab = tab;
  }

  void PackagesMarker::dispatch(pkgCache::PkgIterator pkg, pkgCache::VerIterator ver, PackagesAction action)
  {
    if (!ver.end())
    {
      switch(action)
      {
      case Install:
        std::cout << "selected for install : " << pkg.Name() << " (" << ver.VerStr() << ") , status from "
            << selected_state_string(pkg, pkg.VersionList());
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
      case Description:
        display_desc(pkg, ver, tab);
        break;
      }
    }
  }

  void PackagesMarker::callback(const Gtk::TreeModel::iterator& iter, PackagesAction action)
  {
    pkgCache::PkgIterator pkg = (*iter)[tab->packages_columns.PkgIterator];
    pkgCache::VerIterator ver = (*iter)[tab->packages_columns.VerIterator];
    dispatch(pkg, ver, action);
  }

  // TODO: This should maybe rather take a general functor than going through an exhaustive enum
  void PackagesMarker::select(PackagesAction action)
  {
    Glib::RefPtr<Gtk::TreeView::Selection> refSelection = tab->pPackagesTreeView->get_selection();
    if(refSelection)
    {
      refSelection->selected_foreach_iter(sigc::bind(sigc::mem_fun(*this, &PackagesMarker::callback), action));
    }
  }

  PackagesContextMenu::PackagesContextMenu(PackagesTab * tab, PackagesMarker * marker)
  {
    this->tab = tab;
    this->marker = marker;
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

  PackagesView::PackagesView(BaseObjectType* cobject, const Glib::RefPtr<Gnome::Glade::Xml>& refGlade) : Gtk::TreeView(cobject)
  {
    in_PackagesTab = true;
  }

  // FIXME: Hack...
  void PackagesView::link_to_context_menu(PackagesContextMenu * context_menu)
  {
    pPackagesContextMenu = context_menu;
  }

  // FIXME: Hack...
  void PackagesView::link_to_marker(PackagesMarker * marker)
  {
    this->marker = marker;
    in_PackagesTab = true;
  }

  bool PackagesView::on_button_press_event(GdkEventButton* event)
  {
    bool return_value = true;

    if ((event->type == GDK_BUTTON_PRESS) && (event->button == 3))
    {
      //Base class not called because we don't want to deselect...
      //TODO: This has the side effect that the select+context_menu action
      //      with one right-click won't work, which should.
      //      We need information about the selection.
      pPackagesContextMenu->get_menu()->popup(event->button, event->time);
    }
    else if ((event->type == GDK_BUTTON_PRESS) && (event->button == 1))
    {
      //Call base class, to allow normal handling,
      //such as allowing the row to be selected by the right-click:
      return_value = Gtk::TreeView::on_button_press_event(event);
      // TODO: The general behavior of the description display isn't right.
      //       We should display the LAST selected package in case of multiple selection.
      if (in_PackagesTab)
      {
        marker->select(Description);
      }
    }
    return return_value;
  }

  PackagesTab::PackagesTab(const Glib::ustring &label) :
    Tab(Packages, label, Gnome::Glade::Xml::create(glade_main_file, "main_packages_vpaned"), "main_packages_vpaned")
  {
    get_xml()->get_widget("main_packages_textview", pPackagesTextView);
    get_xml()->get_widget_derived("main_packages_treeview", pPackagesTreeView);
    get_widget()->show();

    createstore();

    pPackagesMarker = new PackagesMarker(this);
    // FIXME: Hack...
    pPackagesTreeView->link_to_marker(pPackagesMarker);
    pPackagesContextMenu = new PackagesContextMenu(this, pPackagesMarker);
    // FIXME: Hack...
    pPackagesTreeView->link_to_context_menu(pPackagesContextMenu);

    pPackagesTreeView->append_column(_("Current Status"), packages_columns.CurrentStatus);
    pPackagesTreeView->get_column(0)->set_sort_column(packages_columns.CurrentStatus);
    pPackagesTreeView->append_column(_("Selected Status"), packages_columns.SelectedStatus);
    pPackagesTreeView->get_column(1)->set_sort_column(packages_columns.SelectedStatus);
    pPackagesTreeView->append_column(_("Name"), packages_columns.Name);
    pPackagesTreeView->get_column(2)->set_sort_column(packages_columns.Name);
    pPackagesTreeView->append_column(_("Section"), packages_columns.Section);
    pPackagesTreeView->get_column(3)->set_sort_column(packages_columns.Section);
    pPackagesTreeView->append_column(_("Version"), packages_columns.Version);

    //pPackagesTreeView->signal_button_press_event().connect_notify(sigc::mem_fun(*this, &PackagesTab::on_button_press_event));

    // TODO: There should be a way to do this in Glade maybe.
    pPackagesTreeView->get_selection()->set_mode(Gtk::SELECTION_MULTIPLE);
  }

  void PackagesTab::createstore()
  {
    packages_store = Gtk::ListStore::create(packages_columns);
    pPackagesTreeView->set_model(packages_store);
    pPackagesTreeView->set_search_column(packages_columns.Name);
  }

  // TODO: Shouldn't we populate a ListStore/TreeModel rather then a PackageTab?
  //       or would that be too low-level?
  void populate_packages_tab(guiOpProgress &progress, PackagesTab * tab, bool limited)
  {
    int num=0;
    int total=(*apt_cache_file)->Head().PackageCount;
    tab->createstore();

    //aptitude::matching::pkg_matcher *limit = aptitude::matching::parse_pattern(pPackageSearchLimit->get_text());

    for(pkgCache::PkgIterator pkg=(*apt_cache_file)->PkgBegin(); !pkg.end(); pkg++)
      {
        progress.OverallProgress(num, total, 1, _("Building view"));

        ++num;
        if (num%5000 == 0)
        {
          pMainWindow->get_progress_bar()->pulse();
          gtk_update();
        }

        // Filter useless packages up-front.
        if(pkg.VersionList().end() && pkg.ProvidesList().end())
          continue;
        // TODO: put back the limiting
        if (/*!limited || aptitude::matching::apply_matcher(limit, pkg, *apt_cache_file, *apt_packages_records)*/true)
          {
            for (pkgCache::VerIterator ver = pkg.VersionList(); ver.end() == false; ver++)
              {
                Gtk::TreeModel::iterator iter = tab->packages_store->append();
                Gtk::TreeModel::Row row = *iter;

                row[tab->packages_columns.PkgIterator] = pkg;
                row[tab->packages_columns.VerIterator] = ver;
                row[tab->packages_columns.CurrentStatus] = current_state_string(pkg, ver);
                row[tab->packages_columns.SelectedStatus] = selected_state_string(pkg, ver);
                row[tab->packages_columns.Name] = pkg.Name()?pkg.Name():"";
                row[tab->packages_columns.Section] = pkg.Section()?pkg.Section():"";
                row[tab->packages_columns.Version] = ver.VerStr();

                if (want_to_quit)
                  return;
              }
          }
      }
    gtk_update();
    tab->packages_store->set_sort_column(tab->packages_columns.Name, Gtk::SORT_ASCENDING);
    gtk_update();
    progress.OverallProgress(total, total, 1,  _("Building view"));
  }

  int TabsManager::next_position(TabType type)
  {
    // TODO: implement something more elaborate and workflow-wise intuitive
    return get_n_pages();
  }

  int TabsManager::number_of(TabType type)
  {
    return 0;
  }

  TabsManager::TabsManager(BaseObjectType* cobject, const Glib::RefPtr<Gnome::Glade::Xml>& refGlade) :
    Gtk::Notebook(cobject)
  {
    ;;
  }

  int TabsManager::append_page(Tab& tab)
  {
    int rval;
    // TODO: Should do something about this. Create a dedicated toplevel for these widgets.
    Glib::RefPtr<Gnome::Glade::Xml> refGlade = Gnome::Glade::Xml::create(glade_main_file, "main_notebook_download_label_hbox");
    Gtk::HBox * label_widget;
    refGlade->get_widget("main_notebook_download_label_hbox", label_widget);
    Gtk::Label * label_label;
    refGlade->get_widget("main_notebook_download_label", label_label);
    Gtk::Button * label_button;
    refGlade->get_widget("main_notebook_download_close", label_button);
    // Maybe we should create a close() method on the Tab so it can clean itself up or make a destructor.
    label_button->signal_pressed().connect(sigc::bind(sigc::mem_fun(this, &TabsManager::remove_page), tab));
    switch (tab.get_type())
      {
    case Dashboard:
      // No more than one Dashboard at once
      if (number_of(Dashboard) == 0)
      {
        label_label->set_text(_("Dashboard"));
        rval = insert_page(*(tab.get_widget()), *(label_widget), 0);
      }
      break;
      // TODO: handle other kinds of tabs
    default:
      label_label->set_text("generic tab: " + tab.get_label());
      rval = insert_page(*(tab.get_widget()), *(label_widget), next_position(tab.get_type()));
      }
    return rval;
  }

  void TabsManager::remove_page(Tab& tab)
  {
    Gtk::Notebook::remove_page(*(tab.get_widget()));
  }

  /**
   * Adds a dashboard tab to the interface.
   * TODO: Get this one out of here!
   */
  DashboardTab * tab_add_dashboard()
  {
    DashboardTab * tab = new DashboardTab("truc dashboard");
    int new_page_idx = pMainWindow->get_notebook()->append_page(*tab);
    pMainWindow->get_notebook()->set_current_page(new_page_idx);
    return tab;
  }

  /**
   * Adds a download tab to the interface.
   * TODO: Get this one out of here!
   */
  DownloadTab * tab_add_download()
  {
    DownloadTab * tab = new DownloadTab("truc download");
    int new_page_idx = pMainWindow->get_notebook()->append_page(*tab);
    pMainWindow->get_notebook()->set_current_page(new_page_idx);
    return tab;
  }

  /**
   * Adds a packages tab to the interface.
   * TODO: Get this one out of here!
   */
  PackagesTab * tab_add_packages()
  {
    PackagesTab * tab = new PackagesTab("truc packages");
    int new_page_idx = pMainWindow->get_notebook()->append_page(*tab);
    pMainWindow->get_notebook()->set_current_page(new_page_idx);
    return tab;
  }

  void check_apt_errors()
  {
    string currerr, tag;
    while (!_error->empty())
    {
      bool iserr = _error->PopMessage(currerr);
      if (iserr)
        tag = "E:";
      else
        tag = "W:";

      Gtk::MessageDialog dialog(*pMainWindow, "There's a problem with apt...", false, Gtk::MESSAGE_INFO, Gtk::BUTTONS_OK,
          true);
      dialog.set_secondary_text(tag + currerr);
      dialog.run();
    }
  }

  class guiPkgAcquireStatus : public pkgAcquireStatus
  { // must also derive to read protected members..
    private:
      DownloadTab * tab;
    public:
      guiPkgAcquireStatus(DownloadTab * tab)
      {
        this->tab = tab;
      }
      bool Pulse(pkgAcquire *Owner)
      {
        pkgAcquireStatus::Pulse(Owner);
        if (TotalItems != 0)
          pMainWindow->get_progress_bar()->set_fraction(((float)CurrentItems)/((float)TotalItems));
        pMainWindow->get_progress_bar()->set_text(ssprintf("%lu of %lu done", CurrentItems, TotalItems));
        gtk_update();
        return !want_to_quit;
      }
      bool MediaChange(std::string, std::string)
      {
        return false;
      }
      void Fetch(pkgAcquire::ItemDesc &Itm)
      {
        std::cout << Itm.Description << std::endl;

        pMainWindow->get_status_bar()->pop(0);
        pMainWindow->get_status_bar()->push(Itm.Description, 0);

        Gtk::TreeModel::iterator iter = tab->download_store->append();
        Gtk::TreeModel::Row row = *iter;
        row[tab->download_columns.URI] = Itm.URI;
        row[tab->download_columns.ShortDesc] = Itm.ShortDesc;
        row[tab->download_columns.Description] = Itm.Description;
        gtk_update();
      }
  };

  void really_do_update_lists(DownloadTab * tab)
  {
    download_update_manager *m = new download_update_manager;

    // downloading now I suppose ?
    guiOpProgress progress;
    guiPkgAcquireStatus acqlog(tab);
    acqlog.Update = true;
    acqlog.MorePulses = true;
    if (m->prepare(progress, acqlog, NULL))
      {
        std::cout << "m->prepare succeeded" << std::endl;
      }
    else
      {
        std::cout << "m->prepare failed" << std::endl;
        return;
      }
    acqlog.Update = true;
    acqlog.MorePulses = true;
    m->do_download(100);
    m->finish(pkgAcquire::Continue, progress);
    guiOpProgress * p = gen_progress_bar();
    apt_load_cache(p, true, NULL);
    delete p;
  }

  void do_update_lists(DownloadTab * tab)
  {
    if (!active_download)
      {
        if (getuid()==0)
          {
            pMainWindow->get_progress_bar()->set_text("Updating..");
            pMainWindow->get_progress_bar()->set_fraction(0);
            tab->download_store->clear();
            really_do_update_lists(tab);
            pMainWindow->get_progress_bar()->set_fraction(0);
            pMainWindow->get_status_bar()->pop(0);
          }
        else
          {
            Gtk::MessageDialog dialog(*pMainWindow,
                "There's a problem with you not being root...", false,
                Gtk::MESSAGE_INFO, Gtk::BUTTONS_OK, true);
            dialog.set_secondary_text("You're supposed to be a super-user to be allowed to break stuff you know ?");

            dialog.run();
          }
      }
    else
      std::cout << "A package-list update or install run is already taking place."
          << std::endl;
  }

  void do_dashboard()
  {
    /*DashboardTab * tab = */tab_add_dashboard();
  }

  void do_update()
  {
    DownloadTab * tab = tab_add_download();
    do_update_lists(tab);
  }

  void do_packages()
  {
    PackagesTab * tab = tab_add_packages();
    guiOpProgress * p = gen_progress_bar();
    populate_packages_tab(*p, tab, false);
    delete p;
  }

  AptitudeWindow::AptitudeWindow(BaseObjectType* cobject, const Glib::RefPtr<Gnome::Glade::Xml>& refGlade) : Gtk::Window(cobject)
  {
    refGlade->get_widget_derived("main_notebook", pNotebook);

    refGlade->get_widget("main_toolbutton_dashboard", pToolButtonDashboard);
    pToolButtonDashboard->signal_clicked().connect(&do_dashboard);

    refGlade->get_widget("main_toolbutton_packages", pToolButtonPackages);
    pToolButtonPackages->signal_clicked().connect(&do_packages);

    refGlade->get_widget("main_toolbutton_update", pToolButtonUpdate);
    pToolButtonUpdate->signal_clicked().connect(&do_update);

    refGlade->get_widget("main_progressbar", pProgressBar);
    refGlade->get_widget("main_statusbar", pStatusBar);
    pStatusBar->push("Aptitude-gtk v2", 0);
  }

  void main(int argc, char *argv[])
  {
    Gtk::Main kit(argc, argv);
    // Use the basename of argv0 to find the Glade file.
    // TODO: note that the .glade file will ultimately
    //       go to /usr/share/aptitude/glade or something,
    //       so a more general solution will be needed.
    std::string argv0(argv[0]);
    std::string argv0_path;
    std::string::size_type last_slash = argv0.rfind('/');
    if(last_slash != std::string::npos)
      {
        while(last_slash > 0 && argv0[last_slash - 1] == '/')
          --last_slash;
        argv0_path = std::string(argv0, 0, last_slash);
      }
    else
      argv0_path = '.';

    glade_main_file = argv0_path + "/gtk/ui-main.glade";

    //Loading the .glade file and widgets
    refXml = Gnome::Glade::Xml::create(glade_main_file);

    refXml->get_widget_derived("main_window", pMainWindow);

    // TODO: this is unnecessary if consume_errors is connected for the GUI.
    check_apt_errors();

    guiOpProgress * p=gui::gen_progress_bar();
    char *status_fname=NULL;
    apt_init(p, true, status_fname);
    if(status_fname)
      free(status_fname);
    check_apt_errors();
    delete p;

    //This is the loop
    Gtk::Main::run(*pMainWindow);
  }
}
