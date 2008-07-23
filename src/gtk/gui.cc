#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "gui.h"

#include <vector>
#include <map>

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
  Gtk::Main * pKit;
  Glib::RefPtr<Gnome::Glade::Xml> refXml;
  AptitudeWindow * pMainWindow;
  std::string glade_main_file;
  undo_group * undo;

  sigc::signal<void, std::set<pkgCache::PkgIterator> > signal_on_changed_packages;

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

  float guiOpProgress::sanitizePercentFraction(float percent)
  {
    float rval = percent / 100;
    if (percent < 0)
      rval = 0;
    if (percent > 1)
      rval = 1;
    return rval;
  }

  guiOpProgress::~guiOpProgress()
  {
    pMainWindow->get_progress_bar()->set_text("");
    pMainWindow->get_progress_bar()->set_fraction(0);
  }

  void guiOpProgress::Update()
  {
    if (CheckChange(0.25))
    {
      pMainWindow->get_progress_bar()->set_text(Op);
      pMainWindow->get_progress_bar()->set_fraction(sanitizePercentFraction(Percent));
      gtk_update();
    }
  }

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

    // TODO: Should do something about this. Create a dedicated toplevel for these widgets.
    Glib::RefPtr<Gnome::Glade::Xml> refGlade = Gnome::Glade::Xml::create(glade_main_file, "main_notebook_download_label_hbox");
    //Gtk::HBox * label_widget;
    refGlade->get_widget("main_notebook_download_label_hbox", label_widget);
    //Gtk::Label * label_label;
    refGlade->get_widget("main_notebook_download_label", label_label);
    Gtk::Button * label_button;
    refGlade->get_widget("main_notebook_download_close", label_button);
    // Maybe we should create a close() method on the Tab so it can clean itself up or make a destructor.
    label_button->signal_clicked().connect(sigc::bind(sigc::mem_fun(*(pMainWindow->get_notebook()), &TabsManager::remove_page), *this));
    if (_label != "")
    {
      label_label->set_text(_label);
    }
    else
    {
      label_label->set_text("generic tab: " + label);
    }
  }

  void Tab::set_label(Glib::ustring label)
  {
    this->label_label->set_text(label);
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
      {
        std::set<pkgCache::PkgIterator> changed_packages;
        {
          aptitudeDepCache::action_group group(*apt_cache_file, NULL, &changed_packages);
          std::cout << "selected for install : " << pkg.Name() << " (" << ver.VerStr() << ") , status from "
          << selected_state_string(pkg, pkg.VersionList());
          (*apt_cache_file)->set_candidate_version(ver, undo);
          (*apt_cache_file)->mark_install(pkg, true, false, undo);
          std::cout << " to " << selected_state_string(pkg, ver) << std::endl;
        }
        signal_on_changed_packages(changed_packages);
      }
      break;
      case Remove:
      {
        std::set<pkgCache::PkgIterator> changed_packages;
        {
          aptitudeDepCache::action_group group(*apt_cache_file, NULL, &changed_packages);
          std::cout << "selected for remove : " << pkg.Name() << " (" << ver.VerStr() << ") , status from " << selected_state_string(pkg, pkg.VersionList());
          (*apt_cache_file)->mark_delete(pkg, false, false, undo);
          std::cout << " to " << selected_state_string(pkg, ver) << std::endl;
        }
        signal_on_changed_packages(changed_packages);
      }
      break;
      case Purge:
      {
        std::set<pkgCache::PkgIterator> changed_packages;
        {
          aptitudeDepCache::action_group group(*apt_cache_file, NULL, &changed_packages);
          std::cout << "selected for purge : " << pkg.Name() << " (" << ver.VerStr() << ") , status from " << selected_state_string(pkg, pkg.VersionList());
          (*apt_cache_file)->mark_delete(pkg, true, false, undo);
          std::cout << " to " << selected_state_string(pkg, ver) << std::endl;
        }
        signal_on_changed_packages(changed_packages);
      }
      break;
      case Keep:
      {
        std::set<pkgCache::PkgIterator> changed_packages;
        {
          aptitudeDepCache::action_group group(*apt_cache_file, NULL, &changed_packages);
          std::cout << "selected for keep : " << pkg.Name() << " (" << ver.VerStr() << ") , status from " << selected_state_string(pkg, pkg.VersionList());
          (*apt_cache_file)->mark_keep(pkg, false, false, undo);
          std::cout << " to " << selected_state_string(pkg, ver) << std::endl;
        }
        signal_on_changed_packages(changed_packages);
      }
      break;
      case Hold:
      {
        std::set<pkgCache::PkgIterator> changed_packages;
        {
          aptitudeDepCache::action_group group(*apt_cache_file, NULL, &changed_packages);
          std::cout << "selected for hold : " << pkg.Name() << " (" << ver.VerStr() << ") , status from " << selected_state_string(pkg, pkg.VersionList());
          (*apt_cache_file)->mark_delete(pkg, false, true, undo);
          std::cout << " to " << selected_state_string(pkg, ver) << std::endl;
        }
        signal_on_changed_packages(changed_packages);
      }
      break;
      case Description:
        view->signal_on_package_selection(pkg, ver);
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
      refSelection->selected_foreach_iter(sigc::bind(sigc::mem_fun(*this, &PackagesMarker::callback), action));
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
      // TODO: The general behavior of the description display isn't right.
      //       We should display the LAST selected package in case of multiple selection.
      signal_selection();
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
        if (num % 1000 == 0)
        {
          gtk_update();
          pMainWindow->get_progress_bar()->pulse();
        }

        // Filter useless packages up-front.
        if(pkg.VersionList().end() && pkg.ProvidesList().end())
          continue;
        // TODO: put back the limiting
        if (!limited || aptitude::matching::apply_matcher(limiter, pkg, *apt_cache_file, *apt_package_records))
          {
            for (pkgCache::VerIterator ver = pkg.VersionList(); ver.end() == false; ver++)
              {
		generator->add(pkg, ver, reverse_packages_store);
              }
          }
      }
    gtk_update();
    generator->finish();
    gtk_update();

    p->OverallProgress(total, total, 1,  _("Building view"));
    delete p;

    return generator->get_model();
  }

  PackagesView::PackagesView(const GeneratorK &_generatorK,
			     Glib::RefPtr<Gnome::Glade::Xml> refGlade)
  {
    refGlade->get_widget_derived("main_packages_treeview", treeview);

    generatorK = _generatorK;

    packages_columns = new PackagesColumns();
    marker = new PackagesMarker(this);
    context = new PackagesContextMenu(this);

    treeview->signal_context_menu.connect(sigc::mem_fun(*this, &PackagesView::context_menu_handler));
    treeview->signal_selection.connect(sigc::bind(sigc::mem_fun(*marker, &PackagesMarker::select), Description));
    signal_on_changed_packages.connect(sigc::mem_fun(*this, &PackagesView::refresh_packages_view));

    treeview->append_column(_("C"), packages_columns->CurrentStatus);
    treeview->get_column(0)->set_sort_column(packages_columns->CurrentStatus);
    treeview->append_column(_("S"), packages_columns->SelectedStatus);
    treeview->get_column(1)->set_sort_column(packages_columns->SelectedStatus);
    treeview->append_column(_("Name"), packages_columns->Name);
    treeview->get_column(2)->set_sort_column(packages_columns->Name);
    treeview->append_column(_("Section"), packages_columns->Section);
    treeview->get_column(3)->set_sort_column(packages_columns->Section);
    treeview->append_column(_("Version"), packages_columns->Version);
    treeview->set_search_column(packages_columns->Name);

    reverse_packages_store = new std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator>;
    packages_store = build_store(generatorK,
				 packages_columns,
				 reverse_packages_store,
				 "");

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
        if (num % 10 == 0)
        {
          gtk_update();
          pMainWindow->get_progress_bar()->pulse();
        }
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


  class PackagesTabGenerator : public PackagesTreeModelGenerator
  {
    Glib::RefPtr<Gtk::ListStore> store;
    PackagesColumns *packages_columns;

  private:
    PackagesTabGenerator(PackagesColumns *_packages_columns)
    {
      packages_columns = _packages_columns;
      store = Gtk::ListStore::create(*packages_columns);
    }

  public:
    /** \brief Create a preview tab generator.
     *
     *  \param packages_columns  The columns of the new store.
     *
     *  \note This is mainly a workaround for the fact that either
     *  sigc++ doesn't provide convenience functors for constructors
     *  or I can't find them.
     */
    static PackagesTabGenerator *create(PackagesColumns *packages_columns)
    {
      return new PackagesTabGenerator(packages_columns);
    }

    void init(PackagesColumns *_packages_columns)
    {
      packages_columns = _packages_columns;
      store = Gtk::ListStore::create(*packages_columns);
    }

    void add(const pkgCache::PkgIterator &pkg, const pkgCache::VerIterator &ver,
	     std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator> * reverse_packages_store)
    {
      Gtk::TreeModel::iterator iter = store->append();
      Gtk::TreeModel::Row row = *iter;

      reverse_packages_store->insert(std::make_pair(pkg, iter));

      row[packages_columns->PkgIterator] = pkg;
      row[packages_columns->VerIterator] = ver;
      row[packages_columns->CurrentStatus] = current_state_string(pkg, ver);
      row[packages_columns->SelectedStatus] = selected_state_string(pkg, ver);
      row[packages_columns->Name] = pkg.Name()?pkg.Name():"";
      row[packages_columns->Section] = pkg.Section()?pkg.Section():"";
      row[packages_columns->Version] = ver.VerStr();
    }

    void finish()
    {
      store->set_sort_column(packages_columns->Name, Gtk::SORT_ASCENDING);
    }

    Glib::RefPtr<Gtk::TreeModel> get_model()
    {
      return store;
    }
  };

  PackagesTab::PackagesTab(const Glib::ustring &label) :
    Tab(Packages, label, Gnome::Glade::Xml::create(glade_main_file, "main_packages_vbox"), "main_packages_vbox")
  {
    get_xml()->get_widget("main_packages_textview", pPackagesTextView);
    get_xml()->get_widget("main_notebook_packages_limit_entry", pLimitEntry);
    pLimitEntry->signal_activate().connect(sigc::mem_fun(*this, &PackagesTab::repopulate_model));
    get_xml()->get_widget("main_notebook_packages_limit_button", pLimitButton);
    pLimitButton->signal_clicked().connect(sigc::mem_fun(*this, &PackagesTab::repopulate_model));

    pPackagesView = new PackagesView(sigc::ptr_fun(PackagesTabGenerator::create), get_xml());

    pPackagesView->signal_on_package_selection.connect(sigc::mem_fun(*this, &PackagesTab::display_desc));

    get_widget()->show();
  }

  void PackagesTab::repopulate_model()
  {
    pPackagesView->relimit_packages_view(pLimitEntry->get_text());
    set_label(_("Packages: ") + pLimitEntry->get_text());
  }

  void PackagesTab::display_desc(pkgCache::PkgIterator pkg, pkgCache::VerIterator ver)
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
      pPackagesTextView->get_buffer()->set_text(misc + _("Description: ") + desc);
    }
    else
    {
      pPackagesTextView->get_buffer()->set_text(ssprintf("%s%s\n", _("Name: "), pkg.Name()));
    }
  }

  class PreviewTabGenerator : public PackagesTreeModelGenerator
  {
    Glib::RefPtr<Gtk::TreeStore> store;
    PackagesColumns *packages_columns;

    // \todo Swiped from pkg_grouppolicy_mode; should be pushed into
    // low-level code.
    const static char * const child_names[];

    std::map<int, Gtk::TreeStore::iterator> state_trees;

  private:
    PreviewTabGenerator(PackagesColumns *_packages_columns)
    {
      packages_columns = _packages_columns;
      store = Gtk::TreeStore::create(*packages_columns);
    }

  public:
    /** \brief Create a preview tab generator.
     *
     *  \param packages_columns  The columns of the new store.
     *
     *  \note This is mainly a workaround for the fact that either
     *  sigc++ doesn't provide convenience functors for constructors
     *  or I can't find them.
     */
    static PreviewTabGenerator *create(PackagesColumns *packages_columns)
    {
      return new PreviewTabGenerator(packages_columns);
    }

    void add(const pkgCache::PkgIterator &pkg, const pkgCache::VerIterator &ver,
	     std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator> * reverse_packages_store)
    {
      int group = find_pkg_state(pkg, *apt_cache_file);
      if(group != pkg_unchanged)
	{
	  const std::map<int, Gtk::TreeModel::iterator>::const_iterator found =
	    state_trees.find(group);

	  Gtk::TreeModel::iterator tree;
	  if(found == state_trees.end())
	    {
	      tree = store->append();
	      Gtk::TreeModel::Row tree_row = *tree;
	      tree_row[packages_columns->Name] = _(child_names[group]);
	      state_trees[group] = tree;
	    }
	  else
	    tree = found->second;

	  Gtk::TreeModel::iterator iter = store->append(tree->children());
	  Gtk::TreeModel::Row row = *iter;

	  reverse_packages_store->insert(std::make_pair(pkg, iter));

	  row[packages_columns->PkgIterator] = pkg;
	  row[packages_columns->VerIterator] = ver;
	  row[packages_columns->CurrentStatus] = current_state_string(pkg, ver);
	  row[packages_columns->SelectedStatus] = selected_state_string(pkg, ver);
	  row[packages_columns->Name] = pkg.Name()?pkg.Name():"";
	  row[packages_columns->Section] = pkg.Section()?pkg.Section():"";
	  row[packages_columns->Version] = ver.VerStr();
	}
    }

    void finish()
    {
      store->set_sort_column(packages_columns->Name, Gtk::SORT_ASCENDING);
    }

    Glib::RefPtr<Gtk::TreeModel> get_model()
    {
      return store;
    }
  };


  // \todo This is proof-of-concept only; the child_names list should
  // be in common code.
  const char * const PreviewTabGenerator::child_names[num_pkg_action_states]=
    {
      N_("Packages with unsatisfied dependencies\n The dependency requirements of these packages will be unmet after the install is complete.\n .\n The presence of this tree probably indicates that something is broken, either on your system or in the Debian archive."),
      N_("Packages being removed because they are no longer used\n These packages are being deleted because they were automatically installed to fulfill dependencies, and the planned action will result in no installed package declaring an 'important' dependency on them.\n"),
      N_("Packages being automatically held in their current state\n These packages could be upgraded, but they have been kept in their current state to avoid breaking dependencies."),
      N_("Packages being automatically installed to satisfy dependencies\n These packages are being installed because they are required by another package you have chosen for installation."),
      N_("Packages being deleted due to unsatisfied dependencies\n These packages are being deleted because one or more of their dependencies is no longer available, or because another package conflicts with them."),
      N_("Packages to be downgraded\n An older version of these packages than is currently installed will be installed."),
      N_("Packages being held back\n These packages could be upgraded, but you have asked for them to be held at their current version."),
      N_("Packages to be reinstalled\n These packages will be reinstalled."),
      N_("Packages to be installed\n These packages have been manually selected for installation on your computer."),
      N_("Packages to be removed\n These packages have been manually selected for removal."),
      N_("Packages to be upgraded\n These packages will be upgraded to a newer version."),
      N_("Packages that are partially installed\n These packages are not fully installed and configured; an attempt will be made to complete their installation."),
    };

  PreviewTab::PreviewTab(const Glib::ustring &label) :
    Tab(Preview, label, Gnome::Glade::Xml::create(glade_main_file, "main_packages_vbox"), "main_packages_vbox")
  {
    get_xml()->get_widget("main_packages_textview", pPackagesTextView);
    get_xml()->get_widget("main_notebook_packages_limit_entry", pLimitEntry);
    pLimitEntry->signal_activate().connect(sigc::mem_fun(*this, &PreviewTab::repopulate_model));
    get_xml()->get_widget("main_notebook_packages_limit_button", pLimitButton);
    pLimitButton->signal_clicked().connect(sigc::mem_fun(*this, &PreviewTab::repopulate_model));

    pPackagesView = new PackagesView(sigc::ptr_fun(PreviewTabGenerator::create), get_xml());;

    pPackagesView->signal_on_package_selection.connect(sigc::mem_fun(*this, &PreviewTab::display_desc));
    pPackagesView->get_treeview()->expand_all();

    get_widget()->show();
  }

  void PreviewTab::repopulate_model()
  {
    pPackagesView->relimit_packages_view(pLimitEntry->get_text());
    pPackagesView->get_treeview()->expand_all();
    set_label(_("Preview: ") + pLimitEntry->get_text());
  }

  void PreviewTab::display_desc(pkgCache::PkgIterator pkg, pkgCache::VerIterator ver)
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
      pPackagesTextView->get_buffer()->set_text(misc + _("Description: ") + desc);
    }
    else
    {
      pPackagesTextView->get_buffer()->set_text(ssprintf("%s%s\n", _("Name: "), pkg.Name()));
    }
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
    switch (tab.get_type())
      {
    case Dashboard:
      // No more than one Dashboard at once
      if (number_of(Dashboard) == 0)
      {
        rval = insert_page(*(tab.get_widget()), *(tab.get_label_widget()), 0);
      }
      break;
      // TODO: handle other kinds of tabs
    default:
      rval = insert_page(*(tab.get_widget()), *(tab.get_label_widget()), next_position(tab.get_type()));
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
    // TODO: *Tab Constructors should also get to decide tab labels
    DownloadTab * tab = new DownloadTab(_("Download:"));
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
    // TODO: *Tab Constructors should also get to decide tab labels
    PackagesTab * tab = new PackagesTab(_("Packages:"));
    int new_page_idx = pMainWindow->get_notebook()->append_page(*tab);
    pMainWindow->get_notebook()->set_current_page(new_page_idx);
    return tab;
  }

  /**
   * Adds a packages tab to the interface.
   * TODO: Get this one out of here!
   * TODO: There's too much copy-pasting going on here.
   */
  PreviewTab * tab_add_preview()
  {
    // TODO: *Tab Constructors should also get to decide tab labels
    PreviewTab * tab = new PreviewTab(_("Preview:"));
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
    /*PackagesTab * tab = */tab_add_packages();
  }

  void do_preview()
  {
    /*PreviewTab * tab = */tab_add_preview();
  }

  bool do_want_quit()
  {
    want_to_quit = true;
    return false;
  }

  void do_quit()
  {
    do_want_quit();
    pKit->quit();
  }

  AptitudeWindow::AptitudeWindow(BaseObjectType* cobject, const Glib::RefPtr<Gnome::Glade::Xml>& refGlade) : Gtk::Window(cobject)
  {
    refGlade->get_widget_derived("main_notebook", pNotebook);

    refGlade->get_widget("main_toolbutton_dashboard", pToolButtonDashboard);
    pToolButtonDashboard->signal_clicked().connect(&do_dashboard);

    refGlade->get_widget("main_toolbutton_update", pToolButtonUpdate);
    pToolButtonUpdate->signal_clicked().connect(&do_update);

    refGlade->get_widget("main_toolbutton_packages", pToolButtonPackages);
    pToolButtonPackages->signal_clicked().connect(&do_packages);

    refGlade->get_widget("main_toolbutton_preview", pToolButtonPreview);
    pToolButtonPreview->signal_clicked().connect(&do_preview);

    refGlade->get_widget("main_toolbutton_update", pToolButtonUpdate);
    pToolButtonUpdate->signal_clicked().connect(&do_update);

    // TODO: Give this a proper name.
    refGlade->get_widget("imagemenuitem5", pMenuFileExit);
    pMenuFileExit->signal_activate().connect(&do_quit);

    refGlade->get_widget("main_progressbar", pProgressBar);
    refGlade->get_widget("main_statusbar", pStatusBar);
    pStatusBar->push("Aptitude-gtk v2", 0);
  }

  void main(int argc, char *argv[])
  {
    pKit = new Gtk::Main(argc, argv);
    Gtk::Main::signal_quit().connect(&do_want_quit);
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
