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

 /**
  * This is a list of tab types.
  */
  enum TabType
  {
    DashboardTab, DownloadTab, PackagesTab, InfoTab, PreviewTab, ResolverTab
  };

  /**
   * A Tab is a widget with some metadata for inserting into the notebook.
   */
  class Tab : public Gtk::Widget
  {
    private:
      Glib::ustring label;
      TabType type;
    public:
      /**
       * Glade::Xml derived widget constructor.
       */
      Tab(BaseObjectType* cobject, const Glib::RefPtr<Gnome::Glade::Xml>& refGlade) : Gtk::Widget(cobject)
      {
        label = "";
        type = DashboardTab;
      }
      void set_metadata(Glib::ustring new_label, TabType new_type)
      {
        label = new_label;
        type = new_type;
      }
      Glib::ustring get_label()
      {
        return label;
      }
      TabType get_type()
      {
        return type;
      }
  };

  /**
   * This is a custom widget that handles placement of tabs
   */
  class TabsManager : public Gtk::Notebook
  {
    private:
      /**
       * Gives the position for the next tab of given type
       * @param type type of tab
       * @return position of the next tab of this type
       */
      int next_position(TabType type)
      {
        // TODO: implement something more elaborate and workflow-wise intuitive
        return get_n_pages();
      }
      /**
       * Gives the number of tabs of given type
       * @param type type of tab
       * @return number of tabs of this type
       */
      int number_of(TabType type)
      {
        return 0;
      }
    public:
      /**
       * Glade::Xml derived widget constructor.
       */
      TabsManager(BaseObjectType* cobject, const Glib::RefPtr<Gnome::Glade::Xml>& refGlade) : Gtk::Notebook(cobject)
      {
        ;;
      }
      /**
       * Appends a tab to the notebook
       * @param tab tab to append
       * @return position of the appended tab
       */
      int append_page(Tab& tab)
      {
        int rval;
        switch (tab.get_type())
        {
        case DashboardTab:
          // No more than one DashboardTab at once
          if (number_of(DashboardTab) == 0)
          {
            rval = insert_page(tab, _("Dashboard"), 0);
          }
          break;
        // TODO: handle other kinds of tabs
        default:
          rval = insert_page(tab, "generic tab: " + tab.get_label(), next_position(tab.get_type()));
        }
        return rval;
      }
  };

  TabsManager * pMainNotebook;

  /**
   * Adds a dashboard tab to the interface.
   * TODO: Get this one out of here!
   */
  void tab_add_dashboard()
  {
    Tab * tab;
    Glib::RefPtr<Gnome::Glade::Xml> refLocalXml = Gnome::Glade::Xml::create(glade_main_file, "label1");
    refLocalXml->get_widget_derived("label1", tab);
    tab->set_metadata("truc", DashboardTab);
    pMainNotebook->set_current_page(pMainNotebook->append_page(*tab));
  }

  /**
   * Adds a download tab to the interface.
   * TODO: Get this one out of here!
   */
  Tab * tab_add_download()
  {
    Tab * tab;
    Glib::RefPtr<Gnome::Glade::Xml> refLocalXml = Gnome::Glade::Xml::create(glade_main_file, "label2");
    refLocalXml->get_widget_derived("label2", tab);
    tab->set_metadata("truc download", DownloadTab);
    pMainNotebook->set_current_page(pMainNotebook->append_page(*tab));
    return tab;
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
        pMainWindow->pProgressBar->set_text("");
        pMainWindow->pProgressBar->set_fraction(0);
      }
      void Update()
      {
        if (CheckChange(0.25))
        {
          pMainWindow->pProgressBar->set_text(Op);
          pMainWindow->pProgressBar->set_fraction(sanitizePercentFraction(Percent));
          gtk_update();
        }
      }
  };

  guiOpProgress * gen_progress_bar()
  {
    return new guiOpProgress;
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
    public:
      bool Pulse(pkgAcquire *Owner)
      {
        pkgAcquireStatus::Pulse(Owner);
        if (TotalItems != 0)
          pMainWindow->pProgressBar->set_fraction(((float)CurrentItems)/((float)TotalItems));
        pMainWindow->pProgressBar->set_text(ssprintf("%lu of %lu done", CurrentItems, TotalItems));
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

        pMainWindow->pStatusBar->pop(0);
        pMainWindow->pStatusBar->push(Itm.Description, 0);

        /*Gtk::TreeModel::iterator iter = pdownloadstore->append();
        Gtk::TreeModel::Row row = *iter;
        row[downloadcolumns.URI] = Itm.URI;
        row[downloadcolumns.ShortDesc] = Itm.ShortDesc;
        row[downloadcolumns.Description] = Itm.Description;*/
        gtk_update();
      }
  };

  void really_do_update_lists()
  {
    download_update_manager *m = new download_update_manager;

    // downloading now I suppose ?
    guiOpProgress progress;
    guiPkgAcquireStatus acqlog;
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

  void do_update_lists(Tab * tab)
  {
    if (!active_download)
      {
        if (getuid()==0)
          {
            pMainWindow->pProgressBar->set_text("Updating..");
            pMainWindow->pProgressBar->set_fraction(0);
            //pdownloadstore->clear();
            really_do_update_lists();
            pMainWindow->pProgressBar->set_fraction(0);
            pMainWindow->pStatusBar->pop(0);
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

  void do_update()
  {
    Tab * tab = tab_add_download();
    do_update_lists(tab);
  }

  AptitudeWindow::AptitudeWindow(BaseObjectType* cobject, const Glib::RefPtr<Gnome::Glade::Xml>& refGlade) : Gtk::Window(cobject)
  {
    refGlade->get_widget("main_toolbutton_dashboard", pToolButtonDashboard);
    pToolButtonDashboard->signal_clicked().connect(&tab_add_dashboard);
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

    refXml->get_widget_derived("main_notebook", pMainNotebook);

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
