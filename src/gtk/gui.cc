// gui.cc
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "gui.h"

#include "aptitude.h"

#include <map>

#undef OK
#include <gtkmm.h>
#include <libglademm/xml.h>

#include <apt-pkg/error.h>

#include <generic/apt/apt.h>
#include <generic/apt/apt_undo_group.h>
#include <generic/apt/download_install_manager.h>
#include <generic/apt/download_update_manager.h>

#include <sigc++/signal.h>

#include <cwidget/generic/util/transcode.h>

#include <gtk/progress.h>
#include <gtk/tab.h>
#include <gtk/resolver.h>
#include <gtk/download.h>
#include <gtk/info.h>
#include <gtk/packagestab.h>
#include <gtk/previewtab.h>

namespace gui
{
  Glib::RefPtr<Gnome::Glade::Xml> refXml;
  std::string glade_main_file;
  Gtk::Main * pKit;
  AptitudeWindow * pMainWindow;

  /**
   * Signal called when a package status change happens, usually when marking it.
   * Used by PackagesView for global updating.
   * TODO: Check if it doesn't already exists.
   */
  sigc::signal<void, std::set<pkgCache::PkgIterator> > signal_on_changed_packages;

  // True if a download or package-list update is proceeding.  This hopefully will
  // avoid the nasty possibility of collisions between them.
  // FIXME: uses implicit locking -- if multithreading happens, should use a mutex
  //       instead.
  static bool active_download;
  bool want_to_quit = false;

  void gtk_update()
  {
    while (Gtk::Main::events_pending())
      Gtk::Main::iteration();
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

  class UpdateTab : public DownloadTab
  {
    private:
    void really_do_update_lists()
    {
      download_update_manager *m = new download_update_manager;

      // downloading now I suppose ?
      guiOpProgress progress;
      guiPkgAcquireStatus acqlog(this);
      acqlog.Update = true;
      acqlog.MorePulses = true;
      if (!m->prepare(progress, acqlog, NULL))
        return;
      acqlog.Update = true;
      acqlog.MorePulses = true;
      download_manager::result result = download_manager::do_again;
      while (result == download_manager::do_again)
      {
        m->do_download(100);
        result = m->finish(pkgAcquire::Continue, progress);
      }
      guiOpProgress * p = gen_progress_bar();
      apt_load_cache(p, true, NULL);
      delete p;
    }
    public:
      UpdateTab(const Glib::ustring &label)
      : DownloadTab(label)
      {
        ;;
      }
    void do_update_lists()
    {
      if (!active_download)
        {
          if (getuid()==0)
            {
              pMainWindow->get_progress_bar()->set_text("Updating..");
              pMainWindow->get_progress_bar()->set_fraction(0);
              download_store->clear();
              really_do_update_lists();
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
      {
        Gtk::MessageDialog dialog(*pMainWindow,
            "Na...", false,
            Gtk::MESSAGE_INFO, Gtk::BUTTONS_OK, true);
        dialog.set_secondary_text("A package-list update or install run is already taking place.");

        dialog.run();
      }
    }
  };

  class InstallRemoveTab : public DownloadTab
  {
    // FIXME: Hack while finding a nonblocking thread join or something else.
    bool finished;
    bool in_dpkg;
    public:
      InstallRemoveTab(const Glib::ustring &label)
      : DownloadTab(label)
      {
        // FIXME: Hack while finding a nonblocking thread join or something else.
        finished = false;
        in_dpkg = false;
      }
      void handle_result(pkgPackageManager::OrderResult result)
      {
        Gtk::MessageDialog dialog(*pMainWindow, "Install run finished", false, Gtk::MESSAGE_INFO, Gtk::BUTTONS_OK,
            true);
        switch(result)
        {
        case pkgPackageManager::Completed:
          dialog.set_secondary_text("Successfully completed!");
          break;
        case pkgPackageManager::Incomplete:
          dialog.set_secondary_text("Partially completed!");
          break;
        case pkgPackageManager::Failed:
          dialog.set_secondary_text("Failed!");
          break;
        }
        dialog.run();
      }
      void handle_install(download_install_manager *m, OpProgress &progress)
      {
        download_manager::result result = download_manager::do_again;
        while (result == download_manager::do_again)
        {
          m->do_download(100);
          in_dpkg = true;
          result = m->finish(pkgAcquire::Continue, progress);
          in_dpkg = false;
        }
        finished = true;
      }
      void install_or_remove_packages()
      {
        download_install_manager *m = new download_install_manager(false);

        guiOpProgress progress;
        guiPkgAcquireStatus acqlog(this);
        acqlog.Update = true;
        acqlog.MorePulses = true;
        if (!m->prepare(progress, acqlog, NULL))
          return;
        acqlog.Update = true;
        acqlog.MorePulses = true;
        download_store->clear();
        // FIXME: Hack while finding a nonblocking thread join or something else.
        Glib::Thread * install_thread =
          Glib::Thread::create(sigc::bind(sigc::mem_fun(*this, &InstallRemoveTab::handle_install), m, progress), true);
        m->post_install_hook.connect(sigc::mem_fun(*this, &InstallRemoveTab::handle_result));
        while(!finished)
        {
          if (in_dpkg)
            pMainWindow->get_progress_bar()->pulse();
          gtk_update();
          Glib::usleep(100000);
        }
        install_thread->join();

        //m->finish(pkgAcquire::Continue, progress);
      }
  };

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


  void do_mark_upgradable()
  {
    if(apt_cache_file)
    {
      std::set<pkgCache::PkgIterator> changed_packages;
      {
        aptitudeDepCache::action_group group(*apt_cache_file, NULL, &changed_packages);
        undo_group *undo=new apt_undo_group;

        (*apt_cache_file)->mark_all_upgradable(true, true, undo);

        if(!undo->empty())
          apt_undos->add_item(undo);
        else
          delete undo;
      }
      signal_on_changed_packages(changed_packages);
    }
  }

  void do_keep_all()
  {
    std::auto_ptr<undo_group> undo(new apt_undo_group);
    std::set<pkgCache::PkgIterator> changed_packages;
    {
      aptitudeDepCache::action_group group(*apt_cache_file, undo.get(), &changed_packages);

      for(pkgCache::PkgIterator i=(*apt_cache_file)->PkgBegin();
          !i.end(); ++i)
        (*apt_cache_file)->mark_keep(i, false, false, undo.get());

      if(!undo.get()->empty())
        apt_undos->add_item(undo.release());
    }
    signal_on_changed_packages(changed_packages);
  }

  /**
   * Adds a Tab_Type tab to the interface.
   * TODO: Get this one out of here!
   */
  void tab_add(Tab *tab)
  {
    int new_page_idx = pMainWindow->get_notebook()->append_page(*tab);
    pMainWindow->get_notebook()->set_current_page(new_page_idx);
  }

  void do_dashboard()
  {
    tab_add(new DashboardTab(_("Dashboard:")));
  }

  void do_update()
  {
    UpdateTab * tab = new UpdateTab(_("Update"));
    tab_add(tab);
    tab->do_update_lists();
  }

  void do_packages()
  {
    tab_add(new PackagesTab(_("Packages:")));
  }

  void do_preview()
  {
    tab_add(new PreviewTab(_("Preview:")));
  }

  void do_resolver()
  {
    tab_add(new ResolverTab(_("Resolver:")));
  }

  void do_installremove()
  {
    InstallRemoveTab * tab = new InstallRemoveTab(_("Install/Remove:"));
    tab_add(tab);
    tab->install_or_remove_packages();
  }

  void do_sweep()
  {
    system("/usr/games/gnomine&");
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

    refGlade->get_widget("main_toolbutton_resolver", pToolButtonResolver);
    pToolButtonResolver->signal_clicked().connect(&do_resolver);

    refGlade->get_widget("main_toolbutton_installremove", pToolButtonInstallRemove);
    pToolButtonInstallRemove->signal_clicked().connect(&do_installremove);

    refGlade->get_widget("menu_do_mark_upgradable", pMenuFileMarkUpgradable);
    pMenuFileMarkUpgradable->signal_activate().connect(&do_mark_upgradable);

    refGlade->get_widget("menu_do_keep_all", pMenuFileKeepAll);
    pMenuFileKeepAll->signal_activate().connect(&do_keep_all);

    refGlade->get_widget("menu_do_sweep", pMenuFileSweep);
    pMenuFileSweep->signal_activate().connect(&do_sweep);

    refGlade->get_widget("menu_do_quit", pMenuFileExit);
    pMenuFileExit->signal_activate().connect(&do_quit);

    refGlade->get_widget("main_progressbar", pProgressBar);
    refGlade->get_widget("main_statusbar", pStatusBar);
    pStatusBar->push("Aptitude-gtk v2", 0);
  }

  void init_glade(int argc, char *argv[])
  {
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
  }

  void main(int argc, char *argv[])
  {
    Glib::init();
    Glib::thread_init();
    pKit = new Gtk::Main(argc, argv);
    Gtk::Main::signal_quit().connect(&do_want_quit);
    init_glade(argc, argv);

    refXml->get_widget_derived("main_window", pMainWindow);

    // TODO: this is unnecessary if consume_errors is connected for the GUI.
    check_apt_errors();

    guiOpProgress * p=gen_progress_bar();
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
