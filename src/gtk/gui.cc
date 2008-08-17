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
#include <generic/apt/tags.h>

#include <sigc++/signal.h>

#include <cwidget/generic/threads/event_queue.h>
#include <cwidget/generic/util/transcode.h>

#include <gtk/dependency_chains_tab.h>
#include <gtk/download.h>
#include <gtk/info.h>
#include <gtk/packagestab.h>
#include <gtk/previewtab.h>
#include <gtk/progress.h>
#include <gtk/resolver.h>
#include <gtk/tab.h>

namespace gui
{
  // \todo Some of these icon choices suck.
  std::pair<std::string, Gtk::StockID> not_installed_columns("p", Gtk::Stock::CANCEL);
  std::pair<std::string, Gtk::StockID> virtual_columns("p", Gtk::StockID());
  std::pair<std::string, Gtk::StockID> unpacked_columns("u", Gtk::Stock::DIALOG_WARNING);
  std::pair<std::string, Gtk::StockID> half_configured_columns("C", Gtk::Stock::DIALOG_WARNING);
  std::pair<std::string, Gtk::StockID> half_installed_columns("H", Gtk::Stock::DIALOG_WARNING);
  std::pair<std::string, Gtk::StockID> config_files_columns("c", Gtk::Stock::PROPERTIES);
  std::pair<std::string, Gtk::StockID> triggers_awaited_columns("W", Gtk::Stock::DIALOG_WARNING);
  std::pair<std::string, Gtk::StockID> triggers_pending_columns("T", Gtk::Stock::DIALOG_WARNING);
  std::pair<std::string, Gtk::StockID> installed_columns("i", Gtk::Stock::APPLY);
  std::pair<std::string, Gtk::StockID> error_columns("E", Gtk::Stock::DIALOG_ERROR);
  
  std::pair<std::string, Gtk::StockID> install_columns("i", Gtk::Stock::ADD);
  std::pair<std::string, Gtk::StockID> reinstall_columns("r", Gtk::Stock::ADD);
  std::pair<std::string, Gtk::StockID> upgrade_columns("u", Gtk::Stock::GO_UP);
  std::pair<std::string, Gtk::StockID> downgrade_columns("u", Gtk::Stock::GO_DOWN);
  std::pair<std::string, Gtk::StockID> remove_columns("d", Gtk::Stock::REMOVE);
  std::pair<std::string, Gtk::StockID> purge_columns("p", Gtk::Stock::CLEAR);
  std::pair<std::string, Gtk::StockID> hold_columns("h", Gtk::Stock::MEDIA_PAUSE);
  std::pair<std::string, Gtk::StockID> forbid_columns("F", Gtk::Stock::STOP);
  std::pair<std::string, Gtk::StockID> broken_columns("B", Gtk::Stock::DIALOG_ERROR);


  Glib::RefPtr<Gnome::Glade::Xml> refXml;
  std::string glade_main_file;
  Gtk::Main * pKit;
  AptitudeWindow * pMainWindow;

  // True if a download or package-list update is proceeding.  This hopefully will
  // avoid the nasty possibility of collisions between them.
  // FIXME: uses implicit locking -- if multithreading happens, should use a mutex
  //       instead.
  static bool active_download;
  bool want_to_quit = false;

  namespace
  {
    // The Glib::dispatch mechanism only allows us to wake the main
    // thread up; it doesn't allow us to pass actual information across
    // the channel.  To hack this together, I borrow the event_queue
    // abstraction from cwidget, and use a dispatcher for the sole
    // purpose of waking the main thread up.
    //
    // I believe that putting sigc++ slots on this list should be OK:
    // from the sigc++ code, it looks like they get passed by value, not
    // by reference.  Once the slot is safely stuck into the list,
    // everything should be OK.
    cwidget::threads::event_queue<sigc::slot0<void> > background_events;
    Glib::Dispatcher background_events_dispatcher;

    void run_background_events()
    {
      sigc::slot0<void> f;
      while(background_events.try_get(f))
	{
	  f();
	}
    }
  }

  void post_event(const sigc::slot0<void> &event)
  {
    background_events.put(event);
    background_events_dispatcher();
  }

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


  void do_mark_upgradable()
  {
    if(apt_cache_file)
    {
      aptitudeDepCache::action_group group(*apt_cache_file, NULL);
      undo_group *undo=new apt_undo_group;

      (*apt_cache_file)->mark_all_upgradable(true, true, undo);

      if(!undo->empty())
	apt_undos->add_item(undo);
      else
	delete undo;
    }
  }

  void do_keep_all()
  {
    std::auto_ptr<undo_group> undo(new apt_undo_group);
    aptitudeDepCache::action_group group(*apt_cache_file, undo.get());

    for(pkgCache::PkgIterator i=(*apt_cache_file)->PkgBegin();
	!i.end(); ++i)
      (*apt_cache_file)->mark_keep(i, false, false, undo.get());

    if(!undo.get()->empty())
      apt_undos->add_item(undo.release());
  }

  /**
   * Adds a Tab_Type tab to the interface.
   * TODO: Get this one out of here!
   */
  void tab_add(Tab *tab)
  {
    pMainWindow->tab_add(tab);
  }

  void tab_del(Tab *tab)
  {
    pMainWindow->tab_del(tab);
  }

  void AptitudeWindow::tab_add(Tab *tab)
  {
    int new_page_idx = get_notebook()->append_page(*tab);
    get_notebook()->set_current_page(new_page_idx);
  }

  void AptitudeWindow::tab_del(Tab *tab)
  {
    get_notebook()->remove_page(*tab);
  }

  void AptitudeWindow::do_resolver()
  {
    tab_add(new ResolverTab(_("Resolver:")));
  }

  void AptitudeWindow::do_preview()
  {
    tab_add(new PreviewTab(_("Preview:")));
  }

  void AptitudeWindow::do_show_broken()
  {
    PackagesTab *tab = new PackagesTab(_("Broken packages"));
    tab->get_pkg_view()->set_limit("?broken");
    tab_add(tab);
  }

  namespace
  {
    bool do_hyperlink_callback(const Glib::RefPtr<Glib::Object> &event_object,
			       GdkEvent *event,
			       const Gtk::TextBuffer::iterator &iter,
			       sigc::slot0<void> link_action)
    {
      // TODO: draw a nice "box" / change the style on
      // GDK_BUTTON_PRESS.
      switch(event->type)
	{
	case GDK_BUTTON_RELEASE:
	  {
	    if(event->button.button == 1)
	      link_action();
	  }
	default:
	  break;
	}

      return false;
    }
  }


  class BrokenPackagesNotification : public Notification
  {
  private:
    Gtk::Button *show_broken_button;
    Gtk::Button *resolve_dependencies_button;

    // Used to tell whether we need to update.
    int last_broken_count;

    void do_cache_reloaded()
    {
      if(apt_cache_file)
	(*apt_cache_file)->package_state_changed.connect(sigc::mem_fun(*this, &BrokenPackagesNotification::update));
    }

  public:
    BrokenPackagesNotification(AptitudeWindow *main_window)
      : Notification(false)
    {
      last_broken_count = 0;

      show_broken_button = new Gtk::Button(_("Show broken packages"));
      show_broken_button->signal_clicked().connect(sigc::mem_fun(*main_window, &AptitudeWindow::do_show_broken));
      add_button(show_broken_button);

      resolve_dependencies_button = new Gtk::Button(_("Resolve dependencies"));
      resolve_dependencies_button->signal_clicked().connect(sigc::mem_fun(*main_window, &AptitudeWindow::do_resolver));
      add_button(resolve_dependencies_button);

      update();
      finalize();

      if(apt_cache_file)
	(*apt_cache_file)->package_state_changed.connect(sigc::mem_fun(*this, &BrokenPackagesNotification::update));

      cache_reloaded.connect(sigc::mem_fun(*this, &BrokenPackagesNotification::do_cache_reloaded));

      set_color(Gdk::Color("#FFE0E0"));
    }

    void update()
    {
      int broken_count = apt_cache_file ? (*apt_cache_file)->BrokenCount() : 0;

      if(broken_count == last_broken_count)
	return;

      Glib::RefPtr<Gtk::TextBuffer> buffer = Gtk::TextBuffer::create();

      Glib::RefPtr<Gtk::TextBuffer::Tag> broken_tag = buffer->create_tag();
      broken_tag->property_weight() = Pango::WEIGHT_BOLD;

      buffer->insert_with_tag(buffer->end(),
			      ssprintf(ngettext(_("%d package is broken"),
						_("%d packages are broken."),
						broken_count),
				       broken_count),
			      broken_tag);

      bool something_is_broken = broken_count > 0;

      resolve_dependencies_button->set_sensitive(something_is_broken);

      property_visible() = something_is_broken;

      set_buffer(buffer);
      last_broken_count = broken_count;
    }
  };

  class NotificationInstallRemove : public Notification
  {
  private:
    Gtk::Button *preview_button;
    Gtk::Button *install_remove_button;

    // Used to tell whether we need to update.
    int last_broken_count;
    int last_download_size;
    int last_install_count;
    int last_remove_count;

    void do_cache_reloaded()
    {
      if(apt_cache_file)
	(*apt_cache_file)->package_state_changed.connect(sigc::mem_fun(*this, &NotificationInstallRemove::update));
    }

  public:
    NotificationInstallRemove(AptitudeWindow *main_window)
      : Notification(false)
    {
      last_broken_count = 0;
      last_download_size = 0;
      last_install_count = 0;
      last_remove_count = 0;

      preview_button = new Gtk::Button(_("View changes"));
      preview_button->signal_clicked().connect(sigc::mem_fun(main_window, &AptitudeWindow::do_preview));
      add_button(preview_button);

      install_remove_button = new Gtk::Button(_("Apply changes"));
      install_remove_button->signal_clicked().connect(sigc::ptr_fun(&do_installremove));
      add_button(install_remove_button);

      update();
      finalize();

      if(apt_cache_file)
	(*apt_cache_file)->package_state_changed.connect(sigc::mem_fun(*this, &NotificationInstallRemove::update));

      cache_reloaded.connect(sigc::mem_fun(*this, &NotificationInstallRemove::do_cache_reloaded));
    }

    void update()
    {
      int dl_size = apt_cache_file ? (*apt_cache_file)->DebSize() : 0;
      int broken_count = apt_cache_file ? (*apt_cache_file)->BrokenCount() : 0;
      int install_count = apt_cache_file ? (*apt_cache_file)->InstCount() : 0;
      int remove_count = apt_cache_file ? (*apt_cache_file)->DelCount() : 0;

      if(dl_size == last_download_size && broken_count == last_broken_count &&
	 install_count == last_install_count && remove_count == last_remove_count)
	return;

      Glib::RefPtr<Gtk::TextBuffer> buffer = Gtk::TextBuffer::create();

      if(install_count > 0 || remove_count > 0)
	{
	  if(buffer->size() > 0)
	    buffer->insert(buffer->end(), "\n");

	  Glib::RefPtr<Gtk::TextBuffer::Mark> start_msg_mark = buffer->create_mark(buffer->end());

	  if(install_count > 0)
	    {
	      buffer->insert(buffer->end(),
			     // ForTranslators: any numbers in this
			     // string will be displayed in a larger
			     // font.
			     ssprintf(ngettext(_("%d package to install"),
					       _("%d packages to install"),
					       install_count),
				      install_count));

	      if(remove_count > 0)
		buffer->insert(buffer->end(), "; ");
	    }

	  if(remove_count > 0)
	    {
	      buffer->insert(buffer->end(),
			     // ForTranslators: any numbers in this
			     // string will be displayed in a larger
			     // font.
			     ssprintf(ngettext(_("%d package to remove"),
					       _("%d packages to remove"),
					       remove_count),
				      remove_count));

	      buffer->insert(buffer->end(), ".");
	    }

	  // HACK.  I want to make the numbers bigger, but I can't do
	  // that above because of translation considerations (we
	  // don't have a way of embedding markup into the string, so
	  // the markup can't be part of the string).  Instead I just
	  // magically know that the only digits in the string are the
	  // relevant numbers.
	  Gtk::TextBuffer::iterator start = buffer->get_iter_at_mark(start_msg_mark);
	  Gtk::TextBuffer::iterator end = buffer->end();
	  Glib::RefPtr<Gtk::TextBuffer::Tag> number_tag = buffer->create_tag();
	  number_tag->property_scale() = Pango::SCALE_LARGE;

	  while(start != end)
	    {
	      while(start != end && !isdigit(*start))
		++start;

	      if(start != end)
		{
		  Gtk::TextBuffer::iterator number_start = start;
		  while(start != end && isdigit(*start))
		    ++start;
		  buffer->apply_tag(number_tag, number_start, start);
		}
	    }
	}

      if(dl_size > 0)
	{
	  if(buffer->size() > 0)
	    buffer->insert(buffer->end(), "\n");
	  buffer->insert(buffer->end(),
			 ssprintf(_("Download size: %sB."),
				  SizeToStr(dl_size).c_str()));
	}

      bool something_is_broken = broken_count > 0;
      bool download_planned = install_count > 0 || remove_count > 0;

      preview_button->set_sensitive(download_planned);
      install_remove_button->set_sensitive(download_planned && !something_is_broken);

      property_visible() = !active_download && download_planned;

      set_buffer(buffer);
      last_broken_count = broken_count;
      last_download_size = dl_size;
      last_install_count = install_count;
      last_remove_count = remove_count;
    }
  };

  // TODO: make the mouse cursor change on hyperlinks.  The only
  // advice I can find on how to do this is to connect a signal to the
  // TextView that examines all the tags under the mouse and sets the
  // pointer depending on whether it finds a special one; otherwise it
  // sets the cursor to "edit". (in our case this would involve
  // dynamic_casting each tag to a derived class that implements a
  // "get_cursor()" method) This is gross and will require our own
  // special version of TextView, but OTOH it should work pretty well.
  Gtk::TextBuffer::iterator add_hyperlink(const Glib::RefPtr<Gtk::TextBuffer> &buffer,
					  Gtk::TextBuffer::iterator where,
					  const Glib::ustring &link_text,
					  const sigc::slot0<void> &link_action)
  {
    Glib::RefPtr<Gtk::TextBuffer::Tag> tag = buffer->create_tag();

    tag->property_foreground() = "#3030FF";
    tag->property_underline() = Pango::UNDERLINE_SINGLE;
    tag->signal_event().connect(sigc::bind(sigc::ptr_fun(do_hyperlink_callback),
					   link_action));

    return buffer->insert_with_tag(where, link_text, tag);
  }

  namespace
  {
    void make_debtags_tab(const std::string &tag)
    {
      PackagesTab *tab = new PackagesTab("Packages: ?tag(^" + tag + "$)");
      tab_add(tab);
      tab->get_limit_entry()->set_text("?tag(^" + tag + "$)");
      tab->get_pkg_view()->set_limit("?tag(^" + tag + "$)");
    }
  }

  Gtk::TextBuffer::iterator add_debtags(const Glib::RefPtr<Gtk::TextBuffer> &buffer,
					Gtk::TextBuffer::iterator where,
					const pkgCache::PkgIterator &pkg,
					const Glib::RefPtr<Gtk::TextBuffer::Tag> &headerTag)
  {
    if(pkg.end())
      return where;

#ifdef HAVE_EPT
    typedef ept::debtags::Tag tag;
    using aptitude::apt::get_tags;

    const std::set<tag> realS(get_tags(pkg));
    const std::set<tag> * const s(&realS);
#else
    const std::set<tag> * const s(get_tags(pkg));
#endif

    if(s != NULL && !s->empty())
      {
	bool first = true;
	where = buffer->insert_with_tag(where,
					ssprintf(_("Tags of %s:\n"), pkg.Name()),
					headerTag);
	// TODO: indent all the tags.
	for(std::set<tag>::const_iterator it = s->begin();
	    it != s->end(); ++it)
	  {
#ifdef HAVE_EPT
	    const std::string name(it->fullname());
#else
	    const std::string name(it->str());
#endif

	    if(first)
	      first = false;
	    else
	      where = buffer->insert(where, ", ");

	    where = add_hyperlink(buffer, where,
				  name,
				  sigc::bind(sigc::ptr_fun(&make_debtags_tab),
					     name));
	  }
      }

    return where;
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
    pToolButtonPreview->signal_clicked().connect(sigc::mem_fun(*this, &AptitudeWindow::do_preview));

    refGlade->get_widget("main_toolbutton_resolver", pToolButtonResolver);
    pToolButtonResolver->signal_clicked().connect(sigc::mem_fun(*this, &AptitudeWindow::do_resolver));

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

    {
      Gtk::MenuItem *menu_view_apt_errors;
      refGlade->get_widget("menu_view_apt_errors", menu_view_apt_errors);
      menu_view_apt_errors->signal_activate().connect(sigc::mem_fun(this, &AptitudeWindow::show_apt_errors));
    }

    {
      Gtk::MenuItem *menu_view_dependency_chains;
      refGlade->get_widget("menu_item_find_dependency_paths", menu_view_dependency_chains);
      menu_view_dependency_chains->signal_activate().connect(sigc::mem_fun(this, &AptitudeWindow::show_dependency_chains_tab));
    }

    refGlade->get_widget_derived("main_notify_rows", pNotifyView);

    pNotifyView->add_notification(Gtk::manage(new BrokenPackagesNotification(this)));
    pNotifyView->add_notification(Gtk::manage(new NotificationInstallRemove(this)));

    refGlade->get_widget("main_progressbar", pProgressBar);
    refGlade->get_widget("main_statusbar", pStatusBar);
    pStatusBar->push("Aptitude-gtk v2", 0);

    activeErrorTab = NULL;
    errorStore.error_added.connect(sigc::mem_fun(*this, &AptitudeWindow::show_apt_errors));
    if(!errorStore.get_model()->children().empty())
      {
	// Show the apt error tab in the idle callback so we don't
	// kill ourselves.  The problem is that the global pointer to
	// this window isn't set up yet and show_apt_errors() expects
	// to be able to find it (ew).
	Glib::signal_idle().connect(sigc::bind_return(sigc::mem_fun(*this, &AptitudeWindow::show_apt_errors),
						      false));
      }

    // We need to be shown before we become not-sensitive, or GDK gets
    // cranky and spits out warnings.
    show();

    // Use a big global lock to keep the user from shooting themselves
    // while the cache is loading.
    if(!apt_cache_file)
      set_sensitive(false);
    cache_closed.connect(sigc::bind(sigc::mem_fun(*this, &Gtk::Widget::set_sensitive),
				    false));
    cache_reloaded.connect(sigc::bind(sigc::mem_fun(*this, &Gtk::Widget::set_sensitive),
				      true));

    // When the cache is reloaded, attach to the new resolver-manager.
    cache_reloaded.connect(sigc::mem_fun(*this, &AptitudeWindow::update_resolver_sensitivity_callback));
    update_resolver_sensitivity_callback();
  }

  void AptitudeWindow::update_resolver_sensitivity_callback()
  {
    if(resman != NULL)
      resman->state_changed.connect(sigc::mem_fun(*this, &AptitudeWindow::update_resolver_sensitivity));

    update_resolver_sensitivity();
  }

  void AptitudeWindow::update_resolver_sensitivity()
  {
    bool resolver_exists = resman != NULL && resman->resolver_exists();

    pToolButtonResolver->set_sensitive(resolver_exists);
  }

  void AptitudeWindow::apt_error_tab_closed()
  {
    activeErrorTab = NULL;
  }

  void AptitudeWindow::show_apt_errors()
  {
    if(activeErrorTab != NULL)
      activeErrorTab->show();
    else
      {
	activeErrorTab = new ErrorTab("Errors", errorStore);
	activeErrorTab->closed.connect(sigc::mem_fun(this, &AptitudeWindow::apt_error_tab_closed));
	tab_add(activeErrorTab);
      }
  }

  void AptitudeWindow::show_dependency_chains_tab()
  {
    tab_add(new DependencyChainsTab("Find Dependency Chains"));
  }

  void init_glade(int argc, char *argv[])
  {
    // Use the basename of argv0 to find the Glade file.
    // TODO: note that the .glade file will ultimately
    //       go to /usr/share/aptitude/glawith referede or something,
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

  namespace
  {
    void do_apt_init()
    {
      std::auto_ptr<guiOpProgress> p(gen_progress_bar());
      apt_init(p.get(), true, NULL);
    }
  }

  void main(int argc, char *argv[])
  {
    Glib::init();
    Glib::thread_init();

    background_events_dispatcher.connect(sigc::ptr_fun(&run_background_events));

    pKit = new Gtk::Main(argc, argv);
    Gtk::Main::signal_quit().connect(&do_want_quit);
    init_glade(argc, argv);

    // Set up the resolver-triggering signals.
    init_resolver();

    // Postpone apt_init until we enter the main loop, so we get a GUI
    // progress bar.
    Glib::signal_idle().connect(sigc::bind_return(sigc::ptr_fun(&do_apt_init),
						  false));

    refXml->get_widget_derived("main_window", pMainWindow);

    //This is the loop
    Gtk::Main::run(*pMainWindow);

    delete pMainWindow;
    delete pKit;
  }
}
