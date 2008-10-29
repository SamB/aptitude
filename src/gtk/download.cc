// download.cc
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

#include "download.h"
#include "aptitude.h"

#undef OK
#include <gtkmm.h>

#include <generic/apt/apt.h>
#include <generic/apt/config_signal.h>
#include <generic/util/util.h>

#include <gui.h>
#include <apt-pkg/strutl.h>
#include <apt-pkg/acquire-item.h>
#include <apt-pkg/acquire-worker.h>

#include "notify.h"

namespace cw = cwidget;

namespace gui
{
  void download_list_model::update_workers(pkgAcquire *Owner)
  {
    pkgAcquire::Worker *serf = Owner->WorkersBegin();
    while (serf)
    {
      if (serf->CurrentItem)
      {
        pkgAcquire::ItemDesc * Item = serf->CurrentItem;
        maybe_new_item(*Item);
        // TODO: We should use convertPercent from progress.cc
        if (serf->TotalSize != 0)
          update_item(serf->CurrentItem->URI, 100 * serf->CurrentSize / serf->TotalSize, serf->Status);
        else
          update_item(serf->CurrentItem->URI, 0, serf->Status);
      }
      serf=Owner->WorkerStep(serf);
    }
  }

  void download_list_model::update_item(string URI, int progress, string status)
  {
    std::map<string, Gtk::TreeModel::iterator>::iterator item_iter = item_map.find(URI);
    if (item_iter == item_map.end())
      std::cout << "oops!"  << URI << std::endl;
    Gtk::TreeModel::Row row = *(item_iter->second);
    row[download_columns.ProgressPerc] = progress;
    row[download_columns.Status] = status;
  }

  void download_list_model::maybe_new_item(pkgAcquire::ItemDesc &Itm)
  {
    if (item_map.find(Itm.URI) == item_map.end())
    {
      Gtk::TreeModel::iterator store_iter = download_store->append();
      Gtk::TreeModel::Row row = *store_iter;
      item_map.insert(std::make_pair(Itm.URI, store_iter));
      row[download_columns.URI] = Itm.URI;
      row[download_columns.ShortDesc] = Itm.ShortDesc;
      row[download_columns.Description] = Itm.Description;
    }
  }

  download_list_model::download_list_model()
    : download_columns(),
      download_store(Gtk::ListStore::create(download_columns)),
      failed(false)
  {
  }

  void download_list_model::connect(download_signal_log *log)
  {
    log->IMSHit_sig.connect(sigc::mem_fun(*this, &download_list_model::IMSHit));
    log->Fetch_sig.connect(sigc::mem_fun(*this, &download_list_model::Fetch));
    log->Done_sig.connect(sigc::mem_fun(*this, &download_list_model::Done));
    log->Fail_sig.connect(sigc::mem_fun(*this, &download_list_model::Fail));
    log->Pulse_sig.connect(sigc::mem_fun(*this, &download_list_model::Pulse));
    log->Start_sig.connect(sigc::mem_fun(*this, &download_list_model::Start));
  }

  void download_list_model::Start(download_signal_log &manager)
  {
    Gtk::TreeModel::iterator store_iter = download_store->append();
    Gtk::TreeModel::Row row = *store_iter;
    row[download_columns.URI] = "";
    row[download_columns.ShortDesc] = "";
    row[download_columns.Description] = "Download started";
  }

  void download_list_model::Pulse(pkgAcquire *Owner,
				  download_signal_log &manager,
				  const sigc::slot1<void, bool> &k)
  {
    update_workers(Owner);
  }

  void download_list_model::Fail(pkgAcquire::ItemDesc &Itm,
				 download_signal_log &log)
  {
    switch(Itm.Owner->Status)
      {
      case pkgAcquire::Item::StatIdle:
	break;

      case pkgAcquire::Item::StatDone:
	maybe_new_item(Itm);
	update_item(Itm.URI, 100, _("Ignored"));
	break;

      default:
	// \todo Display an error icon and shade the row.
	failed = true;
	maybe_new_item(Itm);
	update_item(Itm.URI, 100, _("Failed"));
	break;
      }
  }

  void download_list_model::IMSHit(pkgAcquire::ItemDesc &Itm,
				   download_signal_log &manager)
  {
    maybe_new_item(Itm);
    update_item(Itm.URI, 100, _("Already downloaded"));
  }

  void download_list_model::Fetch(pkgAcquire::ItemDesc &Itm,
				  download_signal_log &manager)
  {
    maybe_new_item(Itm);
    update_item(Itm.URI, 100, _("Fetch"));
  }

  void download_list_model::Done(pkgAcquire::ItemDesc &Itm,
				 download_signal_log &manager)
  {
    update_item(Itm.URI, 100, _("Done"));
  }

  DownloadColumns::DownloadColumns()
  {
    add(URI);
    add(Status);
    add(ProgressPerc);
    add(ShortDesc);
    add(Description);
  }

  template <class ColumnType>
  int DownloadTab::append_column(Glib::ustring title,
      Gtk::TreeViewColumn * treeview_column,
      const Gtk::TreeModelColumn<ColumnType>& model_column,
      int size)
  {
    treeview_column = manage(new Gtk::TreeViewColumn(title, model_column));
    treeview_column->set_sizing(Gtk::TREE_VIEW_COLUMN_FIXED);
    treeview_column->set_fixed_width(size);
    treeview_column->set_resizable(true);
    treeview_column->set_reorderable(true);
    treeview_column->set_sort_column(model_column);
    return treeview->append_column(*treeview_column);
  }

  DownloadTab::DownloadTab(const Glib::ustring &label,
			   const cw::util::ref_ptr<download_list_model> &status)
    : Tab(Download, label,
          Gnome::Glade::Xml::create(glade_main_file, "main_download_scrolledwindow"),
          "main_download_scrolledwindow")
  {
    get_xml()->get_widget("main_download_treeview", treeview);

    const Glib::RefPtr<Gtk::TreeModel> download_store = status->get_model();
    const DownloadColumns &download_columns = status->get_columns();

    treeview->set_model(download_store);
    void (Gtk::TreeView::*scroll_to_row)(const Gtk::TreeModel::Path &)
      = &Gtk::TreeView::scroll_to_row;
    download_store->signal_row_inserted().connect(sigc::hide(sigc::mem_fun(*treeview, scroll_to_row)));

    append_column(Glib::ustring(_("URI")), URI, download_columns.URI, 250);
    append_column(Glib::ustring(_("Status")), Status, download_columns.Status, 100);

    {
      // Custom renderer to show a percentage progress bar
      Gtk::CellRendererProgress* progress_renderer = Gtk::manage(new Gtk::CellRendererProgress);
      ProgressPerc = manage(new Gtk::TreeViewColumn(_("Progress"), *progress_renderer));
      ProgressPerc->set_sizing(Gtk::TREE_VIEW_COLUMN_FIXED);
      ProgressPerc->set_fixed_width(100);
      ProgressPerc->add_attribute(progress_renderer->property_value(), download_columns.ProgressPerc);
      treeview->append_column(*ProgressPerc);
    }

    append_column(Glib::ustring(_("Short Description")), ShortDesc, download_columns.ShortDesc, 100);
    append_column(Glib::ustring(_("Description")), Description, download_columns.Description, 200);

    get_widget()->show();
  }

  namespace
  {
    // This is the main class that's responsible for managing a GUI
    // download.
    class DownloadNotification : public Notification
    {
      cw::util::ref_ptr<download_list_model> status;
      DownloadTab *tab;
      Gtk::ProgressBar *progress;
      const std::string title;
      // True if we should use a pulse-style progress bar instead of a
      // percent-based one.
      bool pulse;
      bool cancelled;
      bool success;

      void tab_destroyed()
      {
	tab = NULL;
      }

      void cancel()
      {
	cancelled = true;
      }

      void view_details()
      {
	if(tab != NULL)
	  tab->get_widget()->show();
	else
	  {
	    tab = new DownloadTab(title, status);
	    tab->closed.connect(sigc::mem_fun(*this, &DownloadNotification::tab_destroyed));
	    tab_add(tab);
	  }
      }

      void set_progress_text(const std::string &text)
      {
	std::string progress_text;

	if(text.empty())
	  progress_text = title;
	else
	  progress_text = title + ": " + text;

	progress->set_text(progress_text);
	progress->set_tooltip_text(progress_text);
      }

      static void finishMediaChange(int response_id,
				    sigc::slot1<void, bool> k)
      {
	k(response_id == Gtk::RESPONSE_OK);
      }

      void do_pulse()
      {
	progress->pulse();
      }

    public:
      DownloadNotification(const std::string &_title,
			   bool _pulse,
			   const cw::util::ref_ptr<download_list_model> &_status)
	: Notification(true),
	  status(_status),
	  tab(NULL),
	  progress(new Gtk::ProgressBar),
	  title(_title),
	  pulse(_pulse),
	  cancelled(false),
	  success(false)
      {
	progress->set_text(title);
	progress->set_pulse_step(0.1);
	progress->set_ellipsize(Pango::ELLIPSIZE_END);
	progress->show();
	prepend_widget(progress);

	Gtk::Button *view_details_button = new Gtk::Button(_("View Details"));
	view_details_button->signal_clicked().connect(sigc::mem_fun(*this, &DownloadNotification::view_details));
	view_details_button->show();
	add_button(view_details_button);

	close_clicked.connect(sigc::mem_fun(*this, &DownloadNotification::cancel));

	if(pulse)
	  Glib::signal_timeout().connect(sigc::bind_return(sigc::mem_fun(*this, &DownloadNotification::do_pulse),
							   true),
					 100);

	finalize();
	show();
      }

      void connect(download_signal_log *log)
      {
	log->MediaChange_sig.connect(sigc::mem_fun(*this,
						   &DownloadNotification::MediaChange));

	log->Pulse_sig.connect(sigc::mem_fun(*this,
					     &DownloadNotification::Pulse));

	log->Stop_sig.connect(sigc::mem_fun(*this,
					    &DownloadNotification::Stop));
      }

      void Fail(pkgAcquire::ItemDesc &Itm,
		download_signal_log &log)
      {
	success = false;
      }

      void MediaChange(string media, string drive,
		       download_signal_log &manager,
		       const sigc::slot1<void, bool> &k)
      {
	const Glib::ustring msg = _("Change media");
	Gtk::Dialog dialog(msg, *pMainWindow, true, true);
	Gtk::Label label(ssprintf(_("Please insert the disc labeled \"%s\" into the drive \"%s\""),
				  media.c_str(), drive.c_str()));
	dialog.get_vbox()->pack_end(label, true, true, 0);
	dialog.add_button(_("Continue"), Gtk::RESPONSE_OK);
	dialog.add_button(_("Abort"), Gtk::RESPONSE_CANCEL);
	dialog.signal_response().connect(sigc::bind(sigc::ptr_fun(&finishMediaChange), k));
	dialog.show();
      }

      void Pulse(pkgAcquire *Owner,
		 download_signal_log &manager,
		 const sigc::slot1<void, bool> &k)
      {
	if(pulse)
	  {
	    std::string rate_string;
	    if(manager.get_current_bytes() > 0 || manager.get_current_items() > 0)
	      rate_string = ssprintf(_("%sB/s"),
				     SizeToStr(manager.get_currentCPS()).c_str());

	    progress->set_text(rate_string);
	  }
	else
	  {
	    double fraction = 0;
	    if (manager.get_total_items() != 0)
	      fraction = ((double)(manager.get_current_bytes() + manager.get_current_items())) / ((double)(manager.get_total_bytes() + manager.get_total_items()));

	    progress->set_fraction(fraction);

	    std::string progress_string;
	    if(manager.get_currentCPS() > 0)
	      progress_string = ssprintf(_("%sB of %sB at %sB/s, %s remaining"),
					 SizeToStr(manager.get_current_bytes()).c_str(),
					 SizeToStr(manager.get_total_bytes()).c_str(),
					 SizeToStr(manager.get_currentCPS()).c_str(),
					 TimeToStr(((manager.get_total_bytes() - manager.get_current_bytes()) / manager.get_currentCPS())).c_str());
	    else if(manager.get_current_bytes() > 0 || manager.get_current_items() > 0)
	      progress_string = ssprintf(_("%sB of %sB, stalled"),
					 SizeToStr(manager.get_current_bytes()).c_str(),
					 SizeToStr(manager.get_total_bytes()).c_str());

	    progress->set_text(progress_string);
	  }

	k(!cancelled);
      }

      void Stop(download_signal_log &manager, const sigc::slot0<void> &k)
      {
	// \todo Maybe use some other condition here?
	if(!success)
	  {
	    if(!success)
	      // \todo The "error" color is copied around; it should
	      // be a constant somewhere.
	      set_color(Gdk::Color("#FFE0E0"));

	    progress->hide();
	    Glib::RefPtr<Gtk::TextBuffer> buffer(Gtk::TextBuffer::create());

	    if(success)
	      buffer->set_text(title + ": " + _("Completed"));
	    else
	      buffer->set_text(title + ": " + _("Completed with errors"));
	    set_buffer(buffer);
	  }

	k();
      }
    };
  }

  Notification *make_download_notification(const std::string &title,
					   bool pulse,
					   const cw::util::ref_ptr<download_list_model> &status,
					   download_signal_log *log)
  {
    DownloadNotification *rval = new DownloadNotification(title, pulse, status);
    rval->connect(log);
    return rval;
  }
}
