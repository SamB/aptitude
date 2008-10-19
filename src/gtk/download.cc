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
  void guiPkgAcquireStatus::update_workers(pkgAcquire *Owner)
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

  void guiPkgAcquireStatus::update_item(string URI, int progress, string status)
  {
    std::map<string, Gtk::TreeModel::iterator>::iterator item_iter = item_map.find(URI);
    if (item_iter == item_map.end())
      std::cout << "oops!"  << URI << std::endl;
    Gtk::TreeModel::Row row = *(item_iter->second);
    row[download_columns.ProgressPerc] = progress;
    row[download_columns.Status] = status;
  }

  void guiPkgAcquireStatus::maybe_new_item(pkgAcquire::ItemDesc &Itm)
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

  guiPkgAcquireStatus::guiPkgAcquireStatus()
    : download_columns(),
      download_store(Gtk::ListStore::create(download_columns)),
      cancelled(false),
      failed(false)
  {
  }

  bool guiPkgAcquireStatus::Pulse(pkgAcquire *Owner)
  {
    if(cancelled)
      return false;

    pkgAcquireStatus::Pulse(Owner);
    double fraction = 0;
    if (TotalItems != 0)
      fraction = ((double)(CurrentBytes+CurrentItems))/((double)(TotalBytes+TotalItems));

    std::string rate_string;
    if(CurrentBytes > 0 || CurrentItems > 0)
      rate_string = ssprintf(_("%sB/s"),
			     SizeToStr(CurrentCPS).c_str());

    std::string progress_string;
    if(CurrentCPS > 0)
      progress_string = ssprintf(_("%sB of %sB at %sB/s, %s remaining"),
				 SizeToStr(CurrentBytes).c_str(),
				 SizeToStr(TotalBytes).c_str(),
				 SizeToStr(CurrentCPS).c_str(),
				 TimeToStr(((TotalBytes - CurrentBytes)/CurrentCPS)).c_str());
    else if(CurrentBytes > 0 || CurrentItems > 0)
      progress_string = ssprintf(_("%sB of %sB, stalled"),
				 SizeToStr(CurrentBytes).c_str(),
				 SizeToStr(TotalBytes).c_str());

    progress(fraction);
    progress_rate(rate_string);
    progress_rate_and_estimate(progress_string);

    update_workers(Owner);
    gtk_update();
    return !want_to_quit;
  }

  bool guiPkgAcquireStatus::MediaChange(string media, string drive)
  {
    if(cancelled)
      return false;

    const Glib::ustring msg = _("Change media");
    Gtk::Dialog dialog(msg, *pMainWindow, true, true);
    Gtk::Label label(ssprintf(_("Please insert the disc labeled \"%s\" into the drive \"%s\""),
        media.c_str(), drive.c_str()));
    dialog.get_vbox()->pack_end(label, true, true, 0);
    dialog.add_button(_("Continue"), Gtk::RESPONSE_OK);
    dialog.add_button(_("Abort"), Gtk::RESPONSE_CANCEL);
    return dialog.run() == Gtk::RESPONSE_OK;
  }

  void guiPkgAcquireStatus::Fail(pkgAcquire::ItemDesc &Itm)
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

  void guiPkgAcquireStatus::Fetch(pkgAcquire::ItemDesc &Itm)
  {
    maybe_new_item(Itm);
    update_item(Itm.URI, 100, _("Fetch"));
  }

  void guiPkgAcquireStatus::Done(pkgAcquire::ItemDesc &Itm)
  {
    update_item(Itm.URI, 100, _("Done"));
  }

  void guiPkgAcquireStatus::Stop()
  {
    done(!failed);
    //pMainWindow->get_progress_bar()->set_text("Download done");
  }

  void guiPkgAcquireStatus::cancel()
  {
    cancelled = true;
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
			   const cw::util::ref_ptr<guiPkgAcquireStatus> &status)
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
    class DownloadNotification : public Notification
    {
      cw::util::ref_ptr<guiPkgAcquireStatus> status;
      DownloadTab *tab;
      Gtk::ProgressBar *progress;
      const std::string title;

      void tab_destroyed()
      {
	tab = NULL;
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

      void handle_done(bool success)
      {
	string s = aptcfg->Find(PACKAGE "::UI::Pause-After-Download", "OnlyIfError");

	bool keep_summary = false;

	if(s == "OnlyIfError")
	  keep_summary = !success;
	else if(StringToBool(s, 0))
	  keep_summary = true;

	if(keep_summary)
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
	else
	  close_clicked();
      }

    public:
      DownloadNotification(const std::string &_title,
			   bool pulse,
			   const cw::util::ref_ptr<guiPkgAcquireStatus> &_status)
	: Notification(true),
	  status(_status),
	  tab(NULL),
	  progress(new Gtk::ProgressBar),
	  title(_title)
      {
	progress->set_text(title);
	// It seems like we pulse about 100 times a second; this is
	// probably too fast!
	progress->set_pulse_step(0.02);
	progress->set_ellipsize(Pango::ELLIPSIZE_END);
	if(pulse)
	  {
	    status->progress.connect(sigc::hide(sigc::mem_fun(*progress,
							      &Gtk::ProgressBar::pulse)));
	    status->progress_rate.connect(sigc::mem_fun(*this, &DownloadNotification::set_progress_text));
	  }
	else
	  {
	    status->progress.connect(sigc::mem_fun(*progress,
						   &Gtk::ProgressBar::set_fraction));
	    status->progress_rate_and_estimate.connect(sigc::mem_fun(*this, &DownloadNotification::set_progress_text));
	  }
	progress->show();
	prepend_widget(progress);

	Gtk::Button *view_details_button = new Gtk::Button(_("View Details"));
	view_details_button->signal_clicked().connect(sigc::mem_fun(*this, &DownloadNotification::view_details));
	view_details_button->show();
	add_button(view_details_button);

	close_clicked.connect(sigc::mem_fun(status.unsafe_get_ref(), &guiPkgAcquireStatus::cancel));

	status->done.connect(sigc::mem_fun(*this, &DownloadNotification::handle_done));

	finalize();
	show();
      }
    };
  }

  Notification *make_download_notification(const std::string &title,
					   bool pulse,
					   const cw::util::ref_ptr<guiPkgAcquireStatus> &status)
  {
    return new DownloadNotification(title, pulse, status);
  }
}
