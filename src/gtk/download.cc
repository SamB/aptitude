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

#include <generic/util/util.h>

#include <gui.h>
#include <apt-pkg/strutl.h>
#include <apt-pkg/acquire-worker.h>


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
    row[tab->download_columns.ProgressPerc] = progress;
    row[tab->download_columns.Status] = status;
  }

  void guiPkgAcquireStatus::maybe_new_item(pkgAcquire::ItemDesc &Itm)
  {
    if (item_map.find(Itm.URI) == item_map.end())
    {
      Gtk::TreeModel::iterator store_iter = tab->download_store->append();
      Gtk::TreeModel::Row row = *store_iter;
      item_map.insert(std::make_pair(Itm.URI, store_iter));
      row[tab->download_columns.URI] = Itm.URI;
      row[tab->download_columns.ShortDesc] = Itm.ShortDesc;
      row[tab->download_columns.Description] = Itm.Description;
      tab->get_treeview()->scroll_to_row(tab->get_download_store()->get_path(store_iter));
    }
  }

  guiPkgAcquireStatus::guiPkgAcquireStatus(DownloadTab * tab)
  {
    this->tab = tab;
  }

  bool guiPkgAcquireStatus::Pulse(pkgAcquire *Owner)
  {
    pkgAcquireStatus::Pulse(Owner);
    if (TotalItems != 0)
      pMainWindow->get_progress_bar()->set_fraction(((float)(CurrentBytes+CurrentItems))/((float)(TotalBytes+TotalItems)));
    pMainWindow->get_progress_bar()->set_text(ssprintf("%lu of %lu done",
        CurrentItems, TotalItems)+" ("+SizeToStr(CurrentBytes)+" of "+SizeToStr(TotalBytes)
        +" at "+SizeToStr(CurrentCPS)+"/s, "+TimeToStr(((TotalBytes - CurrentBytes)/CurrentCPS))+" left)");
    update_workers(Owner);
    gtk_update();
    return !want_to_quit;
  }

  bool guiPkgAcquireStatus::MediaChange(string media, string drive)
  {
    const Glib::ustring msg = _("Change media");
    Gtk::Dialog dialog(msg, *pMainWindow, true, true);
    Gtk::Label label(ssprintf(_("Please insert the disc labeled \"%s\" into the drive \"%s\""),
        media.c_str(), drive.c_str()));
    dialog.get_vbox()->pack_end(label, true, true, 0);
    dialog.add_button(_("Continue"), Gtk::RESPONSE_OK);
    dialog.add_button(_("Abort"), Gtk::RESPONSE_CANCEL);
    return dialog.run() == Gtk::RESPONSE_OK;
  }

  void guiPkgAcquireStatus::Fetch(pkgAcquire::ItemDesc &Itm)
  {
    maybe_new_item(Itm);
    update_item(Itm.URI, 0, _("Fetch"));
  }

  void guiPkgAcquireStatus::Done(pkgAcquire::ItemDesc &Itm)
  {
    update_item(Itm.URI, 100, _("Done"));
  }

  void guiPkgAcquireStatus::Stop()
  {
    //pMainWindow->get_progress_bar()->set_text("Download done");
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
      Gtk::TreeModelColumn<ColumnType>& model_column,
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

  DownloadTab::DownloadTab(const Glib::ustring &label)
    : Tab(Download, label,
          Gnome::Glade::Xml::create(glade_main_file, "main_download_scrolledwindow"),
          "main_download_scrolledwindow")
  {
    get_xml()->get_widget("main_download_treeview", treeview);
    createstore();

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

  void DownloadTab::createstore()
  {
    download_store = Gtk::ListStore::create(download_columns);
    treeview->set_model(download_store);
  }

}
