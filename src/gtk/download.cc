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


namespace gui
{

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
    gtk_update();
    return !want_to_quit;
  }

  bool guiPkgAcquireStatus::MediaChange(std::string, std::string)
  {
    return false;
  }

  void guiPkgAcquireStatus::Fetch(pkgAcquire::ItemDesc &Itm)
  {
    pMainWindow->get_status_bar()->pop(0);
    pMainWindow->get_status_bar()->push(Itm.Description, 0);

    Gtk::TreeModel::iterator iter = tab->download_store->append();
    Gtk::TreeModel::Row row = *iter;
    row[tab->download_columns.URI] = Itm.URI;
    row[tab->download_columns.ProgressPerc] = 42;
    row[tab->download_columns.ShortDesc] = Itm.ShortDesc;
    row[tab->download_columns.Description] = Itm.Description;
    tab->get_treeview()->scroll_to_row(tab->get_download_store()->get_path(iter));
    gtk_update();
  }

  void guiPkgAcquireStatus::Stop()
  {
    pMainWindow->get_progress_bar()->set_text("Download done");
  }

  DownloadColumns::DownloadColumns()
  {
    add(URI);
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

    append_column(Glib::ustring(_("URI")), URI, download_columns.URI, 350);

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
