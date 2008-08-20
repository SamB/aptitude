// -*-c++-*-

// download.h
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

#ifndef DOWNLOAD_H_
#define DOWNLOAD_H_

#undef OK
#include <gtkmm.h>

#include <map>

#include <apt-pkg/acquire.h>

#include <gtk/tab.h>
#include <generic/apt/download_signal_log.h>

namespace gui
{
  class DownloadTab;

  class guiPkgAcquireStatus : public pkgAcquireStatus
  { // must also derive to read protected members..
    private:
      DownloadTab * tab;

      std::map<string, Gtk::TreeModel::iterator> item_map;
      void update_workers(pkgAcquire *Owner);
      void update_item(string URI, int progress, string status);
      void maybe_new_item(pkgAcquire::ItemDesc &Itm);

    public:
      guiPkgAcquireStatus(DownloadTab * tab);
      bool Pulse(pkgAcquire *Owner);
      bool MediaChange(std::string, std::string);
      void Fetch(pkgAcquire::ItemDesc &Itm);
      void Done(pkgAcquire::ItemDesc &Itm);
      void Stop();
  };

  class DownloadColumns : public Gtk::TreeModel::ColumnRecord
  {
    public:
      Gtk::TreeModelColumn<Glib::ustring> URI;
      Gtk::TreeModelColumn<Glib::ustring> Status;
      Gtk::TreeModelColumn<int> ProgressPerc;
      Gtk::TreeModelColumn<Glib::ustring> Description;
      Gtk::TreeModelColumn<Glib::ustring> ShortDesc;

      DownloadColumns();
  };

  class DownloadTab : public Tab
  {
    private:
      Gtk::TreeViewColumn * URI;
      Gtk::TreeViewColumn * Status;
      Gtk::TreeViewColumn * ProgressPerc;
      Gtk::TreeViewColumn * ShortDesc;
      Gtk::TreeViewColumn * Description;
      template <class ColumnType>
      int append_column(Glib::ustring title,
          Gtk::TreeViewColumn * treeview_column,
          Gtk::TreeModelColumn<ColumnType>& model_column,
          int size);
    public:
      Glib::RefPtr<Gtk::ListStore> download_store;
      DownloadColumns download_columns;
      Gtk::TreeView * treeview;

      DownloadTab(const Glib::ustring &label);
      void createstore();

      Glib::RefPtr<Gtk::ListStore> get_download_store() { return download_store; };
      Gtk::TreeView * get_treeview() { return treeview; };
  };

}

#endif /* DOWNLOAD_H_ */
