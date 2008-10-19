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
#include <generic/util/refcounted_base.h>

#include <cwidget/generic/util/ref_ptr.h>

namespace gui
{
  class DownloadTab;

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

  /** \brief Uses a GTK+ tree model to display the status of an
   *  Acquire download.
   *
   *  This is a heap-allocated, reference-counted structure so that
   *  situations where the status object doesn't belong to any
   *  particular scope can be handled cleanly and safely.
   */
  class guiPkgAcquireStatus : public pkgAcquireStatus,
			      public aptitude::util::refcounted_base
  {
    private:
      DownloadColumns download_columns;
      Glib::RefPtr<Gtk::ListStore> download_store;
      bool cancelled;
      bool failed; // True if an item failed to download.

      std::map<string, Gtk::TreeModel::iterator> item_map;
      void update_workers(pkgAcquire *Owner);
      void update_item(string URI, int progress, string status);
      void maybe_new_item(pkgAcquire::ItemDesc &Itm);

      // Noncopyable.
      guiPkgAcquireStatus(const guiPkgAcquireStatus &);

      guiPkgAcquireStatus();

    public:
      static cwidget::util::ref_ptr<guiPkgAcquireStatus> create()
      {
	return new guiPkgAcquireStatus;
      }

      bool Pulse(pkgAcquire *Owner);
      bool MediaChange(string media, string drive);
      void Fail(pkgAcquire::ItemDesc &Itm);
      void Fetch(pkgAcquire::ItemDesc &Itm);
      void Done(pkgAcquire::ItemDesc &Itm);
      void Stop();

      const DownloadColumns &get_columns() const { return download_columns; }
      Glib::RefPtr<Gtk::TreeModel> get_model() const { return download_store; }
      /** \brief Cancel the download by returning "false" from the
       *  next Pulse or MediaChange.
       */
      void cancel();

      /** \brief Emitted when the download completes.
       *
       *  The parameter is "true" if the download succeeded and
       *  "false" if something failed to download.
       */
      sigc::signal1<void, bool> done;

      /** \brief Signal emitted when any overall progress bar should
       *  be updated.
       */
      sigc::signal1<void, double> progress;

      /** \brief Signal emitted when the progress bar text should
       *  change to indicate the current download rate.
       */
      sigc::signal1<void, std::string> progress_rate;

      /** \brief Signal emitted when the progress bar text should change
       *  to indicate the current download rate and an estimate of the
       *  time remaining.
       */
      sigc::signal1<void, std::string> progress_rate_and_estimate;
  };

  class DownloadTab : public Tab
  {
    private:
      Gtk::TreeViewColumn * URI;
      Gtk::TreeViewColumn * Status;
      Gtk::TreeViewColumn * ProgressPerc;
      Gtk::TreeViewColumn * ShortDesc;
      Gtk::TreeViewColumn * Description;

      Gtk::TreeView * treeview;

      template <class ColumnType>
      int append_column(Glib::ustring title,
          Gtk::TreeViewColumn * treeview_column,
          const Gtk::TreeModelColumn<ColumnType>& model_column,
          int size);

    public:
      DownloadTab(const Glib::ustring &label,
		  const cwidget::util::ref_ptr<guiPkgAcquireStatus> &status);

      Gtk::TreeView * get_treeview() { return treeview; };
  };

  class Notification;

  /** \brief Create a notification object that displays the current
   *  download status.
   *
   *  The notification lets the user cancel the download, by clicking
   *  the "close" button, or view its status in a new tab.  When the
   *  download is complete, the notification is destroyed.
   *
   *  \param title                 A description of the download.
   *                               For instance, "Downloading packages"
   *
   *  \param pulse                 If true, a "pulsing" progress bar
   *                               will be used instead of one that
   *                               shows progress.  For instance,
   *                               when updating the package lists,
   *                               the progress percentage is not
   *                               meaningful, so we don't try to
   *                               use it.
   *
   *  \param guiPkgAcquireStatus   The GUI status object that
   *                               this notification will track.
   */
  Notification *make_download_notification(const std::string &title,
					   bool pulse,
					   const cwidget::util::ref_ptr<guiPkgAcquireStatus> &status);
}

#endif /* DOWNLOAD_H_ */
