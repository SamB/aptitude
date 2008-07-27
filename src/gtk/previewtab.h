// -*-c++-*-

// previewtab.h
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

#ifndef PREVIEWTAB_H_
#define PREVIEWTAB_H_

#undef OK
#include <gtkmm.h>

#include <apt-pkg/pkgcache.h>

#include <gtk/tab.h>

namespace gui
{
  class PackagesView;

  // TODO: This needs to share more code with PackagesTab.
  //       A PreviewTab is really a PackagesTab with a TreeStore.
  class PreviewTab : public Tab
  {
    private:
      PackagesView * pPackagesView;
      Gtk::TextView * pPackagesTextView;
      Gtk::Entry * pLimitEntry;
      Gtk::Button * pLimitButton;
    public:
      PreviewTab(const Glib::ustring &label);
      Glib::RefPtr<Gtk::TreeStore> create_store();
      std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator> * create_reverse_store();
      void activated_package_handler();
      void repopulate_model();
      void display_desc(pkgCache::PkgIterator pkg, pkgCache::VerIterator ver);
      PackagesView * get_packages_view() { return pPackagesView; };
  };

}

#endif /* PREVIEWTAB_H_ */
