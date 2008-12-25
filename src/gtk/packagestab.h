// -*-c++-*-

// packagestab.h
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

#ifndef PACKAGESTAB_H_
#define PACKAGESTAB_H_

#undef OK
#include <gtkmm.h>

#include <apt-pkg/pkgcache.h>

#include <gtk/tab.h>

#include <cwidget/generic/util/ref_ptr.h>

namespace gui
{
  class PkgView;
  class Entity;

  class PackagesTab : public Tab
  {
    private:
      cwidget::util::ref_ptr<PkgView> pPkgView;
      Gtk::TextView * pPackagesTextView;
      Gtk::Entry * pLimitEntry;
      Gtk::Label * pLimitErrorLabel;
      Gtk::Button * pLimitButton;
      Gtk::ComboBox * pLimitComboBox;

      void after_repopulate_model();
    public:
      PackagesTab(const Glib::ustring &label);
      void activated_package_handler();
      void display_desc(const cwidget::util::ref_ptr<Entity> &ent);
      Gtk::Entry * get_limit_entry() const { return pLimitEntry; }
      const cwidget::util::ref_ptr<PkgView> &get_pkg_view() const { return pPkgView; }

      std::set<PackagesAction> get_package_menu_actions();
      void dispatch_package_menu_action(PackagesAction action);
      bool get_undo_available();
      void dispatch_undo();
      void set_limit(const std::string &limit);

      bool get_edit_columns_available();
      void dispatch_edit_columns();
  };

  /** \brief Set up a package view to be searchable.
   *
   *  Sets up signal connections so that the user can enter search
   *  terms into a search entry to control the list of packages
   *  displayed in a package view.
   *
   *  \param search_entry   The text entry where the user enters search terms.
   *  \param search_error_label   A label that will be used to display error messages.
   *  \param search_button  The button the user presses to immediately search.
   *  \param limit_combo_box  A combo-box used to select an auxiliary limit (e.g.,
   *                          "Installed packages").
   *  \param packages_view  The package list to manage; will initially be
   *                        set to only contain a message asking the
   *                        user to enter a search term.
   *  \param after_repopulate_hook   A callback invoked after the tree is rebuilt
   *                                 when the user enters a new search term.
   */
  void setup_searchable_view(Gtk::Entry *search_entry,
			     Gtk::Label *search_error_label,
			     Gtk::Button *search_button,
			     Gtk::ComboBox *limit_combo_box,
			     const cwidget::util::ref_ptr<PkgView> packages_view,
			     const sigc::slot0<void> &after_repopulate_hook);

}

#endif /* PACKAGESTAB_H_ */
