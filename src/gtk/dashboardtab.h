// dashboardtab.h            -*-c++-*-
//
//   Copyright (C) 2008 Daniel Burrows
//
//   This program is free software; you can redistribute it and/or
//   modify it under the terms of the GNU General Public License as
//   published by the Free Software Foundation; either version 2 of
//   the License, or (at your option) any later version.
//
//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//   General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with this program; see the file COPYING.  If not, write to
//   the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//   Boston, MA 02111-1307, USA.

#ifndef DASHBOARD_TAB_H
#define DASHBOARD_TAB_H

#include <cwidget/generic/util/ref_ptr.h>

#include <gtkmm.h>

#include "tab.h"

/** \file dashboardtab.h */

namespace gui
{
  class PkgView;

  /** \brief The main dashboard tab. */
  class DashboardTab : public Tab
  {
    cwidget::util::ref_ptr<PkgView> upgrades_pkg_view;
    Gtk::TextView *upgrades_changelog_view;
    Gtk::TextView *upgrades_summary_textview;

    Gtk::Entry *search_entry;
    Gtk::Button *search_button;

    Gtk::Button *upgrade_button;

    Gtk::Label *available_upgrades_label;

    void do_search();

    void do_upgrade();

    // Download all the changelogs and show the new entries.
    void create_upgrade_summary();

    void handle_cache_closed();

    void handle_upgrades_store_reloaded();

    void activated_upgrade_package_handler();

  public:
    DashboardTab(Glib::ustring label);
  };
}



#endif // DASHBOARD_TAB_H

