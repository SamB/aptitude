// dashboardtab.h            -*-c++-*-
//
//   Copyright (C) 2008-2009 Obey Arthur Liu
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

namespace aptitude
{
  namespace matching
  {
    class pattern;
  };
}

#include <generic/apt/aptitude_resolver_universe.h>
#include <generic/problemresolver/solution.h>
class resolver_manager;

namespace gui
{
  class PkgView;
  class PackageSearchEntry;

  /** \brief The main dashboard tab. */
  class DashboardTab : public Tab
  {
    // The internal resolver that we use to calculate an upgrade
    // solution.  By convention, this is set to NULL if there's no
    // upgrade to calculate.  Note that although this handles
    // restarting the resolver when a package's state changes, we must
    // actually discard and recreate it ourselves as well: e.g., if a
    // "hold" state changes, that can affect the upgrade calculation.
    resolver_manager *upgrade_resolver;
    // The internal resolver solution that we use to calculate an
    // upgrade solution.  If this is invalid, the solution hasn't been
    // calculated yet.
    generic_solution<aptitude_universe> upgrade_solution;
    // The timeout connection that's used to pulse the progress bar
    // while we're calculating the upgrade.
    sigc::connection pulse_progress_connection;

    // An internal object that serves as the connection point for
    // slots from the background thread.  The only reason for this to
    // exist is so that we can easily disconnect all those slots at
    // once (by deleting it) and avoid "stale" events from a previous
    // resolver run.
    class redirect_from_background
    {
      DashboardTab &parent;
    public:
      redirect_from_background(DashboardTab &_parent)
	: parent(_parent)
      {
      }

      void success(generic_solution<aptitude_universe> sol)
      {
	parent.upgrade_resolver_success(sol);
      }

      void no_more_solutions()
      {
	parent.upgrade_resolver_no_more_solutions();
      }

      void aborted(std::string errmsg)
      {
	parent.upgrade_resolver_aborted(errmsg);
      }
    };

    redirect_from_background *background_upgrade_redirect;


    cwidget::util::ref_ptr<PkgView> upgrades_pkg_view;
    Gtk::TextView *upgrades_changelog_view;
    Gtk::TextView *upgrades_summary_textview;

    cwidget::util::ref_ptr<PackageSearchEntry> package_search_entry;

    Gtk::ProgressBar *upgrade_resolver_progress;
    Gtk::Label *upgrade_resolver_label;
    Gtk::Button *upgrade_button;

    Gtk::Label *available_upgrades_label;

    void do_search();

    void do_upgrade();

    // Download all the changelogs and show the new entries.
    void create_upgrade_summary();

    void handle_cache_closed();
    void handle_cache_reloaded();

    void handle_upgrades_store_reloaded();

    void activated_upgrade_package_handler();

    bool pulse_progress_timeout();


    // NB: the resolver callbacks pass their argument by value to
    // avoid any possible confusion about whether they could refer to
    // a destroyed object.

    /** \brief Invoked in the foreground when the background thread
     *  computes an upgrade solution.
     *
     *  \param sol   The solution that was calculated, or an invalid
     *               solution to indicate that nothing needs to be
     *               calculated.
     *
     *  Hides the progress bar, shows the label, and activates the
     *  button; changes the label text to suggest to the user that
     *  they can press the button to install the upgrades that are
     *  available.
     */
    void upgrade_resolver_success(generic_solution<aptitude_universe> sol);

    /** \brief Invoked in the foreground when the background thread
     *  can't find an upgrade solution.
     *
     *  Hides the progress bar, shows the label, deactivates the
     *  button, and informs the user that it was unable to find an
     *  upgrade solution.
     */
    void upgrade_resolver_no_more_solutions();

    /** \brief Invoked in the foreground when the background thread
     *  is aborted (by an exception) while searching for an upgrade solution.
     */
    void upgrade_resolver_aborted(std::string errmsg);



    class upgrade_continuation;
    /** \brief Create the internal resolver if it doesn't exist and
     *  start its calculation.
     *
     *  Shows the progress bar and hides the label; makes the button
     *  inactive and kicks off a timer to pulse the progress bar.
     */
    void make_resolver();
    /** \brief Throw away the internal resolver and solution, and hide
     *  the progress bar and label.
     */
    void discard_resolver();

  public:
    DashboardTab(Glib::ustring label);
    ~DashboardTab();

    bool get_edit_columns_available();
    void dispatch_edit_columns();
  };
}



#endif // DASHBOARD_TAB_H

