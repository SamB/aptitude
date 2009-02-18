// -*-c++-*-

// resolver.h
//
//  Copyright 1999-2009 Daniel Burrows
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

#ifndef RESOLVER_H_
#define RESOLVER_H_

#undef OK
#include <gtkmm.h>
#include <libglademm/xml.h>

#include <generic/apt/apt.h>
#include <generic/apt/aptitude_resolver_universe.h>
#include <generic/apt/resolver_manager.h>
#include <generic/problemresolver/solution.h>
#include <generic/util/util.h>

#include <gtk/tab.h>

namespace gui
{

  class ResolverColumns : public Gtk::TreeModel::ColumnRecord
  {
    public:
      Gtk::TreeModelColumn<pkgCache::PkgIterator> PkgIterator;
      Gtk::TreeModelColumn<pkgCache::VerIterator> VerIterator;
      Gtk::TreeModelColumn<Glib::ustring> Name;
      Gtk::TreeModelColumn<Glib::ustring> Action;

      ResolverColumns();
  };

  class ResolverView : public Gtk::TreeView
  {
    private:
      Gtk::TreeViewColumn * Name;
      Gtk::TreeViewColumn * Section;
      template <class ColumnType>
      int append_column(Glib::ustring title,
          Gtk::TreeViewColumn * treeview_column,
          Gtk::TreeModelColumn<ColumnType>& model_column,
          int size);

    public:
      Glib::RefPtr<Gtk::TreeStore> resolver_store;
      ResolverColumns resolver_columns;

      ResolverView(BaseObjectType* cobject, const Glib::RefPtr<Gnome::Glade::Xml>& refGlade);
  };

  class ResolverTab : public Tab
  {
    private:
      typedef generic_solution<aptitude_universe> aptitude_solution;

    // The resolver manager that we're using.
    resolver_manager *resolver;

    // True if we're using an internal resolver; false otherwise.
    bool using_internal_resolver;

    resolver_manager *get_resolver() const
    {
      return using_internal_resolver ? resolver : resman;
    }

    sigc::connection resolver_state_changed_connection;

      ResolverView * pResolverView;
      Gtk::Label * pResolverStatus;
      Gtk::Button * pResolverPrevious;
      Gtk::Button * pResolverNext;
      Gtk::Button * pResolverApply;

    // The top container for the "fixing upgrade..." message.  Used to
    // show and hide it.
    Gtk::Widget * resolver_fixing_upgrade_message;

    Gtk::ProgressBar * resolver_fixing_upgrade_progress_bar;
    Gtk::Label * resolver_fixing_upgrade_label;

    Gtk::RadioButton * pButtonGroupByAction;
    Gtk::RadioButton * pButtonShowExplanation;

      // The last solution that was displayed, or invalid if there
      // was no last solution.
      //
      // The sole purpose of this member is to avoid destroying and
      // rebuilding the tree if the solution didn't actually change.
      aptitude_solution last_sol;

    /** \brief Create a new, empty tree store with the correct columns
     *  for the resolver view.
     */
    Glib::RefPtr<Gtk::TreeStore> createstore();
    /** \brief Create a new tree store and populate it with the given
     *  solution, rendered with actions collected by type.
     */
    Glib::RefPtr<Gtk::TreeStore> render_as_action_groups(const aptitude_solution &sol);

    /** \brief Create a new tree store and populate it with
     *  the given solution, rendered as a chronological explanation
     *  of each action.
     */
    Glib::RefPtr<Gtk::TreeStore> render_as_explanation(const aptitude_solution &sol);

      std::string archives_text(const pkgCache::VerIterator &ver);
      std::string dep_targets(const pkgCache::DepIterator &start);
      std::wstring dep_text(const pkgCache::DepIterator &d);
      bool do_previous_solution_enabled();
      bool do_previous_solution_enabled_from_state(const resolver_manager::state &state);
      void do_previous_solution();
      bool do_next_solution_enabled();
      bool do_next_solution_enabled_from_state(const resolver_manager::state &state);
      void do_next_solution();
      bool do_apply_solution_enabled_from_state(const resolver_manager::state &state);
      void do_apply_solution();

      /** \brief Updates the tab with the given resolver state.
       *
       *  \param force_update if \b true, the tree is rebuilt even if
       *  the current solution hasn't changed.
       */
    void update_from_state(const resolver_manager::state &state,
			   bool force_update);

      /** \brief Updates the tab with the current resolver state.
       *
       *  This is connected to the global state-changed signal; in
       *  functions that check or read the state before triggering an
       *  update, invoke update(state) instead to ensure
       *  consistency.
       *
       *  \param force_update if \b true, the tree is rebuilt even if
       *  the current solution hasn't changed.
       */
      void update(bool force_update);
    public:
      ResolverTab(const Glib::ustring &label);
      ResolverView * get_packages_view() { return pResolverView; };

    /** \brief Enable the "fix upgrade manually" mode on this resolver
     *  tab, using the given resolver manager.
     *
     *  This just adjusts the UI slightly to make it clear that the
     *  resolver is specifically focused on finding a solution for an
     *  upgrade problem and to provide feedback when the upgrade
     *  resolver's initial state is being recalculated for some reason
     *  (e.g., a cache reload).  It also prevents the resolver from
     *  being automatically destroyed when there are no more broken
     *  dependencies.
     *
     *  \param manager The manager to use in this tab, or \b NULL to
     *  disable the UI and show the "recalculating upgrade" progress
     *  bar.  The caller is responsible for invoking
     *  set_is_fix_upgrade_resolver() again, should "manager" ever
     *  become invalid, with a valid resolver manager object.
     */
    void set_fix_upgrade_resolver(resolver_manager *manager);

    /** \brief Invoked by the dashboard tab to pulse the progress bar
     *	in this tab.
     */
    void pulse_fix_upgrade_resolver_progress();
  };

  /** \brief Set up the global resolver tracker.
   *
   *  This is responsible for connecting up the signals that cause the
   *  resolver to be automatically triggered when there are broken
   *  packages, and for making sure that those signals are properly
   *  destroyed and recreated when the cache is closed and reopened.
   *  It should be called exactly once from main().
   *
   *  This also ensures that resman->state_changed() is triggered in
   *  the main thread whenever new resolver solutions are available.
   */
  void init_resolver();
}

#endif /* RESOLVER_H_ */
