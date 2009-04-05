// resolver.cc
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


#include "resolver.h"
#include "aptitude.h"

#undef OK
#include <gtkmm.h>
#include <libglademm/xml.h>

#include <apt-pkg/error.h>

#include <loggers.h>
#include <solution_fragment.h> // For archives_text.
#include <solution_item.h> // For action_type.

#include <generic/apt/apt_undo_group.h>
#include <generic/problemresolver/exceptions.h>

#include <gui.h>

#include <cwidget/generic/util/ssprintf.h>

using aptitude::Loggers;

namespace gui
{
  namespace
  {
    void do_start_solution_calculation(bool blocking,
				       resolver_manager *resolver);

    class gui_resolver_continuation : public resolver_manager::background_continuation
    {
      typedef generic_solution<aptitude_universe> aptitude_solution;

      resolver_manager *manager;

      // This is static because the continuation will be deleted
      // before it gets invoked.
      static void do_error(const std::string &msg,
			   resolver_manager& manager)
      {
	_error->Error(_("Error in dependency resolver: %s"), msg.c_str());

	std::string notification =
	  ssprintf(_("Fatal error in dependency resolver.  You can continue "
		     "searching, but some solutions might be impossible to generate.\n\n%s"),
		   msg.c_str());
	pMainWindow->get_notifyview()->add_notification(Gtk::manage(new Notification(notification.c_str())));
	manager.state_changed();
      }

    public:
      gui_resolver_continuation(resolver_manager *_manager)
	: manager(_manager)
      {
      }

      void success(const aptitude_solution &sol)
      {
	log4cxx::LoggerPtr logger(Loggers::getAptitudeGtkResolver());

	LOG_TRACE(logger, "Resolver tab: got solution: " << sol);

	// Tell the main loop to pretend the state changed.
	sigc::slot0<void> state_changed =
	  manager->state_changed.make_slot();
	post_event(make_safe_slot(state_changed));
      }

      void no_more_solutions()
      {
	log4cxx::LoggerPtr logger(Loggers::getAptitudeGtkResolver());

	LOG_TRACE(logger, "Resolver tab: ran out of solutions.");

	sigc::slot0<void> state_changed =
	  manager->state_changed.make_slot();
	post_event(make_safe_slot(state_changed));
      }

      void no_more_time()
      {
	log4cxx::LoggerPtr logger(Loggers::getAptitudeGtkResolver());

	LOG_TRACE(logger, "Resolver tab: ran out of time, restarting the calculation.");

	do_start_solution_calculation(false, manager);
      }

      void interrupted()
      {
	log4cxx::LoggerPtr logger(Loggers::getAptitudeGtkResolver());

	LOG_TRACE(logger, "Resolver tab: interrupted.");

	sigc::slot0<void> state_changed =
	  manager->state_changed.make_slot();
	post_event(make_safe_slot(state_changed));
      }

      void aborted(const cwidget::util::Exception &e)
      {
	log4cxx::LoggerPtr logger(Loggers::getAptitudeGtkResolver());
	LOG_TRACE(logger, "Resolver tab: aborted by exception: " << e.errmsg());

	sigc::slot0<void> do_error_slot =
	  sigc::bind(sigc::ptr_fun(&gui_resolver_continuation::do_error),
		     e.errmsg(), sigc::ref(*manager));
	post_event(make_safe_slot(do_error_slot));
      }
    };

    void do_start_solution_calculation(bool blocking, resolver_manager *resolver)
    {
      resolver->maybe_start_solution_calculation(blocking, new gui_resolver_continuation(resolver));
    }

    // This isn't parameterized by the resolver, because only the
    // global resolver should automatically be started calculating.
    void do_connect_resolver_callback()
    {
      resman->state_changed.connect(sigc::bind(sigc::ptr_fun(&do_start_solution_calculation), true, resman));
      // We may have missed a signal before making the connection:
      do_start_solution_calculation(false, resman);
    }
  }

  void init_resolver()
  {
    cache_reloaded.connect(sigc::ptr_fun(&do_connect_resolver_callback));
    if(apt_cache_file)
      do_connect_resolver_callback();
  }

  //std::string glade_main_file;
  //AptitudeWindow * pMainWindow;

  ResolverColumns::ResolverColumns()
  {
    add(PkgIterator);
    add(VerIterator);
    add(Name);
    add(Action);
  }

  template <class ColumnType>
  int ResolverView::append_column(Glib::ustring title,
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
    return Gtk::TreeView::append_column(*treeview_column);
  }

  ResolverView::ResolverView(BaseObjectType* cobject, const Glib::RefPtr<Gnome::Glade::Xml>& refGlade)
  : Gtk::TreeView(cobject) //Calls the base class constructor
  {
    append_column(Glib::ustring(_("Name")), Name, resolver_columns.Name, 200);
    append_column(Glib::ustring(_("Action")), Section, resolver_columns.Action, 200);
    set_search_column(resolver_columns.Name);
  }

  ResolverTab::ResolverTab(const Glib::ustring &label) :
    Tab(Resolver, label, Gnome::Glade::Xml::create(glade_main_file, "main_resolver_vbox"), "main_resolver_vbox"),
    resolver(NULL),
    using_internal_resolver(false)
  {
    get_xml()->get_widget("main_resolver_status", pResolverStatus);
    get_xml()->get_widget("main_resolver_previous", pResolverPrevious);
    pResolverPrevious->signal_clicked().connect(sigc::mem_fun(*this, &ResolverTab::do_previous_solution));
    get_xml()->get_widget("main_resolver_next", pResolverNext);
    pResolverNext->signal_clicked().connect(sigc::mem_fun(*this, &ResolverTab::do_next_solution));
    get_xml()->get_widget("main_resolver_apply", pResolverApply);
    get_xml()->get_widget("resolver_fixing_upgrade_message", resolver_fixing_upgrade_message);
    get_xml()->get_widget("resolver_fixing_upgrade_progress_bar", resolver_fixing_upgrade_progress_bar);
    get_xml()->get_widget("resolver_fixing_upgrade_label", resolver_fixing_upgrade_label);
    pResolverApply->signal_clicked().connect(sigc::mem_fun(*this, &ResolverTab::do_apply_solution));

    get_xml()->get_widget("resolver_group_by_action", pButtonGroupByAction);
    get_xml()->get_widget("resolver_show_explanation", pButtonShowExplanation);

    // TODO: ideally, instead of rereading the state, we should
    // trigger an update using the last seen state.  Or maybe buildtwo
    // models and swap between them.
    pButtonGroupByAction->signal_toggled().connect(sigc::bind(sigc::mem_fun(*this, &ResolverTab::update),
							      true));
    pButtonShowExplanation->signal_toggled().connect(sigc::bind(sigc::mem_fun(*this, &ResolverTab::update),
								true));

    get_xml()->get_widget_derived("main_resolver_treeview", pResolverView);

    update(true);

    get_widget()->show();

    // \todo This is never reconnected after a cache reload!
    resolver_state_changed_connection =
      get_resolver()->state_changed.connect(sigc::bind(sigc::mem_fun(*this, &ResolverTab::update),
						       false));
  }

  void ResolverTab::update(bool force_update)
  {
    if(get_resolver() == NULL)
      return;

    resolver_manager::state state = get_resolver()->state_snapshot();
    update_from_state(state, force_update);
  }

  string ResolverTab::archives_text(const pkgCache::VerIterator &ver)
  {
    string rval;

    bool is_first = true;

    for(pkgCache::VerFileIterator vf=ver.FileList(); !vf.end(); ++vf)
      {
        if(is_first)
          is_first = false;
        else
          rval += ", ";

        if(vf.File().Archive())
          rval += vf.File().Archive();
        else
          rval += _("<NULL>");
      }

    return rval;
  }

  std::string ResolverTab::dep_targets(const pkgCache::DepIterator &start)
  {
    std::string rval;

    bool is_first = true;

    eassert(!start.end());

    for(pkgCache::DepIterator d = start; !d.end(); ++d)
      {
        if(is_first)
          is_first = false;
        else
          rval += " | ";

        rval += d.TargetPkg().Name();

        if(d.TargetVer())
          {
            rval += " (";
            rval += d.CompType();
            rval += " ";
            rval += d.TargetVer();
            rval += ")";
          }

        if((d->CompareOp & pkgCache::Dep::Or) == 0)
          break;
      }

    return rval;
  }

  std::wstring ResolverTab::dep_text(const pkgCache::DepIterator &d)
  {
    const char *name = const_cast<pkgCache::DepIterator &>(d).ParentPkg().Name();

    std::string targets = dep_targets(d);

    switch(d->Type)
      {
      case pkgCache::Dep::Depends:
        return swsprintf(W_("%s depends upon %s").c_str(),
                         name, targets.c_str());
      case pkgCache::Dep::PreDepends:
        return swsprintf(W_("%s pre-depends upon %s").c_str(),
                         name, targets.c_str());
      case pkgCache::Dep::Suggests:
        return swsprintf(W_("%s suggests %s").c_str(),
                         name, targets.c_str());
      case pkgCache::Dep::Recommends:
        return swsprintf(W_("%s recommends %s").c_str(),
                         name, targets.c_str());
      case pkgCache::Dep::Conflicts:
        return swsprintf(W_("%s conflicts with %s").c_str(),
                         name, targets.c_str());
      case pkgCache::Dep::DpkgBreaks:
        return swsprintf(W_("%s breaks %s").c_str(),
                         name, targets.c_str());
      case pkgCache::Dep::Replaces:
        return swsprintf(W_("%s replaces %s").c_str(),
                                   name, targets.c_str());
      case pkgCache::Dep::Obsoletes:
        return swsprintf(W_("%s obsoletes %s").c_str(),
                                   name, targets.c_str());
      default:
        abort();
      }
  }

  Glib::RefPtr<Gtk::TreeStore> ResolverTab::createstore()
  {
    return Gtk::TreeStore::create(pResolverView->resolver_columns);
  }

  Glib::RefPtr<Gtk::TreeStore> ResolverTab::render_as_action_groups(const aptitude_solution &sol)
  {
    Glib::RefPtr<Gtk::TreeStore> store(createstore());

    if(sol.get_choices().size() == 0)
      {
	Gtk::TreeModel::iterator iter = store->append();
	Gtk::TreeModel::Row row = *iter;
	row[pResolverView->resolver_columns.Name] = _("Internal error: unexpected null solution.");
      }
    else
      {
	// Bin packages according to what will happen to them.
	vector<pkgCache::PkgIterator> remove_packages;
	vector<pkgCache::PkgIterator> keep_packages;
	vector<pkgCache::VerIterator> install_packages;
	vector<pkgCache::VerIterator> downgrade_packages;
	vector<pkgCache::VerIterator> upgrade_packages;
	vector<pkgCache::DepIterator> unresolved;

	typedef generic_choice<aptitude_universe> choice;
	typedef generic_choice_set<aptitude_universe> choice_set;

	for(choice_set::const_iterator i = sol.get_choices().begin();
	    i != sol.get_choices().end(); ++i)
	  {
	    switch(i->get_type())
	      {
	      case choice::install_version:
		{
		  pkgCache::PkgIterator pkg = i->get_ver().get_pkg();
		  pkgCache::VerIterator curver=pkg.CurrentVer();
		  pkgCache::VerIterator newver = i->get_ver().get_ver();

		  if(curver.end())
		    {
		      if(newver.end())
			keep_packages.push_back(pkg);
		      else
			install_packages.push_back(newver);
		    }
		  else if(newver.end())
		    remove_packages.push_back(pkg);
		  else if(newver == curver)
		    keep_packages.push_back(pkg);
		  else
		    {
		      int cmp=_system->VS->CmpVersion(curver.VerStr(),
						      newver.VerStr());

		      // The versions shouldn't be equal -- otherwise
		      // something is majorly wrong.
		      // eassert(cmp!=0);
		      //
		      // The above is not true: consider, eg, the case of a
		      // locally compiled package and a standard package.

		      /** \todo indicate "sidegrades" separately? */
		      if(cmp<=0)
			upgrade_packages.push_back(newver);
		      else if(cmp>0)
			downgrade_packages.push_back(newver);
		    }
		}

		break;

	      case choice::break_soft_dep:
		unresolved.push_back(i->get_dep().get_dep());
		break;
	      }
	  }

	sort(remove_packages.begin(), remove_packages.end(), pkg_name_lt());
	sort(keep_packages.begin(), keep_packages.end(), pkg_name_lt());
	sort(install_packages.begin(), install_packages.end(), ver_name_lt());
	sort(downgrade_packages.begin(), downgrade_packages.end(), ver_name_lt());
	sort(upgrade_packages.begin(), upgrade_packages.end(), ver_name_lt());

	if(!remove_packages.empty())
	  {
	    Gtk::TreeModel::iterator parent_iter = store->append();
	    Gtk::TreeModel::Row parent_row = *parent_iter;
	    parent_row[pResolverView->resolver_columns.Name] = _("Remove the following packages:");
	    for(vector<pkgCache::PkgIterator>::const_iterator i=remove_packages.begin();
		i!=remove_packages.end(); ++i)
	      {
		Gtk::TreeModel::iterator iter = store->append(parent_row.children());
		Gtk::TreeModel::Row row = *iter;
		row[pResolverView->resolver_columns.Name] = i->Name();
		row[pResolverView->resolver_columns.Action] = "";
	      }
	  }

	if(!install_packages.empty())
	  {
	    Gtk::TreeModel::iterator parent_iter = store->append();
	    Gtk::TreeModel::Row parent_row = *parent_iter;
	    parent_row[pResolverView->resolver_columns.Name] = _("Install the following packages:");
	    for(vector<pkgCache::VerIterator>::const_iterator i=install_packages.begin();
		i!=install_packages.end(); ++i)
	      {
		Gtk::TreeModel::iterator iter = store->append(parent_row.children());
		Gtk::TreeModel::Row row = *iter;
		row[pResolverView->resolver_columns.Name] = i->ParentPkg().Name();
		row[pResolverView->resolver_columns.Action] = ssprintf("[%s (%s)]",
								       i->VerStr(),
								       archives_text(*i).c_str());
	      }
	  }

	if(!keep_packages.empty())
	  {
	    Gtk::TreeModel::iterator parent_iter = store->append();
	    Gtk::TreeModel::Row parent_row = *parent_iter;
	    parent_row[pResolverView->resolver_columns.Name] = _("Keep the following packages:");
	    for(vector<pkgCache::PkgIterator>::const_iterator i=keep_packages.begin();
		i!=keep_packages.end(); ++i)
	      {
		Gtk::TreeModel::iterator iter = store->append(parent_row.children());
		Gtk::TreeModel::Row row = *iter;
		if(i->CurrentVer().end())
		  {
		    row[pResolverView->resolver_columns.Name] = i->Name();
		    row[pResolverView->resolver_columns.Action] = ssprintf("[%s]",
									   _("Not Installed"));
		  }
		else
		  {
		    row[pResolverView->resolver_columns.Name] = i->Name();
		    row[pResolverView->resolver_columns.Action] = ssprintf("[%s (%s)]",
									   i->CurrentVer().VerStr(),
									   archives_text(i->CurrentVer()).c_str());
		  }
	      }
	  }

	if(!upgrade_packages.empty())
	  {
	    Gtk::TreeModel::iterator parent_iter = store->append();
	    Gtk::TreeModel::Row parent_row = *parent_iter;
	    parent_row[pResolverView->resolver_columns.Name] = _("Upgrade the following packages:");
	    for(vector<pkgCache::VerIterator>::const_iterator i=upgrade_packages.begin();
		i!=upgrade_packages.end(); ++i)
	      {
		Gtk::TreeModel::iterator iter = store->append(parent_row.children());
		Gtk::TreeModel::Row row = *iter;
		row[pResolverView->resolver_columns.Name] = i->ParentPkg().Name();
		row[pResolverView->resolver_columns.Action] = ssprintf("[%s (%s) -> %s (%s)]",
								       i->ParentPkg().CurrentVer().VerStr(),
								       archives_text(i->ParentPkg().CurrentVer()).c_str(),
								       i->VerStr(),
								       archives_text(*i).c_str());
	      }
	  }

	if(!downgrade_packages.empty())
	  {
	    Gtk::TreeModel::iterator parent_iter = store->append();
	    Gtk::TreeModel::Row parent_row = *parent_iter;
	    parent_row[pResolverView->resolver_columns.Name] = _("Downgrade the following packages:");
	    for(vector<pkgCache::VerIterator>::const_iterator i=downgrade_packages.begin();
		i!=downgrade_packages.end(); ++i)
	      {
		Gtk::TreeModel::iterator iter = store->append(parent_row.children());
		Gtk::TreeModel::Row row = *iter;
		row[pResolverView->resolver_columns.Name] = i->ParentPkg().Name();
		row[pResolverView->resolver_columns.Action] = ssprintf("[%s (%s) -> %s (%s)]",
								       i->ParentPkg().CurrentVer().VerStr(),
								       archives_text(i->ParentPkg().CurrentVer()).c_str(),
								       i->VerStr(),
								       archives_text(*i).c_str());
	      }
	  }

	if(!unresolved.empty())
	  {
	    Gtk::TreeModel::iterator parent_iter = store->append();
	    Gtk::TreeModel::Row parent_row = *parent_iter;
	    parent_row[pResolverView->resolver_columns.Name] = _("Leave the following dependencies unresolved:");
	    for(std::vector<pkgCache::DepIterator>::const_iterator i = unresolved.begin();
		i != unresolved.end(); ++i)
	      {
		Gtk::TreeModel::iterator iter = store->append(parent_row.children());
		Gtk::TreeModel::Row row = *iter;
		row[pResolverView->resolver_columns.Name] = cwidget::util::transcode(dep_text(*i).c_str(), "UTF-8");
		row[pResolverView->resolver_columns.Action] = "";
	      }
	  }
      }

    return store;
  }

  Glib::RefPtr<Gtk::TreeStore> ResolverTab::render_as_explanation(const aptitude_solution &sol)
  {
    Glib::RefPtr<Gtk::TreeStore> store(createstore());

    if(sol.get_choices().size() == 0)
      {
	Gtk::TreeModel::iterator iter = store->append();
	Gtk::TreeModel::Row row = *iter;
	row[pResolverView->resolver_columns.Name] = _("Internal error: unexpected null solution.");
      }
    else
      {
	typedef generic_choice_set<aptitude_universe> choice_set;
	typedef generic_choice<aptitude_universe> choice;

	// Store just the choices that modify the state of the world.
	std::vector<choice> actions;

	for(choice_set::const_iterator it = sol.get_choices().begin();
	    it != sol.get_choices().end(); ++it)
	  {
	    switch(it->get_type())
	      {
	      case choice::install_version:
		actions.push_back(*it);
		break;

	      default:
		break;
	      }
	  }

	std::sort(actions.begin(), actions.end(),
		  aptitude_solution::choice_id_compare());

	for(std::vector<choice>::const_iterator
	      it = actions.begin(); it != actions.end(); ++it)
	  {
	    switch(it->get_type())
	      {
	      case choice::install_version:
		{
		  Gtk::TreeModel::iterator parent_iter = store->append();
		  Gtk::TreeModel::Row parent_row = *parent_iter;

		  parent_row[pResolverView->resolver_columns.Name] = cwidget::util::transcode(dep_text((*it).get_dep().get_dep()), "UTF-8");

		  Gtk::TreeModel::iterator iter = store->append(parent_row.children());
		  Gtk::TreeModel::Row row = *iter;
		  Glib::ustring name;

		  aptitude_resolver_version ver((*it).get_ver());
		  pkgCache::PkgIterator pkg = ver.get_pkg();
		  action_type action(analyze_action(ver));

		  using cwidget::util::ssprintf;

		  switch(action)
		    {
		    case action_remove:
		      name = ssprintf(_("Remove %s [%s (%s)]"),
				      pkg.Name(),
				      pkg.CurrentVer().VerStr(),
				      archives_text(pkg.CurrentVer()).c_str());
		      break;

		    case action_install:
		      name = ssprintf(_("Install %s [%s (%s)]"),
				      pkg.Name(),
				      ver.get_ver().VerStr(),
				      archives_text(ver.get_ver()).c_str());
		      break;

		    case action_keep:
		      if(ver.get_ver().end())
			name = ssprintf(_("Cancel the installation of %s"),
					pkg.Name());
		      else if(ver.get_package().current_version().get_ver().end())
			name = ssprintf(_("Cancel the removal of %s"),
					pkg.Name());
		      else
			name = ssprintf(_("Keep %s at version %s (%s)"),
					pkg.Name(), 
					ver.get_ver().VerStr(),
					archives_text(ver.get_ver()).c_str());
		      break;

		    case action_upgrade:
		      name = ssprintf(_("Upgrade %s [%s (%s) -> %s (%s)]"),
				      pkg.Name(),
				      pkg.CurrentVer().VerStr(),
				      archives_text(pkg.CurrentVer()).c_str(),
				      ver.get_ver().VerStr(),
				      archives_text(ver.get_ver()).c_str());
		      break;

		    case action_downgrade:
		      name = ssprintf(_("Downgrade %s [%s (%s) -> %s (%s)]"),
				      pkg.Name(),
				      pkg.CurrentVer().VerStr(),
				      archives_text(pkg.CurrentVer()).c_str(),
				      ver.get_ver().VerStr(),
				      archives_text(ver.get_ver()).c_str());
		      break;

		    default:
		      name = "Internal error: bad action type";
		      break;
		    }

		  row[pResolverView->resolver_columns.Name] = name;
		}

		break;

	      default:
		// \todo Add items for breaking soft dependencies?
		break;
	      }
	  }
      }

    return store;
  }

  void ResolverTab::update_from_state(const resolver_manager::state &state,
				      bool force_update)
  {
    log4cxx::LoggerPtr logger(Loggers::getAptitudeGtkResolver());
    LOG_TRACE(logger, "Updating the resolver tab (" << (force_update ? "forced" : "not forced") << ")");
    // Maybe I should log more information about the resolver state as
    // we log it?

    Glib::RefPtr<Gtk::TreeStore> store = Gtk::TreeStore::create(pResolverView->resolver_columns);

    if(!state.resolver_exists)
      {
	LOG_DEBUG(logger, "Resolver tab: no resolver has been created.");
	Gtk::TreeModel::iterator iter = store->append();
	Gtk::TreeModel::Row row = *iter;
	row[pResolverView->resolver_columns.Name] = _("Nothing to do: there are no broken packages.");
	pResolverView->set_model(store);
      }
    else if(state.solutions_exhausted && state.generated_solutions == 0)
      {
	LOG_DEBUG(logger, "Resolver tab: search exhausted before any solutions were produced.");
	Gtk::TreeModel::iterator iter = store->append();
	Gtk::TreeModel::Row row = *iter;
	row[pResolverView->resolver_columns.Name] = _("No resolutions found.");
	last_sol.nullify();
      }
    else if(state.selected_solution >= state.generated_solutions)
      {
	// TODO: in this case we probably ought to avoid blowing away
	// the tree, but that requires more complex state management.
	if(state.background_thread_aborted)
	  {
	    LOG_DEBUG(logger, "Resolver tab: resolver aborted: " << state.background_thread_abort_msg);
	    Gtk::TreeModel::iterator iter = store->append();
	    Gtk::TreeModel::Row row = *iter;
	    row[pResolverView->resolver_columns.Name] = state.background_thread_abort_msg;
	  }
	else
	  {
	    LOG_DEBUG(logger, "Resolver tab: resolver running: open: "
		      << state.open_size
		      << "; closed: " << state.closed_size
		      << "; defer: " << state.deferred_size
		      << "; conflict: " << state.conflicts_size);
	    std::string generation_info = ssprintf(_("open: %d; closed: %d; defer: %d; conflict: %d"),
						   (int)state.open_size, (int)state.closed_size,
						   (int)state.deferred_size, (int)state.conflicts_size).c_str();
	    std::string msg = _("Resolving dependencies...");

	    const Gtk::TreeModel::iterator parent_iter = store->append();
	    Gtk::TreeModel::Row parent_row = *parent_iter;
	    parent_row[pResolverView->resolver_columns.Name] = msg;

	    Gtk::TreeModel::iterator child_iter = store->append(parent_iter->children());
	    Gtk::TreeModel::Row child_row = *child_iter;
	    child_row[pResolverView->resolver_columns.Name] = generation_info;
	  }

	last_sol.nullify();
      }
    else
      {
	aptitude_solution sol = get_resolver()->get_solution(state.selected_solution, 0);

	LOG_DEBUG(logger, "Resolver tab: showing solution: " << sol);

	// Break out without doing anything if the current solution
	// isn't changed.
	if(!force_update && sol == last_sol)
	  return;

	last_sol = sol;

	if(pButtonShowExplanation->get_active())
	  store = render_as_explanation(sol);
	else
	  store = render_as_action_groups(sol);
      }

    pResolverView->set_model(store);
    pResolverView->expand_all();

    // NB: here I rely on the fact that last_sol is set above to the
    // last solution we saw.
    if(!last_sol)
      pResolverStatus->set_text(state.solutions_exhausted ? _("No solutions.") : _("No solutions yet."));
    else
      pResolverStatus->set_text(ssprintf(_("Solution %d of %d (tier %s)"), state.selected_solution + 1, state.generated_solutions,
					 aptitude_universe::get_tier_name(last_sol.get_tier()).c_str()));

    pResolverPrevious->set_sensitive(do_previous_solution_enabled_from_state(state));
    pResolverNext->set_sensitive(do_next_solution_enabled_from_state(state));
    pResolverApply->set_sensitive(do_apply_solution_enabled_from_state(state));

    // WARNING:Minor hack.
    //
    // We should always hide and delete the resolver view when there
    // isn't a resolver yet.  But if one somehow gets created before
    // the resolver exists, we should show something sensible.  And
    // tab_del will really-and-truly destroy the parts of this tab
    // when it is invoked (not to mention the Tab object itself), so
    // we have to do the just-in-case setting up of the view before
    // calling tab_del.
    if(!state.resolver_exists)
      tab_del(this);
  }

  bool ResolverTab::do_previous_solution_enabled_from_state(const resolver_manager::state &state)
  {
    return state.selected_solution > 0;
  }

  bool ResolverTab::do_previous_solution_enabled()
  {
    if (get_resolver() == NULL)
      return false;

    resolver_manager::state state = get_resolver()->state_snapshot();

    return do_previous_solution_enabled_from_state(state);
  }

  void ResolverTab::do_previous_solution()
  {
    if (do_previous_solution_enabled())
      {
	LOG_TRACE(Loggers::getAptitudeGtkResolver(), "Resolver tab: Moving to the previous solution.");
	get_resolver()->select_previous_solution();
      }
  }

  bool ResolverTab::do_next_solution_enabled_from_state(const resolver_manager::state &state)
  {
    return state.selected_solution < state.generated_solutions &&
      !(state.selected_solution + 1 == state.generated_solutions &&
	state.solutions_exhausted);
  }

  bool ResolverTab::do_next_solution_enabled()
  {
    if (get_resolver() == NULL)
      return false;

    resolver_manager::state state = get_resolver()->state_snapshot();
    return do_next_solution_enabled_from_state(state);
  }

  void ResolverTab::do_next_solution()
  {
    if (do_next_solution_enabled())
    {
      LOG_TRACE(Loggers::getAptitudeGtkResolver(), "Resolver tab: Moving to the next solution.");
      // If an error was encountered, pressing "next solution"
      // skips it.
      get_resolver()->discard_error_information();
      get_resolver()->select_next_solution();
    }
  }

  bool ResolverTab::do_apply_solution_enabled_from_state(const resolver_manager::state &state)
  {
    return
      state.resolver_exists &&
      state.selected_solution >= 0 &&
      state.selected_solution < state.generated_solutions;
  }

  void ResolverTab::do_apply_solution()
  {
    if (!apt_cache_file || get_resolver() == NULL)
      return;

    resolver_manager::state state = get_resolver()->state_snapshot();

    if (do_apply_solution_enabled_from_state(state))
    {
      LOG_TRACE(Loggers::getAptitudeGtkResolver(), "Resolver tab: Applying the current solution.");

      undo_group *undo = new apt_undo_group;
      try
      {
	aptitudeDepCache::action_group group(*apt_cache_file, NULL);
	(*apt_cache_file)->apply_solution(get_resolver()->get_solution(state.selected_solution, 0), undo);
      }
      catch (NoMoreSolutions)
      {
        Gtk::MessageDialog dialog(*pMainWindow, _("Unable to find a solution to apply."), false, Gtk::MESSAGE_INFO, Gtk::BUTTONS_OK, true);
        dialog.run();
      }
      catch (NoMoreTime)
      {
        Gtk::MessageDialog dialog(*pMainWindow, _("Ran out of time while trying to find a solution."), false, Gtk::MESSAGE_INFO, Gtk::BUTTONS_OK, true);
        dialog.run();
      }

      if (!undo->empty())
      {
        apt_undos->add_item(undo);
        //package_states_changed();
      }
      else
        delete undo;

      // This is a bit of a hack, to ensure that the user doesn't get
      // dropped back at the dashboard after fixing an upgrade
      // manually.
      if(using_internal_resolver)
	pMainWindow->do_preview();
      tab_del(this);
    }
  }

  void ResolverTab::set_fix_upgrade_resolver(resolver_manager *manager)
  {
    resolver_state_changed_connection.disconnect();

    using_internal_resolver = true;
    resolver = manager;
    if(manager != NULL)
      {
	// \todo Is there a better place to put this?
	manager->state_changed.connect(sigc::bind(sigc::ptr_fun(&do_start_solution_calculation), true, manager));
	manager->state_changed.connect(sigc::bind(sigc::mem_fun(*this, &ResolverTab::update),
						  false));

	get_widget()->set_sensitive(true);
	manager->maybe_start_solution_calculation(true, new gui_resolver_continuation(manager));
	resolver_fixing_upgrade_label->show();
	resolver_fixing_upgrade_progress_bar->hide();
      }
    else
      {
	get_widget()->set_sensitive(false);
	resolver_fixing_upgrade_label->hide();
	resolver_fixing_upgrade_progress_bar->show();
	resolver_fixing_upgrade_progress_bar->pulse();
      }

    resolver_fixing_upgrade_message->show();
    update(true);
  }

  void ResolverTab::pulse_fix_upgrade_resolver_progress()
  {
    resolver_fixing_upgrade_progress_bar->pulse();
  }
}
