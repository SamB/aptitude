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

#include "treeview_cell_tooltips.h"

#undef OK
#include <gtkmm.h>
#include <libglademm/xml.h>

#include <apt-pkg/error.h>

#include <loggers.h>
#include <solution_fragment.h> // For archives_text.
#include <solution_item.h> // For action_type.

#include <generic/apt/apt_undo_group.h>
#include <generic/problemresolver/exceptions.h>
#include <generic/problemresolver/solution.h>

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
    add(BgSet);
    add(BgColor);
    add(PreferenceIcon);
    add(PreferenceIconTooltip);
    add(Choice);
  }

  void ResolverView::set_column_properties(Gtk::TreeViewColumn *treeview_column,
					   int size)
  {
    Glib::ListHandle<Gtk::CellRenderer *> renderers = treeview_column->get_cell_renderers();
    for(Glib::ListHandle<Gtk::CellRenderer *>::const_iterator it  =
	  renderers.begin(); it != renderers.end(); ++it)
      {
	treeview_column->add_attribute((*it)->property_cell_background_set(), resolver_columns.BgSet);
	treeview_column->add_attribute((*it)->property_cell_background(), resolver_columns.BgColor);
      }
    if (size < 0)
      treeview_column->set_sizing(Gtk::TREE_VIEW_COLUMN_AUTOSIZE);
    else
      {
        treeview_column->set_sizing(Gtk::TREE_VIEW_COLUMN_FIXED);
        treeview_column->set_fixed_width(size);
        treeview_column->set_resizable(true);
      }
    treeview_column->set_reorderable(true);
  }

  template<typename ColumnType>
  int ResolverView::append_column(const Glib::ustring &title,
				  Gtk::TreeViewColumn *out_treeview_column,
				  const Gtk::TreeModelColumn<ColumnType> &model_column,
				  int size)
  {
    out_treeview_column = manage(new Gtk::TreeViewColumn(title, model_column));
    set_column_properties(out_treeview_column, size);

    return view->append_column(*out_treeview_column);
  }


  ResolverView::preference_info ResolverView::get_preference_info(const generic_choice<aptitude_universe> &c,
								  resolver_manager *manager) const
  {
    typedef generic_choice<aptitude_universe> choice;
    using cwidget::util::ssprintf;

    if(manager == NULL)
      return preference_info();
    else
      switch(c.get_type())
	{
	case choice::install_version:
	  {
	    pkgCache::VerIterator ver = c.get_ver().get_ver();
	    pkgCache::PkgIterator pkg = c.get_ver().get_pkg();

	    if(manager->is_rejected(c.get_ver()))
	      {
		Glib::ustring icon_tooltip;
		// Generate a friendly message about what's happening.
		if(ver.end())
		  icon_tooltip = ssprintf(_("Removing %s is rejected."), pkg.Name());
		else if(ver == pkg.CurrentVer() &&
			pkg->CurrentState != pkgCache::State::NotInstalled &&
			pkg->CurrentState != pkgCache::State::ConfigFiles)
		  icon_tooltip = ssprintf(_("Keeping %s at version %s is rejected."),
					  pkg.Name(),
					  pkg.CurrentVer().VerStr());
		else
		  icon_tooltip = ssprintf(_("Installing %s version %s is rejected."),
					  pkg.Name(),
					  pkg.CurrentVer().VerStr());

		return preference_info(Gtk::Stock::CANCEL.id,
				       icon_tooltip,
				       lightred_background_color,
				       true);
	      }
	    else if(manager->is_mandatory(c.get_ver()))
	      {
		Glib::ustring icon_tooltip;
		if(ver.end())
		  icon_tooltip = ssprintf(_("Removing %s is preferred over all un-accepted alternatives."),
					  pkg.Name());
		else if(ver == pkg.CurrentVer() &&
			pkg->CurrentState != pkgCache::State::NotInstalled &&
			pkg->CurrentState != pkgCache::State::ConfigFiles)
		  icon_tooltip = ssprintf(_("Keeping %s at version %s is preferred over all un-accepted alternatives."),
					  pkg.Name(),
					  pkg.CurrentVer().VerStr());
		else
		  icon_tooltip = ssprintf(_("Installing %s version %s is preferred over all un-accepted alternatives."),
					  pkg.Name(),
					  pkg.CurrentVer().VerStr());

		return preference_info(Gtk::Stock::APPLY.id,
				       icon_tooltip,
				       lightgreen_background_color,
				       true);
	      }
	    else
	      return preference_info();
	  }

	case choice::break_soft_dep:
	  if(manager->is_hardened(c.get_dep()))
	    return preference_info(Gtk::Stock::CANCEL.id,
				   ssprintf(_("Leaving %ls unresolved is rejected."),
					    dep_text(c.get_dep().get_dep()).c_str()),
				   lightred_background_color,
				   true);
	  else if(manager->is_approved_broken(c.get_dep()))
	    return preference_info(Gtk::Stock::APPLY.id,
				   ssprintf(_("Leaving %ls unresolved is preferred over all un-accepted alternatives."),
					    dep_text(c.get_dep().get_dep()).c_str()),
				   lightgreen_background_color,
				   true);
	  else
	    return preference_info();

	default:
	  LOG_ERROR(Loggers::getAptitudeGtkResolver(),
		    "Bad choice type " << c.get_type());
	  return preference_info(Gtk::Stock::DIALOG_ERROR.id,
				 "",
				 "",
				 false);
	}
  }

  void ResolverView::set_preference_info(Gtk::TreeModel::Row &row,
					 const ResolverView::preference_info &pref_inf) const
  {
    row[resolver_columns.PreferenceIcon] = pref_inf.icon;
    row[resolver_columns.PreferenceIconTooltip] = pref_inf.icon_tooltip;
    // Even if the color will have no effect because it isn't set, we
    // need to set a legal color name here; otherwise GTK+ will
    // complain that it doesn't know about the color "".  Black is
    // picked so it's really obvious when this has happened.
    if(pref_inf.background.empty())
      row[resolver_columns.BgColor] = "black";
    else
      row[resolver_columns.BgColor] = pref_inf.background;
    row[resolver_columns.BgSet] = pref_inf.background_set;

    // Sanity-check.
    if(pref_inf.background.empty() && pref_inf.background_set)
      LOG_ERROR(Loggers::getAptitudeGtkResolver(),
		"The row \"" << row[resolver_columns.Name] << "\" has no background but its background is set.");
  }

  void ResolverView::set_preference_info(Gtk::TreeModel::Row &row,
					 const generic_choice<aptitude_universe> &c,
					 resolver_manager *manager) const
  {
    preference_info pref_inf(get_preference_info(c, manager));
    set_preference_info(row, pref_inf);
  }

  bool ResolverView::update_preference_info(const Gtk::TreeModel::iterator &iter,
					    resolver_manager *manager)
  {
    Gtk::TreeModel::Row row(*iter);

    typedef generic_choice<aptitude_universe> choice;
    maybe<choice> maybe_c(row[resolver_columns.Choice]);
    choice c;
    if(maybe_c.extract(c))
      set_preference_info(row,
			  c,
			  manager);
    else
      set_preference_info(row, preference_info());

    return false;
  }

  void ResolverView::update_version(const aptitude_resolver_version &version,
				    resolver_manager *manager)
  {
    std::map<aptitude_resolver_version, Gtk::TreeModel::iterator>::iterator
      found = version_backpointers.find(version);

    if(found != version_backpointers.end())
      {
	typedef generic_choice<aptitude_universe> choice;
	Gtk::TreeModel::Row row(*found->second);
	set_preference_info(row,
			    choice::make_install_version(version, -1),
			    manager);
      }
  }

  void ResolverView::update_dep(const aptitude_resolver_dep &dep,
				resolver_manager *manager)
  {
    std::map<aptitude_resolver_dep, Gtk::TreeModel::iterator>::iterator
      found = dep_backpointers.find(dep);

    if(found != dep_backpointers.end())
      {
	typedef generic_choice<aptitude_universe> choice;
	Gtk::TreeModel::Row row(*found->second);
	set_preference_info(row,
			    choice::make_break_soft_dep(dep, -1),
			    manager);
      }
  }

  ResolverView::ResolverView(Gtk::TreeView *_view)
    : view(_view)
  {
    {
      Gtk::CellRendererPixbuf *preference_icon_renderer = manage(new Gtk::CellRendererPixbuf);
      PreferenceIcon = manage(new Gtk::TreeViewColumn("", *preference_icon_renderer));
      PreferenceIcon->add_attribute(preference_icon_renderer->property_stock_id(),
				    resolver_columns.PreferenceIcon);
      set_column_properties(PreferenceIcon, -1);
      view->append_column(*PreferenceIcon);
    }
    set_text_tooltip(view, PreferenceIcon, resolver_columns.PreferenceIconTooltip);

    append_column(Glib::ustring(_("Name")), Name, resolver_columns.Name, 200);
    append_column(Glib::ustring(_("Action")), Section, resolver_columns.Action, 200);

    view->get_selection()->set_mode(Gtk::SELECTION_MULTIPLE);
    view->set_search_column(resolver_columns.Name);
  }

  cwidget::util::ref_ptr<ResolverView>
  ResolverView::create(Gtk::TreeView *view)
  {
    return new ResolverView(view);
  }

  bool ResolverView::add_backpointer(const Gtk::TreeModel::Path &path,
				     const Gtk::TreeModel::iterator &iter)
  {
    typedef generic_choice<aptitude_universe> choice;
    const maybe<choice> &maybe_c((*iter)[resolver_columns.Choice]);
    choice c;
    if(maybe_c.extract(c))
      {
	switch(c.get_type())
	  {
	  case choice::install_version:
	    version_backpointers[c.get_ver()] = iter;
	    break;

	  case choice::break_soft_dep:
	    dep_backpointers[c.get_dep()] = iter;
	    break;
	  }
      }

    return false;
  }

  bool ResolverView::sanity_check_iter(const Gtk::TreeModel::iterator &iter) const
  {
    Glib::ustring bgColor = (*iter)[resolver_columns.BgColor];
    if((*iter)[resolver_columns.BgSet] && bgColor.empty())
      {
	LOG_ERROR(Loggers::getAptitudeGtkResolver(),
		  "Sanity-check failed: the resolver row \""
		  << (*iter)[resolver_columns.Name]
		  << "\" has an empty background color.");
      }

    return false;
  }

  void ResolverView::set_model(const Glib::RefPtr<Gtk::TreeModel> &model,
			       resolver_manager *manager)
  {
    model->foreach_iter(sigc::bind(sigc::mem_fun(*this, &ResolverView::update_preference_info),
				   manager));
    model->foreach_iter(sigc::mem_fun(*this, &ResolverView::sanity_check_iter));

    view->set_model(model);
    version_backpointers.clear();
    dep_backpointers.clear();

    model->foreach(sigc::mem_fun(*this, &ResolverView::add_backpointer));
  }

  ResolverTab::ResolverTab(const Glib::ustring &label) :
    Tab(Resolver, label, Gnome::Glade::Xml::create(glade_main_file, "main_resolver_vbox"), "main_resolver_vbox"),
    resolver(NULL),
    using_internal_resolver(false),
    toggle_signals_suppressed(false)
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

    get_xml()->get_widget("resolver_acceptreject_label", acceptreject_label);
    get_xml()->get_widget("resolver_reject_button", reject_button);
    get_xml()->get_widget("resolver_no_preference_button", no_preference_button);
    get_xml()->get_widget("resolver_accept_button", accept_button);

    reject_button->set_image(*new Gtk::Image(Gtk::Stock::CANCEL, Gtk::ICON_SIZE_BUTTON));
    accept_button->set_image(*new Gtk::Image(Gtk::Stock::APPLY, Gtk::ICON_SIZE_BUTTON));

    // TODO: ideally, instead of rereading the state, we should
    // trigger an update using the last seen state.  Or maybe build two
    // models and swap between them.
    pButtonGroupByAction->signal_toggled().connect(sigc::bind(sigc::mem_fun(*this, &ResolverTab::update),
							      true));
    pButtonShowExplanation->signal_toggled().connect(sigc::bind(sigc::mem_fun(*this, &ResolverTab::update),
								true));

    reject_button->signal_toggled().connect(sigc::mem_fun(*this, &ResolverTab::reject_button_toggled));
    no_preference_button->signal_toggled().connect(sigc::mem_fun(*this, &ResolverTab::no_preference_button_toggled));
    accept_button->signal_toggled().connect(sigc::mem_fun(*this, &ResolverTab::accept_button_toggled));

    Gtk::TreeView *solution_treeview;
    get_xml()->get_widget("main_resolver_treeview", solution_treeview);
    solution_view = ResolverView::create(solution_treeview);
    solution_treeview->get_selection()->signal_changed().connect(sigc::mem_fun(*this, &ResolverTab::update_reject_accept_buttons));

    update(true);

    get_widget()->show();

    // \todo This is never reconnected after a cache reload!
    resolver_state_changed_connection =
      get_resolver()->state_changed.connect(sigc::bind(sigc::mem_fun(*this, &ResolverTab::update),
						       false));

    resolver_version_accept_reject_changed_connection =
      get_resolver()->version_accept_reject_changed.connect(sigc::mem_fun(*this, &ResolverTab::version_accept_reject_changed));
    resolver_break_dep_accept_reject_changed_connection =
      get_resolver()->break_dep_accept_reject_changed.connect(sigc::mem_fun(*this, &ResolverTab::break_dep_accept_reject_changed));
  }

  void ResolverTab::update(bool force_update)
  {
    update_reject_accept_buttons();

    if(get_resolver() == NULL)
      return;

    resolver_manager::state state = get_resolver()->state_snapshot();
    update_from_state(state, force_update);
  }

  void ResolverTab::version_accept_reject_changed(const aptitude_resolver_version &ver)
  {
    solution_view->update_version(ver, get_resolver());
  }

  void ResolverTab::break_dep_accept_reject_changed(const aptitude_resolver_dep &dep)
  {
    solution_view->update_dep(dep, get_resolver());
  }

  class ResolverTab::update_reject_accept_counter
  {
    // Counts how many things are selected.
    int count;
    bool contains_rejected;
    bool contains_accepted;
    bool contains_no_preference;
    const ResolverTab &parent;
    const ResolverColumns &columns;

  public:
    update_reject_accept_counter(const ResolverTab &_parent,
				 const ResolverColumns &_columns)
      : count(0),
	contains_rejected(false),
	contains_accepted(false),
	contains_no_preference(false),
	parent(_parent),
	columns(_columns)
    {
    }

    void process(const Gtk::TreeModel::iterator &iter)
    {
      typedef generic_choice<aptitude_universe> choice;
      const maybe<choice> &maybe_c((*iter)[columns.Choice]);
      choice c;
      if(maybe_c.extract(c))
	{
	  ++count;

	  switch(c.get_type())
	    {
	    case choice::install_version:
	      if(parent.get_resolver()->is_rejected(c.get_ver()))
		contains_rejected = true;
	      else if(parent.get_resolver()->is_mandatory(c.get_ver()))
		contains_accepted = true;
	      else
		contains_no_preference = true;
	      break;

	    case choice::break_soft_dep:
	      if(parent.get_resolver()->is_hardened(c.get_dep()))
		contains_rejected = true;
	      else if(parent.get_resolver()->is_approved_broken(c.get_dep()))
		contains_accepted = true;
	      else
		contains_no_preference = true;
	      break;
	    }
	}
    }

    int get_count() const { return count; }
    bool get_contains_rejected() const { return contains_rejected; }
    bool get_contains_no_preference() const { return contains_no_preference; }
    bool get_contains_accepted() const { return contains_accepted; }
  };

  void ResolverTab::update_reject_accept_buttons()
  {
    suppress_toggle_signals suppressor(*this);

    if(get_resolver() == NULL ||
       !get_resolver()->resolver_exists())
      {
	accept_button->set_sensitive(false);
	no_preference_button->set_sensitive(false);
	reject_button->set_sensitive(false);

	accept_button->set_active(false);
	no_preference_button->set_active(false);
	reject_button->set_active(false);

	return;
      }

    // Find out what's selected.
    update_reject_accept_counter counter(*this,
					 solution_view->get_columns());;

    Glib::RefPtr<Gtk::TreeSelection> selection =
      solution_view->get_treeview()->get_selection();

    selection->selected_foreach_iter(sigc::mem_fun(counter,
						   &update_reject_accept_counter::process));

    const int count = counter.get_count();
    const bool contains_rejected = counter.get_contains_rejected();
    const bool contains_no_preference = counter.get_contains_no_preference();
    const bool contains_accepted = counter.get_contains_accepted();

    using cwidget::util::ssprintf;
    // TODO: this frame title is lame.
    acceptreject_label->set_markup(ssprintf("<b>%s</b>",
					    Glib::Markup::escape_text(ngettext("How to treat the selected action",
									       "How to treat the selected actions",
									       count)).c_str()));

    reject_button->set_tooltip_text(ngettext("Ignore solutions containing this action.",
					     "Ignore solutions containing these actions.",
					     count));

    no_preference_button->set_tooltip_text(ngettext("Allow solutions containing this action, but do not prefer them to other solutions.",
						    "Allow solutions containing these actions, but do not prefer them to other solutions.",
						    count));

    accept_button->set_tooltip_text(ngettext("Always prefer this action over alternatives that have not been accepted.",
					     "Always prefer these actions over alternatives that have not been accepted.",
					     count));

    if(contains_rejected)
      {
	reject_button->set_active(true);

	reject_button->set_inconsistent(contains_accepted ||
					contains_no_preference);
      }
    else
      {
	reject_button->set_active(false);
	reject_button->set_inconsistent(false);
      }

    if(contains_no_preference)
      {
	no_preference_button->set_active(true);

	no_preference_button->set_inconsistent(contains_rejected ||
					       contains_accepted);
      }
    else
      {
	no_preference_button->set_active(false);
	no_preference_button->set_inconsistent(false);
      }

    if(contains_accepted)
      {
	accept_button->set_active(true);

	accept_button->set_inconsistent(contains_rejected ||
					contains_no_preference);
      }
    else
      {
	accept_button->set_active(false);
	accept_button->set_inconsistent(false);
      }

    bool sensitive;

    if(count == 0)
      {
	// Maybe hide the buttons and/or show a label saying to select
	// an action?
	sensitive = false;

	accept_button->set_active(false);
	no_preference_button->set_active(false);
	reject_button->set_active(false);
      }
    else
      sensitive = true;

    accept_button->set_sensitive(sensitive);
    no_preference_button->set_sensitive(sensitive);
    reject_button->set_sensitive(sensitive);
  }

  void ResolverTab::do_reject_iterator(const Gtk::TreeModel::iterator &iter)
  {
    typedef generic_choice<aptitude_universe> choice;
    const maybe<choice> &maybe_c((*iter)[solution_view->get_columns().Choice]);
    choice c;

    if(maybe_c.extract(c))
      {
	switch(c.get_type())
	  {
	  case choice::install_version:
	    get_resolver()->reject_version(c.get_ver());
	    break;

	  case choice::break_soft_dep:
	    get_resolver()->harden_dep(c.get_dep());
	    break;
	  }
      }
  }

  void ResolverTab::do_no_preference_iterator(const Gtk::TreeModel::iterator &iter)
  {
    typedef generic_choice<aptitude_universe> choice;
    const maybe<choice> &maybe_c((*iter)[solution_view->get_columns().Choice]);
    choice c;
    if(maybe_c.extract(c))
      {
	switch(c.get_type())
	  {
	  case choice::install_version:
	    get_resolver()->unreject_version(c.get_ver());
	    get_resolver()->unmandate_version(c.get_ver());
	    break;

	  case choice::break_soft_dep:
	    get_resolver()->unharden_dep(c.get_dep());
	    get_resolver()->unapprove_broken_dep(c.get_dep());
	    break;
	  }
      }
  }

  void ResolverTab::do_accept_iterator(const Gtk::TreeModel::iterator &iter)
  {
    typedef generic_choice<aptitude_universe> choice;
    const maybe<choice> &maybe_c((*iter)[solution_view->get_columns().Choice]);
    choice c;
    if(maybe_c.extract(c))
      {
	switch(c.get_type())
	  {
	  case choice::install_version:
	    get_resolver()->mandate_version(c.get_ver());
	    break;

	  case choice::break_soft_dep:
	    get_resolver()->approve_broken_dep(c.get_dep());
	    break;
	  }
      }
  }

  void ResolverTab::reject_button_toggled()
  {
    log4cxx::LoggerPtr logger(Loggers::getAptitudeGtkResolver());
    if(toggle_signals_suppressed ||
       get_resolver() == NULL ||
       !get_resolver()->resolver_exists())
      {
	LOG_TRACE(logger, "reject_button_toggled: not rejecting packages.");
	return;
      }

    suppress_toggle_signals suppressor(*this);

    // Don't do anything unless the reject button is newly active.
    if(!reject_button->get_active())
      {
	LOG_TRACE(logger, "reject_button_toggled: the reject button was un-toggled; not doing anything.");
	return;
      }

    LOG_DEBUG(logger, "reject_button_toggled: rejecting the selected choices.");

    Glib::RefPtr<Gtk::TreeSelection> selection =
      solution_view->get_treeview()->get_selection();

    selection->selected_foreach_iter(sigc::mem_fun(*this, &ResolverTab::do_reject_iterator));
  }

  void ResolverTab::no_preference_button_toggled()
  {
    log4cxx::LoggerPtr logger(Loggers::getAptitudeGtkResolver());
    if(toggle_signals_suppressed ||
       get_resolver() == NULL ||
       !get_resolver()->resolver_exists())
      {
	LOG_TRACE(logger, "no_preference_button_toggled: not clearing states.");
	return;
      }

    suppress_toggle_signals suppressor(*this);

    if(!no_preference_button->get_active())
      {
	LOG_TRACE(logger, "no_preference_button_toggled: the no-preference button was un-toggled; not doing anything.");
	return;
      }

    Glib::RefPtr<Gtk::TreeSelection> selection =
      solution_view->get_treeview()->get_selection();

    LOG_DEBUG(logger, "no_preference_button_toggled: clearing reject/accept states of the selected choices.");
    selection->selected_foreach_iter(sigc::mem_fun(*this, &ResolverTab::do_no_preference_iterator));
  }

  void ResolverTab::accept_button_toggled()
  {
    log4cxx::LoggerPtr logger(Loggers::getAptitudeGtkResolver());
    if(toggle_signals_suppressed ||
       get_resolver() == NULL ||
       !get_resolver()->resolver_exists())
      {
	LOG_TRACE(logger, "accept_button_toggled: not clearing states.");
	return;
      }

    suppress_toggle_signals suppressor(*this);

    if(!accept_button->get_active())
      {
	LOG_TRACE(logger, "accept_button_toggled: the accept button was un-toggled; not doing anything.");
	return;
      }

    Glib::RefPtr<Gtk::TreeSelection> selection =
      solution_view->get_treeview()->get_selection();

    LOG_DEBUG(logger, "accept_button_toggled: accepting the selected choices.");
    selection->selected_foreach_iter(sigc::mem_fun(*this, &ResolverTab::do_accept_iterator));
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

  std::string ResolverTab::dep_targets(const pkgCache::DepIterator &start) const
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

  std::wstring ResolverTab::dep_text(const pkgCache::DepIterator &d) const
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

  Glib::RefPtr<Gtk::TreeStore> ResolverTab::render_as_action_groups(const aptitude_solution &sol)
  {
    Glib::RefPtr<Gtk::TreeStore> store(Gtk::TreeStore::create(solution_view->get_columns()));

    if(sol.get_choices().size() == 0)
      {
	Gtk::TreeModel::iterator iter = store->append();
	Gtk::TreeModel::Row row = *iter;
	row[solution_view->get_columns().Name] = _("Internal error: unexpected null solution.");
	row[solution_view->get_columns().BgSet] = false;
      }
    else
      {
	typedef generic_choice<aptitude_universe> choice;

	// Bin packages according to what will happen to them.
	vector<choice> remove_packages;
	vector<choice> keep_packages;
	vector<choice> install_packages;
	vector<choice> downgrade_packages;
	vector<choice> upgrade_packages;
	vector<choice> unresolved;

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
			keep_packages.push_back(*i);
		      else
			install_packages.push_back(*i);
		    }
		  else if(newver.end())
		    remove_packages.push_back(*i);
		  else if(newver == curver)
		    keep_packages.push_back(*i);
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
			upgrade_packages.push_back(*i);
		      else if(cmp>0)
			downgrade_packages.push_back(*i);
		    }
		}

		break;

	      case choice::break_soft_dep:
		unresolved.push_back(*i);
		break;
	      }
	  }

	typedef generic_solution<aptitude_universe>::choice_name_lt choice_name_lt;
	sort(remove_packages.begin(), remove_packages.end(), choice_name_lt());
	sort(keep_packages.begin(), keep_packages.end(), choice_name_lt());
	sort(install_packages.begin(), install_packages.end(), choice_name_lt());
	sort(downgrade_packages.begin(), downgrade_packages.end(), choice_name_lt());
	sort(upgrade_packages.begin(), upgrade_packages.end(), choice_name_lt());
	sort(unresolved.begin(), unresolved.end(), choice_name_lt());

	if(!remove_packages.empty())
	  {
	    Gtk::TreeModel::iterator parent_iter = store->append();
	    Gtk::TreeModel::Row parent_row = *parent_iter;
	    parent_row[solution_view->get_columns().Name] = _("Remove the following packages:");
	    parent_row[solution_view->get_columns().BgSet] = false;
	    for(vector<choice>::const_iterator i = remove_packages.begin();
		i!=remove_packages.end(); ++i)
	      {
		pkgCache::PkgIterator pkg(i->get_ver().get_pkg());

		Gtk::TreeModel::iterator iter = store->append(parent_row.children());
		Gtk::TreeModel::Row row = *iter;
		row[solution_view->get_columns().Name] = pkg.Name();
		row[solution_view->get_columns().Action] = "";
		row[solution_view->get_columns().Choice] = *i;
	      }
	  }

	if(!install_packages.empty())
	  {
	    Gtk::TreeModel::iterator parent_iter = store->append();
	    Gtk::TreeModel::Row parent_row = *parent_iter;
	    parent_row[solution_view->get_columns().Name] = _("Install the following packages:");
	    parent_row[solution_view->get_columns().BgSet] = false;
	    for(vector<choice>::const_iterator i = install_packages.begin();
		i!=install_packages.end(); ++i)
	      {
		aptitude_resolver_version ver(i->get_ver());

		Gtk::TreeModel::iterator iter = store->append(parent_row.children());
		Gtk::TreeModel::Row row = *iter;
		row[solution_view->get_columns().Name] = ver.get_ver().ParentPkg().Name();
		row[solution_view->get_columns().Action] = ssprintf("[%s (%s)]",
								    ver.get_ver().VerStr(),
								    archives_text(ver.get_ver()).c_str());
		row[solution_view->get_columns().Choice] = *i;
	      }
	  }

	if(!keep_packages.empty())
	  {
	    Gtk::TreeModel::iterator parent_iter = store->append();
	    Gtk::TreeModel::Row parent_row = *parent_iter;
	    parent_row[solution_view->get_columns().Name] = _("Keep the following packages:");
	    parent_row[solution_view->get_columns().BgSet] = false;
	    for(vector<choice>::const_iterator i = keep_packages.begin();
		i!=keep_packages.end(); ++i)
	      {
		pkgCache::PkgIterator pkg(i->get_ver().get_pkg());

		Gtk::TreeModel::iterator iter = store->append(parent_row.children());
		Gtk::TreeModel::Row row = *iter;
		if(pkg.CurrentVer().end())
		  {
		    row[solution_view->get_columns().Name] = pkg.Name();
		    row[solution_view->get_columns().Action] = ssprintf("[%s]",
									   _("Not Installed"));
		  }
		else
		  {
		    row[solution_view->get_columns().Name] = pkg.Name();
		    row[solution_view->get_columns().Action] = ssprintf("[%s (%s)]",
									pkg.CurrentVer().VerStr(),
									archives_text(pkg.CurrentVer()).c_str());
		  }
		row[solution_view->get_columns().Choice] = *i;
	      }
	  }

	if(!upgrade_packages.empty())
	  {
	    Gtk::TreeModel::iterator parent_iter = store->append();
	    Gtk::TreeModel::Row parent_row = *parent_iter;
	    parent_row[solution_view->get_columns().Name] = _("Upgrade the following packages:");
	    parent_row[solution_view->get_columns().BgSet] = false;
	    for(vector<choice>::const_iterator i = upgrade_packages.begin();
		i!=upgrade_packages.end(); ++i)
	      {
		aptitude_resolver_version ver(i->get_ver());

		Gtk::TreeModel::iterator iter = store->append(parent_row.children());
		Gtk::TreeModel::Row row = *iter;
		row[solution_view->get_columns().Name] = ver.get_pkg().Name();
		row[solution_view->get_columns().Action] = ssprintf("[%s (%s) -> %s (%s)]",
								    ver.get_pkg().CurrentVer().VerStr(),
								    archives_text(ver.get_pkg().CurrentVer()).c_str(),
								    ver.get_ver().VerStr(),
								    archives_text(ver.get_ver()).c_str());
		row[solution_view->get_columns().Choice] = *i;
	      }
	  }

	if(!downgrade_packages.empty())
	  {
	    Gtk::TreeModel::iterator parent_iter = store->append();
	    Gtk::TreeModel::Row parent_row = *parent_iter;
	    parent_row[solution_view->get_columns().Name] = _("Downgrade the following packages:");
	    parent_row[solution_view->get_columns().BgSet] = false;
	    for(vector<choice>::const_iterator i = downgrade_packages.begin();
		i!=downgrade_packages.end(); ++i)
	      {
		aptitude_resolver_version ver(i->get_ver());

		Gtk::TreeModel::iterator iter = store->append(parent_row.children());
		Gtk::TreeModel::Row row = *iter;
		row[solution_view->get_columns().Name] = ver.get_pkg().Name();
		row[solution_view->get_columns().Action] = ssprintf("[%s (%s) -> %s (%s)]",
								    ver.get_pkg().CurrentVer().VerStr(),
								    archives_text(ver.get_pkg().CurrentVer()).c_str(),
								    ver.get_ver().VerStr(),
								    archives_text(ver.get_ver()).c_str());
		row[solution_view->get_columns().Choice] = *i;
	      }
	  }

	if(!unresolved.empty())
	  {
	    Gtk::TreeModel::iterator parent_iter = store->append();
	    Gtk::TreeModel::Row parent_row = *parent_iter;
	    parent_row[solution_view->get_columns().Name] = _("Leave the following dependencies unresolved:");
	    parent_row[solution_view->get_columns().BgSet] = false;
	    for(std::vector<choice>::const_iterator i = unresolved.begin();
		i != unresolved.end(); ++i)
	      {
		Gtk::TreeModel::iterator iter = store->append(parent_row.children());
		Gtk::TreeModel::Row row = *iter;

		row[solution_view->get_columns().Name] = cwidget::util::transcode(dep_text(i->get_dep().get_dep()).c_str(), "UTF-8");
		row[solution_view->get_columns().Action] = "";
		row[solution_view->get_columns().Choice] = *i;
	      }
	  }
      }

    return store;
  }

  Glib::RefPtr<Gtk::TreeStore> ResolverTab::render_as_explanation(const aptitude_solution &sol)
  {
    Glib::RefPtr<Gtk::TreeStore> store(Gtk::TreeStore::create(solution_view->get_columns()));

    if(sol.get_choices().size() == 0)
      {
	Gtk::TreeModel::iterator iter = store->append();
	Gtk::TreeModel::Row row = *iter;
	row[solution_view->get_columns().Name] = _("Internal error: unexpected null solution.");
	row[solution_view->get_columns().BgSet] = false;
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

		  parent_row[solution_view->get_columns().Name] = cwidget::util::transcode(dep_text((*it).get_dep().get_dep()), "UTF-8");
		  parent_row[solution_view->get_columns().BgSet] = false;

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

		  row[solution_view->get_columns().Name] = name;
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

    Glib::RefPtr<Gtk::TreeStore> store = Gtk::TreeStore::create(solution_view->get_columns());

    if(!state.resolver_exists)
      {
	LOG_DEBUG(logger, "Resolver tab: no resolver has been created.");
	Gtk::TreeModel::iterator iter = store->append();
	Gtk::TreeModel::Row row = *iter;
	row[solution_view->get_columns().Name] = _("Nothing to do: there are no broken packages.");
	row[solution_view->get_columns().BgSet] = false;
	solution_view->set_model(store, get_resolver());
      }
    else if(state.solutions_exhausted && state.generated_solutions == 0)
      {
	LOG_DEBUG(logger, "Resolver tab: search exhausted before any solutions were produced.");
	Gtk::TreeModel::iterator iter = store->append();
	Gtk::TreeModel::Row row = *iter;
	row[solution_view->get_columns().Name] = _("No resolutions found.");
	row[solution_view->get_columns().BgSet] = false;
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
	    row[solution_view->get_columns().Name] = state.background_thread_abort_msg;
	    row[solution_view->get_columns().BgSet] = false;
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
	    parent_row[solution_view->get_columns().Name] = msg;
	    parent_row[solution_view->get_columns().BgSet] = false;

	    Gtk::TreeModel::iterator child_iter = store->append(parent_iter->children());
	    Gtk::TreeModel::Row child_row = *child_iter;
	    child_row[solution_view->get_columns().Name] = generation_info;
	    child_row[solution_view->get_columns().BgSet] = false;
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

    solution_view->set_model(store, get_resolver());
    solution_view->get_treeview()->expand_all();

    // NB: here I rely on the fact that last_sol is set above to the
    // last solution we saw.
    if(!last_sol.valid())
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
      // No need to delete the tab manually here: it will be deleted
      // when there aren't any more dependencies to solve.  Deleting
      // it here would introduce a double-free bug.
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
