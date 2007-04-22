// solution_screen.cc
//
//   Copyright (C) 2005, 2007 Daniel Burrows
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

#include "solution_screen.h"

#include "aptitude.h"
#include "menu_redirect.h"
#include "menu_tree.h"
#include "solution_fragment.h"
#include "solution_item.h"

#include <generic/apt/aptitude_resolver_universe.h>
#include <generic/apt/resolver_manager.h>

#include <generic/problemresolver/solution.h>

#include <generic/util/util.h>

#include <sigc++/adaptors/bind.h>

#include <vscreen/config/style.h>
#include <vscreen/fragment.h>
#include <vscreen/transcode.h>
#include <vscreen/vs_label.h>
#include <vscreen/vs_layout_item.h>
#include <vscreen/vs_multiplex.h>
#include <vscreen/vs_staticitem.h>
#include <vscreen/vs_subtree.h>
#include <vscreen/vs_table.h>

typedef generic_solution<aptitude_universe> aptitude_solution;

using namespace std;

struct act_name_lt
{
public:
  bool operator()(const aptitude_solution::action &v1,
		  const aptitude_solution::action &v2) const
  {
    return strcmp(v1.ver.get_pkg().Name(), v2.ver.get_pkg().Name()) < 0;
  }
};

/** Partition the set of all packages into several vectors,
 *  according to the action to be performed on each package.
 *
 *  \param remove_actions each package to be removed will be placed in this vector.
 *  \param keep_actions each package which is being kept will be placed
 *         in this vector.
 *  \param install_actions if a package is being newly installed,
 *         its target version will be placed in this vector.
 *  \param downgrade_actions if a package is being downgraded,
 *         its target version will be placed in this vector.
 *  \param upgrade_actions if a package is being upgraded,
 *         its target version will be placed in this vector.
 */
void bin_actions(const aptitude_solution &sol,
		 vector<aptitude_solution::action> &remove_actions,
		 vector<aptitude_solution::action> &keep_actions,
		 vector<aptitude_solution::action> &install_actions,
		 vector<aptitude_solution::action> &downgrade_actions,
		 vector<aptitude_solution::action> &upgrade_actions)
{

  for(imm::map<aptitude_universe::package,
	aptitude_solution::action>::const_iterator i=sol.get_actions().begin();
      i!=sol.get_actions().end(); ++i)
    switch(analyze_action(i->second.ver))
      {
      case action_remove:
	remove_actions.push_back(i->second);
	break;
      case action_keep:
	keep_actions.push_back(i->second);
	break;
      case action_install:
	install_actions.push_back(i->second);
	break;
      case action_downgrade:
	downgrade_actions.push_back(i->second);
	break;
      case action_upgrade:
	upgrade_actions.push_back(i->second);
	break;
      default:
	abort();
      }
}

class label_tree : public vs_subtree_generic
{
  wstring my_label;
public:
  label_tree(wstring _label, bool _expanded = true,
	     bool selectable = true,
	     int depth = 0)
    :vs_subtree_generic(_expanded), my_label(_label)
  {
    set_selectable(selectable);
    set_depth(depth);
  }

  void paint(vs_tree *win, int y, bool hierarchical,
	     const style &st)
  {
    vs_subtree<vs_treeitem>::paint(win, y, hierarchical, my_label);
  }

  const wchar_t *tag()
  {
    return my_label.c_str();
  }

  const wchar_t *label()
  {
    return my_label.c_str();
  }
};



vs_subtree_generic *make_dep_solvers_tree(const aptitude_resolver_dep &d)
{
  pkgCache::DepIterator real_dep = d.get_dep();
  pkgCache::PrvIterator prv = d.get_prv();

  vs_subtree_generic *root = new label_tree(L"", true, true, -1);

  vs_staticitem *conflict_item = new vs_staticitem(conflict_text(real_dep, prv), L"");
  root->add_child(conflict_item);

  vs_subtree_generic *resolvers = new label_tree(transcode(_("The following actions will resolve this dependency:")), true, false);

  root->add_child(resolvers);

  for(aptitude_resolver_package::version_iterator
	vi = d.get_source().get_package().versions_begin(); !vi.end(); ++vi)
    if(*vi != d.get_source())
      {
	aptitude_solution::action act(*vi, d, true, 0);

	resolvers->add_child(new solution_act_item(act,
						   sigc::slot1<void, fragment *>(),
						   sigc::slot1<void, aptitude_resolver_dep>()));
      }

  for(aptitude_resolver_dep::solver_iterator
	si = d.solvers_begin(); !si.end(); ++si)
    {
      aptitude_solution::action act(*si, d, false, 0);

      resolvers->add_child(new solution_act_item(act,
						 sigc::slot1<void, fragment *>(),
						 sigc::slot1<void, aptitude_resolver_dep>()));
    }

  if(d.get_dep()->Type == pkgCache::Dep::Recommends)
    resolvers->add_child(new solution_unresolved_item(d, true, sigc::slot1<void, aptitude_resolver_dep>()));

  return root;
}

vs_subtree_generic *make_story_tree(const aptitude_solution &sol,
				    const sigc::slot1<void, fragment *> &set_short_description,
				    const sigc::slot1<void, aptitude_resolver_dep> &set_active_dep)
{
  vector<aptitude_solution::action> actions;

  for(imm::map<aptitude_universe::package, aptitude_solution::action>::const_iterator
	i = sol.get_actions().begin() ; i != sol.get_actions().end(); ++i)
    actions.push_back(i->second);

  sort(actions.begin(), actions.end(), aptitude_solution::action_id_compare());

  vs_subtree_generic *root = new label_tree(L"");

  for(vector<aptitude_solution::action>::const_iterator
	i = actions.begin(); i != actions.end(); ++i)
    {
      vs_subtree_generic *tree = new label_tree(dep_text(i->d.get_dep()), true, false);

      tree->add_child(new solution_act_item(*i, set_short_description, set_active_dep));
      root->add_child(tree);
    }

  return root;
}

vs_subtree_generic *make_solution_tree(const aptitude_solution &sol,
				       const sigc::slot1<void, fragment *> &set_short_description,
				       const sigc::slot1<void, aptitude_resolver_dep> &set_active_dep)
{
  // Bin packages according to what will happen to them.
  vector<aptitude_solution::action> remove_actions;
  vector<aptitude_solution::action> keep_actions;
  vector<aptitude_solution::action> install_actions;
  vector<aptitude_solution::action> downgrade_actions;
  vector<aptitude_solution::action> upgrade_actions;

  bin_actions(sol, remove_actions, keep_actions, install_actions,
	       downgrade_actions, upgrade_actions);

  sort(remove_actions.begin(), remove_actions.end(),
       act_name_lt());
  sort(keep_actions.begin(), keep_actions.end(),
       act_name_lt());
  sort(install_actions.begin(), install_actions.end(),
       act_name_lt());
  sort(downgrade_actions.begin(), downgrade_actions.end(),
       act_name_lt());
  sort(upgrade_actions.begin(), upgrade_actions.end(),
       act_name_lt());

  vs_subtree_generic *root = new label_tree(L"");

  if(!remove_actions.empty())
    {
      vs_subtree_generic *remove_tree = new label_tree(transcode(_("Remove the following packages:")));

      for(vector<aptitude_solution::action>::const_iterator i = remove_actions.begin();
	  i != remove_actions.end(); ++i)
	remove_tree->add_child(new solution_act_item_bare(*i, set_short_description, set_active_dep));

      root->add_child(remove_tree);
    }

  if(!keep_actions.empty())
    {
      vs_subtree_generic *keep_tree = new label_tree(transcode(_("Keep the following packages at their current version:")));

      for(vector<aptitude_solution::action>::const_iterator i = keep_actions.begin();
	  i != keep_actions.end(); ++i)
	keep_tree->add_child(new solution_act_item_bare(*i, set_short_description, set_active_dep));

      root->add_child(keep_tree);
    }

  if(!install_actions.empty())
    {
      vs_subtree_generic *install_tree = new label_tree(transcode(_("Install the following packages:")));

      for(vector<aptitude_solution::action>::const_iterator i = install_actions.begin();
	  i != install_actions.end(); ++i)
	install_tree->add_child(new solution_act_item_bare(*i, set_short_description, set_active_dep));

      root->add_child(install_tree);
    }

  if(!upgrade_actions.empty())
    {
      vs_subtree_generic *upgrade_tree = new label_tree(transcode(_("Upgrade the following packages:")));

      for(vector<aptitude_solution::action>::const_iterator i = upgrade_actions.begin();
	  i != upgrade_actions.end(); ++i)
	upgrade_tree->add_child(new solution_act_item_bare(*i, set_short_description, set_active_dep));

      root->add_child(upgrade_tree);
    }

  if(!downgrade_actions.empty())
    {
      vs_subtree_generic *downgrade_tree = new label_tree(transcode(_("Downgrade the following packages:")));

      for(vector<aptitude_solution::action>::const_iterator i = downgrade_actions.begin();
	  i != downgrade_actions.end(); ++i)
	downgrade_tree->add_child(new solution_act_item_bare(*i, set_short_description, set_active_dep));

      root->add_child(downgrade_tree);
    }

  const imm::set<aptitude_universe::dep> &unresolved = sol.get_unresolved_soft_deps();

  if(!unresolved.empty())
    {
      vs_subtree_generic *unresolved_tree = new label_tree(transcode(_("Leave the following recommendations unresolved:")));

      for(imm::set<aptitude_universe::dep>::const_iterator i = unresolved.begin();
	  i != unresolved.end(); ++i)
	unresolved_tree->add_child(new solution_unresolved_item(*i, false, set_active_dep));

      root->add_child(unresolved_tree);
    }

  return root;
}

/** A class for trees in which 'undo' should be mapped to the solution
 *  undo command.
 */
class solution_undo_tree : public menu_tree
{
protected:
  solution_undo_tree()
  {
  }
public:
  static ref_ptr<solution_undo_tree> create()
  {
    ref_ptr<solution_undo_tree> rval(new solution_undo_tree);
    rval->decref();
    return rval;
  }

  bool undo_undo_enabled()
  {
    return resman != NULL && resman->has_undo_items();
  }

  bool undo_undo()
  {
    return resman != NULL && resman->undo();
  }
};
typedef ref_ptr<solution_undo_tree> solution_undo_tree_ref;

class solution_examiner : public vs_multiplex
{
  aptitude_solution last_sol;

  menu_tree_ref solution_tree;
  menu_tree_ref story_tree;

  sigc::slot1<void, fragment *> set_short_description;
  sigc::slot1<void, aptitude_resolver_dep> set_active_dep;

  void attach_apt_cache_signals()
  {
    if(apt_cache_file)
      resman->state_changed.connect(sigc::mem_fun(*this, &solution_examiner::update));
  }

  /** Re-connects the signals attached to the apt cache file. */
  void handle_cache_reload()
  {
    attach_apt_cache_signals();

    update();
  }

  void set_static_root(const wstring &s)
  {
    solution_tree->set_root(new vs_layout_item(hardwrapbox(text_fragment(s))), true);
    story_tree->set_root(new vs_layout_item(hardwrapbox(text_fragment(s))), true);
  }

  /** Send highlighted/unhighlighted messages to the subwidgets so
   *  that the short description ends up correct.
   */
  void update_highlights()
  {
    vs_widget_ref tmpref(this);

    if(solution_tree == visible_widget())
      {
	story_tree->unhighlight_current();
	solution_tree->highlight_current();
      }
    else if(story_tree == visible_widget())
      {
	solution_tree->unhighlight_current();
	story_tree->highlight_current();
      }
  }

  void tick()
  {
    vs_widget_ref tmpref(this);

    if(resman != NULL && resman->resolver_exists())
      {
	resolver_manager::state state = resman->state_snapshot();

	if(state.background_thread_active &&
	   state.selected_solution >= state.generated_solutions)
	  update_from_state(state);
      }

    vscreen_addtimeout(new slot_event(sigc::mem_fun(this, &solution_examiner::tick)), 1000);
  }

protected:
  solution_examiner(const sigc::slot1<void, fragment *> &_set_short_description,
		    const sigc::slot1<void, aptitude_resolver_dep> &_set_active_dep)
    : vs_multiplex(false),
      solution_tree(solution_undo_tree::create()), story_tree(solution_undo_tree::create()),
      set_short_description(_set_short_description),
      set_active_dep(_set_active_dep)
  {
    add_visible_widget(solution_tree, true);
    add_visible_widget(story_tree, true);
    solution_tree->show();

    attach_apt_cache_signals();

    cache_closed.connect(sigc::mem_fun(*this, &solution_examiner::update));
    cache_reloaded.connect(sigc::mem_fun(*this, &solution_examiner::handle_cache_reload));

    cycled.connect(sigc::mem_fun(*this, &solution_examiner::update_highlights));

    vscreen_addtimeout(new slot_event(sigc::mem_fun(this, &solution_examiner::tick)), 1000);

    // Because the update event might destroy this object, it needs to
    // take place after the constructor returns.
    vscreen_post_event(new slot_event(sigc::mem_fun(this, &solution_examiner::update)));
  }

  bool handle_key(const key &k)
  {
    vs_widget_ref tmpref(this);

    if(global_bindings.key_matches(k, "CycleOrder"))
      cycle_forward();
    else
      return vs_multiplex::handle_key(k);

    return true;
  }
public:
  void connect_menu_signals()
  {
    create_menu_bindings(story_tree.unsafe_get_ref(), story_tree);
    create_menu_bindings(solution_tree.unsafe_get_ref(), solution_tree);
  }

  static ref_ptr<solution_examiner> create(const sigc::slot1<void, fragment *> &set_short_description,
					   const sigc::slot1<void, aptitude_resolver_dep> &set_active_dep)
  {
    ref_ptr<solution_examiner>
      rval(new solution_examiner(set_short_description, set_active_dep));
    rval->decref();
    return rval;
  }

  void paint(const style &st)
  {
    if(apt_cache_file == NULL)
      update();

    vs_multiplex::paint(st);
  }

  void update()
  {
    ref_ptr<solution_examiner> tmpref(this);

    if(!apt_cache_file)
      {
	set_static_root(transcode(_("The package cache is not available.")));
	set_active_dep(aptitude_resolver_dep());
	return;
      }

    if(!resman->resolver_exists())
      {
	set_static_root(transcode(_("No broken packages.")));
	set_short_description(fragf(""));
	set_active_dep(aptitude_resolver_dep());
	return;
      }

    resolver_manager::state state = resman->state_snapshot();
    update_from_state(state);
  }

  void update_from_state(const resolver_manager::state &state)
  {
    vs_widget_ref tmpref(this);

    if(state.solutions_exhausted && state.generated_solutions == 0)
      {
	set_static_root(transcode(_("No resolution found.")));
	last_sol.nullify();
	return;
      }

    if(state.selected_solution >= state.generated_solutions)
      {
	wstring generation_info = swsprintf(transcode(_("open: %d; closed: %d; defer: %d; conflict: %d")).c_str(),
					    state.open_size, state.closed_size,
					    state.deferred_size, state.conflicts_size);

	wstring msg = transcode(_("Resolving dependencies..."));

	vs_subtree_generic *sol_root = new label_tree(msg);
	sol_root->add_child(new vs_layout_item(hardwrapbox(text_fragment(generation_info))));

	vs_subtree_generic *story_root = new label_tree(msg);
	story_root->add_child(new vs_layout_item(hardwrapbox(text_fragment(generation_info))));

	solution_tree->set_root(sol_root, true);
	story_tree->set_root(story_root, true);

	last_sol.nullify();
	return;
      }

    aptitude_solution sol = resman->get_solution(state.selected_solution, 0);

    if(sol == last_sol)
      return;

    last_sol = sol;

    if(sol.get_actions().empty())
      set_static_root(transcode(_("Internal error: unexpected null solution.")));
    else
      {
	solution_tree->set_root(make_solution_tree(sol, set_short_description, set_active_dep));
	story_tree->set_root(make_story_tree(sol, set_short_description, set_active_dep));
      }

    update_highlights();
  }
};

typedef ref_ptr<solution_examiner> solution_examiner_ref;

static
void update_dep_display(aptitude_resolver_dep d, vs_tree *tBare)
{
  vs_tree_ref t(tBare);

  if(d.get_dep().end())
    t->set_root(NULL);
  else
    t->set_root(make_dep_solvers_tree(d));
}

static
void maybe_remove_examiner(vscreen_widget &wBare)
{
  vs_widget_ref w(&wBare);

  if(resman && !resman->resolver_exists())
    w->destroy();
}

vs_widget_ref make_solution_screen()
{
  vs_table_ref rval     = vs_table::create();

  vs_label_ref l        = vs_label::create(L"");
  menu_tree_ref info_tree = solution_undo_tree::create();

  solution_examiner_ref examiner
    = solution_examiner::create(sigc::mem_fun(l.unsafe_get_ref(),
					      (void (vs_label::*) (fragment *)) &vs_label::set_text),
				sigc::bind(sigc::ptr_fun(update_dep_display),
					   info_tree.unsafe_get_ref()));


  examiner->connect_menu_signals();
  create_menu_bindings(info_tree.unsafe_get_ref(), info_tree);


  info_tree->connect_key("ShowHideDescription", &global_bindings,
			 sigc::mem_fun(info_tree.unsafe_get_ref(),
				       &vscreen_widget::toggle_visible));

  l->set_bg_style(get_style("Status"));

  rval->add_widget_opts(examiner,
			0, 0, 1, 1, vs_table::EXPAND | vs_table::FILL,
			vs_table::EXPAND | vs_table::FILL | vs_table::SHRINK);
  rval->add_widget_opts(l,
			1, 0, 1, 1, vs_table::EXPAND | vs_table::FILL,
			0);

  rval->add_widget_opts(info_tree,
			2, 0, 1, 1, vs_table::EXPAND | vs_table::FILL,
			vs_table::EXPAND | vs_table::FILL | vs_table::SHRINK);


  cache_reloaded.connect(sigc::bind(sigc::ptr_fun(&maybe_remove_examiner),
				    rval.weak_ref()));
  if(resman)
    resman->state_changed.connect(sigc::bind(sigc::ptr_fun(&maybe_remove_examiner),
					     rval.weak_ref()));


  l->show();
  examiner->show();
  info_tree->show();

  return rval;
}
