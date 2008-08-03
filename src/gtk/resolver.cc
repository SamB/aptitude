// resolver.cc
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


#include "resolver.h"
#include "aptitude.h"

#undef OK
#include <gtkmm.h>
#include <libglademm/xml.h>

#include <generic/apt/apt_undo_group.h>
#include <generic/problemresolver/exceptions.h>

#include <gui.h>

namespace gui
{
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
    createstore();
    append_column(Glib::ustring(_("Name")), Name, resolver_columns.Name, 200);
    append_column(Glib::ustring(_("Action")), Section, resolver_columns.Action, 200);
  }

  void ResolverView::createstore()
  {
    resolver_store = Gtk::TreeStore::create(resolver_columns);
    set_model(resolver_store);
    set_search_column(resolver_columns.Name);
  }

  ResolverTab::ResolverTab(const Glib::ustring &label) :
    Tab(Resolver, label, Gnome::Glade::Xml::create(glade_main_file, "main_resolver_vbox"), "main_resolver_vbox")
  {
    get_xml()->get_widget("main_resolver_status", pResolverStatus);
    get_xml()->get_widget("main_resolver_previous", pResolverPrevious);
    pResolverPrevious->signal_clicked().connect(sigc::mem_fun(*this, &ResolverTab::do_previous_solution));
    get_xml()->get_widget("main_resolver_next", pResolverNext);
    pResolverNext->signal_clicked().connect(sigc::mem_fun(*this, &ResolverTab::do_next_solution));
    get_xml()->get_widget("main_resolver_apply", pResolverApply);
    pResolverApply->signal_clicked().connect(sigc::mem_fun(*this, &ResolverTab::do_apply_solution));

    get_xml()->get_widget_derived("main_resolver_treeview", pResolverView);
    state = resman->state_snapshot();
    if (state.resolver_exists && state.selected_solution >= 0)
      {
      sol = resman->get_solution(resman->get_selected_solution(), 5000);
      repopulate_model();
      }

    get_widget()->show();
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

  void ResolverTab::repopulate_model()
  {
    // Bin packages according to what will happen to them.
    vector<pkgCache::PkgIterator> remove_packages;
    vector<pkgCache::PkgIterator> keep_packages;
    vector<pkgCache::VerIterator> install_packages;
    vector<pkgCache::VerIterator> downgrade_packages;
    vector<pkgCache::VerIterator> upgrade_packages;

    for(imm::map<aptitude_universe::package,
          generic_solution<aptitude_universe>::action>::const_iterator i=sol.get_actions().begin();
        i!=sol.get_actions().end(); ++i)
      {
        pkgCache::PkgIterator pkg=i->first.get_pkg();
        pkgCache::VerIterator curver=pkg.CurrentVer();
        pkgCache::VerIterator newver=i->second.ver.get_ver();

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

    sort(remove_packages.begin(), remove_packages.end(), pkg_name_lt());
    sort(keep_packages.begin(), keep_packages.end(), pkg_name_lt());
    sort(install_packages.begin(), install_packages.end(), ver_name_lt());
    sort(downgrade_packages.begin(), downgrade_packages.end(), ver_name_lt());
    sort(upgrade_packages.begin(), upgrade_packages.end(), ver_name_lt());

    pResolverView->resolver_store->clear();

    if(!remove_packages.empty())
      {
        Gtk::TreeModel::iterator parent_iter = pResolverView->resolver_store->append();
        Gtk::TreeModel::Row parent_row = *parent_iter;
        parent_row[pResolverView->resolver_columns.Name] = _("Remove the following packages:");
        for(vector<pkgCache::PkgIterator>::const_iterator i=remove_packages.begin();
            i!=remove_packages.end(); ++i)
        {
          Gtk::TreeModel::iterator iter = pResolverView->resolver_store->append(parent_row.children());
          Gtk::TreeModel::Row row = *iter;
          row[pResolverView->resolver_columns.Name] = i->Name();
          row[pResolverView->resolver_columns.Action] = "";
        }
      }

    if(!install_packages.empty())
      {
        Gtk::TreeModel::iterator parent_iter = pResolverView->resolver_store->append();
        Gtk::TreeModel::Row parent_row = *parent_iter;
        parent_row[pResolverView->resolver_columns.Name] = _("Install the following packages:");
        for(vector<pkgCache::VerIterator>::const_iterator i=install_packages.begin();
            i!=install_packages.end(); ++i)
        {
          Gtk::TreeModel::iterator iter = pResolverView->resolver_store->append(parent_row.children());
          Gtk::TreeModel::Row row = *iter;
          row[pResolverView->resolver_columns.Name] = i->ParentPkg().Name();
          row[pResolverView->resolver_columns.Action] = ssprintf("[%s (%s)]",
              i->VerStr(),
              archives_text(*i).c_str());
        }
      }

    if(!keep_packages.empty())
      {
        Gtk::TreeModel::iterator parent_iter = pResolverView->resolver_store->append();
        Gtk::TreeModel::Row parent_row = *parent_iter;
        parent_row[pResolverView->resolver_columns.Name] = _("Keep the following packages:");
        for(vector<pkgCache::PkgIterator>::const_iterator i=keep_packages.begin();
            i!=keep_packages.end(); ++i)
          {
            Gtk::TreeModel::iterator iter = pResolverView->resolver_store->append(parent_row.children());
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
        Gtk::TreeModel::iterator parent_iter = pResolverView->resolver_store->append();
        Gtk::TreeModel::Row parent_row = *parent_iter;
        parent_row[pResolverView->resolver_columns.Name] = _("Upgrade the following packages:");
        for(vector<pkgCache::VerIterator>::const_iterator i=upgrade_packages.begin();
            i!=upgrade_packages.end(); ++i)
        {
          Gtk::TreeModel::iterator iter = pResolverView->resolver_store->append(parent_row.children());
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
        Gtk::TreeModel::iterator parent_iter = pResolverView->resolver_store->append();
        Gtk::TreeModel::Row parent_row = *parent_iter;
        parent_row[pResolverView->resolver_columns.Name] = _("Downgrade the following packages:");
        for(vector<pkgCache::VerIterator>::const_iterator i=downgrade_packages.begin();
            i!=downgrade_packages.end(); ++i)
        {
          Gtk::TreeModel::iterator iter = pResolverView->resolver_store->append(parent_row.children());
          Gtk::TreeModel::Row row = *iter;
          row[pResolverView->resolver_columns.Name] = i->ParentPkg().Name();
          row[pResolverView->resolver_columns.Action] = ssprintf("[%s (%s) -> %s (%s)]",
              i->ParentPkg().CurrentVer().VerStr(),
              archives_text(i->ParentPkg().CurrentVer()).c_str(),
              i->VerStr(),
              archives_text(*i).c_str());
        }
      }

    const imm::set<aptitude_universe::dep> &unresolved = sol.get_unresolved_soft_deps();

    if(!unresolved.empty())
      {
      Gtk::TreeModel::iterator parent_iter = pResolverView->resolver_store->append();
      Gtk::TreeModel::Row parent_row = *parent_iter;
      parent_row[pResolverView->resolver_columns.Name] = _("Leave the following dependencies unresolved:");
        for(imm::set<aptitude_universe::dep>::const_iterator i = unresolved.begin();
            i != unresolved.end(); ++i)
        {
          Gtk::TreeModel::iterator iter = pResolverView->resolver_store->append(parent_row.children());
          Gtk::TreeModel::Row row = *iter;
          row[pResolverView->resolver_columns.Name] = cwidget::util::transcode(dep_text((*i).get_dep()).c_str(), "UTF-8");
          row[pResolverView->resolver_columns.Action] = "";
        }
      }

    pResolverView->expand_all();
    state = resman->state_snapshot();
    pResolverStatus->set_text(ssprintf("solution %d of %d (score: %d)", state.selected_solution + 1, state.generated_solutions, sol.get_score()));
  }

  bool ResolverTab::do_previous_solution_enabled()
  {
    if (resman == NULL)
      return false;

    state = resman->state_snapshot();

    return state.selected_solution > 0;
  }

  void ResolverTab::do_previous_solution()
  {
    if (!do_previous_solution_enabled())
      //beep();
      std::cout << "beep!" << std::endl;
    else
      resman->select_previous_solution();
      std::cout << "Resolver: previous selected" << std::endl;
      state = resman->state_snapshot();
      sol = resman->get_solution(resman->get_selected_solution(), 5000);
      repopulate_model();
  }

  bool ResolverTab::do_next_solution_enabled()
  {
    if (resman == NULL)
      return false;

    state = resman->state_snapshot();

    return state.selected_solution < state.generated_solutions && !(state.selected_solution + 1
        == state.generated_solutions && state.solutions_exhausted);
  }

  void ResolverTab::do_next_solution()
  {
    if (!do_next_solution_enabled())
      //beep();
      std::cout << "beep!" << std::endl;
    else
    {
      // If an error was encountered, pressing "next solution"
      // skips it.
      resman->discard_error_information();
      resman->select_next_solution();
      std::cout << "Resolver: next selected" << std::endl;
      sol = resman->get_solution(resman->get_selected_solution(), 5000);
      repopulate_model();
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
    if (!apt_cache_file)
      return;

    state = resman->state_snapshot();

    if (!do_apply_solution_enabled_from_state(state))
    {
      //beep();
      std::cout << "beep!" << std::endl;
      return;
    }
    else
    {
      undo_group *undo = new apt_undo_group;
      try
      {
	aptitudeDepCache::action_group group(*apt_cache_file, NULL);
	(*apt_cache_file)->apply_solution(resman->get_solution(resman->get_selected_solution(), 5000), undo);
	pResolverView->resolver_store->clear();
        std::cout << "Resolver: selected solution applied" << std::endl;
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
    }
  }

}
