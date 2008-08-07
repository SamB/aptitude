// -*-c++-*-

// info.h
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

#include "info.h"
#include "aptitude.h"

#undef OK
#include <gtkmm.h>

#include <cwidget/generic/util/ssprintf.h>

#include <apt-pkg/pkgcache.h>
#include <apt-pkg/pkgsystem.h>
#include <apt-pkg/version.h>

#include <generic/util/undo.h>

#include <gtk/gui.h>
#include <gtk/entityview.h>
#include <gtk/packageinformation.h>

namespace gui
{
  namespace
  {
    class VersionEntity : public Entity
    {
    private:
      pkgCache::VerIterator ver;

      string current_state_string()
      {
        pkgCache::PkgIterator pkg = ver.ParentPkg();

        if(!ver.end() && ver != pkg.CurrentVer())
          return "p";

        switch(pkg->CurrentState)
          {
          case pkgCache::State::NotInstalled:
            return "p";
          case pkgCache::State::UnPacked:
            return "u";
          case pkgCache::State::HalfConfigured:
            return "C";
          case pkgCache::State::HalfInstalled:
            return "H";
          case pkgCache::State::ConfigFiles:
            return "c";
      #ifdef APT_HAS_TRIGGERS
          case pkgCache::State::TriggersAwaited:
            return "W";
          case pkgCache::State::TriggersPending:
            return "T";
      #endif
          case pkgCache::State::Installed:
            return "i";
          default:
            return "E";
          }
      }

      // We output a flag/color pair
      // TODO: choose more sensible colors?
      std::pair<string,string> action_strings()
      {
        if(ver.end())
          return std::make_pair("","white");

        pkgCache::PkgIterator pkg = ver.ParentPkg();
        aptitudeDepCache::StateCache &state = (*apt_cache_file)[pkg];
        aptitudeDepCache::aptitude_state &estate = (*apt_cache_file)->get_ext_state(pkg);
        pkgCache::VerIterator candver = state.CandidateVerIter(*apt_cache_file);

        if(state.Status!=2 && estate.selection_state == pkgCache::State::Hold && !state.NowBroken())
          return std::make_pair("h","gray");
        else if(ver.VerStr() == estate.forbidver)
          return std::make_pair("F","dim gray");
        else if(state.Delete())
          return (state.iFlags&pkgDepCache::Purge)?std::make_pair("p","sky blue"):std::make_pair("d","dark turquoise");
        else if(state.InstBroken() && state.InstVerIter(*apt_cache_file) == ver)
          return std::make_pair("B","orange red");
        else if(state.NewInstall())
          {
            if(candver==ver)
              return std::make_pair("i","green");
            else
              return std::make_pair("","white");
          }
        else if(state.iFlags & pkgDepCache::ReInstall)
          {
            if(ver.ParentPkg().CurrentVer() == ver)
              return std::make_pair("i","yellow green");
            else
              return std::make_pair("","white");
          }
        else if(state.Upgrade())
          {
            if(ver.ParentPkg().CurrentVer() == ver)
              return std::make_pair("d","orange");
            else if(candver == ver)
              return std::make_pair("i","green yellow");
            else
              return std::make_pair("","white");
          }
        else
          return std::make_pair("","white");
      }

    public:
      VersionEntity(const pkgCache::VerIterator &_ver)
	: ver(_ver)
      {
      }

      const pkgCache::VerIterator &get_ver() const { return ver; }

      void fill_row(const EntityColumns *columns, Gtk::TreeModel::Row &row)
      {
	row[columns->EntObject] = this;
        std::pair<string,string> action_strings_ = action_strings();
	row[columns->BgSet] = (action_strings_.second != "white");
	row[columns->BgColor] = action_strings_.second;
	row[columns->Status] = current_state_string()+action_strings_.first;
	row[columns->Name] = ver.ParentPkg().Name();
	row[columns->Version] = ver.VerStr();
      }

      void add_packages(std::set<pkgCache::PkgIterator> &packages)
      {
	packages.insert(ver.ParentPkg());
      }

      void activated(const Gtk::TreeModel::Path &path,
		     const Gtk::TreeViewColumn *column,
		     const EntityView *view)
      {
	InfoTab::show_tab(ver.ParentPkg(), ver);
      }

      void add_actions(std::set<PackagesAction> &actions)
      {
        // This is all highly hackish
        if(ver==ver.ParentPkg().CurrentVer())
          {
          actions.insert(Hold);
          actions.insert(Purge);
            if(!(*apt_cache_file)[ver.ParentPkg()].Keep())
              actions.insert(Keep);

            if((*apt_cache_file)[ver.ParentPkg()].iFlags&pkgDepCache::ReInstall)
              actions.insert(Keep);
            else
              actions.insert(Remove);
          }
        else if(ver==(*apt_cache_file)[ver.ParentPkg()].CandidateVerIter(*apt_cache_file) && (*apt_cache_file)[ver.ParentPkg()].Install())
          actions.insert(Keep);
        else
          {
          if(!(*apt_cache_file)[ver.ParentPkg()].Keep())
            actions.insert(Keep);
          // TODO: what about downgrade ?
          actions.insert(Upgrade);
          }
      }

      void dispatch_action(PackagesAction action)
      {
        undo_group *undo = new undo_group;
        switch(action)
        {
        case Install:
        case Upgrade:
        case Downgrade:
          (*apt_cache_file)->set_candidate_version(ver, undo);
          (*apt_cache_file)->mark_install(ver.ParentPkg(), true, false, undo);
          break;
        case Remove:
          (*apt_cache_file)->mark_delete(ver.ParentPkg(), false, false, undo);
          break;
        case Purge:
          (*apt_cache_file)->mark_delete(ver.ParentPkg(), true, false, undo);
          break;
        case Keep:
          (*apt_cache_file)->mark_keep(ver.ParentPkg(), false, false, undo);
          break;
        case Hold:
          (*apt_cache_file)->mark_delete(ver.ParentPkg(), false, (*apt_cache_file)->get_ext_state(ver.ParentPkg()).selection_state!=pkgCache::State::Hold, undo);
          break;
        default:
          break;
        }
        if(undo->empty())
          delete undo;
        else
          apt_undos->add_item(undo);
      }
    };

    class NotAvailableEntity : public Entity
    {
      Glib::ustring text;
    public:
      NotAvailableEntity(const Glib::ustring &_text) : text(_text) { }

      void fill_row(const EntityColumns *columns, Gtk::TreeModel::Row &row)
      {
	row[columns->EntObject] = EntityRef(this);

	row[columns->BgSet] = true;
	row[columns->BgColor] = "#FFA0A0";
	row[columns->Status] = "";
	row[columns->Name] = text;
	row[columns->Version] = "";
      }

      void add_packages(std::set<pkgCache::PkgIterator> &packages)
      {
      }

      void activated(const Gtk::TreeModel::Path &path,
		     const Gtk::TreeViewColumn *column,
		     const EntityView *view)
      {
      }

      void add_actions(std::set<PackagesAction> &actions)
      {
      }

      void dispatch_action(PackagesAction action)
      {
      }
    };

    class DependencyEntity : public HeaderEntity
    {
    public:
      DependencyEntity(pkgCache::DepIterator dep)
	: HeaderEntity("")
      {
	Glib::ustring text = dep.DepType();
	text += ": ";
	do
	  {
	    text += dep.TargetPkg().Name();
	    if((dep->CompareOp & (~pkgCache::Dep::Or)) != pkgCache::Dep::NoOp &&
	       dep.TargetVer() != NULL)
	      {
		text += " (";
		text += dep.CompType();
		text += " ";
		text += dep.TargetVer();
		text += ")";
	      }

	    if(dep->CompareOp & pkgCache::Dep::Or)
	      text += " | ";

	    ++dep;
	  } while(dep->CompareOp & pkgCache::Dep::Or);

	set_text(text);
      }

      void fill_row(const EntityColumns *columns, Gtk::TreeModel::Row &row)
      {
	HeaderEntity::fill_row(columns, row);
	row[columns->Status] = "\t"+cwidget::util::transcode(L"\x2022 ");
	// TODO: set the color according to whether the dependency is
	// broken.  Need backend support for detecting changes to
	// dependency state first.
      }
    };

    class DependencyResolverEntity : public HeaderEntity
    {
      Glib::ustring version_text;

    public:
      DependencyResolverEntity(const Glib::ustring &package_text,
			       const Glib::ustring &_version_text)
	: HeaderEntity(package_text), version_text(_version_text)
      {
      }

      void fill_row(const EntityColumns *columns, Gtk::TreeModel::Row &row)
      {
	HeaderEntity::fill_row(columns, row);
	row[columns->Status] = "\t-";
	row[columns->Version] = version_text;
      }
    };

    // Build a tree of dependencies for the given package version.
    Glib::RefPtr<Gtk::TreeModel> make_depends_tree(const EntityColumns *columns,
						   const pkgCache::VerIterator &ver)
    {
      Glib::RefPtr<Gtk::TreeStore> store = Gtk::TreeStore::create(*columns);

      if(ver.end())
	return store;

      pkgCache::DepIterator dep = ver.DependsList();
      while(!dep.end())
      {
        pkgCache::DepIterator start, end;
        surrounding_or(dep, start, end);
        bool first = true;

        Gtk::TreeModel::iterator tree;
        Gtk::TreeModel::Row row;

        for(pkgCache::DepIterator todisp = start;
	    todisp != end; ++todisp)
        {
          Gtk::TreeModel::iterator tree2;
          Gtk::TreeModel::Row row2;

          const bool is_or_continuation = !first;
          first = false;

          if(!is_or_continuation)
            {
              tree = store->append();
              row = *tree;
	      Entity *ent = new DependencyEntity(todisp);
	      ent->fill_row(columns, row);
            }

          tree2 = store->append(tree->children());
          row2 = *tree2;

	  Glib::ustring version_text;

          if(todisp->CompareOp != pkgCache::Dep::NoOp &&
              todisp.TargetVer() != NULL)
          {
            version_text = Glib::Markup::escape_text(Glib::ustring(todisp.CompType())+" "+Glib::ustring(todisp.TargetVer()));
          }
          else
          {
            version_text = "N/A";
          }

	  (new DependencyResolverEntity(todisp.TargetPkg().Name(),
					version_text))->fill_row(columns, row2);

          bool resolvable = false;

          // Insert the various resolutions of this dep.  First direct
          // resolutions:
          {
            std::vector<pkgCache::VerIterator> direct_resolutions;
            for(pkgCache::VerIterator ver = todisp.TargetPkg().VersionList();
            !ver.end(); ++ver)
            {
              if(_system->VS->CheckDep(ver.VerStr(), todisp->CompareOp, todisp.TargetVer()))
              {
                resolvable = true;
                direct_resolutions.push_back(ver);
              }
            }

            if(!direct_resolutions.empty())
            {
              for(std::vector<pkgCache::VerIterator>::const_iterator it = direct_resolutions.begin();
              it != direct_resolutions.end(); ++it)
              {
                Gtk::TreeModel::iterator tree3 = store->append(tree2->children());
                Gtk::TreeModel::Row row3 = *tree3;
		(new VersionEntity(*it))->fill_row(columns, row3);
              }
            }
          }

          // Check for resolutions through virtual deps.
          {
            std::vector<pkgCache::VerIterator> virtual_resolutions;

            for(pkgCache::PrvIterator prv = todisp.TargetPkg().ProvidesList();
            !prv.end(); ++prv)
            {
              if(_system->VS->CheckDep(prv.ProvideVersion(), todisp->CompareOp, todisp.TargetVer()))
              {
                resolvable = true;
                virtual_resolutions.push_back(prv.OwnerVer());
              }
            }


            if(!virtual_resolutions.empty())
            {
              for(std::vector<pkgCache::VerIterator>::const_iterator it = virtual_resolutions.begin();
              it != virtual_resolutions.end(); ++it)
              {
                Gtk::TreeModel::iterator tree3 = store->append(tree2->children());
                Gtk::TreeModel::Row row3 = *tree3;
		(new VersionEntity(*it))->fill_row(columns, row3);
              }
            }
          }

          if(!resolvable)
          {
            Gtk::TreeModel::iterator tree3 = store->append(tree2->children());
            Gtk::TreeModel::Row row3 = *tree3;

	    (new NotAvailableEntity(_("Not available")))->fill_row(columns, row3);
          }
        }

	dep = end;
      }

      store->set_sort_column(columns->Version, Gtk::SORT_ASCENDING);
      return store;
    }

    Glib::RefPtr<Gtk::TreeModel> make_version_list(const EntityColumns *columns,
						   const pkgCache::PkgIterator &pkg)
    {
      Glib::RefPtr<Gtk::ListStore> store = Gtk::ListStore::create(*columns);

      for (pkgCache::VerIterator ver = pkg.VersionList(); ver.end() == false; ver++)
      {
        Gtk::TreeModel::iterator iter = store->append();
        Gtk::TreeModel::Row row = *iter;

	(new VersionEntity(ver))->fill_row(columns, row);
      }

      // \todo Sort according to version number, not according to
      // version string. (see apt-pkg/version.h)
      store->set_sort_column(columns->Version, Gtk::SORT_ASCENDING);
      return store;
    }
  }

  InfoTab::InfoTab(const Glib::ustring &label)
  : Tab(Info, label, Gnome::Glade::Xml::create(glade_main_file, "main_info_hpaned"), "main_info_hpaned")
  {
    get_xml()->get_widget("main_info_textview", textview);
    get_widget()->show();
  }

  void InfoTab::disp_package(pkgCache::PkgIterator pkg, pkgCache::VerIterator ver)
  {
    Glib::RefPtr<Gtk::TextBuffer> textBuffer = textview->get_buffer();
    set_label("Info: " + Glib::ustring(pkg.Name()));

    PackageInformation info(pkg, ver);

    Glib::RefPtr<Gtk::TextBuffer::Tag> nameTag = textBuffer->create_tag();
    nameTag->property_size() = 20 * Pango::SCALE;

    Glib::RefPtr<Gtk::TextBuffer::Tag> fieldNameTag = textBuffer->create_tag();
    fieldNameTag->property_weight() = 2 * Pango::SCALE;

    textBuffer->insert_with_tag(textBuffer->end(),
        info.Name(),
        nameTag);

    textBuffer->insert_with_tag(textBuffer->end(),
        " ",
        nameTag);


    textBuffer->insert_with_tag(textBuffer->end(),
        info.Version(),
        nameTag);

    textBuffer->insert(textBuffer->end(), "\n");
    textBuffer->insert(textBuffer->end(), info.ShortDescription());
    textBuffer->insert(textBuffer->end(), "\n");

    // TODO: insert a horizontal rule here (how?)

    textBuffer->insert(textBuffer->end(), "\n");

    //pkgRecords::Parser &rec=apt_package_records->Lookup(ver.FileList());

    textBuffer->insert(textBuffer->end(), "\n");

    std::wstring longdesc = get_long_description(ver, apt_package_records);

    textBuffer->insert_with_tag(textBuffer->end(), _("Description: "), fieldNameTag);

    textBuffer->insert(textBuffer->end(), info.LongDescription());

    using cwidget::util::ref_ptr;
    pVersionsView = ref_ptr<EntityView>(new EntityView(get_xml(),
						       "main_info_versionsview"));
    pVersionsView->set_model(make_version_list(pVersionsView->get_columns(), pkg));
    pVersionsView->get_treeview()->get_column(1)->set_fixed_width(154);
    pVersionsView->get_treeview()->get_selection()->set_mode(Gtk::SELECTION_BROWSE);
    {
      Gtk::TreeModel::Children entries = pVersionsView->get_treeview()->get_model()->children();
      for(Gtk::TreeModel::Children::const_iterator it = entries.begin();
	  it != entries.end(); ++it)
	{
	  using cwidget::util::ref_ptr;
	  ref_ptr<Entity> ent = (*it)[pVersionsView->get_columns()->EntObject];
	  ref_ptr<VersionEntity> ver_ent = ent.dyn_downcast<VersionEntity>();
          if(ver_ent.valid() && ver == ver_ent->get_ver())
	    pVersionsView->get_treeview()->get_selection()->select(it);
	}
    }

    pDependsView = ref_ptr<EntityView>(new EntityView(get_xml(), "main_info_dependsview"));
    pDependsView->set_model(make_depends_tree(pDependsView->get_columns(), ver));
    pDependsView->get_treeview()->get_column(0)->set_fixed_width(80);
    pDependsView->get_treeview()->get_column(1)->set_fixed_width(280);
    Gtk::TreeModel::Children dependsChildren = pDependsView->get_treeview()->get_model()->children();
    for(Gtk::TreeModel::iterator it = dependsChildren.begin();
	it != dependsChildren.end(); ++it)
      {
	// Expand all the top-level entries, which we magically know
	// contain the individual components of a dependency.
	Gtk::TreeModel::Path path = pDependsView->get_treeview()->get_model()->get_path(it);
	pDependsView->get_treeview()->expand_row(path, false);
      }
  }

  void InfoTab::show_tab(const pkgCache::PkgIterator &pkg,
			 const pkgCache::VerIterator &ver)
  {
    InfoTab * infotab = new InfoTab(_("Info:"));
    tab_add(infotab);
    infotab->disp_package(pkg, ver);
  }
}
