// pkgview.cc
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

#include "pkgview.h"
#include "aptitude.h"

#undef OK
#include <gtkmm.h>

#include <apt-pkg/error.h>
#include <apt-pkg/pkgsystem.h>
#include <apt-pkg/version.h>

#include <generic/apt/apt.h>
#include <generic/apt/apt_undo_group.h>
#include <generic/apt/matching/match.h>
#include <generic/apt/matching/parse.h>
#include <generic/apt/matching/pattern.h>

#include <solution_fragment.h>

#include <gtk/gui.h>
#include <gtk/info.h>
#include <gtk/progress.h>

#include <cwidget/generic/util/ssprintf.h>
#include <cwidget/generic/util/transcode.h>

namespace gui
{
  entity_state_info PkgEntity::current_state_columns()
  {
    pkgCache::VerIterator ver = get_ver();

    if(ver.end())
      return virtual_columns;

    if((*apt_cache_file)[pkg].NowBroken())
      return broken_columns;

    switch(pkg->CurrentState)
      {
      case pkgCache::State::NotInstalled:
        return not_installed_columns;
      case pkgCache::State::UnPacked:
        return unpacked_columns;
      case pkgCache::State::HalfConfigured:
        return half_configured_columns;
      case pkgCache::State::HalfInstalled:
        return half_installed_columns;
      case pkgCache::State::ConfigFiles:
        return config_files_columns;
  #ifdef APT_HAS_TRIGGERS
      case pkgCache::State::TriggersAwaited:
        return triggers_awaited_columns;
      case pkgCache::State::TriggersPending:
        return triggers_pending_columns;
  #endif
      case pkgCache::State::Installed:
        return installed_columns;
      default:
        return error_columns;
      }
  }

  entity_state_info PkgEntity::selected_package_state_columns()
  {
    aptitudeDepCache::StateCache &state=(*apt_cache_file)[pkg];
    aptitudeDepCache::aptitude_state &estate=(*apt_cache_file)->get_ext_state(pkg);
    pkgCache::VerIterator candver=state.CandidateVerIter(*apt_cache_file);

    if(state.Status != 2
       && (*apt_cache_file)->get_ext_state(pkg).selection_state
            == pkgCache::State::Hold && !state.InstBroken())
      return hold_columns;
    else if(state.Upgradable() && !pkg.CurrentVer().end() && !candver.end()
        && candver.VerStr() == estate.forbidver)
      return forbid_columns;
    else if(state.Delete())
      return ((state.iFlags & pkgDepCache::Purge) ? purge_columns : remove_columns);
    else if(state.InstBroken())
      return broken_columns;
    else if(state.NewInstall())
      return install_columns;
    else if(state.iFlags & pkgDepCache::ReInstall)
      return reinstall_columns;
    else if(state.Upgrade())
      {
	pkgCache::VerIterator currver = pkg.CurrentVer();
	pkgCache::VerIterator instver = state.CandidateVerIter(*apt_cache_file);

	if(_system->VS->CmpVersion(currver.VerStr(), instver.VerStr()) > 0)
	  return downgrade_columns;
	else
	  return upgrade_columns;
      }
    else
      return entity_state_info();
  }

  string PkgEntity::selected_package_state_color()
  {
    aptitudeDepCache::StateCache &state=(*apt_cache_file)[pkg];
    aptitudeDepCache::aptitude_state &estate=(*apt_cache_file)->get_ext_state(pkg);
    pkgCache::VerIterator candver=state.CandidateVerIter(*apt_cache_file);

    if (state.Status != 2
        && (*apt_cache_file)->get_ext_state(pkg).selection_state
            == pkgCache::State::Hold && !state.InstBroken())
      return "#FFCCCC";
    if (state.Upgradable() && !pkg.CurrentVer().end() && !candver.end()
        && candver.VerStr() == estate.forbidver)
      // FIXME: does this really deserve its own color?
      return "dark red";
    if (state.Delete())
      return ((state.iFlags & pkgDepCache::Purge) ? "#FFBBFF" : "#FFEEFF");
    if (state.InstBroken())
      return "#FFCCCC";
    if (state.NewInstall())
      return "#DDFFDD";
    if (state.Install() && (state.iFlags & pkgDepCache::ReInstall))
      return "#BBFFBB";
    if (state.Upgrade())
      return "#DDDDFF";
    return "white";
  }

  void PkgEntity::fill_row(const EntityColumns *cols, Gtk::TreeModel::Row &row)
  {
    using cwidget::util::ssprintf;
    using cwidget::util::transcode;

    aptitudeDepCache::StateCache &state=(*apt_cache_file)[pkg];

    pkgCache::VerIterator ver = get_ver();

    row[cols->EntObject] = this;

    string BgColor = selected_package_state_color();
    row[cols->BgColor] = BgColor;
    row[cols->BgSet] = (BgColor != "white");

    entity_state_info current_state(current_state_columns());
    entity_state_info selected_state(selected_package_state_columns());
    row[cols->CurrentStatusIcon] = current_state.get_icon().get_string();
    row[cols->SelectedStatusIcon] = selected_state.get_icon().get_string();
    row[cols->StatusDescriptionMarkup] =
      ssprintf("<b>%s:</b> %s\n<b>%s:</b> %s",
	       _("Current status"),
	       _(current_state.get_description().c_str()),
	       _("Selected status"),
	       _(selected_state.get_description().c_str()));

    Glib::ustring safe_name = Glib::Markup::escape_text(pkg.Name());
    if(ver.end())
      {
	if(pkg.VersionList().end() && !pkg.ProvidesList().end())
	  row[cols->NameMarkup] = ssprintf("<i><b>%s</b></i>\n<span size=\"smaller\"><i>Virtual package</i></span>",
					   safe_name.c_str());
	else
	  row[cols->NameMarkup] = ssprintf("<b>%s</b>", safe_name.c_str());

	row[cols->Description] = "";
      }
    else
      {
	std::string description(transcode(get_short_description(ver,
								apt_package_records),
					  "UTF-8"));
        Glib::ustring safe_description =
          Glib::Markup::escape_text(description);
        row[cols->NameMarkup] =
          ssprintf("<b>%s</b>\n<span size=\"smaller\">%s</span>",
                   safe_name.c_str(), safe_description.c_str());

	row[cols->Description] = description;
      }

    if (!ver.end())
    {
      row[cols->VersionMarkup] = Glib::Markup::escape_text(ver.VerStr());
      pkgCache::VerIterator candver=state.CandidateVerIter(*apt_cache_file);
      if (state.Upgrade() || state.Downgrade())
        row[cols->VersionMarkup] = row[cols->VersionMarkup] + "\n<i>" + Glib::Markup::escape_text(candver.VerStr()) + "</i>";
      row[cols->ArchiveMarkup] = archives_text(ver);
    }
    else
    {
      row[cols->VersionMarkup] = "";
      row[cols->ArchiveMarkup] = "";
    }

    row[cols->Name] = pkg.end() ? "" : pkg.Name();
    row[cols->Version] = ver.end() ? "" : ver.VerStr();
    row[cols->Archive] = ver.end() ? "" : archives_text(ver);

    {
      const bool is_auto = (state.Flags & pkgCache::Flag::Auto) != 0;
      const bool is_installed = (!pkg.CurrentVer().end() &&
				 pkg->CurrentState != pkgCache::State::ConfigFiles);
      if(is_auto)
	{
	  if(is_installed)
	    row[cols->AutomaticallyInstalledTooltip] = ssprintf(_("%s was installed automatically."),
								pkg.Name());
	  else if(state.Install())
	    row[cols->AutomaticallyInstalledTooltip] = ssprintf(_("%s is being installed automatically."),
								pkg.Name());
	  else
	    row[cols->AutomaticallyInstalledTooltip] = "";
	}
      else
	{
	  if(is_installed)
	    row[cols->AutomaticallyInstalledTooltip] = ssprintf(_("%s was installed manually."),
								pkg.Name());
	  else if(state.Install())
	    row[cols->AutomaticallyInstalledTooltip] = ssprintf(_("%s is being installed manually."),
								pkg.Name());
	  else
	    row[cols->AutomaticallyInstalledTooltip] = "";
	}
      row[cols->AutomaticallyInstalled] =
	is_auto && (is_installed || state.Install());
    }
    row[cols->AutomaticallyInstalledVisible] = true;
  }

  void PkgEntity::activated(const Gtk::TreeModel::Path &path,
			    const Gtk::TreeViewColumn *column,
			    const EntityView *view)
  {
    InfoTab::show_tab(get_pkg(), get_ver());
  }

  void PkgEntity::add_packages(std::set<pkgCache::PkgIterator> &packages)
  {
    packages.insert(pkg);
  }

  void PkgEntity::add_actions(std::set<PackagesAction> &actions)
  {
    // Defensiveness.
    if(pkg.end())
      return;

    pkgDepCache::StateCache state = (*apt_cache_file)[pkg];

    if(state.Status == 2 && !state.Install())
      actions.insert(Install);

    if(state.Status == 1 && !state.Install())
      actions.insert(Upgrade);

    if(state.Status == -1 && !state.Install())
      actions.insert(Downgrade);

    if(state.Status != 2 && !(state.Delete() &&
                              ((state.iFlags & pkgDepCache::Purge) == 0)))
      actions.insert(Remove);

    if((state.Status != 2 ||
        (state.Status == 2 && pkg->CurrentState == pkgCache::State::ConfigFiles)) &&
       !(state.Delete() &&
         ((state.iFlags & pkgDepCache::Purge) != 0)))
      actions.insert(Purge);

    if(!state.Keep())
      actions.insert(Keep);

    if((*apt_cache_file)->get_ext_state(pkg).selection_state == pkgCache::State::Hold)
      actions.insert(Keep);
    else
      actions.insert(Hold);

    if(state.Status != 2)
      {
	if(state.Flags & pkgCache::Flag::Auto)
	  actions.insert(MakeManual);
	else
	  actions.insert(MakeAutomatic);
      }
  }

  void PkgEntity::dispatch_action(PackagesAction action, bool first_pass)
  {
    undo_group *undo = new undo_group;
    pkgCache::VerIterator ver = get_ver();
    if (!ver.end())
    {
      switch(action)
      {
      case Install:
      case Upgrade:
      case Downgrade:
        (*apt_cache_file)->mark_install(pkg, !first_pass, false, undo);
        break;
      case Remove:
        (*apt_cache_file)->mark_delete(pkg, false, false, undo);
        break;
      case Purge:
        (*apt_cache_file)->mark_delete(pkg, true, false, undo);
        break;
      case Keep:
        (*apt_cache_file)->mark_keep(pkg, false, false, undo);
        break;
      case Hold:
        (*apt_cache_file)->mark_delete(pkg, false, true, undo);
        break;
      case MakeAutomatic:
	(*apt_cache_file)->mark_auto_installed(pkg, true, undo);
	break;
      case MakeManual:
	(*apt_cache_file)->mark_auto_installed(pkg, false, undo);
	break;
      default:
        break;
      }
    }

    if(undo->empty())
      delete undo;
    else
      apt_undos->add_item(undo);
  }

  pkgCache::VerIterator PkgEntity::get_ver(const pkgCache::PkgIterator &pkg)
  {
    pkgCache::VerIterator ver = (*apt_cache_file)[pkg].CandidateVerIter(*apt_cache_file);
    if(ver.end())
      ver = pkg.CurrentVer();

    if(ver.end())
      ver = pkg.VersionList();

    return ver;
  }

  pkgCache::VerIterator PkgEntity::get_ver() const
  {
    return get_ver(pkg);
  }

  PkgTreeModelGenerator::~PkgTreeModelGenerator()
  {
  }

  PkgViewBase::PkgViewBase(const sigc::slot1<PkgTreeModelGenerator *, const EntityColumns *> _generatorK,
			   const Glib::RefPtr<Gnome::Glade::Xml> &refGlade,
			   const Glib::ustring &gladename,
			   const Glib::ustring &parent_title,
			   const Glib::ustring &_limit)
    : EntityView(refGlade, gladename, parent_title)
  {
    generatorK = _generatorK;
    limit = aptitude::matching::parse(_limit);
    cache_closed.connect(sigc::mem_fun(*this, &PkgViewBase::do_cache_closed));
    cache_reloaded.connect(sigc::mem_fun(*this, &PkgViewBase::rebuild_store));

    get_version_column()->set_visible(false);
    get_archive_column()->set_visible(false);
  }

  PkgViewBase::~PkgViewBase()
  {
  }

  void PkgViewBase::do_cache_closed()
  {
    Glib::RefPtr<Gtk::ListStore> store = Gtk::ListStore::create(*get_columns());
    Gtk::TreeModel::iterator iter = store->append();
    Gtk::TreeModel::Row row = *iter;
    (new HeaderEntity(_("Cache reloading, please wait...")))->fill_row(get_columns(), row);
    set_model(store);
  }

  void PkgViewBase::rebuild_store()
  {
    using namespace aptitude::matching;
    using cwidget::util::ref_ptr;

    if(apt_cache_file == NULL)
      return; // We'll try again when it's loaded.

    store_reloading();
    std::auto_ptr<PkgTreeModelGenerator> generator(generatorK(get_columns()));

    cwidget::util::ref_ptr<guiOpProgress> p =
      guiOpProgress::create();

    bool limited = limit.valid();

    std::vector<std::pair<pkgCache::PkgIterator, ref_ptr<structural_match> > > matches;
    ref_ptr<search_cache> search_info(search_cache::create());
    if(limited)
      {
	search(limit, search_info, matches, *apt_cache_file, *apt_package_records);

	int num = 0;
	const int total = static_cast<int>(matches.size());

	for(std::vector<std::pair<pkgCache::PkgIterator, ref_ptr<structural_match> > >::const_iterator
	      it = matches.begin(); it != matches.end(); ++it)
	  {
	    p->OverallProgress(num, total, 1, _("Building view"));

	    ++num;
	    generator->add(it->first);
	  }

	p->OverallProgress(total, total, 1,  _("Finalizing view"));
      }
    else
      {
	int num = 0;
	const int total = (int)(*apt_cache_file)->Head().PackageCount;

	for(pkgCache::PkgIterator pkg = (*apt_cache_file)->PkgBegin();
	    !pkg.end(); ++pkg)
	  {
	    p->OverallProgress(num, total, 1, _("Building view"));

	    ++num;
	    generator->add(pkg);
	  }

	p->OverallProgress(total, total, 1, _("Finalizing view"));
      }

    Glib::Thread * sort_thread = Glib::Thread::create(sigc::mem_fun(*generator, &PkgTreeModelGenerator::finish), true);
    while(!generator->finished)
    {
      pMainWindow->get_progress_bar()->pulse();
      gtk_update();
      Glib::usleep(100000);
    }
    sort_thread->join();

    // Erase our reference to the progress bar (it should be deleted
    // when we do).
    p = NULL;

    set_model(generator->get_model());
    store_reloaded();
  }

  void PkgViewBase::set_limit(const cwidget::util::ref_ptr<aptitude::matching::pattern> &_limit)
  {
    limit = _limit;
    rebuild_store();
  }

  PkgView::Generator::Generator(const EntityColumns *_columns)
    : columns(_columns)
  {
    store = Gtk::ListStore::create(*columns);
  }

  PkgView::Generator *PkgView::Generator::create(const EntityColumns *columns)
  {
    return new Generator(columns);
  }

  void PkgView::Generator::add(const pkgCache::PkgIterator &pkg)
  {
    Gtk::TreeModel::iterator iter = store->append();
    Gtk::TreeModel::Row row = *iter;
    PkgEntity *ent = new PkgEntity(pkg);
    ent->fill_row(columns, row);
  }

  void PkgView::Generator::finish()
  {
    store->set_sort_column(columns->Name, Gtk::SORT_ASCENDING);
    // FIXME: Hack while finding a nonblocking thread join.
    finished = true;
  }

  Glib::RefPtr<Gtk::TreeModel> PkgView::Generator::get_model()
  {
    return store;
  }

  PkgView::PkgView(const Glib::RefPtr<Gnome::Glade::Xml> &refGlade,
		   const Glib::ustring &gladename,
		   const Glib::ustring &parent_title,
		   const Glib::ustring &limit)
    : PkgViewBase(sigc::ptr_fun(&Generator::create),
		  refGlade,
		  gladename,
		  parent_title,
		  limit)
  {
  }
}
