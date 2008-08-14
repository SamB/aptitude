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
#include <generic/apt/matchers.h>

#include <gtk/gui.h>
#include <gtk/info.h>
#include <gtk/progress.h>

#include <cwidget/generic/util/ssprintf.h>
#include <cwidget/generic/util/transcode.h>

namespace gui
{
  std::pair<std::string, Gtk::StockID> PkgEntity::current_state_columns()
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

  std::pair<std::string, Gtk::StockID> PkgEntity::selected_package_state_columns()
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
      return std::pair<std::string, Gtk::StockID>();
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

    pkgCache::VerIterator ver = get_ver();

    row[cols->EntObject] = this;

    row[cols->BgColor] = selected_package_state_color();
    row[cols->BgSet] = true;

    row[cols->CurrentStatusIcon] = current_state_columns().second.get_string();
    row[cols->SelectedStatusIcon] = selected_package_state_columns().second.get_string();

    Glib::ustring safe_name = Glib::Markup::escape_text(pkg.Name());
    if(ver.end())
      {
	if(pkg.VersionList().end() && !pkg.ProvidesList().end())
	  row[cols->NameMarkup] = ssprintf("<i><b>%s</b></i>\n<span size=\"smaller\"><i>Virtual package</i></span>",
					   safe_name.c_str());
	else
	  row[cols->NameMarkup] = ssprintf("<b>%s</b>", safe_name.c_str());
      }
    else
      {
        Glib::ustring safe_description =
          Glib::Markup::escape_text(transcode(get_short_description(ver,
                                                                    apt_package_records),
                                              "UTF-8"));
        row[cols->NameMarkup] =
          ssprintf("<b>%s</b>\n<span size=\"smaller\">%s</span>",
                   safe_name.c_str(), safe_description.c_str());
      }

    if (!ver.end())
    {
      row[cols->VersionMarkup] = Glib::Markup::escape_text(ver.VerStr());
      aptitudeDepCache::StateCache &state=(*apt_cache_file)[pkg];
      pkgCache::VerIterator candver=state.CandidateVerIter(*apt_cache_file);
      if (state.Upgrade() || state.Downgrade())
        row[cols->VersionMarkup] = row[cols->VersionMarkup] + "\n<i>" + Glib::Markup::escape_text(candver.VerStr()) + "</i>";
    }
    else
      row[cols->VersionMarkup] = "";

    row[cols->Name] = pkg.end() ? "" : pkg.Name();
    row[cols->Version] = ver.end() ? "" : ver.VerStr();
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
  }

  void PkgEntity::dispatch_action(PackagesAction action)
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
        (*apt_cache_file)->mark_install(pkg, true, false, undo);
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
			   const Glib::ustring &_limit)
    : EntityView(refGlade, gladename)
  {
    generatorK = _generatorK;
    limit = _limit;
    cache_closed.connect(sigc::mem_fun(*this, &PkgViewBase::do_cache_closed));
    cache_reloaded.connect(sigc::mem_fun(*this, &PkgViewBase::rebuild_store));

    get_version_column()->set_visible(false);
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
    store_reloading();
    std::auto_ptr<PkgTreeModelGenerator> generator(generatorK(get_columns()));

    guiOpProgress * p = gen_progress_bar();

    int num=0;
    int total=(*apt_cache_file)->Head().PackageCount;
    bool limited = false;
    aptitude::matching::pkg_matcher * limiter = NULL;
    if (limit != "")
    {
      limiter = aptitude::matching::parse_pattern(limit);
      limited = (limiter != NULL);
    }

    for(pkgCache::PkgIterator pkg=(*apt_cache_file)->PkgBegin(); !pkg.end(); pkg++)
      {
        p->OverallProgress(num, total, 1, _("Building view"));

        ++num;

        // Filter useless packages up-front.
        if(pkg.VersionList().end() && pkg.ProvidesList().end())
          continue;
        if (!limited || aptitude::matching::apply_matcher(limiter, pkg, *apt_cache_file, *apt_package_records))
        {
	  generator->add(pkg);
        }
      }

    p->OverallProgress(total, total, 1,  _("Finalizing view"));

    Glib::Thread * sort_thread = Glib::Thread::create(sigc::mem_fun(*generator, &PkgTreeModelGenerator::finish), true);
    while(!generator->finished)
    {
      pMainWindow->get_progress_bar()->pulse();
      gtk_update();
      Glib::usleep(100000);
    }
    sort_thread->join();

    delete p;

    set_model(generator->get_model());
    store_reloaded();
  }

  void PkgViewBase::set_limit(const Glib::ustring &_limit)
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
		   const Glib::ustring &limit)
    : PkgViewBase(sigc::ptr_fun(&Generator::create),
		  refGlade,
		  gladename,
		  limit)
  {
  }
}
