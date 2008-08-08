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
  string PkgEntity::current_state_string()
  {
    pkgCache::VerIterator ver = get_ver();

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

  string PkgEntity::selected_package_state_string()
  {
    aptitudeDepCache::StateCache &state=(*apt_cache_file)[pkg];
    aptitudeDepCache::aptitude_state &estate=(*apt_cache_file)->get_ext_state(pkg);
    pkgCache::VerIterator candver=state.CandidateVerIter(*apt_cache_file);

    string selected_state = string();
    if (state.Status != 2
        && (*apt_cache_file)->get_ext_state(pkg).selection_state
            == pkgCache::State::Hold && !state.InstBroken())
      selected_state += "h";
    if (state.Upgradable() && !pkg.CurrentVer().end() && !candver.end()
        && candver.VerStr() == estate.forbidver)
      selected_state += "F";
    if (state.Delete())
      selected_state += ((state.iFlags & pkgDepCache::Purge) ? "p" : "d");
    if (state.InstBroken())
      selected_state += "B";
    if (state.NewInstall())
      selected_state += "i";
    if (state.iFlags & pkgDepCache::ReInstall)
      selected_state += "r";
    if (state.Upgrade())
      selected_state += "u";
    return selected_state;
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

    row[cols->Status] = current_state_string() + selected_package_state_string();

    Glib::ustring safe_name = Glib::Markup::escape_text(pkg.Name());
    if(ver.end())
      row[cols->Name] = ssprintf("<b>%s</b>", safe_name.c_str());
    else
      {
        Glib::ustring safe_description =
          Glib::Markup::escape_text(transcode(get_short_description(ver,
                                                                    apt_package_records),
                                              "UTF-8"));
        row[cols->Name] =
          ssprintf("<b>%s</b>\n<span size=\"smaller\">%s</span>",
                   safe_name.c_str(), safe_description.c_str());
      }

    if (!ver.end())
    {
      row[cols->Version] = Glib::Markup::escape_text(ver.VerStr());
      aptitudeDepCache::StateCache &state=(*apt_cache_file)[pkg];
      pkgCache::VerIterator candver=state.CandidateVerIter(*apt_cache_file);
      if (state.Upgrade() || state.Downgrade())
        row[cols->Version] = row[cols->Version] + "\n<i>" + Glib::Markup::escape_text(candver.VerStr()) + "</i>";
    }
    else
      row[cols->Version] = "";
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

  pkgCache::VerIterator PkgEntity::get_ver() const
  {
    pkgCache::VerIterator ver = (*apt_cache_file)[pkg].CandidateVerIter(*apt_cache_file);
    if(ver.end())
      ver = pkg.CurrentVer();

    if(ver.end())
      ver = pkg.VersionList();

    return ver;
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
