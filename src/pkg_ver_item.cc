// pkg_ver_item.cc
//
//  Copyright 1999-2005 Daniel Burrows
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
//
//  Implementations of stuff in pkg_ver_item.h

#include "aptitude.h"

#include "dep_item.h"
#include "pkg_columnizer.h"
#include "pkg_info_screen.h"
#include "pkg_item.h"
#include "pkg_subtree.h"
#include "pkg_ver_item.h"
#include "pkg_sortpolicy.h"
#include "ui.h"
#include "view_changelog.h"
#include "vs_progress.h"

#include <generic/apt/apt.h>
#include <generic/apt/apt_undo_group.h>
#include <generic/apt/config_signal.h>

#include "vscreen/vs_multiplex.h"

#include <algorithm>
#include <string>

#include <apt-pkg/configuration.h>
#include <apt-pkg/pkgrecords.h>
#include <apt-pkg/policy.h>
#include <apt-pkg/strutl.h>

#include <signal.h>

using namespace std;

class pkg_ver_columnizer:public pkg_item::pkg_columnizer
{
  bool show_pkg_name;

protected:
  column_disposition setup_column(int type);
public:
  pkg_ver_columnizer(const pkgCache::VerIterator &_ver,
		     bool _show_pkg_name,
		     const column_definition_list &_columns,
		     int _basex):
    pkg_item::pkg_columnizer(_ver.ParentPkg(), _ver, _columns, _basex),
    show_pkg_name(_show_pkg_name)
  {
  }
};

column_disposition pkg_ver_columnizer::setup_column(int type)
{
  pkgCache::VerIterator ver=get_visible_ver();
  int basex=get_basex();

  switch(type)
    {
    case name:
      if(ver.end())
	return column_disposition("", 0);
      else if(show_pkg_name)
	return column_disposition(string(ver.ParentPkg().Name())+" "+ver.VerStr(), basex);
      else
	return column_disposition(ver.VerStr(), basex);

      break;

    case archive:
      if(!ver.end())
	{
	  string buf;
	  for(pkgCache::VerFileIterator verfile=ver.FileList(); !verfile.end(); ++verfile)
	    {
	      pkgCache::PkgFileIterator pkgfile=verfile.File();
	      if(pkgfile.Archive() && strcmp(pkgfile.Archive(), "now"))
		buf+=string(buf.empty()?"":",")+pkgfile.Archive();
	    }
	  return column_disposition(buf,0);
	}

    case installed_size:
      if(ver.end())
	return column_disposition("", 0);
      else if(ver->InstalledSize>0)
	return column_disposition(SizeToStr(ver->InstalledSize)+'B', 0);
      else
	return column_disposition(_("<N/A>"), 0);

      break;

    case pin_priority:
      {
	if(apt_cache_file && !ver.end())
	  {
	    char buf[256];

	    pkgPolicy *policy=dynamic_cast<pkgPolicy *>(&(*apt_cache_file)->GetPolicy());

	    if(!policy)
	      return column_disposition("", 0);

	    pkgCache::VerFileIterator vf=ver.FileList();

	    if(vf.end())
	      return column_disposition("", 0);

	    signed short priority=policy->GetPriority(vf.File());

	    ++vf;

	    // Find the highest priority for this version.
	    while(!vf.end())
	      {
		priority=max(priority, policy->GetPriority(vf.File()));
		++vf;
	      }

	    snprintf(buf, 256, "%d", priority);

	    return column_disposition(buf, 0);
	  }
	else
	  return column_disposition("", 0);
      }

    case sizechange:
      if(ver.end())
	return column_disposition("", 0);

      {
	pkgCache::PkgIterator pkg=ver.ParentPkg();

	pkgCache::VerIterator curver=pkg.CurrentVer();
	pkgCache::VerIterator instver=(*apt_cache_file)[pkg].InstVerIter(*apt_cache_file);

	if((ver==curver && ver==instver) ||
	   (ver!=curver && ver!=instver))
	  return column_disposition("", 0);
	else if(ver==curver)
	  return column_disposition("-"+SizeToStr(ver->InstalledSize)+"B", 0);
	else
	  return column_disposition("+"+SizeToStr(ver->InstalledSize)+"B", 0);
      }

      break;
    case debsize:
      if(ver.end())
	return column_disposition("", 0);
      else if(ver->Size>0)
	return column_disposition(SizeToStr(ver->Size)+'B', 0);
      else
	return column_disposition(_("<N/A>"), 0);
      break;
    case currver:
      return column_disposition("", 0);
    case candver:
      return column_disposition("", 0);
    case stateflag:
      // Can't be virtual since we have a bona fide version
      if(ver.end())
	return column_disposition("", 0);

      if(ver.ParentPkg().CurrentVer()!=ver)
	return column_disposition("p", 0);

      if((*apt_cache_file)[ver.ParentPkg()].NowBroken())
	return column_disposition("B", 0);

      switch(ver.ParentPkg()->CurrentState)
	{
	case pkgCache::State::NotInstalled:
	  return column_disposition("p", 0);
	  // Assume it's purged if we're in this state.  Is this correct?
	  // If it's removed but config-files are present it should be
	  // in the ConfigFiles state.
	case pkgCache::State::UnPacked:
	  return column_disposition("u", 0);
	case pkgCache::State::HalfConfigured:
	  return column_disposition("C", 0);
	case pkgCache::State::HalfInstalled:
	  return column_disposition("H", 0);
	case pkgCache::State::ConfigFiles:
	  return column_disposition("c", 0);
	case pkgCache::State::Installed:
	  return column_disposition("i", 0);
	default:
	  return column_disposition("E", 0);
	}

      break;
    case longstate:
      if(ver.end())
	return column_disposition("", 0);

      if(ver.ParentPkg().CurrentVer()!=ver)
	return column_disposition(_("purged"), 0);

      if((*apt_cache_file)[ver.ParentPkg()].NowBroken())
	return column_disposition(_("broken"), 0);

      switch(ver.ParentPkg()->CurrentState)
	{
	case pkgCache::State::NotInstalled:
	  return column_disposition(_("purged"), 0);
	  // Assume it's purged if we're in this state.  Is this correct?
	  // If it's removed but config-files are present it should be
	  // in the ConfigFiles state.
	case pkgCache::State::UnPacked:
	  return column_disposition(_("unpacked"), 0);
	case pkgCache::State::HalfConfigured:
	  return column_disposition(_("half-config"), 0);
	case pkgCache::State::HalfInstalled:
	  return column_disposition(_("half-install"), 0);
	case pkgCache::State::ConfigFiles:
	  return column_disposition(_("config-files"), 0);
	case pkgCache::State::Installed:
	  return column_disposition(_("installed"), 0);
	default:
	  return column_disposition(_("ERROR"), 0);
	}

      break;
    case actionflag:
      {
	if(ver.end())
	  return column_disposition("", 0);

	pkgCache::PkgIterator pkg=ver.ParentPkg();
	aptitudeDepCache::StateCache &state=(*apt_cache_file)[pkg];
	aptitudeDepCache::aptitude_state &estate=(*apt_cache_file)->get_ext_state(pkg);
	pkgCache::VerIterator candver=state.CandidateVerIter(*apt_cache_file);

	if(state.Status!=2 && estate.selection_state==pkgCache::State::Hold && !state.NowBroken())
	  return column_disposition("h", 0);
	else if(ver.VerStr() == estate.forbidver)
	  return column_disposition("F", 0);
	else if(state.Delete())
	  return column_disposition((state.iFlags&pkgDepCache::Purge)?"p":"d", 0);
	else if(state.InstBroken() && state.InstVerIter(*apt_cache_file)==ver)
	  return column_disposition("B", 0);
	else if(state.NewInstall())
	  {
	    if(candver==ver)
	      return column_disposition("i", 0);
	    else
	      return column_disposition(" ", 0);
	  }
	else if(state.iFlags&pkgDepCache::ReInstall)
	  {
	    if(ver.ParentPkg().CurrentVer()==ver)
	      return column_disposition("i", 0);
	    else
	      return column_disposition(" ", 0);
	  }
	else if(state.Upgrade())
	  {
	    if(ver.ParentPkg().CurrentVer()==ver)
	      return column_disposition("d", 0);
	    else if(candver==ver)
	      return column_disposition("i", 0);
	    else
	      return column_disposition(" ", 0);
	  }
	else
	  return column_disposition(" ", 0);
      }

      break;
    case longaction:
      {
	if(ver.end())
	  return column_disposition("", 0);

	aptitudeDepCache::StateCache state=(*apt_cache_file)[ver.ParentPkg()];
	aptitudeDepCache::aptitude_state &estate=(*apt_cache_file)->get_ext_state(ver.ParentPkg());
	if(state.Status!=2 && (state.Held() || estate.selection_state==pkgCache::State::Hold) && !state.NowBroken())
	  return column_disposition(_("hold"), 0);
	else if(ver.VerStr() == estate.forbidver)
	  return column_disposition("forbidden version", 0);
	else if(state.Delete())
	  return column_disposition((state.iFlags&pkgDepCache::Purge)?"p":"d", 0);
	else if(state.InstBroken() && state.InstVerIter(*apt_cache_file)==ver)
	  return column_disposition(_("broken"), 0);
	else if(state.NewInstall())
	  {
	    if(state.CandidateVerIter(*apt_cache_file)==ver)
	      return column_disposition(_("install"), 0);
	    else
	      return column_disposition(_("none"), 0);
	  }
	else if(state.iFlags&pkgDepCache::ReInstall)
	  {
	    if(ver.ParentPkg().CurrentVer()==ver)
	      return column_disposition(_("install"), 0);
	    else
	      return column_disposition(_("none"), 0);
	  }
	else if(state.Upgrade())
	  {
	    if(ver.ParentPkg().CurrentVer()==ver)
	      return column_disposition(_("delete"), 0);
	    else if((*apt_cache_file)[ver.ParentPkg()].CandidateVerIter(*apt_cache_file)==ver)
	      return column_disposition(_("install"), 0);
	    else
	      return column_disposition(_("none"), 0);
	  }
	else
	  return column_disposition(_("none"), 0);
      }

      break;
    case description:
      return column_disposition(get_short_description(ver), 0);
    case maintainer:
      if(ver.end())
	return column_disposition("", 0);

      return column_disposition(apt_package_records->Lookup(ver.FileList()).Maintainer(), 0);
    case priority:
      if(ver.end())
	return column_disposition("", 0);

      if(ver.PriorityType() && ver.PriorityType()[0])
	return column_disposition(ver.PriorityType(), 0);
      else
	return column_disposition(_("Unknown"), 0);
    case shortpriority:
      if(ver.end())
	return column_disposition("", 0);

      switch(ver->Priority)
	{
	case pkgCache::State::Important:
	  // ForTranslators: Imp = Important
	  return column_disposition(_("Imp"), 0);
	case pkgCache::State::Required:
	  // ForTranslators: Req = Required
	  return column_disposition(_("Req"), 0);
	case pkgCache::State::Standard:
	  // ForTranslators: Std = Standard
	  return column_disposition(_("Std"), 0);
	case pkgCache::State::Optional:
	  // ForTranslators: Opt = Optional
	  return column_disposition(_("Opt"), 0);
	case pkgCache::State::Extra:
	  // ForTranslators: Xtr = Extra
	  return column_disposition(_("Xtr"), 0);
	default:
	  return column_disposition(_("ERR"), 0);
	}

      break;
    case section:
      if(ver.end())
	return column_disposition("", 0);

      if(ver.Section())
	return column_disposition(ver.Section(), 0);
      else
	return column_disposition(_("Unknown"), 0);

      break;
    case revdepcount:
      {
	if(ver.end())
	  return column_disposition("", 0);

	int count=0;
	char buf[100];
	for(pkgCache::DepIterator i=ver.ParentPkg().RevDependsList(); !i.end(); i++)
	  {
	    if(i.ParentVer()==i.ParentPkg().CurrentVer())
	      // That test is CORRECT; we want to see if the version
	      // providing the dependency is correct.
	      // (I'm writing this note because I just looked at the test
	      //  and couldn't remember what it did despite writing it
	      //  5 minutes ago. Maybe I should have my head examined :) )
	      {
		if((i->CompareOp&~pkgCache::Dep::Or)==pkgCache::Dep::NoOp ||
		   _system->VS->CheckDep(ver.VerStr(), i->CompareOp, i.TargetVer()))
		  count++;

		for(pkgCache::PrvIterator j=ver.ProvidesList(); !j.end(); j++)
		  if((i->CompareOp&~pkgCache::Dep::Or)==pkgCache::Dep::NoOp ||
		     _system->VS->CheckDep(j.ProvideVersion(), i->CompareOp, i.TargetVer()))
		    count++;
	      }
	  }
	snprintf(buf, 100, "%i", count);
	return column_disposition(buf, 0);
      }
    case trust_state:
      {
	if(ver.end())
	  return column_disposition(" ", 0);
	else if(package_trusted(ver))
	  return column_disposition(" ", 0);
	else
	  return column_disposition("U", 0);
      }
    default:
      return pkg_columnizer::setup_column(type);
    }
}

class pkg_grouppolicy_ver:public pkg_grouppolicy
{
public:
  pkg_grouppolicy_ver(pkg_signal *_sig, desc_signal *_desc_sig)
    :pkg_grouppolicy(_sig, _desc_sig) {}

  void add_package(const pkgCache::PkgIterator &pkg, pkg_subtree *root);
};

void setup_package_versions(const pkgCache::PkgIterator &pkg, pkg_vertree *tree, pkg_signal *sig)
{
  for(pkgCache::VerIterator i=pkg.VersionList(); !i.end(); i++)
    tree->add_child(new pkg_ver_item(i, sig));

  for(pkgCache::PrvIterator i=pkg.ProvidesList(); !i.end(); i++)
    tree->add_child(new pkg_ver_item(i.OwnerVer(), sig, true));

  std::auto_ptr<pkg_sortpolicy> sorter(pkg_sortpolicy_ver(0, false));
  pkg_sortpolicy_wrapper wrap(sorter.get());
  tree->sort(wrap);
}

void setup_package_versions(const pkgCache::PkgIterator &pkg, pkg_vertree_generic *tree, pkg_signal *sig)
{
  for(pkgCache::VerIterator i=pkg.VersionList(); !i.end(); i++)
    tree->add_child(new pkg_ver_item(i, sig));

  for(pkgCache::PrvIterator i=pkg.ProvidesList(); !i.end(); i++)
    tree->add_child(new pkg_ver_item(i.OwnerVer(), sig, true));

  std::auto_ptr<pkg_sortpolicy> sorter(pkg_sortpolicy_ver(0, false));
  pkg_sortpolicy_wrapper wrap(sorter.get());
  tree->sort(wrap);
}

void pkg_grouppolicy_ver::add_package(const pkgCache::PkgIterator &pkg,
				      pkg_subtree *root)
{
  pkg_vertree *newtree=new pkg_vertree(pkg, get_sig());

  root->add_child(newtree);

  setup_package_versions(pkg, newtree, get_sig());
}

pkg_grouppolicy *pkg_grouppolicy_ver_factory::instantiate(pkg_signal *sig,
							  desc_signal *desc_sig)
{
  return new pkg_grouppolicy_ver(sig, desc_sig);
}

style pkg_ver_item::get_normal_style()
{
  return vs_treeitem::get_normal_style() + ver_style(version, false);
}

style pkg_ver_item::get_highlighted_style()
{
  return vs_treeitem::get_normal_style() + ver_style(version, true);
}

pkg_ver_item::pkg_ver_item(const pkgCache::VerIterator &_version, pkg_signal *_sig,
			   bool _show_pkg_name)
  :version(_version), version_name(transcode(version.VerStr(), "ASCII")),
   show_pkg_name(_show_pkg_name), sig(_sig)
{
}

#define MAYBE_HIGHLIGHTED(x) (highlighted ? (x "Highlighted") : (x))

style pkg_ver_item::ver_style(pkgCache::VerIterator version,
			      bool highlighted)
{
  pkgCache::PkgIterator pkg=version.ParentPkg();
  pkgDepCache::StateCache &state=(*apt_cache_file)[pkg];

  if(pkg.CurrentVer()==version &&
     (state.Delete() ||
      (state.Install() &&
       !state.InstBroken() &&
       state.InstVerIter(*apt_cache_file)!=version)))
    return get_style(MAYBE_HIGHLIGHTED("PkgToRemove"));

  else if(((state.NewInstall() || state.Install()) &&
	   !state.InstBroken() &&
	   state.InstVerIter(*apt_cache_file)==version) ||
	  (version==version.ParentPkg().CurrentVer() &&
	   state.iFlags&pkgDepCache::ReInstall))
    return get_style(MAYBE_HIGHLIGHTED("PkgToInstall"));

  else if(state.InstBroken() && state.InstVerIter(*apt_cache_file)==version)
    return get_style(MAYBE_HIGHLIGHTED("PkgBroken"));

  else if(pkg.CurrentVer()!=version)
    return get_style(MAYBE_HIGHLIGHTED("PkgNotInstalled"));

  else if(state.NowBroken())
    return get_style(MAYBE_HIGHLIGHTED("PkgBroken"));

  else
    return get_style(MAYBE_HIGHLIGHTED("PkgIsInstalled"));
}

void pkg_ver_item::paint(vs_tree *win, int y, bool hierarchical,
			 const style &st)
{
  int basex=hierarchical?2*get_depth():0;
  int width, height;

  win->getmaxyx(height, width);
  pkg_item::pkg_columnizer::setup_columns();

  empty_column_parameters p;
  wstring disp=pkg_ver_columnizer(version,
				  show_pkg_name,
				  pkg_item::pkg_columnizer::get_columns(),
				  basex).layout_columns(width, p);
  win->mvaddnstr(y, 0, disp.c_str(), width);
}

void pkg_ver_item::highlighted(vs_tree *win)
{
  if(sig)
    (*sig)(version.ParentPkg(), version);
}

void pkg_ver_item::unhighlighted(vs_tree *win)
{
  if(sig)
    (*sig)(pkgCache::PkgIterator(),
	   pkgCache::VerIterator(*apt_cache_file));
}

const wchar_t *pkg_ver_item::tag()
{
  return version_name.c_str();
}

const wchar_t *pkg_ver_item::label()
{
  return version_name.c_str();
}

void pkg_ver_item::select(undo_group *undo)
{
  if(version==version.ParentPkg().CurrentVer())
    {
      if(!(*apt_cache_file)[version.ParentPkg()].Keep())
	{
	  (*apt_cache_file)->mark_keep(version.ParentPkg(),
				      false,
				      (*apt_cache_file)[version.ParentPkg()].Install(),
				       undo);
	  // Only put a Hold on it if we were installing a different version
	  // (as opposed to deleting the package altogether)
	  (*apt_cache_file)->get_ext_state(version.ParentPkg()).selection_state=pkgCache::State::Install;
	}
    }
  else
    {
      undo_group *grp=undo?new apt_undo_group:NULL;
      (*apt_cache_file)->set_candidate_version(version, grp);
      (*apt_cache_file)->mark_install(version.ParentPkg(), _config->FindB(PACKAGE "::Auto-Install", true), false, grp);

      if(undo)
	undo->add_item(grp);
    }
}

void pkg_ver_item::remove(undo_group *undo)
{
  if(version==version.ParentPkg().CurrentVer())
    {
      if((*apt_cache_file)[version.ParentPkg()].iFlags&pkgDepCache::ReInstall)
	{
	  (*apt_cache_file)->mark_keep(version.ParentPkg(), false, false, undo);
	  (*apt_cache_file)->get_ext_state(version.ParentPkg()).selection_state=pkgCache::State::Install;
	}
      else
	(*apt_cache_file)->mark_delete(version.ParentPkg(), false, false, undo);
    }
  else if(version==(*apt_cache_file)[version.ParentPkg()].CandidateVerIter(*apt_cache_file) && (*apt_cache_file)[version.ParentPkg()].Install())
    (*apt_cache_file)->mark_keep(version.ParentPkg(), false, true, undo);
}

void pkg_ver_item::hold(undo_group *undo)
{
  if(version==version.ParentPkg().CurrentVer())
    (*apt_cache_file)->mark_keep(version.ParentPkg(),
				 false,
				 (*apt_cache_file)->get_ext_state(version.ParentPkg()).selection_state!=pkgCache::State::Hold,
				 undo);
}

void pkg_ver_item::keep(undo_group *undo)
{
  // Do nothing for now.
}

void pkg_ver_item::purge(undo_group *undo)
{
  if(version==version.ParentPkg().CurrentVer())
    (*apt_cache_file)->mark_delete(version.ParentPkg(), true, false, undo);
}

void pkg_ver_item::reinstall(undo_group *undo)
{
  if(version.ParentPkg().CurrentVer()==version)
    (*apt_cache_file)->mark_install(version.ParentPkg(),
				    aptcfg->FindB(PACKAGE "::Auto-Install", true),
				    true,
				    undo);
}

void pkg_ver_item::set_auto(bool isauto, undo_group *undo)
{
  (*apt_cache_file)->mark_auto_installed(version.ParentPkg(), isauto, undo);
}

void pkg_ver_item::forbid_version(undo_group *undo)
{
  if(version!=version.ParentPkg().CurrentVer())
    (*apt_cache_file)->forbid_upgrade(version.ParentPkg(), version.VerStr(), undo);
}

void pkg_ver_item::show_information()
{
  vs_widget_ref w=make_info_screen(version.ParentPkg(), version);

  char buf[512];
  snprintf(buf, 512, _("Information about %s"), version.ParentPkg().Name());
  string menulabel(buf);
  snprintf(buf, 512, _("%s info"), version.ParentPkg().Name());
  string tablabel(buf);

  insert_main_widget(w, menulabel, "", tablabel);
}

pkg_ver_screen::pkg_ver_screen(const pkgCache::PkgIterator &pkg)
  :apt_info_tree(pkg.Name(), "")
{
  set_root(setup_new_root(pkg, pkg.VersionList()),true);
  //set_header(_("Available versions of ")+string(pkg.Name()));
}

vs_treeitem *pkg_ver_screen::setup_new_root(const pkgCache::PkgIterator &pkg,
					    const pkgCache::VerIterator &ver)
{
  pkg_vertree *newtree=new pkg_vertree(pkg, get_sig(), true);

  setup_package_versions(pkg, newtree, get_sig());

  return newtree;
}

bool pkg_ver_item::dispatch_key(const key &k, vs_tree *owner)
{
  if(bindings->key_matches(k, "Dependencies"))
    {
      char buf[512];
      snprintf(buf, 512, _("Dependencies of %s"), version.ParentPkg().Name());
      string menulabel(buf);
      snprintf(buf, 512, _("%s deps"), version.ParentPkg().Name());
      string tablabel(buf);

      vs_widget_ref w=make_dep_screen(version.ParentPkg(), version);
      insert_main_widget(w, menulabel, "", tablabel);
      return true;
    }
  else if(bindings->key_matches(k, "ReverseDependencies"))
    {
      char buf[512];
      snprintf(buf, 512, _("Packages depending on %s"), version.ParentPkg().Name());
      string menulabel(buf);
      snprintf(buf, 512, _("%s reverse deps"), version.ParentPkg().Name());
      string tablabel(buf);

      vs_widget_ref w=make_dep_screen(version.ParentPkg(), version, true);
      insert_main_widget(w, menulabel, "", tablabel);
      return true;
    }
  else if(bindings->key_matches(k, "InfoScreen"))
    {
      show_information();
      return true;
    }
  else if(bindings->key_matches(k, "Changelog"))
    {
      view_changelog(version);
      return true;
    }
  else if(bindings->key_matches(k, "ForbidUpgrade"))
    {
      undo_group *grp=new apt_undo_group;
      forbid_version(grp);

      if(!grp->empty())
	apt_undos->add_item(grp);
      else
	delete grp;

      return true;
    }
  else if(bindings->key_matches(k, "BugReport"))
    {
      // Try to report a bug on the package.  (ew quoting ew)
      string cmd=string("reportbug '")+version.ParentPkg().Name()+"' -V '"+version.VerStr()+"'";


      struct sigaction oldact;
      struct sigaction act;

      memset(&act,0,sizeof(act));
      act.sa_handler = SIG_DFL;
      sigemptyset(&act.sa_mask);

      sigaction(SIGCONT, &act, &oldact);

      vscreen_suspend();

      apt_cache_file->ReleaseLock();

      printf(_("Reporting a bug in %s:\n"), version.ParentPkg().Name());




      system(cmd.c_str());

      sigaction(SIGCONT, &oldact, NULL);



      vscreen_resume();

      vs_progress_ref p = gen_progress_bar();
      apt_reload_cache(p.unsafe_get_ref(), true);
      p->destroy();

      return true;
    }
  else
    return pkg_tree_node::dispatch_key(k, owner);
}


////////////////////////// Menu redirections: ////////////////////////////

bool pkg_ver_item::package_forbid_enabled()
{
  return true;
}

bool pkg_ver_item::package_forbid()
{
  undo_group *grp = new apt_undo_group;

  forbid_version(grp);

  if(!grp->empty())
    apt_undos->add_item(grp);
  else
    delete grp;

  package_states_changed();
  return true;
}

bool pkg_ver_item::package_changelog_enabled()
{
  return true;
}

bool pkg_ver_item::package_changelog()
{
  view_changelog(get_version());
  return true;
}

bool pkg_ver_item::package_information_enabled()
{
  return true;
}

bool pkg_ver_item::package_information()
{
  show_information();
  return true;
}

