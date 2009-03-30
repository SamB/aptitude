// solution_fragment.cc
//
//
//   Copyright (C) 2005, 2007, 2009 Daniel Burrows
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

#include "solution_fragment.h"

#include <aptitude.h>

#include <generic/apt/aptitude_resolver_universe.h>

#include <generic/problemresolver/solution.h>

#include <generic/util/util.h>

#include <cwidget/fragment.h>
#include <cwidget/generic/util/transcode.h>

#include <algorithm>
#include <vector>

typedef generic_solution<aptitude_universe> aptitude_solution;
typedef generic_choice<aptitude_universe> choice;
typedef generic_choice_set<aptitude_universe> choice_set;

using namespace std;
namespace cw = cwidget;

string archives_text(const pkgCache::VerIterator &ver)
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

string dep_targets(const pkgCache::DepIterator &start)
{
  string rval;

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

wstring dep_text(const pkgCache::DepIterator &d)
{
  const char *name = const_cast<pkgCache::DepIterator &>(d).ParentPkg().Name();

  string targets = dep_targets(d);

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

wstring conflict_text(const pkgCache::DepIterator &conflict,
		      const pkgCache::PrvIterator &prv)
{
  if(prv.end() || !is_conflict(conflict->Type))
    return dep_text(conflict);

  return swsprintf(W_("%s conflicts with %s [provided by %s %s]").c_str(),
		   const_cast<pkgCache::DepIterator &>(conflict).ParentPkg().Name(),
		   const_cast<pkgCache::PrvIterator &>(prv).ParentPkg().Name(),
		   const_cast<pkgCache::PrvIterator &>(prv).OwnerPkg().Name(),
		   const_cast<pkgCache::PrvIterator &>(prv).OwnerVer().VerStr());
}

cw::fragment *choice_fragment(const choice &c)
{
  switch(c.get_type())
    {
    case choice::install_version:
      if(c.get_ver().get_ver().end())
	return cw::fragf(_("Removing %s"), c.get_ver().get_pkg().Name());
      else
	return cw::fragf(_("Installing %s %s (%s)"),
			 c.get_ver().get_pkg().Name(), c.get_ver().get_ver().VerStr(),
			 archives_text(c.get_ver().get_ver()).c_str());

    case choice::break_soft_dep:
      return cw::fragf(_("Leave %ls unresolved."), dep_text(c.get_dep().get_dep()).c_str());

    default:
      return cw::fragf("Unhandled choice type %d.", c.get_type());
    }
}


cw::fragment *solution_fragment(const aptitude_solution &sol)
{
  // Bin packages according to what will happen to them.
  vector<pkgCache::PkgIterator> remove_packages;
  vector<pkgCache::PkgIterator> keep_packages;
  vector<pkgCache::VerIterator> install_packages;
  vector<pkgCache::VerIterator> downgrade_packages;
  vector<pkgCache::VerIterator> upgrade_packages;
  vector<pkgCache::DepIterator> unresolved;

  for(choice_set::const_iterator i = sol.get_choices().begin();
      i != sol.get_choices().end(); ++i)
    {
      switch(i->get_type())
	{
	case choice::install_version:
	  {
	    pkgCache::PkgIterator pkg = i->get_ver().get_pkg();
	    pkgCache::VerIterator curver = pkg.CurrentVer();
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

  sort(remove_packages.begin(), remove_packages.end(),
       pkg_name_lt());
  sort(keep_packages.begin(), keep_packages.end(),
       pkg_name_lt());
  sort(install_packages.begin(), install_packages.end(),
       ver_name_lt());
  sort(downgrade_packages.begin(), downgrade_packages.end(),
       ver_name_lt());
  sort(upgrade_packages.begin(), upgrade_packages.end(),
       ver_name_lt());
  // \todo Sort the unresolved list in some readable fashion.

  vector<cw::fragment *> fragments;

  if(!remove_packages.empty())
    {
      fragments.push_back(cw::fragf(_("%BRemove%b the following packages:%n")));
      for(vector<pkgCache::PkgIterator>::const_iterator i=remove_packages.begin();
	  i!=remove_packages.end(); ++i)
	fragments.push_back(cw::fragf("  %s%n", i->Name()));

      fragments.push_back(cw::newline_fragment());
    }

  if(!install_packages.empty())
    {
      fragments.push_back(cw::fragf(_("%BInstall%b the following packages:%n")));
      for(vector<pkgCache::VerIterator>::const_iterator i=install_packages.begin();
	  i!=install_packages.end(); ++i)
	fragments.push_back(cw::fragf("  %s [%s (%s)]%n",
				  i->ParentPkg().Name(),
				  i->VerStr(),
				  archives_text(*i).c_str()));

      fragments.push_back(cw::newline_fragment());
    }

  if(!keep_packages.empty())
    {
      fragments.push_back(cw::fragf(_("%BKeep%b the following packages at their current version:%n")));
      for(vector<pkgCache::PkgIterator>::const_iterator i=keep_packages.begin();
	  i!=keep_packages.end(); ++i)
	{
	  if(i->CurrentVer().end())
	    fragments.push_back(cw::fragf("  %s [%s]%n",
				      i->Name(),
				      _("Not Installed")));
	  else
	    fragments.push_back(cw::fragf("  %s [%s (%s)]%n",
				      i->Name(),
				      i->CurrentVer().VerStr(),
				      archives_text(i->CurrentVer()).c_str()));
	}

      fragments.push_back(cw::newline_fragment());
    }

  if(!upgrade_packages.empty())
    {
      fragments.push_back(cw::fragf(_("%BUpgrade%b the following packages:%n")));
      for(vector<pkgCache::VerIterator>::const_iterator i=upgrade_packages.begin();
	  i!=upgrade_packages.end(); ++i)
	fragments.push_back(cw::fragf("  %s [%s (%s) -> %s (%s)]%n",
				  i->ParentPkg().Name(),
				  i->ParentPkg().CurrentVer().VerStr(),
				  archives_text(i->ParentPkg().CurrentVer()).c_str(),
				  i->VerStr(),
				  archives_text(*i).c_str()));

      fragments.push_back(cw::newline_fragment());
    }

  if(!downgrade_packages.empty())
    {
      fragments.push_back(cw::fragf(_("%BDowngrade%b the following packages:%n")));
      for(vector<pkgCache::VerIterator>::const_iterator i=downgrade_packages.begin();
	  i!=downgrade_packages.end(); ++i)
	fragments.push_back(cw::fragf("  %s [%s (%s) -> %s (%s)]%n",
				  i->ParentPkg().Name(),
				  i->ParentPkg().CurrentVer().VerStr(),
				  archives_text(i->ParentPkg().CurrentVer()).c_str(),
				  i->VerStr(),
				  archives_text(*i).c_str()));

      fragments.push_back(cw::newline_fragment());
    }

  if(!unresolved.empty())
    {
      fragments.push_back(cw::fragf(_("Leave the following dependencies unresolved:%n")));

      for(std::vector<pkgCache::DepIterator>::const_iterator i = unresolved.begin();
	  i != unresolved.end(); ++i)
	fragments.push_back(cw::fragf("%ls%n", dep_text(*i).c_str()));
    }

  fragments.push_back(cw::fragf(_("Tier: %s"), aptitude_universe::get_tier_name(sol.get_tier()).c_str()));

  return flowbox(cw::sequence_fragment(fragments));
}
