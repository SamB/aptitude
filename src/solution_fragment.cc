// solution_fragment.cc
//
//
//   Copyright (C) 2005 Daniel Burrows
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

#include <vscreen/fragment.h>
#include <vscreen/transcode.h>

#include <vector>

typedef generic_solution<aptitude_universe> aptitude_solution;

using namespace std;

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
      return swsprintf(transcode(_("%s depends upon %s")).c_str(),
		       name, targets.c_str());
    case pkgCache::Dep::PreDepends:
      return swsprintf(transcode(_("%s pre-depends upon %s")).c_str(),
		       name, targets.c_str());
    case pkgCache::Dep::Suggests:
      return swsprintf(transcode(_("%s suggests %s")).c_str(),
		       name, targets.c_str());
    case pkgCache::Dep::Recommends:
      return swsprintf(transcode(_("%s recommends %s")).c_str(),
		       name, targets.c_str());
    case pkgCache::Dep::Conflicts:
      return swsprintf(transcode(_("%s conflicts with %s")).c_str(),
		       name, targets.c_str());
    case pkgCache::Dep::Replaces:
      return swsprintf(transcode(_("%s replaces %s")).c_str(),
				 name, targets.c_str());
    case pkgCache::Dep::Obsoletes:
      return swsprintf(transcode(_("%s obsoletes %s")).c_str(),
				 name, targets.c_str());
    default:
      abort();
    }
}

wstring conflict_text(const pkgCache::DepIterator &conflict,
		      const pkgCache::PrvIterator &prv)
{
  if(prv.end() || conflict->Type != pkgCache::Dep::Conflicts)
    return dep_text(conflict);

  return swsprintf(transcode(_("%s conflicts with %s [provided by %s %s]")).c_str(),
		   const_cast<pkgCache::DepIterator &>(conflict).ParentPkg().Name(),
		   const_cast<pkgCache::PrvIterator &>(prv).ParentPkg().Name(),
		   const_cast<pkgCache::PrvIterator &>(prv).OwnerPkg().Name(),
		   const_cast<pkgCache::PrvIterator &>(prv).OwnerVer().VerStr());
}

fragment *action_fragment(const aptitude_solution::action &a)
{
  if(a.ver.get_ver().end())
    return fragf(_("Removing %s"), a.ver.get_pkg().Name());
  else
    return fragf(_("Installing %s %s (%s)"),
		   a.ver.get_pkg().Name(), a.ver.get_ver().VerStr(),
		   archives_text(a.ver.get_ver()).c_str());
}


fragment *solution_fragment(const aptitude_solution &sol)
{
  // Bin packages according to what will happen to them.
  vector<pkgCache::PkgIterator> remove_packages;
  vector<pkgCache::PkgIterator> keep_packages;
  vector<pkgCache::VerIterator> install_packages;
  vector<pkgCache::VerIterator> downgrade_packages;
  vector<pkgCache::VerIterator> upgrade_packages;

  for(imm::map<aptitude_universe::package,
	aptitude_solution::action>::const_iterator i=sol.get_actions().begin();
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

  vector<fragment *> fragments;

  if(!remove_packages.empty())
    {
      fragments.push_back(fragf(_("%BRemove%b the following packages:%n")));
      for(vector<pkgCache::PkgIterator>::const_iterator i=remove_packages.begin();
	  i!=remove_packages.end(); ++i)
	fragments.push_back(fragf("  %s%n", i->Name()));

      fragments.push_back(newline_fragment());
    }

  if(!install_packages.empty())
    {
      fragments.push_back(fragf(_("%BInstall%b the following packages:%n")));
      for(vector<pkgCache::VerIterator>::const_iterator i=install_packages.begin();
	  i!=install_packages.end(); ++i)
	fragments.push_back(fragf("  %s [%s (%s)]%n",
				  i->ParentPkg().Name(),
				  i->VerStr(),
				  archives_text(*i).c_str()));

      fragments.push_back(newline_fragment());
    }

  if(!keep_packages.empty())
    {
      fragments.push_back(fragf(_("%BKeep%b the following packages at their current version:%n")));
      for(vector<pkgCache::PkgIterator>::const_iterator i=keep_packages.begin();
	  i!=keep_packages.end(); ++i)
	{
	  if(i->CurrentVer().end())
	    fragments.push_back(fragf("  %s [%s]%n",
				      i->Name(),
				      _("Not Installed")));
	  else
	    fragments.push_back(fragf("  %s [%s (%s)]%n",
				      i->Name(),
				      i->CurrentVer().VerStr(),
				      archives_text(i->CurrentVer()).c_str()));
	}

      fragments.push_back(newline_fragment());
    }

  if(!upgrade_packages.empty())
    {
      fragments.push_back(fragf(_("%BUpgrade%b the following packages:%n")));
      for(vector<pkgCache::VerIterator>::const_iterator i=upgrade_packages.begin();
	  i!=upgrade_packages.end(); ++i)
	fragments.push_back(fragf("  %s [%s (%s) -> %s (%s)]%n",
				  i->ParentPkg().Name(),
				  i->ParentPkg().CurrentVer().VerStr(),
				  archives_text(i->ParentPkg().CurrentVer()).c_str(),
				  i->VerStr(),
				  archives_text(*i).c_str()));

      fragments.push_back(newline_fragment());
    }

  if(!downgrade_packages.empty())
    {
      fragments.push_back(fragf(_("%BDowngrade%b the following packages:%n")));
      for(vector<pkgCache::VerIterator>::const_iterator i=downgrade_packages.begin();
	  i!=downgrade_packages.end(); ++i)
	fragments.push_back(fragf("  %s [%s (%s) -> %s (%s)]%n",
				  i->ParentPkg().Name(),
				  i->ParentPkg().CurrentVer().VerStr(),
				  archives_text(i->ParentPkg().CurrentVer()).c_str(),
				  i->VerStr(),
				  archives_text(*i).c_str()));

      fragments.push_back(newline_fragment());
    }

  const imm::set<aptitude_universe::dep> &unresolved = sol.get_unresolved_soft_deps();

  if(!unresolved.empty())
    {
      fragments.push_back(fragf(_("Leave the following dependencies unresolved:%n")));

      for(imm::set<aptitude_universe::dep>::const_iterator i = unresolved.begin();
	  i != unresolved.end(); ++i)
	fragments.push_back(fragf("%ls%n", dep_text((*i).get_dep()).c_str()));
    }

  char buf[512];
  snprintf(buf, 512, _("Score is %d"), sol.get_score());
  fragments.push_back(fragf("%s", buf));

  return flowbox(sequence_fragment(fragments));
}
