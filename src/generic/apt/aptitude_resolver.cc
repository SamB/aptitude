// aptitude_resolver.cc
//
//   Copyright (C) 2005, 2008-2009 Daniel Burrows
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

#include "aptitude_resolver.h"

#include "config_signal.h"

#include <apt-pkg/error.h>

#include <aptitude.h>
#include <generic/apt/matching/compare_patterns.h>
#include <generic/apt/matching/pattern.h>

int aptitude_resolver::resolver_hint::version_selection::compare(const version_selection &other) const
{
  if(type < other.type)
    return -1;
  else if(type > other.type)
    return 1;
  else switch(type)
	 {
	 case select_all:
	   return 0;

	 case select_by_archive:
	   return version_selection_string.compare(other.version_selection_string);

	 case select_inst:
	   return 0;

	 case select_uninst:
	   return 0;

	 case select_by_version:
	   if(compare_op < other.compare_op)
	     return -1;
	   else if(compare_op > other.compare_op)
	     return 1;
	   else
	     return version_selection_string.compare(other.version_selection_string);

	 default:
	   eassert(!"Internal error: we should never get here.");
	   return 0;
	 }
}

bool aptitude_resolver::resolver_hint::version_selection::matches(const aptitude_resolver_version &ver) const
{
  switch(type)
    {
    case select_all:
      return true;

    case select_by_archive:
      if(ver.get_ver().end())
	return false;

      for(pkgCache::VerFileIterator vf = ver.get_ver().FileList();
	  !vf.end(); ++vf)
	{
	  for(pkgCache::PkgFileIterator pf = vf.File();
	      !pf.end(); ++pf)
	    {
	      if(pf.Archive() == version_selection_string)
		return true;
	    }
	}

      return false;

    case select_inst:
      return !ver.get_ver().end();

    case select_uninst:
      return ver.get_ver().end();

    case select_by_version:
      {
	pkgCache::VerIterator real_ver(ver.get_ver());
	if(real_ver.end())
	  return false;

	int comparison =
	  _system->VS->CmpVersion(real_ver.VerStr(), version_selection_string);

	switch(compare_op)
	  {
	  case less_than:
	    return comparison < 0;

	  case less_than_or_equal:
	    return comparison <= 0;

	  case equal:
	    return comparison == 0;

	  case greater_than:
	    return comparison > 0;

	  case greater_than_or_equal:
	    return comparison >= 0;

	  default:
	   eassert(!"Internal error: we should never get here.");
	   return 0;
	  }
      }

    default:
      eassert(!"Internal error: we should never get here.");
      return 0;
    }
}

int aptitude_resolver::resolver_hint::compare(const resolver_hint &other) const
{
  if(type < other.type)
    return -1;
  else if(type > other.type)
    return 1;
  else if(score < other.score)
    return -1;
  else if(score > other.score)
    return 1;
  else
    {
      const int selection_compare = selection.compare(other.selection);
      if(selection_compare != 0)
	return selection_compare;

      return aptitude::matching::compare_patterns(target, other.target);
    }
}

aptitude_resolver::resolver_hint aptitude_resolver::resolver_hint::parse(const std::string &hint)
{
  std::string action;
  std::string::const_iterator start = hint.begin();

  while(start != hint.end() && isspace(*start))
    ++start;

  if(start == hint.end())
    {
      _error->Error(_("Invalid hint: expected an action, but found nothing."));
      // Return what?
    }

  while(start != hint.end() && !isspace(*start))
    break;

  return resolver_hint();
}

aptitude_resolver::resolver_hint::~resolver_hint()
{
}

aptitude_resolver::aptitude_resolver(int step_score,
				     int broken_score,
				     int unfixed_soft_score,
				     int infinity,
				     int resolution_score,
				     const std::vector<resolver_hint> &hints,
				     aptitudeDepCache *cache)
  :generic_problem_resolver<aptitude_universe>(step_score, broken_score, unfixed_soft_score, infinity, resolution_score, aptitude_universe(cache))
{
  using cwidget::util::ref_ptr;
  using aptitude::matching::pattern;

  std::vector<ref_ptr<pattern> > hint_matchers;
  for(std::vector<resolver_hint>::const_iterator it = hints.begin();
      it != hints.end(); ++it)
    {
      //const aptitude::matching::pkg_matcher *pattern = it->get_target();

    }

  set_remove_stupid(aptcfg->FindB(PACKAGE "::ProblemResolver::Remove-Stupid-Pairs", true));

  for(pkgCache::PkgIterator i = cache->PkgBegin(); !i.end(); ++i)
    {
      pkgDepCache::StateCache &s((*cache)[i]);

      if(!s.Keep())
	keep_all_solution.put(package(i, cache),
			      action(version(i, i.CurrentVer(), cache),
				     dep(), false, 0));
    }

  if(aptcfg->FindB(PACKAGE "::ProblemResolver::Discard-Null-Solution", true) &&
     !keep_all_solution.empty())
    add_conflict(keep_all_solution);
}

imm::map<aptitude_resolver::package, aptitude_resolver::action>
aptitude_resolver::get_keep_all_solution() const
{
  return keep_all_solution;
}

bool aptitude_resolver::is_break_hold(const aptitude_resolver::version &v) const
{
  const aptitude_resolver::package p(v.get_package());
  const aptitudeDepCache::aptitude_state &state=get_universe().get_cache()->get_ext_state(p.get_pkg());

  return
    !p.get_pkg().CurrentVer().end() &&
    v != p.current_version() &&
    (state.selection_state == pkgCache::State::Hold ||
     (!v.get_ver().end() && state.forbidver == v.get_ver().VerStr()));
}

namespace
{
  bool version_provides(const pkgCache::VerIterator &ver,
			const pkgCache::PkgIterator &pkg_name)
  {
    for(pkgCache::PrvIterator prv = ver.ProvidesList();
	!prv.end(); ++prv)
      if(prv.ParentPkg() == pkg_name)
	return true;

    return false;
  }
}

void aptitude_resolver::add_full_replacement_score(const pkgCache::VerIterator &src,
						   const pkgCache::PkgIterator &real_target,
						   const pkgCache::VerIterator &provider,
						   int full_replacement_score,
						   int undo_full_replacement_score)
{
  // Drop literal and indirect self-provides: allowing these would
  // have the effect of giving a bonus to a random version of packages
  // that "replace" themselves, which distorts the solutions produced
  // by the resolver (Debian bug #483920).
  if(provider.end())
    {
      if(src.ParentPkg() == real_target)
	return;
    }
  else
    {
      if(src.ParentPkg() == provider.ParentPkg())
	return;
    }

  pkgCache::PkgIterator src_pkg = src.ParentPkg();
  bool src_installed = (src_pkg->CurrentState != pkgCache::State::NotInstalled &&
			src_pkg->CurrentState != pkgCache::State::ConfigFiles) &&
    src_pkg.CurrentVer() == src;

  pkgCache::PkgIterator target = !provider.end() ? provider.ParentPkg() : real_target;

  bool target_installed = (target->CurrentState != pkgCache::State::NotInstalled &&
			   target->CurrentState != pkgCache::State::ConfigFiles);

  pkgDepCache * const cache = get_universe().get_cache();

  if(!target_installed)
    {
      // If the target isn't installed and the source isn't installed,
      // do nothing.
      if(!src_installed)
	return;
      else
	{
	  // Penalize installing any version of the target package
	  // (that provides the name) if the source is being removed.
	  for(pkgCache::VerIterator target_ver = target.VersionList();
	      !target_ver.end(); ++target_ver)
	    {
	      // If we're working through a Provides, only apply the
	      // penalty to versions of the target that provide the
	      // name in question.
	      if(!provider.end())
		{
		  if(!version_provides(target_ver, real_target))
		    continue;
		}

	      // Penalize removing the source and installing the target.
	      //
	      // It's important that we go down this branch at most
	      // once per package (since we only do it if the source
	      // *version* is installed).
	      imm::set<aptitude_universe::version> s;

	      s.insert(aptitude_universe::version(src.ParentPkg(),
						  pkgCache::VerIterator(*cache),
						  cache));
	      s.insert(aptitude_universe::version(target,
						  target_ver,
						  cache));

	      add_joint_score(s, undo_full_replacement_score);
	    }
	}
    }
  else
    {
      // The target *is* installed.  Favor removing it in favor of
      // installing the source.
      //
      // If the source is already installed, just add a bonus to the
      // target's removal.
      //
      // It's important that we go down this branch at most once
      // per package (since we only do it if the source *version*
      // is installed).
      if(src_installed)
	{
	  add_version_score(aptitude_universe::version(target,
						       pkgCache::VerIterator(*cache),
						       cache),
			    full_replacement_score);

	  // If we are working through a provides, find all versions
	  // that don't provide the package being replaced and apply
	  // the same score to them as to removal.
	  if(!provider.end())
	    {
	      for(pkgCache::VerIterator target_ver = target.VersionList();
		  !target_ver.end(); ++target_ver)
		{
		  if(version_provides(target_ver, real_target))
		    add_version_score(aptitude_universe::version(target,
								 target_ver,
								 cache),
				      full_replacement_score);
		}
	    }
	}
      else
	{
	  {
	    imm::set<aptitude_universe::version> s;

	    s.insert(aptitude_universe::version(src.ParentPkg(),
						src,
						cache));
	    s.insert(aptitude_universe::version(target,
						pkgCache::VerIterator(*cache),
						cache));

	    add_joint_score(s, full_replacement_score);
	  }

	  // If we are working through a provides, find all versions
	  // that don't provide the package being replaced and apply
	  // the same score to them as to removal.
	  if(!provider.end())
	    {
	      for(pkgCache::VerIterator target_ver = target.VersionList();
		  !target_ver.end(); ++target_ver)
		{
		  if(version_provides(target_ver, real_target))
		    {
		      imm::set<aptitude_universe::version> s;

		      s.insert(aptitude_universe::version(src.ParentPkg(),
							  src,
							  cache));
		      s.insert(aptitude_universe::version(target,
							  target_ver,
							  cache));

		      add_joint_score(s, full_replacement_score);
		    }
		}
	    }
	}
    }
}

void aptitude_resolver::add_action_scores(int preserve_score, int auto_score,
					  int remove_score, int keep_score,
					  int install_score, int upgrade_score,
					  int non_default_score, int essential_remove,
					  int full_replacement_score,
					  int undo_full_replacement_score,
					  int break_hold_score,
					  bool allow_break_holds_and_forbids)
{
  // Should I stick with APT iterators instead?  This is a bit more
  // convenient, though..
  for(aptitude_universe::package_iterator pi = get_universe().packages_begin();
      !pi.end(); ++pi)
    {
      const aptitude_universe::package &p=*pi;
      aptitudeDepCache::aptitude_state &state=get_universe().get_cache()->get_ext_state(p.get_pkg());
      pkgDepCache::StateCache &apt_state = (*get_universe().get_cache())[p.get_pkg()];

      // Packages are considered "manual" if either they were manually
      // installed, or if they are currently installed and were
      // manually removed.
      //
      // There is NO PENALTY for any change to a non-manual package's
      // state, other than the usual priority-based and non-default
      // version weighting.
      bool manual = ((!p.current_version().get_ver().end()) && (apt_state.Flags & pkgCache::Flag::Auto)) ||
	(p.current_version().get_ver().end() && (p.get_pkg().CurrentVer().end() || state.remove_reason == aptitudeDepCache::manual));

      for(aptitude_universe::package::version_iterator vi=p.versions_begin(); !vi.end(); ++vi)
	{
	  aptitude_universe::version v=*vi;

	  // Remember, the "current version" is the InstVer.
	  if(v==p.current_version())
	    {
	      if(manual)
		add_version_score(v, preserve_score);
	      else
		add_version_score(v, auto_score);
	    }
	  // Ok, if this version is selected it'll be a change.
	  else if(v.get_ver()==p.get_pkg().CurrentVer())
	    {
	      if(manual)
		add_version_score(v, keep_score);
	    }
	  else if(v.get_ver().end())
	    {
	      if(manual)
		add_version_score(v, remove_score);
	    }
	  else if(v.get_ver()==(*get_universe().get_cache())[p.get_pkg()].CandidateVerIter(*get_universe().get_cache()))
	    {
	      if(manual)
		{
		  // Could try harder not to break holds.
		  if(p.get_pkg().CurrentVer().end())
		    add_version_score(v, install_score);
		  else
		    add_version_score(v, upgrade_score);
		}
	    }
	  else
	    // We know that:
	    //  - this version wasn't requested by the user
	    //  - it's not the current version
	    //  - it's not the candidate version
	    //  - it's not a removal
	    //  - it follows that this is a non-default version.
	    add_version_score(v, non_default_score);

	  // This logic is slightly duplicated in resolver_manger.cc,
	  // but it's not trivial to merge.
	  if(is_break_hold(v))
	    {
	      add_version_score(v, break_hold_score);
	      if(!allow_break_holds_and_forbids)
		reject_version(v);
	    }

	  // In addition, add the essential-removal score:
	  if((p.get_pkg()->Flags & (pkgCache::Flag::Essential |
				    pkgCache::Flag::Important)) &&
	     v.get_ver().end())
	    {
	      add_version_score(v, essential_remove);
	      reject_version(v);
	    }

	  // Look for a conflicts/provides/replaces.
	  if(!v.get_ver().end())
	    {
	      std::set<pkgCache::PkgIterator> replaced_packages;
	      for(pkgCache::DepIterator dep = v.get_ver().DependsList();
		  !dep.end(); ++dep)
		{
		  if((dep->Type & ~pkgCache::Dep::Or) == pkgCache::Dep::Replaces &&
		     aptitude::apt::is_full_replacement(dep))
		    {
		      pkgCache::PkgIterator target = dep.TargetPkg();
		      // First replace the literal package the dep
		      // names.
		      if(replaced_packages.find(target) == replaced_packages.end())
			{
			  replaced_packages.insert(target);
			  add_full_replacement_score(v.get_ver(),
						     target,
						     pkgCache::VerIterator(*get_universe().get_cache()),
						     full_replacement_score,
						     undo_full_replacement_score);
			}

		      // Now find the providers and replace them.  NB:
		      // providers are versions, not packages; how do
		      // I handle that?  Add scores to each version
		      // not providing the given name?
		      for(pkgCache::PrvIterator prv = target.ProvidesList();
			  !prv.end(); ++prv)
			{
			  pkgCache::VerIterator provider = prv.OwnerVer();

			  if(replaced_packages.find(provider.ParentPkg()) == replaced_packages.end())
			    {
			      replaced_packages.insert(provider.ParentPkg());
			      add_full_replacement_score(v.get_ver(),
							 target,
							 provider,
							 full_replacement_score,
							 undo_full_replacement_score);
			    }
			}
		    }
		}
	    }
	}
    }
}

void aptitude_resolver::add_priority_scores(int important,
					    int required,
					    int standard,
					    int optional,
					    int extra)
{
  for(aptitude_universe::package_iterator pi = get_universe().packages_begin();
      !pi.end(); ++pi)
    for(aptitude_universe::package::version_iterator vi=(*pi).versions_begin(); !vi.end(); ++vi)
      {
	if(vi.get_ver().end())
	  continue;

	int score_tweak=0;
	switch(vi.get_ver()->Priority)
	  {
	  case pkgCache::State::Important:
	    score_tweak=important;
	    break;
	  case pkgCache::State::Required:
	    score_tweak=required;
	    break;
	  case pkgCache::State::Standard:
	    score_tweak=standard;
	    break;
	  case pkgCache::State::Optional:
	    score_tweak=optional;
	    break;
	  case pkgCache::State::Extra:
	    score_tweak=extra;
	    break;
	  default:
	    // ??????
	    break;
	  }

	add_version_score(*vi, score_tweak);
      }
}
