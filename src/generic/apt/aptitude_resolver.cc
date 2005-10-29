// aptitude_resolver.cc
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

#include "aptitude_resolver.h"

#include "config_signal.h"

aptitude_resolver::aptitude_resolver(int step_score,
				     int broken_score,
				     int unfixed_soft_score,
				     int infinity,
				     int max_successors,
				     int resolution_score,
				     aptitudeDepCache *cache)
  :generic_problem_resolver<aptitude_universe>(step_score, broken_score, unfixed_soft_score, infinity, max_successors, resolution_score, aptitude_universe(cache))
{
  set_remove_stupid(aptcfg->FindB(PACKAGE "::ProblemResolver::Remove-Stupid-Pairs", true));

  if(aptcfg->FindB(PACKAGE "::ProblemResolver::Discard-Null-Solution", true))
    {
      imm::map<package, action> null_solution;

      for(pkgCache::PkgIterator i = cache->PkgBegin(); !i.end(); ++i)
	{
	  pkgDepCache::StateCache &s((*cache)[i]);

	  if(!s.Keep())
	    null_solution.put(package(i, cache),
			      action(version(i, i.CurrentVer(), cache),
				     dep(), false, 0));
	}

      if(!null_solution.empty())
	add_conflict(null_solution);
    }
}

void aptitude_resolver::add_action_scores(int preserve_score, int auto_score,
					  int remove_score, int keep_score,
					  int install_score, int upgrade_score,
					  int non_default_score, int essential_remove,
					  int break_hold_score)
{
  // Should I stick with APT iterators instead?  This is a bit more
  // convenient, though..
  for(aptitude_universe::package_iterator pi = get_universe().packages_begin();
      !pi.end(); ++pi)
    {
      const aptitude_universe::package &p=*pi;
      aptitudeDepCache::aptitude_state &state=get_universe().get_cache()->get_ext_state(p.get_pkg());

      // Packages are considered "manual" if either they were manually
      // installed, or if they are currently installed and were
      // manually removed.
      //
      // There is NO PENALTY for any change to a non-manual package's
      // state, other than the usual priority-based and non-default
      // version weighting.
      bool manual = ((!p.current_version().get_ver().end()) && state.install_reason == aptitudeDepCache::manual) ||
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

	      if(!p.get_pkg().CurrentVer().end() &&
		 (state.selection_state == pkgCache::State::Hold ||
		  state.forbidver == v.get_ver().VerStr()))
		add_version_score(v, break_hold_score);
	    }
	  else
	    // We know that:
	    //  - this version wasn't requrested by the user
	    //  - it's not the current version
	    //  - it's not the candidate version
	    //  - it's not a removal
	    //  - it follows that this is a non-default version.
	    add_version_score(v, non_default_score);

	  // In addition, add the essential-removal score:
	  if((p.get_pkg()->Flags & pkgCache::Flag::Essential) &&
	     v.get_ver().end())
	    add_version_score(v, essential_remove);
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
