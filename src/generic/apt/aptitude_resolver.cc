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

#include <apt-pkg/algorithms.h>
#include <apt-pkg/error.h>
#include <apt-pkg/sptr.h>

#include <aptitude.h>
#include <generic/apt/matching/compare_patterns.h>
#include <generic/apt/matching/match.h>
#include <generic/apt/matching/parse.h>
#include <generic/apt/matching/pattern.h>
#include <generic/apt/matching/serialize.h>

#include <cwidget/generic/util/ssprintf.h>

#include <loggers.h>

using cwidget::util::ssprintf;

namespace
{
  log4cxx::LoggerPtr loggerHints(aptitude::Loggers::getAptitudeResolverHints());
  log4cxx::LoggerPtr loggerHintsCompare(aptitude::Loggers::getAptitudeResolverHintsCompare());
  log4cxx::LoggerPtr loggerHintsMatch(aptitude::Loggers::getAptitudeResolverHintsMatch());
  log4cxx::LoggerPtr loggerHintsParse(aptitude::Loggers::getAptitudeResolverHintsParse());
  log4cxx::LoggerPtr loggerScores(aptitude::Loggers::getAptitudeResolverScores());
}

// Logging operators.
std::ostream &operator<<(std::ostream &out, const pkgCache::DepIterator &dep)
{
  pkgCache::DepIterator d = dep;
  bool first = true;

  while(true)
    {
      if(first)
	first = false;
      else
	out << " | ";

      out << const_cast<pkgCache::DepIterator &>(dep).ParentPkg().Name()
	  << " " << const_cast<pkgCache::DepIterator &>(dep).ParentVer().VerStr()
	  << " " << const_cast<pkgCache::DepIterator &>(dep).DepType()
	  << " " << const_cast<pkgCache::DepIterator &>(dep).TargetPkg().Name();

      if((dep->CompareOp & ~pkgCache::Dep::Or) != pkgCache::Dep::NoOp &&
	 dep.TargetVer() != NULL)
	{
	  out << " (" << const_cast<pkgCache::DepIterator &>(dep).CompType()
	      << " " << dep.TargetVer()
	      << ")";
	}

      if((d->CompareOp & pkgCache::Dep::Or) == 0)
	break;

      ++d;

      if(d.end())
	break; // Shouldn't happen.
    }

  return out;
}

// Should version selections be logged the way they're written?
// That's a little awkward since the syntax is hairy and some of them
// output nothing at all (!)
std::ostream &operator<<(std::ostream &out, const aptitude_resolver::hint::version_selection &sel)
{
  switch(sel .get_type())
    {
    case aptitude_resolver::hint::version_selection::select_all:
      return out << "all";

    case aptitude_resolver::hint::version_selection::select_by_archive:
      return out << "archive(" << sel.get_version_selection_string() << ")";

    case aptitude_resolver::hint::version_selection::select_inst:
      return out << "installed";

    case aptitude_resolver::hint::version_selection::select_uninst:
      return out << "uninstall";

    case aptitude_resolver::hint::version_selection::select_by_version:
      switch(sel.get_version_comparison_operator())
	{
	case aptitude_resolver::hint::version_selection::less_than:
	  out << "<";
	  break;

	case aptitude_resolver::hint::version_selection::less_than_or_equal_to:
	  out << "<=";
	  break;

	case aptitude_resolver::hint::version_selection::equal_to:
	  out << "=";
	  break;

	case aptitude_resolver::hint::version_selection::not_equal_to:
	  out << "<>";
	  break;

	case aptitude_resolver::hint::version_selection::greater_than:
	  out << ">";
	  break;

	case aptitude_resolver::hint::version_selection::greater_than_or_equal_to:
	  out << ">=";
	  break;

	default:
	  // The backslash is to keep the compiler from thinking
	  // that it's a trigraph.
	  out << "?\?(" << sel.get_version_comparison_operator() << ")";
	  break;
	}

      return out << sel.get_version_selection_string();

    default:
      return out << "error(" << sel.get_type() << ")";
    }
}

std::ostream &operator<<(std::ostream &out, const aptitude_resolver::hint &hint)
{
  switch(hint.get_type())
    {
    case aptitude_resolver::hint::reject:
      out << "reject";
      break;

    case aptitude_resolver::hint::mandate:
      out << "mandate";
      break;

    case aptitude_resolver::hint::tweak_score:
      out << "tweak(" << hint.get_score() << ")";
      break;

    default:
      out << "bad-hint-type(" << hint.get_type() << ")";
      break;
    }

  return out << " " << hint.get_target() << " " << hint.get_version_selection();
}

struct write_action
{
  std::ostream &out;
  mutable bool first;

  write_action(std::ostream &_out)
    : out(_out), first(true)
  {
  }

  void operator()(const std::pair<aptitude_resolver::package, aptitude_resolver::action> &entry) const
  {
    if(first)
      first = false;
    else
      out << ", ";

    const aptitude_resolver::package &p(entry.first);
    const aptitude_resolver::action &act(entry.second);
    const aptitude_resolver::version &ver(act.ver);

    out << p.get_name() << ":=" << ver.get_name();
  }
};

#define TRACE_EQUAL(logger, a, b) LOG_TRACE((logger), "compare(" << (a) << ", " << (b) << ") = 0")
#define TRACE_INEQUAL(logger, result, why, a, b) LOG_TRACE((logger), "compare(" << (a) << ", " << (b) << ") = " << (result) << "  " << (why))

#define TRACE_HINTS_EQUAL() TRACE_EQUAL(loggerHintsCompare, *this, other)
#define TRACE_HINTS_INEQUAL(result, why) TRACE_INEQUAL(loggerHintsCompare, result, why, *this, other)

int aptitude_resolver::hint::version_selection::compare(const version_selection &other) const
{
  if(type < other.type)
    {
      TRACE_HINTS_INEQUAL(-1, "[types]");
      return -1;
    }
  else if(type > other.type)
    {
      TRACE_HINTS_INEQUAL(1, "[types]");
      return 1;
    }
  else switch(type)
	 {
	 case select_all:
	   TRACE_HINTS_EQUAL();
	   return 0;

	 case select_by_archive:
	   {
	     int rval = version_selection_string.compare(other.version_selection_string);
	     if(rval == 0)
	       TRACE_HINTS_EQUAL();
	     else
	       TRACE_HINTS_INEQUAL(rval, "[archives]");
	     return rval;
	   }

	 case select_inst:
	   TRACE_HINTS_EQUAL();
	   return 0;

	 case select_uninst:
	   TRACE_HINTS_EQUAL();
	   return 0;

	 case select_by_version:
	   if(compare_op < other.compare_op)
	     {
	       TRACE_HINTS_INEQUAL(-1, "[operators]");
	       return -1;
	     }
	   else if(compare_op > other.compare_op)
	     {
	       TRACE_HINTS_INEQUAL(1, "[operators]");
	       return 1;
	     }
	   else
	     {
	       int rval = version_selection_string.compare(other.version_selection_string);
	       if(rval == 0)
		 TRACE_HINTS_EQUAL();
	       else
		 TRACE_HINTS_INEQUAL(rval, "[versions]");
	       return version_selection_string.compare(other.version_selection_string);
	     }

	 default:
	   LOG_FATAL(loggerHintsCompare, "Bad hint type " << type);
	   eassert(!"Internal error: we should never get here.");
	   return 0;
	 }
}

bool aptitude_resolver::hint::version_selection::matches(const aptitude_resolver_version &ver) const
{
  LOG_TRACE(loggerHintsMatch, "matching " << *this << " against " << ver);
  switch(type)
    {
    case select_all:
      LOG_TRACE(loggerHintsMatch, *this << " matches " << ver);
      return true;

    case select_by_archive:
      if(ver.get_ver().end())
	{
	  LOG_TRACE(loggerHintsMatch, *this << " does not match " << ver << " [it's the removal version]");
	  return false;
	}

      for(pkgCache::VerFileIterator vf = ver.get_ver().FileList();
	  !vf.end(); ++vf)
	{
	  for(pkgCache::PkgFileIterator pf = vf.File();
	      !pf.end(); ++pf)
	    {
	      if(pf.Archive() == version_selection_string)
		{
		  LOG_TRACE(loggerHintsMatch, *this << " matches " << ver);
		  return true;
		}
	    }
	}

      LOG_TRACE(loggerHintsMatch, *this << " does not match " << ver << " [no matching archive]");
      return false;

    case select_inst:
      {
	bool rval = !ver.get_ver().end();
	if(rval)
	  LOG_TRACE(loggerHintsMatch, *this << " matches " << ver);
	else
	  LOG_TRACE(loggerHintsMatch, *this << " does not match " << ver << " [it's the removal version]");

	return rval;
      }

    case select_uninst:
      {
	bool rval = ver.get_ver().end();
	if(rval)
	  LOG_TRACE(loggerHintsMatch, *this << " matches " << ver);
	else
	  LOG_TRACE(loggerHintsMatch, *this << " does not match " << ver << " [it's not the removal version]");

	return rval;
      }

    case select_by_version:
      {
	pkgCache::VerIterator real_ver(ver.get_ver());
	if(real_ver.end())
	  {
	    LOG_TRACE(loggerHintsMatch, *this << " does not match " << ver << " [it's the removal version]");
	    return false;
	  }

	int comparison =
	  _system->VS->CmpVersion(real_ver.VerStr(), version_selection_string);

	bool rval;
	switch(compare_op)
	  {
	  case less_than:
	    rval = comparison < 0;
	    break;

	  case less_than_or_equal_to:
	    rval = comparison <= 0;
	    break;

	  case equal_to:
	    rval = comparison == 0;
	    break;

	  case not_equal_to:
	    rval = comparison == 0;
	    break;

	  case greater_than:
	    rval = comparison > 0;
	    break;

	  case greater_than_or_equal_to:
	    rval = comparison >= 0;
	    break;

	  default:
	    LOG_FATAL(loggerHintsMatch, "Invalid comparison operator.");
	    eassert(!"Internal error: we should never get here.");
	    rval = false;
	    break;
	  }

	if(rval)
	  LOG_TRACE(loggerHintsMatch, *this << " matches " << ver);
	else
	  LOG_TRACE(loggerHintsMatch, *this << " does not match " << ver << " [version comparison failed]");

	return rval;
      }

    default:
      LOG_FATAL(loggerHintsMatch, "Invalid version selection type.");
      eassert(!"Internal error: we should never get here.");
      return 0;
    }
}

int aptitude_resolver::hint::compare(const hint &other) const
{
  if(type < other.type)
    {
      TRACE_HINTS_INEQUAL(-1, "[type]");
      return -1;
    }
  else if(type > other.type)
    {
      TRACE_HINTS_INEQUAL(1, "[type]");
      return 1;
    }
  else if(score < other.score)
    {
      TRACE_HINTS_INEQUAL(-1, "[score]");
      return -1;
    }
  else if(score > other.score)
    {
      TRACE_HINTS_INEQUAL(1, "[score]");
      return 1;
    }
  else
    {
      const int selection_compare = selection.compare(other.selection);
      if(selection_compare != 0)
	{
	  TRACE_HINTS_INEQUAL(selection_compare, "[version]");
	  return selection_compare;
	}

      const int pattern_compare = aptitude::matching::compare_patterns(target, other.target);
      if(pattern_compare != 0)
	{
	  TRACE_HINTS_INEQUAL(pattern_compare, "[pattern]");
	  return pattern_compare;
	}
      else
	{
	  TRACE_HINTS_EQUAL();
	  return 0;
	}
    }
}

bool aptitude_resolver::hint::parse(const std::string &hint, hint &out)
{
  LOG_TRACE(loggerHintsParse, "Parsing " << hint);
  std::string::const_iterator start = hint.begin();

  while(start != hint.end() && isspace(*start))
    ++start;

  if(start == hint.end())
    {
      LOG_ERROR(loggerHintsParse, ssprintf("Invalid hint \"%s\": expected an action, but found nothing.",
					   hint.c_str()));
      _error->Error(_("Invalid hint \"%s\": expected an action, but found nothing."),
		    hint.c_str());
      return false;
    }

  std::string action;
  while(start != hint.end() && !isspace(*start))
    {
      action.push_back(*start);
      ++start;
    }

  while(start != hint.end() && isspace(*start))
    ++start;


  if(start == hint.end())
    {
      LOG_ERROR(loggerHintsParse, ssprintf("Invalid hint \"%s\": expected a target, but found nothing.",
					       hint.c_str()));
      _error->Error(_("Invalid hint \"%s\": expected a target, but found nothing."),
		    hint.c_str());
      return false;
    }

  std::string target_str;
  while(start != hint.end() && !isspace(*start))
    {
      target_str.push_back(*start);
      ++start;
    }

  while(start != hint.end() && isspace(*start))
    ++start;

  cwidget::util::ref_ptr<aptitude::matching::pattern> target;
  if(!aptitude::matching::is_pattern(target_str))
    target = aptitude::matching::pattern::make_exact_name(target_str);
  else
    try
      {
	target = aptitude::matching::parse_with_errors(target_str);
      }
    catch(aptitude::matching::MatchingException &ex)
      {
	LOG_ERROR(loggerHintsParse, ssprintf("Invalid hint \"%s\": invalid target: %s",
						 hint.c_str(), ex.errmsg().c_str()));
	_error->Error(_("Invalid hint \"%s\": invalid target: %s"),
		      hint.c_str(), ex.errmsg().c_str());
	return false;
      }


  std::string version;
  while(start != hint.end() && !isspace(*start))
    {
      version.push_back(*start);
      ++start;
    }

  while(start != hint.end() && isspace(*start))
    ++start;

  if(start != hint.end())
    {
      LOG_ERROR(loggerHintsParse, ssprintf("Invalid hint \"%s\": trailing junk after the version.",
					       hint.c_str()));
      _error->Error(_("Invalid hint \"%s\": trailing junk after the version."),
		    hint.c_str());
      return false;
    }

  version_selection selection;
  if(version.empty())
    selection = version_selection::make_inst();
  else if(version == ":UNINST")
    selection = version_selection::make_uninst();
  else if(version[0] == '/')
    selection = version_selection::make_archive(std::string(version, 1));
  else
  {
    // We must have a version selection.  Parse out the operator and
    // the string.

    version_selection::compare_op_type op;
    std::string version_str;

    std::string::const_iterator vstart = version.begin();
    const std::string::const_iterator vend = version.end();

    eassert(vstart != vend); // Should be true since version is nonempty.
    switch(*vstart)
      {
      case '<':
	++vstart;

	if(vstart != vend && *vstart == '=')
	  {
	    ++vstart;
	    op = version_selection::less_than_or_equal_to;
	  }
	else if(vstart != vend && *vstart == '>')
	  {
	    ++vstart;
	    op = version_selection::not_equal_to;
	  }
	else
	  op = version_selection::less_than;
	break;

      case '>':
	++vstart;

	if(vstart != vend && *vstart == '=')
	  {
	    ++vstart;
	    op = version_selection::greater_than_or_equal_to;
	  }
	else
	  op = version_selection::greater_than;
	break;

      case '=':
	++vstart;
	op = version_selection::equal_to;
	break;

      default:
	op = version_selection::equal_to;
	break;
      }

    version_str = std::string(vstart, vend);

    selection = version_selection::make_version(op, version_str);
  }

  if(action == "reject")
    out = make_reject(target, selection);
  else if(action == "approve")
    out = make_mandate(target, selection);
  else
    {
      unsigned long score_tweak = 0;
      if(!StrToNum(action.c_str(), score_tweak, action.size()))
	{
	  LOG_ERROR(loggerHintsParse,
			ssprintf("Invalid hint \"%s\": the action \"%s\" should be \"approve\", \"reject\", or a number.",
				 hint.c_str(), action.c_str()));
	  _error->Error(_("Invalid hint \"%s\": the action \"%s\" should be \"approve\", \"reject\", or a number."),
			hint.c_str(), action.c_str());
	  return false;
	}

      out = make_tweak_score(target, selection, (int)score_tweak);
    }

  LOG_TRACE(loggerHintsParse, "Successfully parsed \"" << hint << "\" to the hint " << out);

  return true;
}

aptitude_resolver::hint::~hint()
{
}

aptitude_resolver::aptitude_resolver(int step_score,
				     int broken_score,
				     int unfixed_soft_score,
				     int infinity,
				     int resolution_score,
				     aptitudeDepCache *cache)
  :generic_problem_resolver<aptitude_universe>(step_score, broken_score, unfixed_soft_score, infinity, resolution_score,
					       imm::map<aptitude_resolver_package, aptitude_resolver_version>(),
					       aptitude_universe(cache))
{
  using cwidget::util::ref_ptr;
  using aptitude::matching::pattern;

  set_remove_stupid(aptcfg->FindB(PACKAGE "::ProblemResolver::Remove-Stupid-Pairs", true));

  for(pkgCache::PkgIterator i = cache->PkgBegin(); !i.end(); ++i)
    {
      pkgDepCache::StateCache &s((*cache)[i]);

      if(!s.Keep())
	keep_all_solution.put(package(i, cache),
			      action(version(i, i.CurrentVer(), cache),
				     dep(), false, 0));
    }

  bool discardNullSolution = aptcfg->FindB(PACKAGE "::ProblemResolver::Discard-Null-Solution", true);
  if(discardNullSolution && !keep_all_solution.empty())
    {
      LOG_DEBUG(loggerScores, "Rejecting the solution that reverts all the user's actions (" << keep_all_solution << ")");
      add_conflict(keep_all_solution);
    }
  else
    {
      if(keep_all_solution.empty())
	LOG_DEBUG(loggerScores, "There are no user actions that could be reverted by a solution.");
      else
	LOG_DEBUG(loggerScores, "Not rejecting the solution which reverts all the user's settings: " PACKAGE "::ProblemResolver::Discard-Null-Solution is disabled.");
    }
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

  const bool not_currently_installed = p.get_pkg().CurrentVer().end();
  const bool current_version = v == get_initial_state().version_of(p);
  const bool held_back = state.selection_state == pkgCache::State::Hold;
  const bool forbidden = !v.get_ver().end() && state.forbidver == v.get_ver().VerStr();

  if(not_currently_installed)
    {
      LOG_TRACE(loggerScores, v << " does not break a hold/forbid: the package is not installed.");
      return false;
    }
  else if(current_version)
    {
      // Note: the initially selected version, whatever it is, gets a
      // free pass.
      LOG_TRACE(loggerScores, v << " does not break a hold/forbid: it is the currently selected package version.");
      return false;
    }
  else if(!held_back && !forbidden)
    {
      LOG_TRACE(loggerScores, v << " does not break a hold/forbid: it is not held back.");
      return false;
    }
  else
    {
      if(LOG4CXX_UNLIKELY(loggerScores->isTraceEnabled()))
	{
	  if(held_back && forbidden)
	    LOG_TRACE(loggerScores, v << " breaks a hold and a forbid.");
	  else if(held_back)
	    LOG_TRACE(loggerScores, v << " breaks a hold.");
	  else if(forbidden)
	    LOG_TRACE(loggerScores, v << " breaks a forbid.");
	}

      return true;
    }
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
  if(provider.end())
    LOG_TRACE(loggerScores, "Adding scores for the full replacement of "
	      << real_target.Name()
	      << " by " << src.ParentPkg().Name() << " " << src.VerStr());
  else
    LOG_TRACE(loggerScores, "Adding scores for the full replacement of "
	      << real_target.Name() << " (provided by "
	      << provider.ParentPkg().Name() << " " << provider.VerStr()
	      << ") by " << src.ParentPkg().Name() << " " << src.VerStr());

  // Drop literal and indirect self-provides: allowing these would
  // have the effect of giving a bonus to a random version of packages
  // that "replace" themselves, which distorts the solutions produced
  // by the resolver (Debian bug #483920).
  if(provider.end())
    {
      if(src.ParentPkg() == real_target)
	{
	  LOG_TRACE(loggerScores,
		    "Skipping full replacement of "
		    << src.ParentPkg().Name() << " "
		    << src.VerStr() << " by itself.");
	  return;
	}
    }
  else
    {
      if(src.ParentPkg() == provider.ParentPkg())
	{
	  LOG_TRACE(loggerScores,
		    "Skipping full replacement of "
		    << src.ParentPkg().Name() << " "
		    << src.VerStr() << " by itself (through the provided package "
		    << real_target.Name() << ")");
	  return;
	}
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
	{
	  LOG_TRACE(loggerScores,
		    "Neither " << src_pkg.Name()
		    << " nor " << target.Name()
		    << " is installed; not adding a full replacement score.");
	  return;
	}
      else
	{
	  LOG_TRACE(loggerScores,
		    "The replacing package is installed, but the replaced package is not; adding penalties for reverting the replacement.");
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
		    {
		      LOG_TRACE(loggerScores,
				"Not penalizing "
				<< target_ver.ParentPkg().Name()
				<< " " << target_ver.VerStr()
				<< ": it does not provide the replaced package "
				<< real_target.Name());
		      continue;
		    }
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

	      LOG_DEBUG(loggerScores,
			"** Score: " << std::showpos << undo_full_replacement_score << std::noshowpos
			<< " for removing " << src.ParentPkg().Name()
			<< " and installing its replacement, "
			<< target.Name() << " " << target_ver.VerStr()
			<< "  (" PACKAGE "::ProblemResolver::UndoFullReplacementScore)");

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
	  LOG_TRACE(loggerScores,
		    "The replaced package and the replacing package are both installed; adding bonuses for performing the replacement.");

	  LOG_DEBUG(loggerScores,
		    "** Score: " << std::showpos << full_replacement_score << std::noshowpos
		    << " for removing " << target.Name()
		    << ", which is replaced by "
		    << src.ParentPkg().Name() << " " << src.VerStr()
		    << ", which is already installed.  (" PACKAGE "::ProblemResolver::FullReplacementScore)");
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
		  if(!version_provides(target_ver, real_target))
		    {
		      LOG_DEBUG(loggerScores,
				"** Score: " << std::showpos << full_replacement_score << std::noshowpos
				<< " for installing " << target.Name()
				<< " " << target_ver.VerStr()
				<< ", which does not provide the package name "
				<< real_target.Name()
				<< " (replaced by "
				<< src.ParentPkg().Name() << " "
				<< src.VerStr()
				<< ", which is already installed).  (" PACKAGE "::ProblemResolver::FullReplacementScore)");
		      add_version_score(aptitude_universe::version(target,
								   target_ver,
								   cache),
					full_replacement_score);
		    }
		}
	    }
	}
      else
	{
	  LOG_TRACE(loggerScores,
		    "The replaced package is installed, but the replacing package is not; adding bonuses for performing the replacement.");
	  {
	    imm::set<aptitude_universe::version> s;

	    s.insert(aptitude_universe::version(src.ParentPkg(),
						src,
						cache));
	    s.insert(aptitude_universe::version(target,
						pkgCache::VerIterator(*cache),
						cache));

	    LOG_DEBUG(loggerScores,
		      "** Score: " << std::showpos << full_replacement_score << std::noshowpos
		      << " for removing " << target.Name()
		      << " and installing its replacement, "
		      << src.ParentPkg().Name() << " "
		      << src.VerStr() << "  (" PACKAGE "::ProblemResolver::FullReplacementScore)");

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
		  if(!version_provides(target_ver, real_target))
		    {
		      imm::set<aptitude_universe::version> s;

		      s.insert(aptitude_universe::version(src.ParentPkg(),
							  src,
							  cache));
		      s.insert(aptitude_universe::version(target,
							  target_ver,
							  cache));

		      LOG_DEBUG(loggerScores,
				"** Score: " << std::showpos << full_replacement_score << std::noshowpos
				<< " for installing " << target.Name()
				<< " " << target_ver.VerStr()
				<< ", which does not provide the package name "
				<< real_target.Name()
				<< " (replaced by " << src.ParentPkg().Name()
				<< " " << src.VerStr() << ")   (" PACKAGE "::ProblemResolver::FullReplacementScore)");

		      add_joint_score(s, full_replacement_score);
		    }
		}
	    }
	}
    }
}

void aptitude_resolver::add_default_resolution_score(const pkgCache::DepIterator &dep,
						     int default_resolution_score)
{
  // This code is duplicated from MarkInstall(), since that's what
  // package authors expect.  We look at all the targets of the dep
  // (AllTargets() doesn't walk ORs, so we just get the first entry)
  // and pick the one from the highest-priority source.
  SPtrArray<pkgCache::Version *> list = const_cast<pkgCache::DepIterator &>(dep).AllTargets();
  pkgCache::Version ** curr = list;
  pkgCache::PkgIterator p = const_cast<pkgCache::DepIterator &>(dep).TargetPkg();
  aptitudeDepCache *cache(get_universe().get_cache());
  pkgCache::VerIterator instVer(*cache, NULL);

  // See if we have a match that's not through a Provides.
  for( ; *curr != NULL && (*curr)->ParentPkg == p.Index(); ++curr)
    {
      pkgCache::PkgIterator currPkg(*cache, cache->GetCache().PkgP + (*curr)->ParentPkg);
      if((*cache)[currPkg].CandidateVer != *curr)
	{
	  LOG_TRACE(loggerScores,
		    "Skipping " << currPkg.Name()
		    << " " << pkgCache::VerIterator(cache->GetCache(), *curr).VerStr()
		    << ": it is not the candidate version.");
	  continue;
	}
      instVer = pkgCache::VerIterator(cache->GetCache(), *curr);
      break;
    }

  if(instVer.end())
    {
      pkgPrioSortList(*cache, curr);
      for( ; *curr != NULL; ++curr)
	{
	  pkgCache::PkgIterator currPkg(*cache, cache->GetCache().PkgP + (*curr)->ParentPkg);
	  if((*cache)[currPkg].CandidateVer != *curr)
	    {
	      LOG_TRACE(loggerScores,
			"Skipping " << currPkg.Name()
			<< " " << pkgCache::VerIterator(cache->GetCache(), *curr).VerStr()
			<< ": it is not the candidate version.");
	      continue;
	    }
	  instVer = pkgCache::VerIterator(cache->GetCache(), *curr);
	  break;
	}
    }

  if(!instVer.end())
    {
      // Here MarkInstall would install, so we bias the resolver in
      // favor of using this as a solution.
      aptitude_resolver_version source_ver(const_cast<pkgCache::DepIterator &>(dep).ParentPkg(),
					   const_cast<pkgCache::DepIterator &>(dep).ParentVer(),
					   cache);
      aptitude_resolver_version target_ver(instVer.ParentPkg(),
					   instVer,
					   cache);

      // If the source of the dependency is currently (going to be)
      // installed, apply the score only to the target; otherwise,
      // apply it to the pair (the target can't be installed, or we
      // would not have invoked this routine).
      if(get_initial_state().version_of(source_ver.get_package()) == source_ver)
	{
	  LOG_DEBUG(loggerScores,
		    "** Score: " << std::showpos << default_resolution_score
		    << std::noshowpos << " for installing "
		    << target_ver
		    << "; it is the default apt resolution to the dependency \""
		    << dep << "\" (" << source_ver << " is already installed)");
	  add_version_score(source_ver, default_resolution_score);
	}
      else
	{
	  LOG_DEBUG(loggerScores,
		    "** Score: " << std::showpos << default_resolution_score
		    << std::noshowpos << " for installing "
		    << source_ver << " and " << target_ver
		    << " simultaneously; the latter is the default apt resolution to the dependency \""
		    << dep << "\"");

	  imm::set<aptitude_universe::version> s;
	  s.insert(source_ver);
	  s.insert(target_ver);
	  add_joint_score(s, default_resolution_score);
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
					  bool allow_break_holds_and_forbids,
					  int default_resolution_score,
					  const std::map<package, bool> &initial_state_manual_flags,
					  const std::vector<hint> &hints)
{
  cwidget::util::ref_ptr<aptitude::matching::search_cache>
    search_info(aptitude::matching::search_cache::create());
  pkgRecords records(*get_universe().get_cache());
  const resolver_initial_state<aptitude_universe> &initial_state(get_initial_state());

  // Should I stick with APT iterators instead?  This is a bit more
  // convenient, though..
  for(aptitude_universe::package_iterator pi = get_universe().packages_begin();
      !pi.end(); ++pi)
    {
      const aptitude_universe::package &p=*pi;
      aptitudeDepCache::aptitude_state &state=get_universe().get_cache()->get_ext_state(p.get_pkg());
      pkgDepCache::StateCache &apt_state = (*get_universe().get_cache())[p.get_pkg()];

      // Packages are considered "manual" either if they were manually
      // installed, or if they are currently installed and were
      // manually removed.  Packages whose state was set by the
      // initial_state object are always considered to be manual.
      //
      // There is NO PENALTY for any change to a non-manual package's
      // state, other than the usual priority-based and non-default
      // version weighting.
      bool manual;

      if(initial_state.version_of(p) == p.current_version())
	manual = ((!p.current_version().get_ver().end()) && (apt_state.Flags & pkgCache::Flag::Auto)) ||
	  (p.current_version().get_ver().end() && (p.get_pkg().CurrentVer().end() || state.remove_reason == aptitudeDepCache::manual));
      else
	{
	  std::map<package, bool>::const_iterator found(initial_state_manual_flags.find(p));
	  if(found != initial_state_manual_flags.end())
	    manual = found->second;
	  else
	    manual = true;
	}

      for(aptitude_universe::package::version_iterator vi=p.versions_begin(); !vi.end(); ++vi)
	{
	  aptitude_universe::version v=*vi;

	  LOG_TRACE(loggerScores, "Adding scores to " << v);

	  pkgCache::VerIterator apt_ver(v.get_ver());

	  // Apply resolver hints.
	  for(std::vector<hint>::const_iterator it = hints.begin();
	      it != hints.end(); ++it)
	    {
	      const hint &h(*it);

	      using aptitude::matching::get_match;

	      // Check the version selection.  This is quicker than
	      // the target test, so we do it first.
	      if(!h.get_version_selection().matches(v))
		continue;

	      // Now check the target.
	      if(apt_ver.end())
		{
		  if(!get_match(h.get_target(), p.get_pkg(),
				search_info, *get_universe().get_cache(),
				records).valid())
		    continue;
		}
	      else
		{
		  if(!get_match(h.get_target(), p.get_pkg(), v.get_ver(),
				search_info, *get_universe().get_cache(),
				records).valid())
		    continue;
		}

	      // OK, apply the hint.
	      switch(h.get_type())
		{
		case hint::reject:
		  LOG_DEBUG(loggerScores, "** Rejecting " << v << " due to the hint " << h);
		  reject_version(v);
		  break;

		case hint::mandate:
		  LOG_DEBUG(loggerScores, "** Mandating " << v << " due to the hint " << h);
		  mandate_version(v);
		  break;

		case hint::tweak_score:
		  LOG_DEBUG(loggerScores, "** Score: " << std::showpos << h.get_score() << std::noshowpos << " for " << v << " due to the hint " << h);
		  add_version_score(v, h.get_score());
		  break;

		default:
		  LOG_ERROR(loggerScores, "Bad resolver hint type " << h.get_type());
		  _error->Error("Bad resolver hint type %d.", h.get_type());
		  break;
		}
	    }

	  // Remember, the initial version is the InstVer.
	  if(v == initial_state.version_of(p))
	    {
	      if(manual)
		{
		  LOG_DEBUG(loggerScores,
			    "** Score: " << std::showpos << preserve_score
			    << std::noshowpos << " for " << v
			    << " because it is the to-be-installed version of a manually installed package (" PACKAGE "::ProblemResolver::PreserveManualScore).");
		  add_version_score(v, preserve_score);
		}
	      else
		{
		  LOG_DEBUG(loggerScores,
			    "** Score: " << std::showpos << auto_score
			    << std::noshowpos << " for " << v
			    << " because it is the to-be-installed version of an automatically installed package (" PACKAGE "::ProblemResolver::PreserveAutoScore).");
		  add_version_score(v, auto_score);
		}
	    }
	  // Ok, if this version is selected it'll be a change.
	  else if(apt_ver == p.get_pkg().CurrentVer())
	    {
	      if(manual)
		{
		  LOG_DEBUG(loggerScores,
			    "** Score: " << std::showpos << keep_score
			    << std::noshowpos << " for " << v
			    << " because it is the currently installed version of a manually installed package  (" PACKAGE "::ProblemResolver::KeepScore).");
		  add_version_score(v, keep_score);
		}
	    }
	  else if(apt_ver.end())
	    {
	      if(manual)
		{
		  LOG_DEBUG(loggerScores,
			    "** Score: " << std::showpos << remove_score
			    << std::noshowpos << " for " << v
			    << " because it represents the removal of a manually installed package  (" PACKAGE "::ProblemResolver::RemoveScore).");
		  add_version_score(v, remove_score);
		}
	    }
	  else if(apt_ver == (*get_universe().get_cache())[p.get_pkg()].CandidateVerIter(*get_universe().get_cache()))
	    {
	      if(manual)
		{
		  // Could try harder not to break holds.
		  if(p.get_pkg().CurrentVer().end())
		    {
		      LOG_DEBUG(loggerScores,
				"** Score: " << std::showpos << install_score
				<< std::noshowpos << " for " << v
				<< " because it is a new install (" PACKAGE "::ProblemResolver::InstallScore).");
		      add_version_score(v, install_score);
		    }
		  else
		    {
		      LOG_DEBUG(loggerScores,
				"** Score: " << std::showpos << upgrade_score
				<< std::noshowpos << " for " << v
				<< " because it is an upgrade (" PACKAGE "::ProblemResolver::UpgradeScore).");
		      add_version_score(v, upgrade_score);
		    }
		}
	    }
	  else
	    // We know that:
	    //  - this version wasn't requested by the user
	    //  - it's not the current version
	    //  - it's not the candidate version
	    //  - it's not a removal
	    //  - it follows that this is a non-default version.
	    {
	      LOG_DEBUG(loggerScores,
			"** Score: " << std::showpos << non_default_score
			<< std::noshowpos << " for " << v
			<< " because it is a non-default version (" PACKAGE "::ProblemResolver::NonDefaultScore).");
	      add_version_score(v, non_default_score);
	    }

	  // This logic is slightly duplicated in resolver_manger.cc,
	  // but it's not trivial to merge.
	  if(is_break_hold(v))
	    {
	      LOG_DEBUG(loggerScores,
			"** Score: " << std::showpos << break_hold_score
			<< std::noshowpos << " for " << v
			<< " because it breaks a hold/forbid (" PACKAGE "::ProblemResolver::BreakHoldScore).");
	      add_version_score(v, break_hold_score);
	      if(!allow_break_holds_and_forbids)
		{
		  LOG_DEBUG(loggerScores,
			    "** Rejecting " << v << " because it breaks a hold/forbid (" PACKAGE "::ProblemResolver::Allow-Break-Holds).");
		  reject_version(v);
		}
	    }

	  // In addition, add the essential-removal score:
	  if((p.get_pkg()->Flags & (pkgCache::Flag::Essential |
				    pkgCache::Flag::Important)) &&
	     apt_ver.end())
	    {
	      LOG_DEBUG(loggerScores,
			"** Score: " << std::showpos << essential_remove
			<< std::noshowpos << " for " << v
			<< " because it represents removing an essential package (" PACKAGE "::ProblemResolver::EssentialRemoveScore).");
	      add_version_score(v, essential_remove);

	      LOG_DEBUG(loggerScores,
			"** Rejecting " << v << " because it represents removing an essential package.");
	      reject_version(v);
	    }

	  // Look for a conflicts/provides/replaces.
	  if(!apt_ver.end())
	    {
	      std::set<pkgCache::PkgIterator> replaced_packages;
	      // Set to true if we're at the first entry in an OR
	      // group.
	      bool is_or_head = true;
	      for(pkgCache::DepIterator dep = apt_ver.DependsList();
		  !dep.end(); ++dep)
		{
		  if(default_resolution_score != 0 &&
		     is_or_head && (dep->Type == pkgCache::Dep::Depends ||
				    dep->Type == pkgCache::Dep::Recommends))
		    {
		      aptitude_resolver_dep d(dep,
					      pkgCache::PrvIterator(),
					      get_universe().get_cache());

		      if(d.broken_under(initial_state))
			{
			  LOG_TRACE(loggerScores,
				    "Adjusting scores to promote a default resolution for \"" << dep << "\"");
			  // If they aren't satisfied, then give a
			  // bonus to having the depender and the
			  // candidate version of the first entry in
			  // the OR on the system at the same time.
			  add_default_resolution_score(dep,
						       default_resolution_score);
			}
		      else
			{
			  LOG_TRACE(loggerScores,
				    "Not adjusting scores to promote a default resolution for \"" << dep << "\": it is already satisfied.");
			}
		    }

		  if(dep->Type == pkgCache::Dep::Replaces &&
		     aptitude::apt::is_full_replacement(dep))
		    {
		      pkgCache::PkgIterator target = dep.TargetPkg();
		      // First replace the literal package the dep
		      // names.
		      if(replaced_packages.find(target) == replaced_packages.end())
			{
			  replaced_packages.insert(target);
			  add_full_replacement_score(apt_ver,
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
			      add_full_replacement_score(apt_ver,
							 target,
							 provider,
							 full_replacement_score,
							 undo_full_replacement_score);
			    }
			}
		    }

		  // The next entry is an OR head iff this one terminates the OR list.
		  is_or_head = (dep->CompareOp & pkgCache::Dep::Or) == 0;
		}
	    }
	}
    }

  LOG_TRACE(loggerScores, "Done adding action scores to packages.");
}

void aptitude_resolver::add_priority_scores(int important,
					    int required,
					    int standard,
					    int optional,
					    int extra)
{
  LOG_TRACE(loggerScores, "Adding priority scores to packages.");

  for(aptitude_universe::package_iterator pi = get_universe().packages_begin();
      !pi.end(); ++pi)
    for(aptitude_universe::package::version_iterator vi=(*pi).versions_begin(); !vi.end(); ++vi)
      {
	pkgCache::VerIterator ver(vi.get_ver());
	if(ver.end())
	  continue;

	aptitude_resolver_version v(*vi);

	int score_tweak=0;
	switch(ver->Priority)
	  {
	  case pkgCache::State::Important:
	    LOG_DEBUG(loggerScores,
		      "** Score: " << std::showpos << important
		      << std::noshowpos << " for " << v
		      << " (" PACKAGE "::ProblemResolver::ImportantScore).");
	    score_tweak=important;
	    break;
	  case pkgCache::State::Required:
	    LOG_DEBUG(loggerScores,
		      "** Score: " << std::showpos << required
		      << std::noshowpos << " for " << v
		      << " (" PACKAGE "::ProblemResolver::RequiredScore).");
	    score_tweak=required;
	    break;
	  case pkgCache::State::Standard:
	    LOG_DEBUG(loggerScores,
		      "** Score: " << std::showpos << standard
		      << std::noshowpos << " for " << v
		      << " (" PACKAGE "::ProblemResolver::StandardScore).");
	    score_tweak=standard;
	    break;
	  case pkgCache::State::Optional:
	    LOG_DEBUG(loggerScores,
		      "** Score: " << std::showpos << optional
		      << std::noshowpos << " for " << v
		      << " (" PACKAGE "::ProblemResolver::OptionalScore).");
	    score_tweak=optional;
	    break;
	  case pkgCache::State::Extra:
	    LOG_DEBUG(loggerScores,
		      "** Score: " << std::showpos << extra
		      << std::noshowpos << " for " << v
		      << " (" PACKAGE "::ProblemResolver::ExtraScore).");
	    score_tweak=extra;
	    break;
	  default:
	    LOG_ERROR(loggerScores,
		      "Bad priority " << ver->Priority << " for " << v);
	    // ??????
	    break;
	  }

	add_version_score(*vi, score_tweak);
      }
}
