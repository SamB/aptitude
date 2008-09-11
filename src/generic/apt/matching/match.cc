// match.cc
//
//   Copyright (C) 2008 Daniel Burrows
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

#include "match.h"

#include <generic/apt/apt.h>

#include <apt-pkg/pkgrecords.h>

using cwidget::util::ref_ptr;

namespace aptitude
{
  namespace matching
  {
    namespace
    {
      /** \brief A value stored on the match stack. */
      class stack_value
      {
      public:
	/** \brief The type of value that this represents. */
	enum type
	  {
	    /** \brief A package without version information. */
	    package,
	    /** \brief A specific version of a package. */
	    version
	  };

      private:
	type tp;
	pkgCache::PkgIterator pkg;
	pkgCache::VerIterator ver;

	stack_value(type _tp, const pkgCache::PkgIterator &_pkg, const pkgCache::VerIterator &_ver)
	  : tp(_tp), pkg(_pkg), ver(_ver)
	{
	}

      public:
	static stack_value make_package(const pkgCache::PkgIterator &pkg)
	{
	  return stack_value(package, pkg, pkgCache::VerIterator(*const_cast<pkgCache::PkgIterator &>(pkg).Cache()));
	}

	static stack_value make_version(const pkgCache::PkgIterator &pkg,
					const pkgCache::VerIterator &ver)
	{
	  return stack_value(version, pkg, ver);
	}

	type get_type() const
	{
	  return tp;
	}

	const pkgCache::PkgIterator &get_pkg() const { return pkg; }
	const pkgCache::VerIterator &get_ver() const { return ver; }
      };

      /** \brief Evaluate any regular expression-based pattern.
       *
       *  \param p      The pattern to evaluate.
       *  \param inf    The regular expression to apply.
       *  \param s      The string to test the regular expression against.
       *  \param invert \b true if this match is inverted (i.e., in a NOT
       *                context).  For inverted matches, we only return
       *                a match if the regex does \e not match, and the
       *                match region is the whole string.
       *
       *  \return     A match object corresponding to the regexp,
       *              or \b NULL if the match failed.
       */
      ref_ptr<match> evaluate_regexp(const ref_ptr<pattern> &p,
				     const pattern::regex_info &inf,
				     const char *s,
				     bool invert)
      {
	if(!invert)
	  {
	    // Unfortunately, regexec() seems to require a hard limit to
	    // the number of matches that can be returned. :-(
	    regmatch_t matches[30];
	    const int num_matches = sizeof(matches) / sizeof(regmatch_t);

	    bool matched = inf.get_regex_group()->exec(s,
						       matches,
						       num_matches);

	    if(matched)
	      {
		int matches_found = 0;
		while(matches_found < 30 && matches[matches_found].rm_so >= 0)
		  ++matches_found;

		return match::make_regexp(p, matches, matches + matches_found);
	      }
	    else
	      return NULL;
	  }
	else
	  {
	    bool matched = inf.get_regex_nogroup()->exec(s);

	    if(matched)
	      {
		match::regexp_match m(0, strlen(s));
		return match::make_regexp(p, &m, (&m) + 1);
	      }
	    else
	      return NULL;
	  }
      }

      ref_ptr<match> evaluate(const ref_ptr<pattern> &p,
			      const pkgCache::PkgIterator &pkg,
			      const pkgCache::VerIterator &ver,
			      aptitudeDepCache &cache,
			      pkgRecords &records,
			      bool invert)
      {
	switch(p->get_type())
	  {
	    // ?archive
	  case pattern::archive:
	    if(ver.end() || ver.FileList().end())
	      return NULL;


	    for(pkgCache::VerFileIterator f = ver.FileList(); !f.end(); ++f)
	      {
		pkgCache::PkgFileIterator cur = f.File();

		if(!cur.end() && cur.Archive())
		  {
		    ref_ptr<match> m = evaluate_regexp(p,
						       p->get_archive_regex_info(),
						       cur.Archive(),
						       invert);

		    if(!invert)
		      {
			if(m.valid())
			  return m;
		      }
		    else
		      {
			if(!m.valid())
			  return m;
		      }
		  }
	      }

	    return NULL;
	    break;

	    // ?action
	  case pattern::action:
	    {
	      bool matches = false;
	      pattern::action_type type = p->get_action_action_type();

	      // Install, purge, and remove states all match more than
	      // one find_pkg_state return value.
	      switch(type)
		{
		case pattern::action_install:
		  {
		    pkg_action_state thetype = find_pkg_state(pkg, cache);
		    matches = thetype == pkg_install || thetype == pkg_auto_install;
		  }
		  break;

		case pattern::action_purge:
		  if((cache[pkg].iFlags & pkgDepCache::Purge) == 0)
		    matches = false;
		  else
		    {
		      pkg_action_state thetype = find_pkg_state(pkg, cache);
		      matches = thetype == pkg_remove || thetype == pkg_auto_remove || thetype == pkg_unused_remove;
		    }
		  break;

		case pattern::action_remove:
		  {
		    pkg_action_state thetype = find_pkg_state(pkg, cache);

		    matches = thetype == pkg_remove || thetype == pkg_auto_remove || thetype == pkg_unused_remove;
		  }
		  break;

		case pattern::action_hold:
		  matches = !pkg.CurrentVer().end() && cache.get_ext_state(pkg).selection_state == pkgCache::State::Hold;
		  break;

		  // The rest correspond directly to find_pkg_state() return values.
		case pattern::action_reinstall:
		  matches = find_pkg_state(pkg, cache) == pkg_reinstall;

		case pattern::action_upgrade:
		  matches = find_pkg_state(pkg, cache) == pkg_upgrade;

		case pattern::action_downgrade:
		  matches = find_pkg_state(pkg, cache) == pkg_downgrade;

		case pattern::action_keep:
		  matches = cache[pkg].Keep();
		  break;

		default:
		  throw MatchingException("Internal error: bad action-type flag.");
		}

	      if(matches)
		return match::make_atomic(p);
	      else
		return NULL;
	    }

	    break;

	  case pattern::all_versions:
	    return NULL;
	    break;

	  case pattern::any_version:
	    return NULL;
	    break;

	  case pattern::automatic:
	    return NULL;
	    break;

	  case pattern::and_tp:
	    return NULL;
	    break;

	  case pattern::bind:
	    return NULL;
	    break;

	  case pattern::broken:
	    return NULL;
	    break;

	  case pattern::broken_type:
	    return NULL;
	    break;

	  case pattern::candidate_version:
	    return NULL;
	    break;

	  case pattern::config_files:
	    return NULL;
	    break;

	  case pattern::current_version:
	    return NULL;
	    break;

	  case pattern::depends:
	    return NULL;
	    break;

	  case pattern::description:
	    return NULL;
	    break;

	  case pattern::essential:
	    return NULL;
	    break;

	  case pattern::equal:
	    return NULL;
	    break;

	  case pattern::false_tp:
	    return NULL;
	    break;

	  case pattern::for_tp:
	    return NULL;
	    break;

	  case pattern::garbage:
	    return NULL;
	    break;

	  case pattern::install_version:
	    return NULL;
	    break;

	  case pattern::installed:
	    return NULL;
	    break;

	  case pattern::maintainer:
	    return NULL;
	    break;

	  case pattern::name:
	    return NULL;
	    break;

	  case pattern::narrow:
	    return NULL;
	    break;

	  case pattern::new_tp:
	    return NULL;
	    break;

	  case pattern::not_tp:
	    return NULL;
	    break;

	  case pattern::obsolete:
	    return NULL;
	    break;

	  case pattern::or_tp:
	    return NULL;
	    break;

	  case pattern::origin:
	    return NULL;
	    break;

	  case pattern::priority:
	    return NULL;
	    break;

	  case pattern::provides:
	    return NULL;
	    break;

	  case pattern::reverse_depends:
	    return NULL;
	    break;

	  case pattern::reverse_provides:
	    return NULL;
	    break;

	  case pattern::section:
	    return NULL;
	    break;

	  case pattern::source_package:
	    return NULL;
	    break;

	  case pattern::source_version:
	    return NULL;
	    break;

	  case pattern::tag:
	    return NULL;
	    break;

	  case pattern::task:
	    return NULL;
	    break;

	  case pattern::true_tp:
	    return NULL;
	    break;

	  case pattern::upgradable:
	    return NULL;
	    break;

	  case pattern::user_tag:
	    return NULL;
	    break;

	  case pattern::version:
	    return NULL;
	    break;

	  case pattern::virtual_tp:
	    return NULL;
	    break;

	  case pattern::widen:
	    return NULL;
	    break;

	  default:
	    throw MatchingException("Internal error: unhandled pattern type in evaluate()");
	  }
      }
    }
  }
}

