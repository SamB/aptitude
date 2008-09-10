// match.h    -*-c++-*-
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

#ifndef MATCH_H
#define MATCH_H

#include <cwidget/generic/util/ref_ptr.h>

#include <generic/util/refcounted_base.h>

#include <apt-pkg/pkgcache.h>

#include <vector>

#include "pattern.h"

namespace aptitude
{
  namespace matching
  {
    /** \brief Represents information about how a package was matched.
     *
     *  This only represents the information that can't be derived
     *  from the type of matcher.  For instance, it might contain the
     *  string region that was matched or the dependency that was
     *  followed, but it won't contain package status information,
     *  because that is available from the pkgDepCache.
     *
     *  This means that if the user of a match needs information that
     *  will last across depCache changes, it must be copied.
     */
    class match : public util::refcounted_base
    {
    public:
      /** \brief The type of a match. */
      enum type
	{
	  /** \brief A match that has no associated information.
	   *
	   *  This would represent, for instance, a match
	   *  corresponding to a ?action term.
	   */
	  atomic,

	  /** \brief A match using a regular expression.
	   *
	   *  The attached information consists of a list of ranges
	   *  within the input string that were matched.
	   */
	  regexp,

	  /** \brief A match that was made via a dependency.
	   *
	   *  The attached information consists of the package /
	   *  version that the sub-query matched against, the
	   *  dependency the match was made through, and a list of the
	   *  matches that were made through the dependency.
	   */
	  dependency,

	  /** \brief A match that was made against a list of sub-patterns.
	   *
	   *  The semantics of the sub-patterns are determined by the
	   *  pattern corresponding to this match: for instance, an
	   *  "?and" term requires all its sub-terms to match, whereas
	   *  a ?widen term matches if its sub-terms match any version
	   *  of the incoming package.
	   */
	  sub_pattern_list
	};

      /** \brief Represents a match of a regular expression against a string.
       *
       *  aptitude uses the POSIX regular expression engine, which
       *  uses narrow character strings.  Match starting and ending
       *  locations are *byte offsets* from the start of the string.
       */
      class regexp_match
      {
	// Starting and ending bytes of the match.
	int start, end;

      public:
	regexp_match(int _start, int _end)
	  : start(_start), end(_end)
	{
	}

	int get_start() const { return start; }
	int get_end() const { return end; }
      };

    private:
      type tp;

      // The pattern that produced this particular match.
      cwidget::util::ref_ptr<pattern> p;

      // The dependency that was followed, if any.
      pkgCache::DepIterator dep;

      // The package version that produced the match.
      pkgCache::VerIterator ver;

      // The string regions matched by the regular expression, if any.
      std::vector<regexp_match> regexp_matches;

      // Proofs about sub-matches, if any.
      std::vector<cwidget::util::ref_ptr<match> > sub_matches;

      // NB: maybe I should avoid 
      template<typename RegexpMatchIter,
	       typename SubMatchIter>
      match(type _tp,
	    const cwidget::util::ref_ptr<pattern> &_p,
	    pkgCache::DepIterator _dep,
	    pkgCache::VerIterator _ver,
	    RegexpMatchIter regexp_matches_begin,
	    RegexpMatchIter regexp_matches_end,
	    SubMatchIter sub_matches_begin,
	    SubMatchIter sub_matches_end)
	: tp(_tp), p(_p), dep(_dep), ver(_ver),
	  regexp_matches(regexp_matches_begin,
			 regexp_matches_end),
	  sub_matches(sub_matches_begin,
		      sub_matches_end)
      {
      }

    public:
      /** \brief Create a new atomic match.
       *
       *  \param p  The pattern that produced this match.
       */
      static cwidget::util::ref_ptr<match> make_atomic(const cwidget::util::ref_ptr<pattern> &p)
      {
	return new match(atomic, p,
			 pkgCache::DepIterator(),
			 pkgCache::VerIterator(),
			 (regexp_match *)0, (regexp_match *)0,
			 (cwidget::util::ref_ptr<match> *)0, (cwidget::util::ref_ptr<match> *)0);
      }

      /** \brief Create a new regular expression match.
       *
       *  \param p  The pattern that produced this match.
       *  \param regexp_matches_begin  The beginning of the
       *                               range of regular
       *                               expression matches.
       *  \param regexp_matcheS_end    The end of the range
       *                               of regular expression
       *                               matches.
       */
      template<typename RegexpMatchIter>
      static cwidget::util::ref_ptr<match> make_regexp(const cwidget::util::ref_ptr<pattern> &p,
					RegexpMatchIter regexp_matches_begin,
					RegexpMatchIter regexp_matches_end)
      {
	return new match(regexp, p,
			 pkgCache::DepIterator(),
			 pkgCache::VerIterator(),
			 regexp_matches_begin, regexp_matches_end,
			 (cwidget::util::ref_ptr<match> *)0, 0);
      }

      /** \brief Create a new match through a dependency.
       *
       *  \param p   The pattern that produced this match.
       *  \param dep The dependency that was followed.
       *  \param ver The version that was matched.
       *  \param sub_matches_begin   The beginning of the
       *                             range of sub-matches.
       *  \param sub_matches_end     The end of the range
       *                             of sub-matches.
       */
      template<typename SubMatchIter>
      static cwidget::util::ref_ptr<match> make_dependency(const cwidget::util::ref_ptr<pattern> &p,
					    const pkgCache::DepIterator &dep,
					    const pkgCache::VerIterator &ver,
					    SubMatchIter sub_matches_begin,
					    SubMatchIter sub_matches_end)
      {
	return new match(dependency, p,
			 dep,
			 (regexp_match *)0, 0,
			 sub_matches_begin,
			 sub_matches_end);
      }


      /** \brief Create a new match using a list of
       *         sub-matchers.
       *
       *  \param p   The pattern that produced this match.
       *  \param sub_matches_begin   The beginning of the
       *                             range of sub-matches.
       *  \param sub_matches_end     The end of the range
       *                             of sub-matches.
       */
      template<typename SubMatchIter>
      static cwidget::util::ref_ptr<match> make_sub_pattern_list(const cwidget::util::ref_ptr<pattern> &p,
						  SubMatchIter sub_matches_begin,
						  SubMatchIter sub_matches_end)
      {
	return new match(sub_pattern_list, p,
			 pkgCache::DepIterator(),
			 pkgCache::VerIterator(),
			 (regexp_match *)0, 0,
			 sub_matches_begin,
			 sub_matches_end);
      }

      /** \brief Retrieve the type of this match. */
      type get_type() const { return tp; }

      /** \brief Retrieve the pattern that produced
       *  this match.
       */
      const cwidget::util::ref_ptr<pattern> &get_pattern() const
      {
	return p;
      }

      /** \brief For regular expression matches, retrieve the list of
       *  locations in the string that were matched.
       */
      const std::vector<regexp_match> &get_regexp_matches() const
      {
	eassert(tp == regexp);

	return regexp_matches;
      }

      /** \brief For dependency matches, retrieve the dependency that
       *  was followed.
       */
      const pkgCache::DepIterator &get_dep() const
      {
	eassert(tp == dependency);

	return dep;
      }

      /** \brief For dependency matches, retrieve the package version
       *  that produced the match.
       */
      const pkgCache::VerIterator &get_ver() const
      {
	eassert(tp == dependency);

	return ver;
      }

      /** \brief For dependency matches or sub-matches, return the
       *  list of sub-matches.
       */
      const std::vector<cwidget::util::ref_ptr<match> > &get_sub_matches() const
      {
	eassert(tp == dependency || tp == sub_pattern_list);

	return sub_matches;
      }
    };
  }
}

#endif
