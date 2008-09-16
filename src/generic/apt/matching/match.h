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

#include <regex.h>
#include <sys/types.h>

#include "pattern.h"

class aptitudeDepCache;
class pkgRecords;

namespace aptitude
{
  namespace matching
  {
    /** \brief Represents the atomic values that are selected by search
     *  patterns.
     *
     *  Each matchable object is either a virtual package, or a
     *  particular version of a package.
     */
    class matchable
    {
      // The package and version; if the version is invalid, this
      // represents a virtual package.
      pkgCache::Package *pkg;
      pkgCache::Version *ver;

    public:
      matchable(pkgCache::Package *_pkg,
		pkgCache::Version *_ver)
	: pkg(_pkg), ver(_ver)
      {
      }

      pkgCache::Package *get_pkg() const { return pkg; }
      pkgCache::Version *get_ver() const { return ver; }

      pkgCache::PkgIterator get_package_iterator(pkgCache &owner) const
      {
	return pkgCache::PkgIterator(owner, pkg);
      }

      bool get_has_version() const { return ver != NULL; }

      pkgCache::VerIterator get_version_iterator(pkgCache &owner) const
      {
	eassert(ver != NULL);

	return pkgCache::VerIterator(owner, ver);
      }

      /** \brief Compare two matchables.
       *
       *  Matchables are ordered first by their package, then by their
       *  version (if any).  Version ordering is arbitrary but
       *  well-defined.
       */
      bool operator<(const matchable &other) const
      {
	if(pkg < other.pkg)
	  return true;
	else if(pkg > other.pkg)
	  return false;
	else if(ver < other.ver)
	  return true;
	else if(ver > other.ver)
	  return false;
	else
	  return false;
      }
    };

    /** \brief A match for a node that is "atomic" at the level of
     *  matches against a particular pool of possibilities.
     *
     *  Representing matches is tricky because we have to handle
     *  version information in a sensible way.  Each pattern could
     *  match multiple versions, and this information has to be
     *  represented for the user of the match to make sense of it
     *  since, e.g., every version can have a different Maintainer
     *  field.
     *
     *  We can divide term types into two categories: some terms
     *  return true by testing a condition against the packages in the
     *  incoming version pool, while other terms simply delegate to
     *  their sub-parts.  We call the second category \e structural
     *  terms.  Note that terms such as ?depends, which \e ignore the
     *  incoming pool and begin a new match against a new pool, are in
     *  the second category, not the first category.  The structural
     *  terms are:
     *
     *   - ?and
     *   - ?or
     *   - ?not
     *   - ?any
     *   - ?all
     *   - ?widen
     *   - ?narrow
     *
     *  To represent a match, we use a heterogeneous tree.  The inner
     *  nodes represent the structural matchers that are encountered
     *  at the root of the tree, and the leaves represent the
     *  structually-atomic matchers (again, these may be matchers with
     *  sub-expressions, but their sub-expression represents a
     *  completely new search).
     *
     *  Structural matches either represent an internal node (i.e., a
     *  match using one of the above patterns) or an atomic node (a
     *  match to one of the other patterns).
     */
    class structural_match : public util::refcounted_base
    {
    public:
      /** \brief The type of this structural match node. */
      enum type
	{
	  /** \brief Am internal match node; it has a number
	   *  of sub-matches (get_sub_matches()).
	   */
	  internal,
	  /** \brief An atomic match node; it has an attached list of
	   *  pairs containing a matchable object and a match object
	   *  describing how it was matched.
	   */
	  atomic
	};

    private:
      type tp;

      // The corresponding pattern node.
      cwidget::util::ref_ptr<pattern> pattern;

      // The list of structural sub-matches, if any.  Each corresponds
      // to a different sub-pattern (e.g., the list of immediate
      // children of a ?and node).
      std::vector<cwidget::util::ref_ptr<structural_match> > sub_matches;

      // The atomic match values, if any.  Each corresponds to how a
      // single matchable object satisfied the search.
      std::vector<std::pair<matchable, cwidget::util::ref_ptr<match> > > atomic_matches;

      template<typename StructuralMatchIter,
	       typename AtomicMatchIter>
      structural_match(type _tp,
		       const cwidget::util::ref_ptr<pattern> &_pattern,
		       StructuralMatchIter _sub_matches_begin,
		       StructuralMatchIter _sub_matches_end,
		       AtomicMatchIter _atomic_matches_begin,
		       AtomicMatchIter _atomic_matches_end)
	: tp(_tp), pattern(_pattern),
	  sub_matches(_sub_matches_begin, _sub_matches_end),
	  atomic_matches(_atomic_matches_begin, _atomic_matches_end)
      {
      }

    public:
      /** \brief Create a new inner structural match node.
       *
       *  \typeparam StructuralMatchIter An input iterator returning
       *                                 values of type
       *                                 cwidget::util::ref_ptr<structural_match>.
       *
       *  \param pattern   The corresponding match pattern.
       *
       *  \param sub_matches_begin   The beginning of the range
       *                             of sub-nodes.
       *
       *  \param sub_matches_end     The end of the range of
       *                             sub-nodes.
       */
      template<typename StructuralMatchIter>
      static cwidget::util::ref_ptr<structural_match> make_branch(const cwidget::util::ref_ptr<pattern> &pattern,
						   StructuralMatchIter sub_matches_begin,
						   StructuralMatchIter sub_matches_end)
      {
	return new structural_match(internal,
				    pattern,
				    sub_matches_begin,
				    sub_matches_end,
				    (std::pair<matchable, cwidget::util::ref_ptr<match> > *)0,
				    (std::pair<matchable, cwidget::util::ref_ptr<match> > *)0);
      }

      /** \brief Create a new leaf structural match node.
       *
       *  \typeparam AtomicMatchIter An input iterator returning
       *                             values of type
       *                             std::pair<matchable, cwidget::util::ref_ptr<match> >.
       *
       *  \param pattern   The corresponding match pattern.
       *
       *  \param atomic_matches_begin   The beginning of the
       *                                range of atomic matches.
       *  \param atomic_matches_end     The end of the range
       *                                of atomic matches.
       */
      template<typename AtomicMatchIter>
      static cwidget::util::ref_ptr<structural_match> make_leaf(const cwidget::util::ref_ptr<pattern> &pattern,
								AtomicMatchIter atomic_matches_begin,
								AtomicMatchIter atomic_matches_end)
      {
	return new structural_match(atomic,
				    pattern,
				    (cwidget::util::ref_ptr<structural_match> *)0,
				    (cwidget::util::ref_ptr<structural_match> *)0,
				    atomic_matches_begin,
				    atomic_matches_end);
      }
    };

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
      class dependency_match_info
      {
	pkgCache::DepIterator dep;
	
      };

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
	   *  The attached information is a structural matcher
	   *  representing how the sub-expression matched, along with
	   *  the dependency that was followed.
	   */
	  dependency
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

	regexp_match(const regmatch_t &match)
	  : start(match.rm_so), end(match.rm_eo)
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

      // The string regions matched by the regular expression, if any.
      std::vector<regexp_match> regexp_matches;

      // The sub-match, if any.
      cwidget::util::ref_ptr<structural_match> sub_match;

      // NB: maybe I should avoid 
      template<typename RegexpMatchIter,
	       typename SubMatchIter>
      match(type _tp,
	    const cwidget::util::ref_ptr<pattern> &_p,
	    const cwidget::util::ref_ptr<structural_match> &_sub_match,
	    pkgCache::DepIterator _dep,
	    RegexpMatchIter regexp_matches_begin,
	    RegexpMatchIter regexp_matches_end,
	    SubMatchIter sub_matches_begin,
	    SubMatchIter sub_matches_end)
	: tp(_tp), p(_p),
	  match_targets(_match_targets),
	  dep(_dep),
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
			 cwidget::util::ref_ptr<structural_match>(),
			 pkgCache::DepIterator(),
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
			 cwidget::util::ref_ptr<structural_match>(),
			 pkgCache::DepIterator(),
			 regexp_matches_begin, regexp_matches_end,
			 (cwidget::util::ref_ptr<match> *)0,
			 (cwidget::util::ref_ptr<match> *)0);
      }

      /** \brief Create a new match through a dependency.
       *
       *  \param p   The pattern that produced this match.
       *  \param dep The dependency that was followed.
       *  \param sub_matches_begin   The beginning of the
       *                             range of sub-matches.
       *  \param sub_matches_end     The end of the range
       *                             of sub-matches.
       */
      template<typename SubMatchIter>
      static cwidget::util::ref_ptr<match> make_dependency(const cwidget::util::ref_ptr<pattern> &p,
							   const cwidget::util::ref_ptr<structural_match> &m,
							   const pkgCache::DepIterator &dep,
							   const pkgCache::VerIterator &ver,
							   SubMatchIter sub_matches_begin,
							   SubMatchIter sub_matches_end)
      {
	return new match(dependency, p,
			 m,
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
			 cwidget::util::ref_ptr<structural_match>(),
			 pkgCache::DepIterator(),
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

      /** \brief Retrieve the objects that were the target
       *  of this match.
       */
      const cwidget::util::ref_ptr<matchable_list> &get_match_targets() const
      {
	return match_targets;
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

      /** \brief For dependency matches or sub-matches, return the
       *  list of sub-matches.
       */
      const std::vector<cwidget::util::ref_ptr<match> > &get_sub_matches() const
      {
	eassert(tp == dependency || tp == sub_pattern_list);

	return sub_matches;
      }
    };

    /** \brief Test a version of a package against a pattern.
     *
     *  \param p   The pattern to execute.
     *  \param pkg The package to compare.
     *  \param ver The version of pkg to compare, or an end iterator to match the
     *             package itself.
     *  \param cache   The cache in which to search.
     *  \param records The package records with which to perform the match.
     *
     *  \return A match object describing the match, or \b NULL if the
     *  package does not match.
     */
    cwidget::util::ref_ptr<match> get_match(const cwidget::util::ref_ptr<pattern> &p,
					    const pkgCache::PkgIterator &pkg,
					    const pkgCache::VerIterator &ver,
					    aptitudeDepCache &cache,
					    pkgRecords &records);


    /** \brief Test a package against a pattern.
     *
     *  This tests the package as a package, not as a version.
     *
     *  \param p   The pattern to execute.
     *  \param pkg The package to compare.
     *  \param cache   The cache in which to search.
     *  \param records The package records with which to perform the match.
     *
     *  \return A match object describing the match, or \b NULL if the
     *  package does not match.
     */
    cwidget::util::ref_ptr<match> get_match(const cwidget::util::ref_ptr<pattern> &p,
					    const pkgCache::PkgIterator &pkg,
					    const pkgCache::VerIterator &ver,
					    aptitudeDepCache &cache,
					    pkgRecords &records);
  }
}

#endif
