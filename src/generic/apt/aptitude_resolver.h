// aptitude_resolver.h                  -*-c++-*-
//
// 
//   Copyright (C) 2005, 2008 Daniel Burrows

//   This program is free software; you can redistribute it and/or
//   modify it under the terms of the GNU General Public License as
//   published by the Free Software Foundation; either version 2 of
//   the License, or (at your option) any later version.

//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//   General Public License for more details.

//   You should have received a copy of the GNU General Public License
//   along with this program; see the file COPYING.  If not, write to
//   the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//   Boston, MA 02111-1307, USA.
//
// 
//


#ifndef APTITUDE_RESOLVER_H
#define APTITUDE_RESOLVER_H

#include "aptitude_resolver_universe.h"

#include <generic/problemresolver/problemresolver.h>

#include <generic/util/immset.h>

/** \brief Glue code to make the resolver talk to the core aptitude classes.
 *
 *  
 *  General comment on how the iterators are handled: basically the
 *  technique is (generally) to have a normalize() routine that
 *  advances the current iterator(s) to the next "interesting"
 *  iterator.  For instance, broken_dep_iterator::normalize() moves to
 *  the next broken dependency (sort of).  If the current iterator is
 *  already interesting, nothing happens.  This is used on
 *  initialization and in operator++ (after advancing the iterator a
 *  single step manually).
 * 
 *  \file aptitude_resolver.h
 */

namespace aptitude
{
  namespace matching
  {
    class pkg_matcher;
  }
}

class aptitude_resolver:public generic_problem_resolver<aptitude_universe>
{
  imm::map<package, action> keep_all_solution;

  void add_full_replacement_score(const pkgCache::VerIterator &src,
				  const pkgCache::PkgIterator &real_target,
				  const pkgCache::VerIterator &provider,
				  int full_replacement_score,
				  int undo_full_replacement_score);

public:
  class resolver_hint
  {
  public:
    /** \brief The type of hint represented by this object. */
    enum hint_type
      {
	/** \brief A hint that one or more package versions should be
	 *  rejected.
	 */
	reject,
	/** \brief A hint that one or more package versions should be
	 *   mandated.
	 */
	mandate,
	/** \brief A hint that one or more package versions should
	 *  have their scores adjusted by some amount.
	 */
	tweak_score
      };

  private:
    hint_type type;
    int score;
    aptitude::matching::pkg_matcher *target;
    std::string version;

    resolver_hint(hint_type _type, int _score,
		  aptitude::matching::pkg_matcher *_target,
		  const std::string &_version)
      : type(_type), score(_score), target(_target), version(_version)
    {
    }

  public:
    resolver_hint()
      : type((hint_type)-1), score(-1), target(NULL), version()
    {
    }

    ~resolver_hint();

    /** \brief Create a hint that rejects a version or versions of a package. */
    static resolver_hint make_reject(aptitude::matching::pkg_matcher *target,
				     const std::string &version)
    {
      return resolver_hint(reject, 0, target, version);
    }

    /** \brief Create a hint that mandates a version or versions of a package. */
    static resolver_hint make_mandate(aptitude::matching::pkg_matcher *target,
				      const std::string &version)
    {
      return resolver_hint(mandate, 0, target, version);
    }

    /** \brief Create a hint that adjust the score of a package. */
    static resolver_hint make_tweak_score(aptitude::matching::pkg_matcher *target,
					  const std::string &version,
					  int score)
    {
      return resolver_hint(tweak_score, score, target, version);
    }

    /** \brief Parse a resolver hint definition.
     *
     *  Definitions have the form ACTION TARGET [VERSION].  ACTION is
     *  either a number (which will be added to the score of the
     *  selected version), or the special strings "reject" or
     *  "accept".  If TARGET is a match pattern (specifically, if the
     *  portion of the remaining string that parses as a match pattern
     *  includes a question mark or tilde), then it will be treated as
     *  such; otherwise it is the name of the package to match.
     *  VERSION is the version of TARGET that is to be tweaked.  If
     *  VERSION is not present, all versions of the package (except
     *  the removal version) that match TARGET will be selected.  If
     *  VERSION has the form "/<archive>" then the version of the
     *  package from that archive will be selected.  If VERSION is
     *  ":UNINST" then the not-installed version of the package will be
     *  selected.
     */
    static resolver_hint parse(const std::string &definition);

    /** \brief Get the type of this hint.
     *
     *  \sa hint_type
     */
    hint_type get_type() const { return type; }

    /** \brief For score-tweaking hints, get the number of points to be
     *  added to the version's score.
     */
    int get_score() const { return score; }

    /** \brief Return the pattern identifying the package or packages
     *  to be adjusted.
     */
    aptitude::matching::pkg_matcher *get_target() const { return target; }

    /** \brief Return the version selected by this hint.
     *
     *  \todo Perhaps this should just test whether a version matches
     *  instead?
     */
    const std::string &get_version() const { return version; }
  };

  aptitude_resolver(int step_score, int broken_score,
		    int unfixed_soft_score,
		    int infinity, int max_successors,
		    int resolution_score,
		    const std::vector<resolver_hint> &hints,
		    aptitudeDepCache *cache);

  /** \brief Return \b true if the given version will break a hold or
   *  install a forbidden version.
   */
  bool is_break_hold(const version &v) const;

  /** Assign scores to all packages and all package versions according
   *  to its arguments.  All scores are assigned with add_score, so
   *  this can be easily combined with other policies.
   *
   * \param preserve_score the score to assign to the version that the
   * user selected.
   *
   * \param auto_score the score to assign to automatically assigned
   * actions.  By making this smaller than preserve_score you can bias
   * the system towards overriding automatic decisions rather than
   * user actions.
   *
   * \param remove_score the score to assign to removing a package
   * against the user's wishes.
   *
   * \param keep_score the score to assign to cancelling actions on a
   * package against the user's wishes.
   *
   * \param install_score the score to assign to removing a package
   * against the user's wishes.
   *
   * \param upgrade_score the score to assign to upgrading a package
   * against the user's wishes.
   *
   * \param non_default_score the score to assign to installing a
   * non-default version of a package (such as a downgrade or an
   * experimental version).
   *
   * \param essential_remove an additional modification applied to the
   * removal of an essential package (typically used to deep-six such
   * solutions by, eg, setting it to -100000)
   *
   * \param full_replacement_score the score for removing a package p
   * and installing a package that fully replaces p (i.e., conflicts,
   * provides, and replaces it).
   *
   * \param undo_full_replacement_score the score for installing a
   * package p and removing a package that fully replaces p.
   *
   * \param break_hold_score an additional modification applied to
   * solutions that break a hold or violate a forbidding.
   *
   * \param allow_break_holds_and_forbids   if false, versions that
   * would break a package hold or install a forbidden version are
   * rejected up-front.
   */
  void add_action_scores(int preserve_score, int auto_score,
			 int remove_score, int keep_score,
			 int install_score, int upgrade_score,
			 int non_default_score, int essential_remove,
			 int full_replacement_score,
			 int undo_full_replacement_score,
			 int break_hold_score,
			 bool allow_break_holds_and_forbids);

  /** Score packages/versions according to their priorities.  Normally
   *  you want important>=required>=standard>=optional>=extra.
   *
   *  \param important score modification for Important versions
   *  \param required score modification for Required versions
   *  \param standard score modification for Standard versions
   *  \param optional score modification for Optional versions
   *  \param extra score modification for Extra versions
   */
  void add_priority_scores(int important, int required, int standard,
			   int optional, int extra);

  /** \return the "keep-all" solution, the solution that cancels
   *  all of the user's planned actions.
   */
  imm::map<package, action> get_keep_all_solution() const;
};

#endif
