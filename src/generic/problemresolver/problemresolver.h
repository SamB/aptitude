// problemresolver.h                  -*-c++-*-
//
//   Copyright (C) 2005, 2007-2009 Daniel Burrows
//
//   This program is free software; you can redistribute it and/or
//   modify it under the terms of the GNU General Public License as
//   published by the Free Software Foundation; either version 2 of
//   the License, or (at your option) any later version.
//
//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//   General Public License for more details.  You should have
//   received a copy of the GNU General Public License along with this
//   program; see the file COPYING.  If not, write to the Free
//   Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
//   MA 02111-1307, USA.

//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License as
//  published by the Free Software Foundation; either version 2 of the
//  License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//  General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; see the file COPYING.  If not, write to
//  the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//  Boston, MA 02111-1307, USA.
//
//

#ifndef PROBLEMRESOLVER_H
#define PROBLEMRESOLVER_H

#include <algorithm>
#include <deque>
#include <map>
#include <queue>
#include <set>
#include <vector>

#include <iostream>
#include <sstream>

#include <limits.h>

#include "choice.h"
#include "choice_set.h"
#include "dump_universe.h"
#include "exceptions.h"
#include "incremental_expression.h"
#include "promotion_set.h"
#include "solution.h"
#include "resolver_undo.h"
#include "search_graph.h"
#include "tier_limits.h"

#include "log4cxx/consoleappender.h"
#include "log4cxx/logger.h"
#include "log4cxx/patternlayout.h"

#include <loggers.h>

#include <cwidget/generic/threads/threads.h>
#include <cwidget/generic/util/eassert.h>

#include <generic/util/dense_setset.h>
#include <generic/util/maybe.h>

#include <boost/unordered_set.hpp>

/** \brief Generic problem resolver
 *
 * 
 *  Generic problem resolver (generic because I want to be able to do
 *  regression-testing for once, if I can figure out how, and Packages
 *  files make lousy regression-tests).
 * 
 *  \file problemresolver.h
 */

template<typename Obj1, typename Obj2, typename Uni>
inline void eassert_fail_on_2objs_soln(const std::string &file,
				       size_t line,
				       const std::string &func,
				       const std::string &exp,
				       const Obj1 &obj1,
				       const char *obj1name,
				       const Obj2 &obj2,
				       const char *obj2name,
				       const generic_solution<Uni> &soln)
{
  std::ostringstream out;
  out << "In the context ";
  soln.dump(out, true);
  out << " with " << obj1name << "=" << obj1;
  out << " and " << obj2name << "=" << obj2;
  throw cwidget::util::AssertionFailure(file, line, func, exp, out.str());
}

template<typename Obj1, typename Obj2>
inline void eassert_fail_on_2objs(const std::string &file,
				  size_t line,
				  const std::string &func,
				  const std::string &exp,
				  const Obj1 &obj1,
				  const char *obj1name,
				  const Obj2 &obj2,
				  const char *obj2name)
{
  std::ostringstream out;
  out << "With " << obj1name << "=" << obj1;
  out << " and " << obj2name << "=" << obj2;
  throw cwidget::util::AssertionFailure(file, line, func, exp, out.str());
}

template<typename Obj, typename Uni>
inline void eassert_fail_on_soln_obj(const std::string &file,
				     size_t line,
				     const std::string &func,
				     const std::string &exp,
				     const generic_solution<Uni> &soln,
				     const char *objtype,
				     const char *objname,
				     const Obj &o)
{
  std::ostringstream out;
  out << "In context ";
  soln.dump(out, true);
  out << " on " << objtype << " " << objname << "=";
  out << o;
  throw cwidget::util::AssertionFailure(file, line, func, exp, out.str());
}

template<typename Uni>
inline void eassert_fail_on_soln(const std::string &file,
				 size_t line,
				 const std::string &func,
				 const std::string &exp,
				 const generic_solution<Uni> &soln)
{
  std::ostringstream out;
  out << "In context ";
  soln.dump(out, true);
  throw cwidget::util::AssertionFailure(file, line, func, exp, out.str());
}

#define eassert_on_2objs_soln(invariant, obj1, obj2, soln) \
  do { if(!(invariant)) \
         eassert_fail_on_2objs_soln(__FILE__, __LINE__, __PRETTY_FUNCTION__, #invariant, obj1, #obj1, obj2, #obj2, soln); \
     } while(0)

#define eassert_on_2objs(invariant, obj1, obj2) \
  do { if(!(invariant)) \
         eassert_fail_on_2objs(__FILE__, __LINE__, __PRETTY_FUNCTION__, #invariant, obj1, #obj1, obj2, #obj2); \
     } while(0)

#define eassert_on_obj(invariant, soln, objtype, obj) \
  do { if(!(invariant)) \
         eassert_fail_on_soln_obj(__FILE__, __LINE__, __PRETTY_FUNCTION__, #invariant, soln, objtype, #obj, obj); \
     } while(0)

#define eassert_on_dep(invariant, soln, dep) \
  eassert_on_obj(invariant, soln, "dependency", dep)

#define eassert_on_pkg(invariant, soln, pkg) \
  eassert_on_obj(invariant, soln, "package", pkg)

#define eassert_on_ver(invariant, soln, ver) \
  eassert_on_obj(invariant, soln, "version", ver)

#define eassert_on_soln(invariant, soln) \
  do { if(!(invariant)) \
         eassert_fail_on_soln(__FILE__, __LINE__, __PRETTY_FUNCTION__, #invariant, soln); \
     } while(0)

/** A dummy iterator that's always an "end" iterator. */
template<class V>
struct dummy_end_iterator
{
public:
  /** \return \b true */
  static bool end() {return true;}
};

/** Aborts: this iterator is always invalid. */
template<class V>
static inline const V &operator*(const dummy_end_iterator<V>&)
{
  abort();
}

/** Aborts. */
template<class V>
static inline dummy_end_iterator<V> operator++(dummy_end_iterator<V>&)
{
  abort();
}

/** \defgroup problemresolver Aptitude's problem resolver
 *
 *  \section Overview
 *
 *  This is a replacement for the standard apt problem resolver.  It
 *  uses a different algorithm which, while it is less efficient,
 *  allows the program to have more direct control over the results,
 *  allows the user to pick and choose from multiple solutions, and
 *  can produce a more user-friendly explanation of its actions.
 *
 *  The problem resolver class is templated on abstract packages.
 *  Normally this will be the system of apt packages described by
 *  aptitude_universe, but for the purpose of testing the algorithm a
 *  simpler package system, the dummy_universe, is used.
 *
 *  Each package version has an associated "score" which indicates,
 *  essentially, how hard the problem resolver will try to keep it on
 *  the system (or, for negative scores, to remove it from the
 *  system).  The sum of all installed package's scores is used as a
 *  heuristic in a directed search of the solution space.
 *
 *  \sa \subpage abstract_universe, generic_problem_resolver
 *
 *  \page abstract_universe The abstract package universe interface
 *
 *  The package universe interface consists of the following
 *  interrelated Concepts (see the <A
 *  HREF="http://www.sgi.com/tech/stl/stl_introduction.html">STL
 *  documentation</A> for a definition of what a Concept is):
 *
 *  - \subpage universe_universe
 *  - \subpage universe_package
 *  - \subpage universe_version
 *  - \subpage universe_dep
 *  - \subpage universe_installation
 *
 *  Note that in order to allow APT structures to be wrapped with
 *  minimal overhead, all iterators in this section are "APT-style"
 *  iterators: instead of calculating container bounds by invoking an
 *  "end" method on the container, each iterator has a predicate
 *  method end() which returns \b true if the iterator is an "end"
 *  iterator.
 *
 *  \sa \ref problemresolver
 *
 *  \page universe_universe Universe concept
 *
 *  The package universe is the base type representing a domain of
 *  package relationships.  It contains classes representing the
 *  various objects in the domain, along with methods to retrieve
 *  information about the number of entities in the universe and to
 *  iterate over global lists of entities.
 *
 *  aptitude_universe and dummy_universe are models of the generic
 *  universe concept.
 *
 *  \sa \ref universe_package, \ref universe_version, \ref universe_dep
 *
 *  A class modelling the Universe concept should provide the
 *  following members:
 *
 *  - <b>package</b>: a model of \ref universe_package "the Package concept".
 *
 *  - <b>version</b>: a model of \ref universe_version "the Version concept".
 *
 *  - <b>dep</b>: a model of \ref universe_dep "the Dependency concept".
 *
 *  - <b>package_iterator</b>: an iterator over the list of all the
 *  \ref universe_package "package"s in this universe.
 *
 *  - <b>dep_iterator</b>: an iterator over the list of all the \ref
 *  universe_dep "dependencies" in this universe.
 *
 *  - <b>tier</b>: a model of \ref universe_tier "the Tier concept".
 *
 *  - <b>get_package_count()</b>: returns the number of \ref
 *  universe_package "package"s in the universe.
 *
 *  - <b>get_version_count()</b>: returns the number of \ref
 *  universe_version "version"s in the universe.
 *
 *  - <b>packages_begin()</b>: returns a <b>package_iterator</b>
 *  pointing at the first \ref universe_package "package" (in an
 *  arbitrary ordering) in the universe.
 *
 *  - <b>deps_begin()</b>: returns a \b dep_iterator pointing at the
 *  first \ref universe_dep "dependency" (in an arbitrary ordering) in
 *  the universe.
 *
 *  \page universe_package Package concept
 *
 *  A package is simply a unique collection of \ref universe_version
 *  "versions".  No two packages in the same universe may share
 *  versions, and \e exactly one version of a package is installed at
 *  any given time.  A package has a "currently installed version",
 *  which is the version that should be included in the starting point
 *  of a solution search.
 *
 *  \sa \ref universe_universe, \ref universe_version, \ref universe_dep
 *
 *  A class modelling the Package concept should provide the following
 *  members:
 *
 *  - <b>version_iterator</b>: an iterator over the list of \ref
 *  universe_version "versions" of a package.
 *
 *  - <b>get_name()</b>: returns a string that uniquely names this
 *  package.  This may be used for debugging output or when dumping a
 *  portable representation of a dependency problem.
 *
 *  - <b>get_id()</b>: returns an integer between 0 and
 *  U.get_package_count()-1 (where U is the \ref universe_universe
 *  "universe" to which the package belongs) that uniquely identifies
 *  this package in U.
 *
 *  - <b>bool operator==(package)</b>, <b>operator!=(package)</b>:
 *  compare packages by identity.  Two package objects compare equal
 *  if and only if they represent the same package.
 *
 *  - <b>bool operator<(package)</b>: an arbitrary total ordering on
 *  packages.  This should be appropriate for, eg, placing packages
 *  into a balanced tree structure.
 *
 *  - \anchor universe_package_current_version
 *  <b>current_version()</b>: returns the "currently installed \ref
 *  universe_version "version"" of the package.  For instance, the
 *  apt_universe class considers the InstVersion of a package to be
 *  its "current version".
 *
 *  - <b>versions_begin()</b>: returns a version_iterator pointing to
 *  the head of the list of versions of this package, provided in an
 *  arbitrary order.
 *
 *  \page universe_version Version concept
 *
 *  A version is simply a particular variant of a package that may be
 *  installed.  When the abstract package system is modelling a
 *  concrete universe (such as the APT universe), versions typically
 *  correspond either to a real version of the package, or to the
 *  package's removal.
 *
 *  Each version contains a link to its parent package, as well as
 *  lists of forward and reverse dependencies.
 *
 *  \sa \ref universe_universe, \ref universe_package, \ref universe_dep
 *
 *  A class modelling the Version concept should provide the following
 *  members:
 *
 *  - <b>dep_iterator</b>: an iterator class for forward dependencies.
 *
 *  - <b>revdep_iterator</b>: an iterator class for reverse dependencies.
 *
 *  - <b>get_name()</b>: returns a string that uniquely identifies
 *  this version among all the versions of the same package.
 *
 *  - <b>get_id()</b>: returns a number between 0 and
 *  U.get_version_count()-1 (where U is the \ref universe_universe
 *  "universe" to which this version belongs) uniquely identifying
 *  this version.
 *
 *  - <b>package get_package()</b>: returns the \ref universe_package
 *  "package" of which this is a version.
 *
 *  - <b>dep_iterator deps_begin()</b>: returns a \b dep_iterator
 *  pointing to the first \ref universe_dep "dependency" in the list
 *  of forward dependencies.
 *
 *  - <b>revdep_iterator revdeps_begin()</b>: returns a \b
 *  revdep_iterator pointing to the first \ref universe_dep
 *  "dependency" in the list of reverse dependencies.
 *
 *    \note Although it would be straightforward to define the reverse
 *    dependencies of a version as the set of dependencies that
 *    impinge on that version, they are \e not defined in this manner.
 *    For technical reasons and in order to keep the wrapper to the
 *    APT package system thin, the reverse dependencies are only
 *    required to obey the following rule: if \e v1 and \e v2 are
 *    versions of the same \ref universe_package "package", then for
 *    any \ref universe_dep "dependency" \e d such that \e v1 is a
 *    target of \e d and \e v2 is not, or vice versa, \e d appears in
 *    \e either the reverse dependency list of \e v1 or the reverse
 *    dependency list of \e v2.
 *
 *  - <b>operator==(version)</b>, <b>operator!=(version)</b>:
 *  compare versions by identity.  Two version objects compare equal
 *  if and only if they represent the same version of the same
 *  package.
 *
 *  - <b>operator<(version)</b>: an arbitrary total ordering on
 *  versions.  This should be appropriate for, eg, placing versions
 *  into a balanced tree structure.
 *
 *  \page universe_dep Dependency concept
 *
 *  A dependency indicates that if a particular \ref universe_version
 *  "version" is installed, at least one member of a set of \ref
 *  universe_version "version"s must also be installed.  The first
 *  \ref universe_version "version" is the "source" of the dependency,
 *  while the remaining versions are "solvers" of the dependency.  A
 *  dependency may be "soft", indicating that it is legal (but
 *  undesirable) for the dependency to remain broken at the end of a
 *  solution search.
 *
 *  \todo "solvers" should be renamed to "targets", as a dependency
 *  can also be resolved by removing its source.
 *
 *  \sa \ref universe_universe, \ref universe_package, \ref
 *  universe_version, \ref universe_installation
 *
 *  A class modelling the Dependency concept should provide the
 *  following members:
 *
 *  - <b>solver_iterator</b>: an iterator over the versions that are
 *  targets of this dependency.
 *
 *  - <b>version get_source()</b>: returns the source \ref
 *  universe_version "version" of this dependency.
 *
 *  - <b>solver_iterator solvers_begin()</b>: returns a
 *  solver_iterator pointing to the first target of this dependency.
 *
 *  - <b>bool is_soft()</b>: returns \b true if the dependency is "soft".
 *  For instance, in the Debian package system, a Recommends
 *  relationship is considered to be a soft dependency.
 *
 *  - <b>bool solved_by(version)</b>: return \b true if the given \ref
 *  universe_version "version" solves this dependency, either by
 *  removing the source of the dependency or by installing one of its
 *  targets.
 *
 *  - <b>template &lt;typename Installation&gt; bool broken_under(Installation)</b>: return \b true if this dependency
 *  is broken by the given \ref universe_installation "installation";
 *  a solution breaks a dependency if and only if it installs the
 *  dependency's source and none of its targets.
 *
 *  - <b>operator==(dependency)</b>, <b>operator!=(dependency)</b>:
 *  compare dependencies by identity.  Two dependency objects compare
 *  equal if and only if they represent the same dependency.
 *  Duplicated dependencies may compare as distinct entities.
 *
 *  - <b>operator<(dependency)</b>: an arbitrary total ordering on
 *  dependencies.  This should be appropriate for, eg, placing
 *  dependencies into a balanced tree structure.
 *
 *  \page universe_installation Installation concept
 *
 *  An Installation represents a potential state of an abstract
 *  dependency system; that is, a set of installed versions (or
 *  rather, a total function from packages to versions).
 *
 *  The generic_solution class is a model of the Installation concept.
 *
 *  \page universe_tier   Tier concept
 *
 *  A Tier represents a value that controls the order in which
 *  solutions are generated.  Tiers are conceptually tuples of
 *  integers ordered lexicographically, but how they are represented
 *  is up to the implementation.  (in the dummy implementation they
 *  are std::vectors; in the apt implementation they are fixed-size
 *  arrays of integers)
 *
 *  Three special tiers are created by the dependency solver, by
 *  storing the three highest integers in the first entry of a Tier
 *  object.  All tiers above the lowest of these tiers are reserved
 *  for use by the solver.  In the apt implementation, the first entry
 *  is used to store the "policy" set by the user or the solver, with
 *  the remaining entries tracking things like the APT priority of the
 *  version.
 *
 *  Note that a single integer is a model of Tier if it is augmented
 *  with appropriate implementations of the Tier methods.
 *
 *  A class modeling the Tier concept should provide the following
 *  members:
 *
 * - <b>Tier(n)</b>: construct a tier with the given first element.
 *
 * - <b>const_iterator</b>: a type name that is used to iterate over
 *    the elements of the tuple (used only for debugging output).
 *
 * - <b>begin() const</b>, <b>end() const</b>: first and last iterators in the
 *   tuple.
 *
 * - <b>operator&lt;(Tier) const</b>: a total ordering on Tiers
 *   corresponding to lexicographic ordering on the corresponding
 *   tuples.
 *
 * - <b>operator&gt;=(Tier) const</b>: a total ordering on Tiers
 *   corresponding to lexicographic ordering on the corresponding
 *   tuples.
 *
 * - <b>operator==(Tier) const</b>, <b>operator!=(Tier) const</b>:
 *   comparison of tiers.
 *
 * - <b>operator=(Tier)<b/>: assignment.
 *
 * - <b>operator&lt;&lt;(std::ostream &, Tier)</b>: write a Tier
 *   as a parenthesized list of integers.
 *
 *  \sa \ref abstract_universe
 *
 *  A class modelling the Installation concept should provide the
 *  following members:
 *
 *  - <b>version version_of(package)</b>: look up the currently
 *  installed \ref universe_version "version" of the given \ref
 *  universe_package "package" according to this installation.
 */

/** \brief A generic package problem resolver.
 *
 *  \param PackageUniverse A model of the \ref universe_universe
 *  Universe concept.
 *
 *  Searches from the starting node on a best-first basis; i.e., it
 *  repeatedly pulls the "best" node off its work queue, returns it if
 *  it is a solution, and enqueues its successors if not.  The
 *  successor nodes to a given search node are generated by selecting
 *  a single dependency that is broken at that node and enqueuing all
 *  possibly ways of fixing it.  The score of a node is affected by:
 *
 *  - A penalty/bonus applied to each package version.
 *
 *  - A bonus for each step of the solution (used to discourage the
 *  resolver from "backing up" unnecessarily).
 *
 *  - A penalty is added for each broken dependency that has not yet
 *  been processed.  This aims at directing the search towards "less
 *  broken" situations.
 *
 *  - A penalty for soft dependencies (i.e., Recommends) which were
 *  processed and left broken.
 *
 *  - A bonus for nodes that have no unprocessed broken dependencies.
 *
 *  Note that these are simply the default biases set by aptitude; any
 *  of these scores may be changed at will (including changing a
 *  penalty to a bonus or vice versa!).
 *
 *  \sa \ref problemresolver, \ref universe_universe
 */
template<class PackageUniverse>
class generic_problem_resolver : public promotion_set_callbacks<PackageUniverse>
{
public:
  friend class ResolverTest;

  typedef typename PackageUniverse::package package;
  typedef typename PackageUniverse::version version;
  typedef typename PackageUniverse::dep dep;
  typedef typename PackageUniverse::tier tier;

  typedef generic_solution<PackageUniverse> solution;
  typedef generic_choice<PackageUniverse> choice;
  typedef generic_choice_set<PackageUniverse> choice_set;
  typedef generic_promotion<PackageUniverse> promotion;
  typedef generic_promotion_set<PackageUniverse> promotion_set;
  typedef generic_search_graph<PackageUniverse> search_graph;
  typedef generic_tier_limits<PackageUniverse> tier_limits;
  typedef generic_compare_choices_by_effects<PackageUniverse> compare_choices_by_effects;
  typedef generic_promotion_queue_entry<PackageUniverse> promotion_queue_entry;

  typedef typename search_graph::step step;

  /** Information about the sizes of the various resolver queues. */
  struct queue_counts
  {
    size_t open;
    size_t closed;
    size_t deferred;
    size_t conflicts;
    /** \brief The number of deferred packages that are not at
     *  defer_tier.
     */
    size_t promotions;

    /** \b true if the resolver has finished searching for solutions.
     *  If open is empty, this member distinguishes between the start
     *  and the end of a search.
     */
    bool finished;

    /** \brief The search tier of the next solution to consider.
     */
    tier current_tier;

    queue_counts()
      : open(0), closed(0), deferred(0), conflicts(0), promotions(0),
	finished(false),
	current_tier(tier_limits::minimum_tier)
    {
    }
  };

private:
  log4cxx::LoggerPtr logger;
  log4cxx::AppenderPtr appender; // Used for the "default" appending behavior.

  search_graph graph;

  /** Hash function for packages: */
  struct ExtractPackageId
  {
  public:
    size_t operator()(const package &p) const
    {
      return p.get_id();
    }
  };

  typedef ExtractPackageId PackageHash;

  /** Compares steps according to their "goodness": their tier, then
   *  thier score, then their contents.
   *
   *  The comparisons are reversed, so better solutions compare
   *  "below" worse ones.
   */
  struct step_goodness_compare
  {
    const search_graph &graph;

    step_goodness_compare(const search_graph &_graph)
      : graph(_graph)
    {
    }

    bool operator()(int step_num1, int step_num2) const
    {
      // Optimization: a step always equals itself.
      if(step_num1 == step_num2)
	return false;

      const step &step1(graph.get_step(step_num1));
      const step &step2(graph.get_step(step_num2));

      // Note that *lower* tiers come "before" higher tiers, hence the
      // reversed comparison there.
      if(step1.step_tier < step2.step_tier)
	return true;
      else if(step2.step_tier < step1.step_tier)
	return false;
      else if(step2.score < step1.score)
	return true;
      else if(step1.score < step2.score)
	return false;
      else
	return step2.actions < step1.actions;
    }
  };

  /** \brief Represents the "essential" information about a step.
   *
   *  This information consists of the step's scores and its actions.
   *  Since these values never change over the course of a search and
   *  are unique to a step, they can be used to form an index of steps
   *  to avoid duplicates.  Also, since the scores are "mostly"
   *  unique, they can be used as an up-front "hash" of the step, to
   *  avoid an expensive set comparison operation.
   *
   *  Note that we don't try to pre-detect duplicates, because that
   *  would be a lot more complicated and it's not clear that it's
   *  worth the trouble (plus, it's not clear what should happen if
   *  all the children of a step are duplicates; it should be thrown
   *  out, but not go to the conflict tier!).
   */
  class step_contents
  {
    int score;
    int action_score;
    choice_set actions;

  public:
    step_contents()
      : score(0), action_score(0), actions()
    {
    }

    step_contents(int _score, int _action_score,
		  const choice_set &_actions)
      : score(_score), action_score(_action_score), actions(_actions)
    {
    }

    step_contents(const step &s)
      : score(s.score), action_score(s.action_score),
	actions(s.actions)
    {
    }

    bool operator<(const step_contents &other) const
    {
      if(score < other.score)
	return true;
      else if(other.score < score)
	return false;
      else if(action_score < other.action_score)
	return true;
      else if(other.action_score < action_score)
	return false;


      // Speed hack: order by size first to avoid traversing the whole
      // tree.
      if(actions.size() < other.actions.size())
	return true;
      else if(other.actions.size() < actions.size())
	return false;
      else
	return actions < other.actions;
    }
  };

  class instance_tracker;
  friend class instance_tracker;
  class instance_tracker
  {
    generic_problem_resolver &r;
  public:
    instance_tracker(generic_problem_resolver &_r)
      :r(_r)
    {
      cwidget::threads::mutex::lock l(r.execution_mutex);
      if(r.solver_executing)
	throw DoubleRunException();
      else
	r.solver_executing = true;
    }

    ~instance_tracker()
    {
      cwidget::threads::mutex::lock l(r.execution_mutex);
      eassert(r.solver_executing);

      r.solver_executing = false;
      r.solver_cancelled = false;
    }
  };

  /** \brief Used to convert a choice set into a model of Installation. */
  class choice_set_installation
  {
    const choice_set &actions;
    const resolver_initial_state<PackageUniverse> &initial_state;

  public:
    choice_set_installation(const choice_set &_actions,
			    const resolver_initial_state<PackageUniverse> &_initial_state)
      : actions(_actions),
	initial_state(_initial_state)
    {
    }

    version version_of(const package &p) const
    {
      version rval;
      if(actions.get_version_of(p, rval))
	return rval;
      else
	return initial_state.version_of(p);
    }
  };

  /** \brief The initial state of the resolver.
   *
   *  If this is not NULL, we need to use a more clever technique to
   *  get all the broken deps.  Can we get away with just iterating
   *  over all deps and calling broken_under()?
   */
  resolver_initial_state<PackageUniverse> initial_state;

  // Information regarding the weight given to various parameters;
  // packaged up in a struct so it can be easily used by the solution
  // constructors.
  solution_weights<PackageUniverse> weights;

  /** Solutions whose score is smaller than this value will be
   *  discarded rather than being enqueued.
   */
  int minimum_score;

  /** The number of "future" steps to examine after we find a solution
   *  in order to find a better one.
   */
  int future_horizon;

  /** The universe in which we are solving problems. */
  const PackageUniverse universe;

  /** If \b true, we have exhausted the list of solutions. */
  bool finished:1;

  /** If \b true, so-called "stupid" pairs of actions will be
   *  eliminated from solutions. (see paper)
   */
  bool remove_stupid:1;


  // Multithreading support variables.
  //
  //  These variables ensure (as a sanity-check) that only one thread
  //  executes the solver function at once, and allow the executing
  //  instance to be cleanly terminated.  They are managed by the
  //  instance_tracker class (see above).

  /** If \b true, a thread is currently executing in the solver. */
  bool solver_executing : 1;

  /** If \b true, the currently executing thread should stop at the
   *  next opportunity.
   */
  bool solver_cancelled : 1;

  /** Mutex guarding the solver_executing and stop_solver variables.
   *
   *  If a routine wants to execute some code conditionally based on
   *  whether the resolver is currently executing, it should grab this
   *  mutex, test solver_executing, and run the code if
   *  solver_executing is \b false.
   */
  cwidget::threads::mutex execution_mutex;



  queue_counts counts;

  /** Mutex guarding the cache of resolver status information. */
  cwidget::threads::mutex counts_mutex;



  /** All the steps that have not yet been processed.
   *
   *  Steps are sorted by tier, then by score, then by their contents.
   */
  std::set<int, step_goodness_compare> pending;

  /** \brief Counts how many steps are deferred. */
  int num_deferred;

  /** \brief The current minimum search tier.
   *
   *  Solutions generated below this tier are discarded.  Defaults to
   *  minimum_tier.
   */
  tier minimum_search_tier;

  /** \brief The current maximum search tier.
   *
   *  Solutions generated above this tier are placed into deferred.
   *  This is advanced automatically when the current tier is
   *  exhausted; it can also be advanced manually by the user.
   *
   *  Defaults to minimum_tier.
   */
  tier maximum_search_tier;

  /** Solutions generated "in the future", stored by reference to
   *  their step numbers.
   *
   *  The main reason this is persistent at the moment is so we don't
   *  lose solutions if find_next_solution() throws an exception.
   */
  std::set<int, step_goodness_compare> pending_future_solutions;

  /** \brief Stores already-seen search nodes that had their
   *  successors generated.
   *
   *  Each search node is mapped to the "canonical" step number that
   *  corresponds to it.  A list of clones is accumulated at that
   *  step, along with a single copy of the promotion set for all the
   *  clones.
   */
  std::map<step_contents, int> closed;

  /** Stores tier promotions: sets of installations that will force a
   *  solution to a higher tier of the search.
   */
  promotion_set promotions;

  /** \brief Stores the tail of the queue of tier promotions.
   *
   *  Steps hold reference-counted pointers to links in the chain
   *  corresponding to where the tail was when they were inserted.  If
   *  there have not been "very many" insertions since a step was
   *  generated, then instead of checking the global promotion set,
   *  we'll just apply the individual promotions one at a time.  New
   *  promotions will be filled in here.
   */
  boost::shared_ptr<promotion_queue_entry> promotion_queue_tail;

  /** The initial set of broken dependencies.  Kept here for use in
   *  the stupid-elimination algorithm.
   */
  imm::set<dep> initial_broken;

  /** The intrinsic tier of each version (indexed by version).
   *
   *  Store here instead of in the weights table because tiers are the
   *  resolver's responsibility, not the solution object's (because
   *  they are not a pure function of the contents of the solution; a
   *  solution might get a higher tier if we can prove that it will
   *  eventually have one anyway).
   */
  tier *version_tiers;

  /** \brief Used to track whether a single choice is approved or
   *  rejected.
   *
   *  The variables are used as the leaves in a large Boolean
   *  expression tree that is used to efficiently update the deferred
   *  status of steps and solvers.
   */
  class approved_or_rejected_info
  {
    // True if the choice is rejected.
    cwidget::util::ref_ptr<var_e<bool> > rejected; 
    // True if the choice is approved.
    cwidget::util::ref_ptr<var_e<bool> > approved;

  public:
    approved_or_rejected_info()
      : rejected(var_e<bool>::create(false)),
	approved(var_e<bool>::create(false))
    {
    }

    const cwidget::util::ref_ptr<var_e<bool> > &get_rejected() const
    {
      return rejected;
    }

    const cwidget::util::ref_ptr<var_e<bool> > &get_approved() const
    {
      return approved;
    }
  };

  /** \brief Stores the approved and rejected status of versions. */
  std::map<version, approved_or_rejected_info> user_approved_or_rejected_versions;

  /** \brief Stores the approved and rejected status of dependencies. */
  std::map<dep, approved_or_rejected_info> user_approved_or_rejected_broken_deps;

  /** \brief Expression class that calls back into the resolver when
   *         the value of its sub-expression changes.
   *
   *  The attached information is the choice that this expression
   *  affects; that choice must have an associated dependency.
   */
  class deferral_updating_expression : public expression_wrapper<bool>
  {
    choice deferred_choice;

    generic_problem_resolver &resolver;

    deferral_updating_expression(const cwidget::util::ref_ptr<expression<bool> > &_child,
				 const choice &_deferred_choice,
				 generic_problem_resolver &_resolver)
      : expression_wrapper<bool>(_child),
	deferred_choice(_deferred_choice),
	resolver(_resolver)
    {
      // Sanity-check.
      if(deferred_choice.get_type() == choice::install_version)
	eassert(deferred_choice.get_has_dep());
    }

  public:
    static cwidget::util::ref_ptr<deferral_updating_expression>
    create(const cwidget::util::ref_ptr<expression<bool> > &child,
	   const choice &deferred_choice,
	   generic_problem_resolver &resolver)
    {
      return new deferral_updating_expression(child,
					      deferred_choice,
					      resolver);
    }

    void changed(bool new_value)
    {
      if(!new_value)
	resolver.deferral_retracted(deferred_choice,
				    deferred_choice.get_dep());
      else
	{
	  LOG_TRACE(resolver.logger, deferred_choice << " is now deferred; marking it as such in all active steps.");
	  // Note that this is not quite right in logical terms.
	  // Technically, the promotion we generate should contain the
	  // choice that led to the deferral.  However, that's not
	  // right either: we don't have a choice object that can
	  // fully describe the reason this deferral occurred.
	  //
	  // However, this isn't actually a problem: the promotion
	  // will be used only to produce a generalized promotion, and
	  // generalization would remove the choice that was deferred
	  // anyway.  So we can just produce an (incorrect) empty
	  // promotion.
	  //
	  // (the alternative is to expand the types of choices we can
	  // handle, but that would impose costs on the rest of the
	  // program for a feature that would never be used)
	  promotion p(choice_set(), tier_limits::defer_tier,
		      get_child());
	  resolver.increase_solver_tier_everywhere(deferred_choice, p);
	}
    }
  };

  /** \brief Comparison operator on choices that treats choices which
   *  could have distinct deferral status as different.
   *
   *  In particular, dependencies are always significant, even if the
   *  choice is not a from-dep-source choice.
   */
  struct compare_choices_for_deferral
  {
    bool operator()(const choice &c1, const choice &c2) const
    {
      if(c1.get_type() < c2.get_type())
	return true;
      else if(c2.get_type() < c1.get_type())
	return false;
      else switch(c1.get_type())
	     {
	     case choice::install_version:
	       if(c1.get_from_dep_source() < c2.get_from_dep_source())
		 return true;
	       else if(c2.get_from_dep_source() < c1.get_from_dep_source())
		 return false;
	       else if(c1.get_ver() < c2.get_ver())
		 return true;
	       else if(c2.get_ver() < c1.get_ver())
		 return false;
	       else if(c1.get_has_dep() < c2.get_has_dep())
		 return true;
	       else if(c2.get_has_dep() < c1.get_has_dep())
		 return false;
	       else if(!c1.get_has_dep())
		 return false;
	       else if(c1.get_dep() < c2.get_dep())
		 return true;
	       else
		 return false;

	     case choice::break_soft_dep:
	       return c1.get_dep() < c2.get_dep();

	     default: return false; // Treat all invalid choices as equal.
	     }
    }
  };

  /** \brief Memoizes "is this deferred?" expressions for every choice
   *  in the search.
   *
   *  Each expression is contained in a wrapper whose sole purpose is
   *  to recompute the tier of the corresponding choice in all steps
   *  when it fires.
   *
   *  \sa build_is_deferred, build_is_deferred_real
   */
  std::map<choice, expression_weak_ref<expression_box<bool> >,
	   compare_choices_for_deferral> memoized_is_deferred;

  /** \todo This should hold weak references to heap-allocated
   *  structures so that memoizations wouldn't last longer than needed
   *  (if every instance of a solver set was changed, that solver set
   *  would be dropped).
   */
  boost::unordered_set<typename step::dep_solvers> memoized_dep_solvers;

  typename step::dep_solvers memoize_dep_solvers(const typename step::dep_solvers &solvers)
  {
    // std::set will insert a new element only if it doesn't already
    // contain an element with the same key, which is exactly what we
    // want here.
    std::pair<typename boost::unordered_set<typename step::dep_solvers>::const_iterator, bool>
      insert_result = memoized_dep_solvers.insert(solvers);

    if(insert_result.second)
      LOG_TRACE(logger, "Memoized new solver set: " << solvers);

    return *insert_result.first;
  }

  /** \brief If the given step is already "seen", mark it as a clone
   *  and return true (telling our caller to abort).
   */
  bool is_already_seen(int stepNum)
  {
    step &s(graph.get_step(stepNum));

    typename std::map<step_contents, int>::const_iterator found =
      closed.find(step_contents(s));
    if(found != closed.end() && found->second != stepNum)
      {
	LOG_TRACE(logger, "Step " << s.step_num << " is irrelevant: it was already encountered in this search.");
	graph.add_clone(found->second, stepNum);
	return true;
      }
    else
      return false;
  }

  /** \return \b true if the given step is "irrelevant": that is,
   *  either it was already generated and placed in the closed queue,
   *  or it was marked as having a conflict, or it is infinitely
   *  "bad".
   */
  bool irrelevant(const step &s)
  {
    const tier &s_tier = s.step_tier;
    if(s_tier >= tier_limits::conflict_tier)
      {
	LOG_TRACE(logger, "Step " << s.step_num << " is irrelevant: it contains a conflict.");
	return true;
      }
    else if(s_tier >= tier_limits::already_generated_tier)
      {
	LOG_TRACE(logger, "Step " << s.step_num << " is irrelevant: it was already produced as a result.");
	return true;
      }

    if(s.score < minimum_score)
      {
	LOG_TRACE(logger, "Step " << s.step_num << "is irrelevant: it has infinite badness " << s.score << "<" << minimum_score);
	return true;
      }

    return false;
  }

  /** \brief Adjust the tier of a step, keeping everything consistent. */
  void set_step_tier(int step_num,
		     const tier &t)
  {
    step &s(graph.get_step(step_num));

    if(s.step_tier == t)
      return;

    if(s.is_blessed_solution && t >= tier_limits::already_generated_tier)
      {
	LOG_TRACE(logger, "Step " << s.step_num
		  << " is a blessed solution; ignoring the attempt to promote it to tier " << t);
	return;
      }

    LOG_TRACE(logger, "Setting the tier of step " << step_num
	      << " to " << t);

    bool was_in_pending =  (pending.erase(step_num) > 0);
    bool was_in_pending_future_solutions =  (pending_future_solutions.erase(step_num) > 0);


    if(s.step_tier >= tier_limits::defer_tier &&
       s.step_tier < tier_limits::already_generated_tier)
      {
	if(was_in_pending)
	  --num_deferred;
	if(was_in_pending_future_solutions)
	  --num_deferred;
      }

    s.step_tier = t;


    if(was_in_pending)
      pending.insert(step_num);
    if(was_in_pending_future_solutions)
      pending_future_solutions.insert(step_num);


    if(s.step_tier >= tier_limits::defer_tier &&
       s.step_tier < tier_limits::already_generated_tier)
      {
	if(was_in_pending)
	  ++num_deferred;
	if(was_in_pending_future_solutions)
	  ++num_deferred;
      }
  }

  /** \brief Add a promotion to the global set of promotions.
   *
   *  This routine handles all the book-keeping that needs to take
   *  place.
   */
  void add_promotion(const promotion &p)
  {
    if(p.get_choices().size() == 0)
      LOG_TRACE(logger, "Ignoring the empty promotion " << p);
    else if(promotions.insert(p) != promotions.end())
      {
	LOG_TRACE(logger, "Added the promotion " << p
		  << " to the global promotion set.");

	promotion_queue_tail->set_promotion(p);
	eassert(promotion_queue_tail->get_has_contents());
	promotion_queue_tail = promotion_queue_tail->get_next();

	LOG_TRACE(logger, "The promotion queue now contains "
		  << promotion_queue_tail->get_index() << " promotions with "
		  << promotion_queue_tail->get_action_sum() << " total actions.");
      }
    else
      LOG_TRACE(logger, "Did not add " << p
		<< " to the global promotion set: it was redundant with an existing promotion.");
  }

  // Used as a callback by subroutines that want to add a promotion to
  // the global set.
  class promotion_adder
  {
    generic_problem_resolver &resolver;

  public:
    promotion_adder(generic_problem_resolver &_resolver)
      : resolver(_resolver)
    {
    }

    void operator()(int step_num, const promotion &p) const
    {
      resolver.add_promotion(step_num, p);
    }
  };

  /** \brief Add a promotion to the global set of promotions
   *  for a particular step.
   *
   *  This routine handles all the book-keeping that needs to take
   *  place: it adds the promotion to the global set, adds it to the
   *  list of promotions that need to be tested against all existing
   *  steps, and also attaches it to the step graph.
   */
  void add_promotion(int step_num, const promotion &p)
  {
    add_promotion(p);
    graph.schedule_promotion_propagation(step_num, p);
  }

  // Helper function for find_steps_containing_incipient_promotion.
  // Processes mapping information and updates the counters; when a
  // hit is found, inserts it into an output set.
  //
  // We don't immediately invoke a callback, because that might
  // actually lead to doing the wrong thing.  If several different
  // solvers (say, different from-source-dep solvers that install the
  // same version) hit the same promotion, then applying the promotion
  // immediately when we see the first solver might remove the other
  // ones.  When the caller finishes tracing the tree for the first
  // solver, it'll notice that the root step lacks the other solver
  // and not visit anything.
  class update_incipient_promotion_information
  {
    generic_problem_resolver &resolver;
    const unsigned int promotion_search_index;
    const unsigned int promotion_size;
    const choice &promotion_c;
    std::set<int> &active_hits; // Steps that contain the promotion in
			        // their action set.
    std::set<int> &incipient_hits; // Steps that contain the promotion
  				   // as an incipient promotion.

    void too_many_choices(int choices) const
    {
      LOG_ERROR(resolver.logger, "Internal error: the choice " << promotion_c
		<< " is match number " << choices
		<< " against a promotion of size " << promotion_size);
    }

  public:
    update_incipient_promotion_information(generic_problem_resolver &_resolver,
					   unsigned int _promotion_search_index,
					   unsigned int _promotion_size,
					   const choice &_promotion_c,
					   std::set<int> &_active_hits,
					   std::set<int> &_incipient_hits)
      : resolver(_resolver),
	promotion_search_index(_promotion_search_index),
	promotion_size(_promotion_size),
	promotion_c(_promotion_c),
	active_hits(_active_hits),
	incipient_hits(_incipient_hits)
    {
    }

    bool operator()(const choice &c,
		    const typename search_graph::choice_mapping_type how,
		    int step_num) const
    {
      step &s(resolver.graph.get_step(step_num));
      LOG_TRACE(resolver.logger, "update_incipient_promotion_information: visiting step " << step_num << " for " << c);

      if(s.last_promotion_search != promotion_search_index)
	{
	  s.last_promotion_search = promotion_search_index;
	  s.choice_set_hit_count = 0;
	  s.solver_set_hit_count = 0;
	}
      // Short-circuit if we can't possibly match.
      else if(s.solver_set_hit_count > 1)
	return true;

      switch(how)
	{
	case search_graph::choice_mapping_action:
	  LOG_TRACE(resolver.logger, "Hit step " << step_num << " as an action with " << c);
	  ++s.choice_set_hit_count;
	  if(s.choice_set_hit_count > promotion_size)
	    {
	      too_many_choices(s.choice_set_hit_count);
	      return true;
	    }
	  break;

	case search_graph::choice_mapping_solver:
	  LOG_TRACE(resolver.logger, "Hit step " << step_num << " as a solver with " << c);
	  if(s.solver_set_hit_count == 0)
	    {
	      s.solver_set_hit_count = 1;
	      s.first_solver_hit = promotion_c;
	    }
	  else if(s.first_solver_hit.contains(promotion_c))
	    // This is the same choice we already stored, so do
	    // nothing.
	    return true;
	  else if(promotion_c.contains(s.first_solver_hit))
	    // This is a more general version of the choice we already
	    // stored.
	    {
	      s.first_solver_hit = promotion_c;
	      return true;
	    }
	  else
	    // If we see a new solver, increment the solver hit count
	    // (thus marking this solver as invalid).
	    ++s.solver_set_hit_count;

	  break;
	}

      if(s.solver_set_hit_count <= 1 &&
	 s.choice_set_hit_count + s.solver_set_hit_count == promotion_size)
	{
	  if(s.solver_set_hit_count == 0)
	    active_hits.insert(step_num);
	  else
	    incipient_hits.insert(step_num);
	}

      return true;
    }
  };

  template<typename Callbacks>
  class find_steps_containing_incipient_promotion
  {
    generic_problem_resolver &resolver;
    unsigned int promotion_search_index;
    const promotion &p;
    Callbacks callbacks;

  public:
    find_steps_containing_incipient_promotion(generic_problem_resolver &_resolver,
					      unsigned int _promotion_search_index,
					      const promotion &_p,
					      Callbacks _callbacks)
      : resolver(_resolver),
	promotion_search_index(_promotion_search_index),
	p(_p),
	callbacks(_callbacks)
    {
    }

    bool operator()(const choice &c) const
    {
      std::set<int> active_hits;
      std::set<int> incipient_hits;

      update_incipient_promotion_information
	update_f(resolver,
		 promotion_search_index,
		 p.get_choices().size(),
		 c,
		 active_hits,
		 incipient_hits);

      resolver.graph.for_each_step_related_to_choice(c, update_f);

      for(std::set<int>::const_iterator it = active_hits.begin();
	  it != active_hits.end(); ++it)
	{
	  int step_num(*it);

	  LOG_DEBUG(resolver.logger, "Step " << step_num
		    << " contains " << p
		    << " as an active promotion.");
	  callbacks.active(step_num);
	}

      for(std::set<int>::const_iterator it = incipient_hits.begin();
	  it != incipient_hits.end(); ++it)
	{
	  int step_num(*it);
	  step &s(resolver.graph.get_step(step_num));

	  callbacks.incipient(step_num, s.first_solver_hit);
	}

      return true;
    }
  };

  /** \brief For every step containing the promotion, invoke
   *  callbacks.incipient(step_num, solver) if the promotion is
   *  contained as an incipient promotion, and
   *  callbacks.active(step_num) if it's contained in the action set.
   */
  template<typename Callbacks>
  void for_each_promotion_hit(const promotion &p, Callbacks callbacks)
  {
    find_steps_containing_incipient_promotion<Callbacks>
      find_promotion_f(*this,
		       graph.get_and_increment_promotion_search_index(),
		       p,
		       callbacks);
    p.get_choices().for_each(find_promotion_f);
  }

  /** \brief For each promotion hit, increase its tier in a solver set
   *  if it's incipient and in an action set if it's not.
   */
  class do_process_promotion
  {
    generic_problem_resolver &resolver;
    const promotion &p;

  public:
    do_process_promotion(generic_problem_resolver &_resolver,
			 const promotion &_p)
      : resolver(_resolver), p(_p)
    {
    }

    void incipient(int step_num, const choice &solver) const
    {
      resolver.increase_solver_tier(resolver.graph.get_step(step_num),
				    p, solver);
    }

    void active(int step_num) const
    {
      resolver.set_step_tier(step_num, p.get_tier());
      resolver.graph.schedule_promotion_propagation(step_num, p);
    }
  };

  /** \brief Find existing steps that contain the given promotion and
   *  apply it to them.
   */
  void process_promotion(const promotion &p)
  {
    LOG_TRACE(logger, "Processing the promotion " << p << " and applying it to all existing steps.");

    for_each_promotion_hit(p, do_process_promotion(*this, p));
  }


  // NB: this might visit more than is strictly necessary, since the
  // solver might only need to be recomputed when it's attached to a
  // particular dependency.  It would take too much book-keeping to
  // figure out which dependency needs to be re-examined ahead of
  // time.
  //
  // Only solvers whose tier equals the given tier are recomputed.
  class recompute_solver_tier_for_dep
  {
    generic_problem_resolver &resolver;
    step &s;
    const tier &t;

  public:
    recompute_solver_tier_for_dep(generic_problem_resolver &_resolver,
				  step &_s,
				  const tier &_t)
      : resolver(_resolver), s(_s), t(_t)
    {
    }

    bool operator()(const choice &solver,
		    const imm::list<dep> &deps) const
    {
      for(typename imm::list<dep>::const_iterator it = deps.begin();
	  it != deps.end(); ++it)
	resolver.recompute_solver_tier(s, *it, solver, t);

      return true;
    }
  };

  /** \brief For each promotion hit, recompute the solver tier if it's
   *  an incipient promotion and the step tier otherwise.
   */
  class recompute_tier_hits
  {
    generic_problem_resolver &resolver;
    const tier &t;

  public:
    recompute_tier_hits(generic_problem_resolver &_resolver,
			const tier &_t)
      : resolver(_resolver), t(_t)
    {
    }

    void incipient(int step_num, const choice &solver) const
    {
      step &s(resolver.graph.get_step(step_num));

      recompute_solver_tier_for_dep
	recompute_solver_tier_f(resolver, s, t);

      s.deps_solved_by_choice.for_each_key_contained_in(solver,
							recompute_solver_tier_f);
    }

    void active(int step_num) const
    {
      step &s(resolver.graph.get_step(step_num));

      // The promotion could only have affected the step's tier if the
      // step's tier is at least as high.
      if(s.step_tier >= t)
	resolver.recompute_step_tier(resolver.graph.get_step(step_num));
    }
  };

  void promotion_retracted(const promotion &p)
  {
    LOG_TRACE(logger, "Retracting all tier assignments that might be linked to " << p);

    for_each_promotion_hit(p, recompute_tier_hits(*this, p.get_tier()));
  }



  /** \brief Process all pending promotions. */
  void process_pending_promotions()
  {
#if 0
    // Processing promotions like this turned out to be too expensive.
    LOG_TRACE(logger, "Processing pending promotions and applying them to all steps.");
    while(!pending_promotions.empty())
      {
	promotion p(pending_promotions.front());
	pending_promotions.pop_front();

	process_promotion(p);
      }
#endif
  }

  class do_drop_deps_solved_by
  {
    step &s;
    log4cxx::LoggerPtr logger;

  public:
    do_drop_deps_solved_by(step &_s, const log4cxx::LoggerPtr &_logger)
      : s(_s), logger(_logger)
    {
    }

    bool operator()(const choice &c, const imm::list<dep> &deps) const
    {
      LOG_TRACE(logger, "Removing dependencies solved by " << c);

      for(typename imm::list<dep>::const_iterator it = deps.begin();
	  it != deps.end(); ++it)
	{
	  const dep &d(*it);

	  // Need to look up the solvers of the dep in order to know
	  // the number of solvers that it was entered into the
	  // by-num-solvers set with.
	  typename imm::map<dep, typename search_graph::step::dep_solvers>::node
	    solvers = s.unresolved_deps.lookup(d);

	  if(solvers.isValid())
	    {
	      const typename search_graph::step::dep_solvers &
		dep_solvers(solvers.getVal().second);

	      const imm::map<choice, typename step::solver_information, compare_choices_by_effects> &solver_map =
		dep_solvers.get_solvers();
	      LOG_TRACE(logger,
			"Removing the dependency " << d
			<< " with a solver set of " << solver_map);
	      int num_solvers = solver_map.size();
	      s.unresolved_deps_by_num_solvers.erase(std::make_pair(num_solvers, d));
	    }
	  else
	    LOG_TRACE(logger, "The dependency " << d
		      << " has no solver set, assuming it was already solved.");

	  s.unresolved_deps.erase(d);
	}

      s.deps_solved_by_choice.erase(c);
      return true;
    }
  };

  /** \brief Drop all dependencies from the given set that are solved
   *  by the given choice.
   */
  void drop_deps_solved_by(const choice &c, step &s) const
  {
    choice c_general(c.generalize());
    LOG_TRACE(logger, "Dropping dependencies in step "
	      << s.step_num << " that are solved by " << c_general);

    s.deps_solved_by_choice.for_each_key_contained_in(c_general,
						      do_drop_deps_solved_by(s, logger));

    LOG_TRACE(logger, "Done dropping dependencies in step "
	      << s.step_num << " that are solved by " << c_general);
  }

  class add_to_structural_reasons
  {
    typename step::dep_solvers &target;

  public:
    add_to_structural_reasons(typename step::dep_solvers &_target)
      : target(_target)
    {
    }

    bool operator()(const choice &c) const
    {
      target.add_structural_reason(c);
      return true;
    }
  };

  // Helper for strike_choice.  For each dependency that's solved by a
  // choice, remove the choice from the solvers list of that
  // dependency.  Also, update the global search graph's reverse index
  // so it doesn't map the choice to that dependency any more.
  class do_strike_choice
  {
    step &s;
    const choice_set &reasons;
    search_graph &graph;
    generic_problem_resolver &resolver;
    log4cxx::LoggerPtr logger;

  public:
    do_strike_choice(step &_s,
		     const choice_set &_reasons,
		     search_graph &_graph,
		     generic_problem_resolver &_resolver,
		     const log4cxx::LoggerPtr &_logger)
      : s(_s),
	reasons(_reasons),
	graph(_graph),
	resolver(_resolver),
	logger(_logger)
    {
    }

    // One slight subtlety here: the "victim" passed in might differ
    // from the "victim" passed to strike_choice.  Here, "victim" is
    // the choice linked to the actual solver; i.e., it could be a
    // from-dep-source choice.  In strike_choice, "victim" could be
    // more general.  Using the more specific victim means that we
    // remove the correct entries (there could be both general and
    // specific entries that need to be removed when striking a broad
    // choice).
    bool operator()(const choice &victim,
		    imm::list<dep> solved_by_victim) const
    {
      LOG_TRACE(logger, "Removing the choice " << victim
		<< " from the solver lists of " << solved_by_victim
		<< " in step " << s.step_num);

      for(typename imm::list<dep>::const_iterator it = solved_by_victim.begin();
	  it != solved_by_victim.end(); ++it)
	{
	  const dep &d(*it);
	  choice victim_with_dep(victim.copy_and_set_dep(d));

	  // Remove this step from the set of steps related to the
	  // solver that was deleted.
	  graph.remove_choice(victim_with_dep, s.step_num, d);

	  // Find the current number of solvers so we can yank the
	  // dependency out of the unresolved-by-num-solvers set.
	  typename imm::map<dep, typename step::dep_solvers>::node
	    current_solver_set_found = s.unresolved_deps.lookup(d);

	  if(current_solver_set_found.isValid())
	    {
	      const typename step::dep_solvers &
		current_solvers(current_solver_set_found.getVal().second);
	      const int current_num_solvers = current_solvers.get_solvers().size();

	      typename step::dep_solvers new_solvers(current_solvers);

	      LOG_TRACE(logger,
			"Removing the choice " << victim_with_dep
			<< " from the solver set of " << d
			<< " in step " << s.step_num
			<< ", new solvers: " << new_solvers.get_solvers());

	      new_solvers.remove_solver(victim_with_dep);
	      add_to_structural_reasons adder(new_solvers);
	      reasons.for_each(adder);

	      // Actually update the solvers of the dep.
	      s.unresolved_deps.put(d, resolver.memoize_dep_solvers(new_solvers));

	      const int new_num_solvers = new_solvers.get_solvers().size();

	      if(current_num_solvers != new_num_solvers)
		{
		  LOG_TRACE(logger, "Changing the number of solvers of "
			    << d << " from " << current_num_solvers
			    << " to " << new_num_solvers
			    << " in step " << s.step_num);
		  // Update the number of solvers.
		  s.unresolved_deps_by_num_solvers.erase(std::make_pair(current_num_solvers, d));
		  s.unresolved_deps_by_num_solvers.insert(std::make_pair(new_num_solvers, d));
		}

	      // Rescan the solvers, maybe updating the step's tier.
	      resolver.check_solvers_tier(s, new_solvers);
	    }
	  else
	    LOG_TRACE(logger, "The dependency " << d
		      << " has no solver set, assuming it was already solved.");
	}

      LOG_TRACE(logger, "Removing all solved-by links for " << victim
		<< " in step " << s.step_num);
      s.deps_solved_by_choice.erase(victim);

      return true;
    }
  };

  /** \brief Strike the given choice and any choice that it contains
   *         from all solver lists in the given step.
   */
  void strike_choice(step &s,
		     const choice &victim,
		     const choice_set &reason)
  {
    LOG_TRACE(logger, "Striking " << victim
	      << " from all solver lists in step " << s.step_num
	      << " with the reason set " << reason);


    do_strike_choice striker_f(s, reason, graph, *this, logger);
    s.deps_solved_by_choice.for_each_key_contained_in(victim, striker_f);
  }

  /** \brief Strike choices that are structurally forbidden by the
   *  given choice from the given step, and update the set of
   *  forbidden versions.
   */
  void strike_structurally_forbidden(step &s,
				     const choice &c)
  {
    switch(c.get_type())
      {
      case choice::install_version:
	{
	  {
	    choice_set reason;
	    // Throw out the from-dep-sourceness.
	    reason.insert_or_narrow(choice::make_install_version(c.get_ver(),
								 c.get_dep(),
								 c.get_id()));

	    for(typename package::version_iterator vi =
		  c.get_ver().get_package().versions_begin();
		!vi.end(); ++vi)
	      {
		version current(*vi);

		if(current != c.get_ver())
		  {
		    LOG_TRACE(logger,
			      "Discarding " << current
			      << ": monotonicity violation");
		    strike_choice(s,
				  choice::make_install_version(current, -1),
				  reason);
		  }
	      }
	  }


	  if(c.get_from_dep_source())
	    {
	      const version &c_ver = c.get_ver();
	      choice_set reason;
	      reason.insert_or_narrow(c);
	      for(typename dep::solver_iterator si =
		    c.get_dep().solvers_begin();
		  !si.end(); ++si)
		{
		  version current(*si);

		  if(current != c_ver)
		    {
		      LOG_TRACE(logger,
				"Discarding " << current
				<< ": forbidden by the resolution of "
				<< c.get_dep());
		      strike_choice(s,
				    choice::make_install_version(current, -1),
				    reason);
		      s.forbidden_versions.put(current, c);
		    }
		}
	    }
	}
	break;

      case choice::break_soft_dep:
	// \todo For broken soft deps, forbid each target of the
	// dependency.
	break;
      }
  }

  /** \brief Build an expression that computes whether the given
   *  choice is deferred.
   *
   *  This is normally invoked through its memoized frontend (without
   *  the _real suffix).
   */
  cwidget::util::ref_ptr<expression<bool> > build_is_deferred_real(const choice &c)
  {
    switch(c.get_type())
      {
      case choice::install_version:
	{
	  // We can't correctly compute deferral information for a
	  // choice with no dependency.  And since this should only be
	  // invoked on a solver, it's an error if there is no
	  // dependency: all solvers ought to have a dependency
	  // attached.
	  eassert(c.get_has_dep());

	  const version &c_ver(c.get_ver());
	  const approved_or_rejected_info &c_info =
	    user_approved_or_rejected_versions[c_ver];

	  const cwidget::util::ref_ptr<expression<bool> > &c_rejected(c_info.get_rejected());
	  const cwidget::util::ref_ptr<expression<bool> > &c_approved(c_info.get_approved());

	  // Versions are deferred if they are rejected, OR if they
	  // are NOT approved AND some other solver of the same
	  // dependency (which might include breaking it!) is
	  // approved.
	  std::vector<cwidget::util::ref_ptr<expression<bool> > >
	    others_approved;
	  const dep &c_dep(c.get_dep());
	  for(typename dep::solver_iterator si = c_dep.solvers_begin();
	      !si.end(); ++si)
	    {
	      version solver(*si);
	      if(solver != c_ver)
		others_approved.push_back(user_approved_or_rejected_versions[solver].get_approved());
	    }

	  if(c_dep.is_soft())
	    others_approved.push_back(user_approved_or_rejected_broken_deps[c_dep].get_approved());

	  return
	    or_e::create(c_rejected,
			 and_e::create(not_e::create(c_approved),
				       or_e::create(others_approved.begin(),
						    others_approved.end())));
	}
	break;

      case choice::break_soft_dep:
	{
	  const dep &c_dep(c.get_dep());
	  const approved_or_rejected_info &c_info =
	    user_approved_or_rejected_broken_deps[c_dep];



	  const cwidget::util::ref_ptr<expression<bool> > &c_rejected(c_info.get_rejected());
	  const cwidget::util::ref_ptr<expression<bool> > &c_approved(c_info.get_approved());

	  // Broken dependencies are deferred if they are rejected, OR
	  // if they are NOT approved AND some solver of the same
	  // dependency is approved.
	  std::vector<cwidget::util::ref_ptr<expression<bool> > >
	    others_approved;
	  for(typename dep::solver_iterator si = c_dep.solvers_begin();
	      !si.end(); ++si)
	    {
	      version solver(*si);
	      others_approved.push_back(user_approved_or_rejected_versions[solver].get_approved());
	    }

	  return
	    or_e::create(c_rejected,
			 and_e::create(not_e::create(c_approved),
				       or_e::create(others_approved.begin(),
						    others_approved.end())));
	}
	break;

      default:
	LOG_ERROR(logger, "Internal error: bad choice type " << c.get_type());
	return var_e<bool>::create(false);
      }
  }

  /** \brief Memoized version of build_is_deferred. */
  cwidget::util::ref_ptr<expression_box<bool> > build_is_deferred_listener(const choice &c)
  {
    typename std::map<choice, expression_weak_ref<expression_box<bool> > >::const_iterator
      found = memoized_is_deferred.find(c);

    if(found != memoized_is_deferred.end())
      {
	const expression_weak_ref<expression_box<bool> > &ref(found->second);
	if(ref.get_valid())
	  return ref.get_value();
      }

    cwidget::util::ref_ptr<expression<bool> > expr(build_is_deferred_real(c));
    cwidget::util::ref_ptr<expression_box<bool> > rval(deferral_updating_expression::create(expr, c, *this));
    memoized_is_deferred[c] = rval;
    return rval;
  }

  class invoke_recompute_solver_tier
  {
    generic_problem_resolver &resolver;
    const dep &d;

  public:
    invoke_recompute_solver_tier(generic_problem_resolver &_resolver,
				 const dep &_d)
      : resolver(_resolver),
	d(_d)
    {
    }

    bool operator()(const choice &c,
		    typename search_graph::choice_mapping_type how,
		    int step_num) const
    {
      step &s(resolver.graph.get_step(step_num));
      resolver.recompute_solver_tier(s, d, c);

      return true;
    }
  };

  /** \brief Invoked when a solver's tier needs to be recomputed.
   *
   *  Locates the solver in each step that it solves, tosses its tier,
   *  and recomputes it from scratch.
   */
  void deferral_retracted(const choice &deferral_choice,
			  const dep &deferral_dep)
  {
    invoke_recompute_solver_tier recompute_f(*this, deferral_dep);
    graph.for_each_step_related_to_choice_with_dep(deferral_choice,
						   deferral_dep,
						   recompute_f);
  }

  /** \brief Recompute the solver of a single tier in the given
   *  step.
   *
   *  The tier is only recomputed if it equals check_tier.
   *  recompute_solver_tier() is used to retract promotions; if the
   *  tier of the solver is not equal to the tier of the promotion
   *  (passed as check_tier), then another promotion must have
   *  elevated the tier even farther, so there's no need to recompute
   *  it.
   *
   *  check_tier and do_check_tier are a bit hacky, but they avoid
   *  having to have two instantiations of this code.
   */
  void recompute_solver_tier(step &s,
			     const dep &solver_dep,
			     const choice &solver,
			     const tier &check_tier = tier_limits::minimum_tier,
			     bool do_check_tier = false)
  {
    typename imm::map<dep, typename step::dep_solvers>::node
      found_solvers(s.unresolved_deps.lookup(solver_dep));

    if(found_solvers.isValid())
      {
	typename step::dep_solvers new_dep_solvers(found_solvers.getVal().second);
	const imm::map<choice, typename step::solver_information, compare_choices_by_effects> &
	  new_solvers(new_dep_solvers.get_solvers());

	typename imm::map<choice, typename step::solver_information, compare_choices_by_effects>::node
	  found_solver(new_solvers.lookup(solver));
	if(!found_solver.isValid())
	  LOG_ERROR(logger, "Internal error: the choice " << solver
		    << " is listed in the reverse index for step "
		    << s.step_num << " as a solver for "
		    << solver_dep << ", but it doesn't appear in that step.");
	else if(!do_check_tier || found_solver.getVal().second.get_tier() == check_tier)
	  {
	    tier new_tier;
	    cwidget::util::ref_ptr<expression_box<bool> > new_tier_is_deferred;
	    cwidget::util::ref_ptr<expression<bool> > new_tier_valid;
	    choice solver_with_dep(solver.copy_and_set_dep(solver_dep));
	    get_solver_tier(solver_with_dep,
			    new_tier, new_tier_is_deferred, new_tier_valid);

	    typename step::solver_information
	      new_solver_inf(new_tier,
			     choice_set(),
			     new_tier_valid,
			     new_tier_is_deferred);
	    new_dep_solvers.set_solver_information(solver_with_dep, new_solver_inf);
	    s.unresolved_deps.put(solver_dep, memoize_dep_solvers(new_dep_solvers));
	    LOG_TRACE(logger, "Recomputed the tier of "
		      << solver << " in the solver list of "
		      << solver_dep << " in step " << s.step_num
		      << "; new solver list: " << new_dep_solvers);


	    find_promotions_for_solver(s, solver_with_dep);
	    // Recompute the step's tier from scratch.
	    //
	    // \todo Only do this if the tier went down, and just do a
	    // local recomputation otherwise?
	    recompute_step_tier(s);
	  }
      }
  }

  /** \brief Predicate used to throw away promotions whose tier is too
   *  high.
   */
  class not_above_tier
  {
    const tier &t;

  public:
    not_above_tier(const tier &_t)
      : t(_t)
    {
    }

    bool operator()(const promotion &p) const
    {
      return !(p.get_tier() >= t);
    }
  };

  /** \brief Find promotions triggered by the given solver and
   *  increment its tier accordingly.
   */
  void find_promotions_for_solver(step &s,
				  const choice &solver)
  {
    // \note imm::list<dep> is used so we don't have two separate
    // instantiations of
    // find_highest_incipient_promotions_containing().
    generic_choice_indexed_map<PackageUniverse, imm::list<dep> > output_domain;
    boost::unordered_map<choice, promotion> triggered_promotions;

    output_domain.put(solver, imm::list<dep>());

    not_above_tier not_above_p(s.is_blessed_solution
			       ? tier_limits::already_generated_tier
			       : tier_limits::maximum_tier);
    promotions.find_highest_incipient_promotions_containing(s.actions,
							    solver,
							    output_domain,
							    not_above_p,
							    triggered_promotions);

    // Sanity-check.
    if(triggered_promotions.size() > 1)
      LOG_ERROR(logger,
		"Internal error: found " << triggered_promotions.size()
		<< " (choice -> promotion) mappings for a single choice.");

    for(typename boost::unordered_map<choice, promotion>::const_iterator it =
	  triggered_promotions.begin();
	it != triggered_promotions.end(); ++it)
      increase_solver_tier(s, it->second, it->first);
  }

  /** \brief Compute the basic tier information of a choice.
   *
   *  This is the tier it will have unless it's hit by a promotion.
   */
  void get_solver_tier(const choice &c,
		       tier &out_tier,
		       cwidget::util::ref_ptr<expression_box<bool> > &out_c_is_deferred,
		       cwidget::util::ref_ptr<expression<bool> > &out_tier_valid)
  {
    // Right now only deferrals can be retracted; other tier
    // assignments are immutable.
    out_c_is_deferred = build_is_deferred_listener(c);

    out_tier = tier_limits::minimum_tier;
    if(out_c_is_deferred->get_value())
      {
	out_tier = tier_limits::defer_tier;
	out_tier_valid = out_c_is_deferred->get_child();
      }
    else
      {
	out_tier_valid = cwidget::util::ref_ptr<expression<bool> >();
	switch(c.get_type())
	  {
	  case choice::install_version:
	    out_tier = version_tiers[c.get_ver().get_id()];
	    break;

	  case choice::break_soft_dep:
	    // \todo Have a tier based on breaking soft deps?
	    out_tier = tier_limits::minimum_tier;
	    break;
	  }
      }
  }

  /** \brief Add a solver to a list of dependency solvers for a
   *  particular step.
   *
   *  \param s         The step to update.
   *  \param solvers   The solvers set to fill in.
   *  \param d         The dependency whose solvers are being updated
   *                   (used to update the reverse map).
   *  \param solver    The choice to add.
   *
   *  If the solver is structurally forbidden in the step, the reason
   *  is added to the structural reasons list of the solvers
   *  structure; otherwise, the choice is added to the solvers list.
   */
  void add_solver(step &s,
		  typename step::dep_solvers &solvers,
		  const dep &d,
		  const choice &solver)
  {
    // The solver is structurally forbidden if it is in the forbidden
    // map, OR if another version of the same package is selected.
    //
    // First we check if the solver is forbidden, and return early if
    // it is.
    if(solver.get_type() == choice::install_version)
      {
	const version ver(solver.get_ver());
	version selected;
	if(s.actions.get_version_of(ver.get_package(), selected))
	  {
	    if(selected == ver)
	      // There's not really anything we can do to fix this: the
	      // dependency just shouldn't be inserted at all!
	      LOG_ERROR(logger,
			"Internal error: the solver "
			<< solver << " of a supposedly unresolved dependency is already installed in step "
			<< s.step_num);
	    else
	      {
		LOG_TRACE(logger,
			  "Not adding " << solver
			  << ": monotonicity violation due to "
			  << selected);
		choice reason(choice::make_install_version(selected, -1));
		solvers.add_structural_reason(reason);
	      }

	    return; // If the package is already modified, abort.
	  }
	else
	  {
	    typename imm::map<version, choice>::node forbidden_found =
	      s.forbidden_versions.lookup(ver);

	    if(forbidden_found.isValid())
	      {
		const choice &reason(forbidden_found.getVal().second);

		LOG_TRACE(logger,
			  "Not adding " << solver
			  << ": it is forbidden due to the action "
			  << reason);
		solvers.add_structural_reason(reason);

		return;
	      }
	  }
      }

    // The choice is added with its intrinsic tier to start with.
    // Later, in step 6 of the update algorithm, we'll find promotions
    // that include the new solvers and update tiers appropriately.
    tier choice_tier;
    cwidget::util::ref_ptr<expression_box<bool> > choice_is_deferred;
    cwidget::util::ref_ptr<expression<bool> > choice_tier_valid;
    get_solver_tier(solver, choice_tier, choice_is_deferred, choice_tier_valid);

    LOG_TRACE(logger, "Adding the solver " << solver
	      << " with initial tier " << choice_tier);
    {
      typename step::solver_information
	new_solver_inf(choice_tier,
		       choice_set(),
		       choice_tier_valid,
		       choice_is_deferred);
      solvers.set_solver_information(solver, new_solver_inf);
    }

    // Update the deps-solved-by-choice map (add the dep being
    // processed to the list).
    imm::list<dep> old_deps_solved;

    if(!s.deps_solved_by_choice.try_get(solver, old_deps_solved))
      s.deps_solved_by_choice.put(solver, imm::list<dep>::make_singleton(d));
    else
      {
	imm::list<dep> new_deps_solved(imm::list<dep>::make_cons(d, old_deps_solved));
	s.deps_solved_by_choice.put(solver, new_deps_solved);
      }

    graph.bind_choice(solver, s.step_num, d);
  }

  /** \brief Find the smallest tier out of the solvers of a single
   *  dependency.
   */
  class find_solvers_tier
  {
    tier &output_tier;

  public:
    find_solvers_tier(tier &_output_tier)
      : output_tier(_output_tier)
    {
      output_tier = tier_limits::maximum_tier;
    }

    bool operator()(const std::pair<choice, typename step::solver_information> &p) const
    {
      const tier &p_tier(p.second.get_tier());
      if(p_tier < output_tier)
	output_tier = p_tier;

      return true;
    }
  };

  /** \brief Find the largest tier of any dependency. */
  class find_largest_dep_tier
  {
    tier &output_tier;

  public:
    find_largest_dep_tier(tier &_output_tier)
      : output_tier(_output_tier)
    {
    }

    bool operator()(const std::pair<dep, typename step::dep_solvers> &p) const
    {
      tier dep_tier;

      p.second.get_solvers().for_each(find_solvers_tier(dep_tier));

      if(output_tier < dep_tier)
	output_tier = dep_tier;

      return true;
    }
  };

  /** \brief Recompute a step's tier from scratch.
   *
   *  It is assumed that all the solvers in the step have the correct
   *  tier; the recomputation is based on them.
   *
   *  This is a bit of a sledgehammer.  The places where this is used
   *  could be tuned to not need it; currently I'm just hoping it's
   *  invoked few enough times to not matter.
   */
  void recompute_step_tier(step &s)
  {
    LOG_TRACE(logger, "Recomputing the tier of step " << s.step_num
	      << " (was " << s.step_tier << ")");

    s.unresolved_deps.for_each(find_largest_dep_tier(s.step_tier));
  }

  // Build a generalized promotion from the entries of a dep-solvers
  // set.
  class build_promotion
  {
    tier &output_tier;
    choice_set &output_reasons;
    std::vector<cwidget::util::ref_ptr<expression<bool> > > & output_valid_conditions;

  public:
    build_promotion(tier &_output_tier, choice_set &_output_reasons,
		    std::vector<cwidget::util::ref_ptr<expression<bool> > > &_output_valid_conditions)
      : output_tier(_output_tier),
	output_reasons(_output_reasons),
	output_valid_conditions(_output_valid_conditions)
    {
      output_tier = tier_limits::maximum_tier;
      output_reasons = choice_set();
      output_valid_conditions.clear();
    }

    bool operator()(const std::pair<choice, typename step::solver_information> &entry) const
    {
      if(entry.second.get_tier() < output_tier)
	// Maybe we have a new, lower tier.
	output_tier = entry.second.get_tier();

      // Correctness here depends on the fact that the reason set is
      // pre-generalized (the solver itself is already removed).
      output_reasons.insert_or_narrow(entry.second.get_reasons());
      cwidget::util::ref_ptr<expression<bool> > tier_valid(entry.second.get_tier_valid());
      if(tier_valid.valid())
	output_valid_conditions.push_back(tier_valid);

      return true;
    }
  };

  /** \brief If the given dependency solver set implies a promotion,
   *         attempt to insert that promotion; also, update the tier
   *         of the step to be at least the lowest tier of any of the
   *         solvers in the given list.
   *
   *  If the set is empty, this just inserts a conflict.
   */
  void check_solvers_tier(step &s, const typename step::dep_solvers &solvers)
  {
    tier t;
    choice_set reasons;
    std::vector<cwidget::util::ref_ptr<expression<bool> > > valid_conditions;

    solvers.get_solvers().for_each(build_promotion(t, reasons, valid_conditions));

    for(typename imm::list<choice>::const_iterator it =
	  solvers.get_structural_reasons().begin();
	it != solvers.get_structural_reasons().end(); ++it)
      {
	reasons.insert_or_narrow(*it);
      }

    cwidget::util::ref_ptr<expression<bool> > valid_condition;
    switch(valid_conditions.size())
      {
      case 0:
	// If there are no validity conditions, don't create one for
	// the promotion.
	break;

      case 1:
	// If there's just one validity condition, copy it to the
	// promotion.
	valid_condition = valid_conditions.front();
	break;

      default:
	// If there are multiple validity conditions, the promotion
	// depends on them all.
	valid_condition = and_e::create(valid_conditions.begin(),
					valid_conditions.end());
	break;
      }


    if(get_current_search_tier() < t)
      {
	promotion p(reasons, t, valid_condition);
	LOG_TRACE(logger, "Emitting a new promotion " << p
		  << " at step " << s.step_num);

	add_promotion(s.step_num, p);
      }

    if(s.step_tier < t)
      set_step_tier(s.step_num, t);
  }

  /** \brief Increases the tier of a single step. */
  void increase_step_tier(step &s,
			  const promotion &p)
  {
    const tier &p_tier(p.get_tier());

    if(s.step_tier < p_tier)
      set_step_tier(s.step_num, p_tier);
  }

  // Increases the tier of each dependency in each dependency list
  // this is applied to.  Helper for increase_solver_tier.
  struct do_increase_solver_tier
  {
    step &s;
    const tier &new_tier;
    const choice_set &new_choices;
    const cwidget::util::ref_ptr<expression<bool> > &valid_condition;
    generic_problem_resolver &resolver;
    log4cxx::LoggerPtr logger;

  public:
    do_increase_solver_tier(step &_s,
			    const tier &_new_tier,
			    const choice_set &_new_choices,
			    const cwidget::util::ref_ptr<expression<bool> > &_valid_condition,
			    generic_problem_resolver &_resolver,
			    const log4cxx::LoggerPtr &_logger)
      : s(_s), new_tier(_new_tier), new_choices(_new_choices),
	valid_condition(_valid_condition),
	resolver(_resolver),
	logger(_logger)
    {
    }

    bool operator()(const choice &solver, const imm::list<dep> &solved) const
    {
      for(typename imm::list<dep>::const_iterator it = solved.begin();
	  it != solved.end(); ++it)
	{
	  const dep &d(*it);
	  choice solver_with_dep(solver.copy_and_set_dep(d));

	  typename imm::map<dep, typename step::dep_solvers>::node current_solver_set_found =
	    s.unresolved_deps.lookup(d);

	  if(current_solver_set_found.isValid())
	    {
	      const typename step::dep_solvers &current_solvers(current_solver_set_found.getVal().second);

	      typename step::dep_solvers new_solvers(current_solvers);

	      // Sanity-check: verify that the solver really
	      // resides in the solver set of this dependency.
	      typename imm::map<choice, typename step::solver_information, compare_choices_by_effects>::node
		solver_found(new_solvers.get_solvers().lookup(solver_with_dep));

	      if(!solver_found.isValid())
		LOG_ERROR(logger, "Internal error: in step " << s.step_num
			  << ", the solver " << solver
			  << " is claimed to be a solver of " << d
			  << " but does not appear in its solvers list.");
	      else
		{
		  const typename step::solver_information &old_inf =
		    solver_found.getVal().second;

		  // Don't do anything if the tier won't increase.
		  // Empirically, the resolver was wasting lots of
		  // time and memory increasing tiers when the solver
		  // tiers hadn't actually changed.
		  if(old_inf.get_tier() < new_tier)
		    {
		      typename step::solver_information
			new_inf(new_tier,
				new_choices,
				valid_condition,
				old_inf.get_is_deferred_listener());
		      new_solvers.set_solver_information(solver_with_dep, new_inf);

		      s.unresolved_deps.put(d, resolver.memoize_dep_solvers(new_solvers));
		      resolver.check_solvers_tier(s, new_solvers);

		      LOG_TRACE(logger, "Increased the tier of "
				<< solver_with_dep << " to " << new_tier
				<< " in the solvers list of "
				<< d << " in step " << s.step_num
				<< " with the reason set " << new_choices
				<< " and validity condition " << valid_condition
				<< "; new solvers list: " << new_solvers);

		    }
		}
	    }
	}

      return true;
    }
  };

  /** \brief Increase the tier of a solver (for instance, because a
   *  new incipient promotion was detected).
   */
  void increase_solver_tier(step &s,
			    const promotion &p,
			    const choice &solver)
  {
    LOG_TRACE(logger, "Applying the promotion " << p
	      << " to the solver " << solver
	      << " in the step " << s.step_num);
    const tier &new_tier(p.get_tier());
    // There are really two cases here: either the tier was increased
    // to the point that the solver should be ejected, or the tier
    // should just be bumped up a bit.  Either way, we might end up
    // changing the tier of the whole step.
    if(new_tier >= tier_limits::conflict_tier ||
       new_tier >= tier_limits::already_generated_tier)
      {
	// \todo this throws away information about whether we're at
	// the already-generated tier.  This isn't that important,
	// except that it means that the already-generated tier will
	// become fairly meaningless.  I could store this information
	// at the cost of a little extra space in every solver cell,
	// or I could get rid of the already-generated tier (just use
	// the conflict tier), or I could not worry about it.

	strike_choice(s, solver, p.get_choices());
      }
    else
      {
	choice_set new_choices(p.get_choices());
	new_choices.remove_overlaps(solver);

	const cwidget::util::ref_ptr<expression<bool> > &
	  valid_condition(p.get_valid_condition());

	LOG_TRACE(logger, "Increasing the tier of " << solver
		  << " to " << new_tier << " in all solver lists in step "
		  << s.step_num << " with the reason set " << new_choices);

	do_increase_solver_tier
	  do_increase_solver_tier_f(s, new_tier, new_choices,
				    valid_condition,
				    *this,
				    logger);

	s.deps_solved_by_choice.for_each_key_contained_in(solver, 
							  do_increase_solver_tier_f);
      }
  }

  /** \brief Increase the tier of each solver that it's applied to.
   */
  class do_increase_solver_tier_everywhere
  {
    generic_problem_resolver &r;
    const choice &solver;
    const promotion &p;

  public:
    do_increase_solver_tier_everywhere(generic_problem_resolver &_r,
				       const choice &_solver,
				       const promotion &_p)
      : r(_r), solver(_solver), p(_p)
    {
    }

    bool operator()(const choice &c,
		    typename search_graph::choice_mapping_type tp,
		    int step_num) const
    {
      step &s(r.graph.get_step(step_num));

      switch(tp)
	{
	case search_graph::choice_mapping_solver:
	  r.increase_solver_tier(s, p, solver);
	  break;

	case search_graph::choice_mapping_action:
	  r.increase_step_tier(s, p);
	}

      return true;
    }
  };

  /** \brief Increase the tier of a solver everywhere it appears: that
   *  is, both in solver lists and in action sets.
   */
  void increase_solver_tier_everywhere(const choice &solver,
				       const promotion &p)
  {
    LOG_TRACE(logger, "Increasing the tier of " << solver
	      << " according to the promotion " << p
	      << " in all active steps.");
    do_increase_solver_tier_everywhere
      increase_solver_tier_everywhere_f(*this, solver, p);

    graph.for_each_step_related_to_choice(solver,
					  increase_solver_tier_everywhere_f);
  }

  class do_find_promotions_for_solver
  {
    generic_problem_resolver &r;
    step &s;

  public:
    do_find_promotions_for_solver(generic_problem_resolver &_r,
				  step &_s)
      : r(_r), s(_s)
    {
    }

    bool operator()(const std::pair<choice, typename step::solver_information> &p) const
    {
      r.find_promotions_for_solver(s, p.first);
      return true;
    }
  };

  /** \brief Check for promotions at each solver of the given
   *  dependency.
   */
  void find_promotions_for_dep_solvers(step &s, const dep &d)
  {
    typename imm::map<dep, typename step::dep_solvers>::node found =
      s.unresolved_deps.lookup(d);

    if(found.isValid())
      {
	do_find_promotions_for_solver find_promotions_f(*this, s);
	const typename step::dep_solvers &dep_solvers(found.getVal().second);
	dep_solvers.get_solvers().for_each(find_promotions_f);
      }
  }

  /** \brief Add a single unresolved dependency to a step.
   *
   *  This routine does not check that the dependency is really
   *  unresolved.
   */
  void add_unresolved_dep(step &s, const dep &d)
  {
    if(s.unresolved_deps.domain_contains(d))
      {
	LOG_TRACE(logger, "The dependency " << d << " is already unresolved in step "
		  << s.step_num << ", not adding it again.");
	return;
      }

    LOG_TRACE(logger, "Marking the dependency " << d << " as unresolved in step "
	      << s.step_num);

    // Build up a list of the possible solvers of the dependency.
    typename step::dep_solvers solvers;
    for(typename dep::solver_iterator si = d.solvers_begin();
	!si.end(); ++si)
      add_solver(s, solvers, d, choice::make_install_version(*si, d, -1));

    // If it isn't a soft dependency, consider removing the source to
    // fix it.
    if(!d.is_soft())
      {
	version source(d.get_source());
	package source_pkg(source.get_package());

	for(typename package::version_iterator vi = source_pkg.versions_begin();
	    !vi.end(); ++vi)
	  {
	    version ver(*vi);

	    if(ver != source)
	      add_solver(s, solvers, d,
			 choice::make_install_version_from_dep_source(ver, d, -1));
	  }
      }
    else
      add_solver(s, solvers, d,
		 choice::make_break_soft_dep(d, -1));

    s.unresolved_deps.put(d, memoize_dep_solvers(solvers));
    LOG_TRACE(logger, "Marked the dependency " << d
	      << " as unresolved in step " << s.step_num
	      << " with solver list " << solvers);

    const int num_solvers = solvers.get_solvers().size();
    s.unresolved_deps_by_num_solvers.insert(std::make_pair(num_solvers, d));

    find_promotions_for_dep_solvers(s, d);
    check_solvers_tier(s, solvers);
  }

  /** \brief Find all the dependencies that are unresolved in step s
   *  and that involve c in some way, then add them to the unresolved
   *  set.
   *
   *  c must already be contained in s.actions.
   */
  void add_new_unresolved_deps(step &s, const choice &c)
  {
    switch(c.get_type())
      {
      case choice::install_version:
	{
	  choice_set_installation
	    test_installation(s.actions, initial_state);

	  version new_version = c.get_ver();
	  version old_version = initial_state.version_of(new_version.get_package());

	  LOG_TRACE(logger, "Finding new unresolved dependencies in step "
		    << s.step_num << " caused by replacing "
		    << old_version << " with " << new_version << ".");

	  // Check reverse deps of the old version.
	  for(typename version::revdep_iterator rdi = old_version.revdeps_begin();
	      !rdi.end(); ++rdi)
	    {
	      dep rd(*rdi);

	      if(rd.broken_under(test_installation))
		{
		  if(!(rd.is_soft() &&
		       s.actions.contains(choice::make_break_soft_dep(rd, -1))))
		    add_unresolved_dep(s, rd);
		}
	    }

	  for(typename version::revdep_iterator rdi = new_version.revdeps_begin();
	      !rdi.end(); ++rdi)
	    {
	      dep rd(*rdi);

	      if(rd.broken_under(test_installation))
		{
		  if(!(rd.is_soft() &&
		       s.actions.contains(choice::make_break_soft_dep(rd, -1))))
		    add_unresolved_dep(s, rd);
		}
	    }

	  for(typename version::dep_iterator di = new_version.deps_begin();
	      !di.end(); ++di)
	    {
	      dep d(*di);

	      if(d.broken_under(test_installation))
		// Note: no need to test if this was chosen to be
		// broken, because it can't possibly have been
		// broken until now.
		add_unresolved_dep(s, d);
	    }
	}

	break;

      case choice::break_soft_dep:
	// Leaving a soft dependency broken never causes any other
	// dependency to become broken.
	break;
      }
  }

  /** \brief Find incipient promotions for the given step that contain
   *  the given choice.
   */
  void find_new_incipient_promotions(step &s,
				     const choice &c)
  {
    boost::unordered_map<choice, promotion> output;

    not_above_tier not_above_p(s.is_blessed_solution
			       ? tier_limits::already_generated_tier
			       : tier_limits::maximum_tier);
    promotions.find_highest_incipient_promotions_containing(s.actions,
							    c,
							    s.deps_solved_by_choice,
							    not_above_p,
							    output);

    for(typename boost::unordered_map<choice, promotion>::const_iterator it =
	  output.begin(); it != output.end(); ++it)
      increase_solver_tier(s, it->second, it->first);
  }

  /** \brief Update a step's score to compute its successor, given
   *  that the given choice was added to its action set.
   */
  void extend_score_to_new_step(step &s, const choice &c) const
  {
    s.action_score += weights.step_score;

    switch(c.get_type())
      {
      case choice::break_soft_dep:
	s.action_score += weights.unfixed_soft_score;
	break;

      case choice::install_version:
	{
	  const version &ver(c.get_ver());
	  const version old_ver(initial_state.version_of(ver.get_package()));
	  const int new_score = weights.version_scores[ver.get_id()];
	  const int old_score = weights.version_scores[old_ver.get_id()];

	  LOG_TRACE(logger, "Modifying the score of step "
		    << s.step_num << " by "
		    << std::showpos << (new_score - old_score)
		    << std::noshowpos << " to account for the replacement of "
		    << old_ver << " (score " << old_score << ") by "
		    << ver << " (score " << new_score << ")");

	  s.action_score += new_score;
	  s.action_score -= old_score;

	  // Look for joint score constraints triggered by adding this
	  // choice.
	  const typename solution_weights<PackageUniverse>::joint_score_set::const_iterator
	    joint_scores_found = weights.get_joint_scores().find(c);

	  if(joint_scores_found != weights.get_joint_scores().end())
	    {
	      typedef typename solution_weights<PackageUniverse>::joint_score joint_score;
	      for(typename std::vector<joint_score>::const_iterator it =
		    joint_scores_found->second.begin();
		  it != joint_scores_found->second.end(); ++it)
		{
		  if(s.actions.contains(it->get_choices()))
		    {
		      LOG_TRACE(logger, "Adjusting the score of "
				<< s.step_num << " by "
				<< std::showpos << it->get_score()
				<< std::noshowpos
				<< " for a joint score constraint on "
				<< it->get_choices());
		      s.action_score += it->get_score();
		    }
		}
	    }
	}
      }

    s.score = s.action_score + s.unresolved_deps.size() * weights.broken_score;
    if(s.unresolved_deps.empty())
      {
	LOG_TRACE(logger, "Modifying the score of step "
		  << s.step_num << " by "
		  << std::showpos << weights.full_solution_score << std::noshowpos
		  << " because it is a full solution.");
	s.score += weights.full_solution_score;
      }
    LOG_TRACE(logger, "Updated the score of step "
	      << s.step_num << " to " << s.score);
  }

  /** \brief Fill in a new step with a successor of the parent step
   *         generated by performing the given action.
   */
  void generate_single_successor(const step &parent,
				 step &output,
				 const choice &c_original,
				 const tier &output_tier)
  {
    choice c(c_original);
    c.set_id(parent.actions.size());

    // Copy all the state information over so we can work in-place on
    // the output set.
    output.actions = parent.actions;
    // A brief note on scores.
    //
    // These values are wrong.  They will be corrected at the bottom
    // of this routine.  However, they will be referenced before that,
    // in order to check whether this step exists in "pending" yet.
    // It doesn't, so it won't be found no matter what we include here
    // (which is as it should be).  But by including dummy values, we
    // avoid reading uninitialized memory, which means that (a) any
    // problems that do occur will be deterministic, and (b) valgrind
    // won't spit out false positives.
    output.action_score = parent.action_score;
    output.score = parent.score;
    output.reason = c;
    output.step_tier = output_tier;
    output.is_deferred_listener = build_is_deferred_listener(c);
    output.unresolved_deps = parent.unresolved_deps;
    output.unresolved_deps_by_num_solvers = parent.unresolved_deps_by_num_solvers;
    output.deps_solved_by_choice = parent.deps_solved_by_choice;
    output.forbidden_versions = parent.forbidden_versions;
    output.promotion_queue_location = promotion_queue_tail;


    LOG_TRACE(logger, "Generating a successor to step " << parent.step_num
	      << " for the action " << c << " with tier "
	      << output_tier << " and outputting to step " << output.step_num);

    if(c.get_type() == choice::install_version && !c.get_has_dep())
      LOG_ERROR(logger, "No dependency attached to the choice " << c
		<< " used to generate step " << output.step_num
		<< ", expect trouble ahead.");

    // Insert the new choice into the output list of choices.  This
    // will be used below (in steps 3, 4, 5, 6 and 7).
    output.actions.insert_or_narrow(c);

    // Don't do this, because it isn't necessary and will cause
    // trouble.
    //
    // By definition, this step is a child of a step where c occurs as
    // a solver, hence it will be contained in the tree trace rooted
    // at the step where c was introduced as a solver.  If we also
    // added c as an action here, we would have to somehow guard in
    // the search graph code against traversing this subtree twice,
    // which is a bit of a nuisance and not necessary.
    //
    //  graph.bind_choice(c, output.step_num, c.get_dep());


    // 1. Find the list of solved dependencies and drop each one from
    // the map of unresolved dependencies, and from the set sorted by
    // the number of solvers.
    //
    // Note that some of these dependencies might not be unresolved
    // any more.
    drop_deps_solved_by(c, output);

    // 2. Drop the version from the reverse index of choices to solved
    // dependencies.
    output.deps_solved_by_choice.erase(c);

    // 3. For any versions that are structurally forbidden by this
    // choice, locate those choices in the reverse index, strike them
    // from the corresponding solver lists, add forcing reasons to the
    // corresponding force list, and drop them from the reverse index.
    //
    // 4. Add solvers of the dep, if it was a from-source choice, to
    // the set of forbidden versions.
    strike_structurally_forbidden(output, c);

    // 5. Find newly unsatisfied dependencies.  For each one that's
    // not in the unresolved map, add it to the unresolved map with
    // its solvers paired with their intrinsic tiers.  Structurally
    // forbidden solvers are not added to the solvers list; instead,
    // they are used to create the initial list of forcing reasons.
    // Also, insert the dependency into the by-num-solvers list and
    // insert it into the reverse index for each one of its solvers.
    //
    // This also processes the incipient promotions that are completed
    // by each solver of a dependency.  \todo If the solvers were
    // stored in a central list, the number of promotion lookups
    // required could be vastly decreased.
    add_new_unresolved_deps(output, c);

    // 6. Find incipient promotions for the new step.
    find_new_incipient_promotions(output, c);

    // Compute the new score.  Do this after everything else so
    // solutions get the expected bonus (otherwise we don't know
    // whether this step has unresolved dependencies).
    extend_score_to_new_step(output, c);

    LOG_TRACE(logger, "Generated step " << output.step_num
	      << ": " << output.actions << ";T" << output.step_tier
	      << "S" << output.score);

    if(output.step_tier < tier_limits::defer_tier)
      pending.insert(output.step_num);
  }

  class do_generate_single_successor
  {
    int parent_step_num;
    generic_problem_resolver &resolver;

    bool &first;

  public:
    do_generate_single_successor(int _parent_step_num,
				 generic_problem_resolver &_resolver,
				 bool &_first)
      : parent_step_num(_parent_step_num), resolver(_resolver), first(_first)
    {
      first = true;
    }

    bool operator()(const std::pair<choice, typename step::solver_information> &solver_pair) const
    {
      if(first)
	first = false;
      else
	resolver.graph.get_last_step().is_last_child = false;

      const choice &solver(solver_pair.first);
      const typename step::solver_information &inf(solver_pair.second);

      step &parent = resolver.graph.get_step(parent_step_num);
      step &output = resolver.graph.add_step();
      output.parent = parent.step_num;
      if(parent.first_child == -1)
	parent.first_child = output.step_num;
      output.is_last_child = true;

      resolver.generate_single_successor(parent,
					 output,
					 solver,
					 std::max<tier>(inf.get_tier(),
							parent.step_tier));

      return true;
    }
  };

  /** Build the successors of a search step for the best target
   *  dependency.
   */
  void generate_successors(int step_num, std::set<package> *visited_packages)
  {
    LOG_TRACE(logger, "Generating successors for step " << step_num);

    // \todo Should thread visited_packages through all the machinery
    // above.

    step &s(graph.get_step(step_num));

    // Find the "best" unresolved dependency.
    typename imm::set<std::pair<int, dep> >::node best =
      s.unresolved_deps_by_num_solvers.get_minimum();

    if(!best.isValid())
      {
	LOG_ERROR(logger, "Internal error: can't generate successors at step "
		  << step_num << " since it has no unresolved dependencies.");
	return;
      }

    typename imm::map<dep, typename step::dep_solvers>::node bestSolvers =
      s.unresolved_deps.lookup(best.getVal().second);

    if(!bestSolvers.isValid())
      {
	LOG_ERROR(logger, "Internal error: step " << step_num
		  << " contains the dependency " << best.getVal().second
		  << " in the list of unresolved dependencies by number of solvers, but not in the main list of unresolved dependencies.");
	return;
      }

    if(bestSolvers.getVal().second.get_solvers().empty())
      LOG_ERROR(logger, "Internal error: a step containing a dependency with no solvers was not promoted to the conflict tier.");

    LOG_TRACE(logger, "Generating successors for step " << step_num
	      << " for the dependency " << best.getVal().second
	      << " with " << best.getVal().first << " solvers: "
	      << bestSolvers.getVal().second.get_solvers());
    bool first_successor = false;
    do_generate_single_successor generate_successor_f(s.step_num, *this,
						      first_successor);
    bestSolvers.getVal().second.get_solvers().for_each(generate_successor_f);
  }

public:

  /** Construct a new generic_problem_resolver.
   *
   *  \param _score_score the score per "step" of a (partial) solution.  Typically negative.
   *  \param _broken_score the score to add per broken dependency of a (partial) solution.  Typically negative.
   *  \param _unfixed_soft_score the score to add per soft dependency LEFT UNFIXED.  Typically negative.
   *  \param infinity a score value that will be considered to be "infinite".  Solutions
   *  with less than -infinity points will be immediately discarded.
   *  \param _full_solution_score a bonus for goal nodes (things
   *  that solve all dependencies)
   *  \param _future_horizon  The number of steps to keep searching after finding a
   *                          solution in the hope that a better one is "just around
   *                          the corner".
   *  \param _initial_state   A set of package actions to treat as part
   *                          of the initial state (empty to just
   *                          use default versions for everything).
   *  \param _universe the universe in which we are working.
   */
  generic_problem_resolver(int _step_score, int _broken_score,
			   int _unfixed_soft_score,
			   int infinity,
			   int _full_solution_score,
			   int _future_horizon,
			   const imm::map<package, version> &_initial_state,
			   const PackageUniverse &_universe)
    :logger(aptitude::Loggers::getAptitudeResolverSearch()),
     appender(new log4cxx::ConsoleAppender(new log4cxx::PatternLayout("%m%n"))),
     graph(promotions),
     initial_state(_initial_state, _universe.get_package_count()),
     weights(_step_score, _broken_score, _unfixed_soft_score,
	     _full_solution_score, _universe.get_version_count(),
	     initial_state),
     minimum_score(-infinity),
     future_horizon(_future_horizon),
     universe(_universe), finished(false),
     remove_stupid(true),
     solver_executing(false), solver_cancelled(false),
     pending(step_goodness_compare(graph)),
     num_deferred(0),
     minimum_search_tier(tier_limits::minimum_tier),
     maximum_search_tier(tier_limits::minimum_tier),
     pending_future_solutions(step_goodness_compare(graph)),
     closed(),
     promotions(_universe, *this),
     promotion_queue_tail(new promotion_queue_entry(0, 0)),
     version_tiers(new tier[_universe.get_version_count()])
  {
    LOG_DEBUG(logger, "Creating new problem resolver: step_score = " << _step_score
	      << ", broken_score = " << _broken_score
	      << ", unfixed_soft_score = " << _unfixed_soft_score
	      << ", infinity = " << infinity
	      << ", full_solution_score = " << _full_solution_score
	      << ", future_horizon = " << _future_horizon
	      << ", initial_state = " << _initial_state);

    for(unsigned int i = 0; i < _universe.get_version_count(); ++i)
      version_tiers[i] = tier_limits::minimum_tier;

    // Used for sanity-checking below.
    choice_set_installation empty_step(choice_set(),
				       initial_state);

    // Find all the broken deps.
    for(typename PackageUniverse::dep_iterator di = universe.deps_begin();
	!di.end(); ++di)
      {
	dep d(*di);

	if(d.broken_under(initial_state))
	  {
	    if(!d.broken_under(empty_step))
	      LOG_ERROR(logger, "Internal error: the dependency "
			<< d << " is claimed to be broken, but it doesn't appear to be broken in the initial state.");
	    else
	      initial_broken.insert(d);
	  }
      }
  }

  ~generic_problem_resolver()
  {
    delete[] version_tiers;
  }

  /** \brief Get the dependencies that were initially broken in this
   *  resolver.
   *
   *  This might be different from the dependencies that are
   *  "intrinsically" broken in the universe, if there are
   *  hypothesized initial installations.
   */
  const imm::set<dep> get_initial_broken() const
  {
    return initial_broken;
  }

  const PackageUniverse &get_universe() const
  {
    return universe;
  }

  int get_step_score() {return weights.step_score;}
  int get_broken_score() {return weights.broken_score;}
  int get_unresolved_soft_dep_score() {return weights.unfixed_soft_score;}
  int get_infinity() {return -minimum_score;}
  int get_full_solution_score() {return weights.full_solution_score;}

  /** Enables or disables debugging.  Debugging is initially
   *  disabled.
   *
   *  This is a backwards-compatibility hook; in the future, the
   *  log4cxx framework should be used to enable debugging.  This
   *  function enables all possible debug messages by setting the
   *  level for all resolver domains to TRACE.
   */
  void set_debug(bool new_debug)
  {
    if(new_debug)
      {
	logger->setLevel(log4cxx::Level::getTrace());
	logger->addAppender(appender);
      }
    else
      {
	logger->setLevel(log4cxx::Level::getOff());
	logger->removeAppender(appender);
      }
  }

  /** Enables or disables the removal of "stupid pairs".  Initially
   *  enabled.
   */
  void set_remove_stupid(bool new_remove_stupid)
  {
    remove_stupid = new_remove_stupid;
  }

  /** Clears all the internal state of the solver, discards solutions,
   *  zeroes out scores.  Call this routine after changing the state
   *  of packages to avoid inconsistent results.
   */
  void reset()
  {
    finished=false;
    pending.clear();
    pending_future_solutions.clear();
    promotion_queue_tail = boost::shared_ptr<promotion_queue_entry>(new promotion_queue_entry(0, 0));
    graph.clear();
    closed.clear();

    for(size_t i=0; i<universe.get_version_count(); ++i)
      weights.version_scores[i]=0;
  }

  /** \return \b true if no solutions have been examined yet.
   *  This implies that it is safe to modify package scores.
   */
  bool fresh() const
  {
    // \todo Have a Boolean flag that tracks this explicitly.
    return
      pending.empty() && pending_future_solutions.empty() &&
      closed.empty() && !finished;
  }

  /** \return \b true if the open queue is empty. */
  bool exhausted() const
  {
    return !(pending_contains_candidate() || pending_future_solutions_contains_candidate())
      && finished;
  }

  /** \return the initial state of the resolver. */
  const resolver_initial_state<PackageUniverse> &get_initial_state() const
  {
    return initial_state;
  }

  /** \brief Manually promote the given set of choices to the given tier.
   *
   *  If tier is defer_tier, the promotion will be lost when the user
   *  changes the set of rejected packages.
   */
  void add_promotion(const choice_set &choices, const tier &promotion_tier)
  {
    add_promotion(promotion(choices, promotion_tier));
  }

  /** Tells the resolver how highly to value a particular package
   *  version.  All scores are relative, and a higher score will
   *  result in a bias towards that version appearing in the final
   *  solution.
   */
  void set_version_score(const version &ver, int score)
  {
    eassert(ver.get_id()<universe.get_version_count());
    weights.version_scores[ver.get_id()]=score;
  }

  /** As set_version_score, but instead of replacing the current score
   *  increment it.
   */
  void add_version_score(const version &ver, int score)
  {
    eassert(ver.get_id()<universe.get_version_count());
    weights.version_scores[ver.get_id()]+=score;
  }

  /** \brief Set the tier of a version.
   *
   *  Adding this version to a solution with a lower tier will
   *  increase the solution's tier to the given value.
   */
  void set_version_tier(const version &ver, const tier &t)
  {
    eassert(ver.get_id() < universe.get_version_count());
    version_tiers[ver.get_id()] = t;
  }

  /** \brief Set the tier of a version to at least the given value.
   *
   *  If the tier is less than t, it will be increased to t; otherwise
   *  it will be left unchanged.
   */
  void set_version_min_tier(const version &ver, const tier &t)
  {
    eassert(ver.get_id() < universe.get_version_count());
    tier &version_tier = version_tiers[ver.get_id()];

    if(version_tier < t)
      version_tier = t;
  }

  /** \return the score of the version ver. */
  int get_version_score(const version &ver)
  {
    eassert(ver.get_id()<universe.get_version_count());
    return weights.version_scores[ver.get_id()];
  }

  /** \brief Add a score to all solutions that install the given
   *  collection of versions.
   *
   *  Note that this does not mean "all solutions that result in the
   *  given set of versions being installed".  The versions must be
   *  newly installed in the solution; if any version is the current
   *  version of its package, this call has no effect.
   */
  void add_joint_score(const imm::set<version> &versions, int score)
  {
    weights.add_joint_score(versions, score);
  }

  /** Reject future solutions containing this version.
   */
  void reject_version(const version &ver, undo_group *undo = NULL)
  {
    approved_or_rejected_info &inf(user_approved_or_rejected_versions[ver]);

    if(!inf.get_rejected()->get_value())
      {
	LOG_TRACE(logger, "Rejecting " << ver);

	if(undo != NULL)
	  undo->add_item(new undo_resolver_manipulation<PackageUniverse, version>(this, ver, &generic_problem_resolver<PackageUniverse>::unreject_version));

	inf.get_rejected()->set_value(true);
	unmandate_version(ver, undo);
      }
  }

  /** Cancel any rejection of ver, allowing the resolver to once
   *  again generate solutions containing it.
   */
  void unreject_version(const version &ver, undo_group *undo = NULL)
  {
    approved_or_rejected_info &inf(user_approved_or_rejected_versions[ver]);

    if(inf.get_rejected()->get_value())
      {
	LOG_TRACE(logger, "Canceling the rejection of " << ver);

	if(undo != NULL)
	  undo->add_item(new undo_resolver_manipulation<PackageUniverse, version>(this, ver, &generic_problem_resolver<PackageUniverse>::reject_version));

	inf.get_rejected()->set_value(false);
      }
  }

  void mandate_version(const version &ver, undo_group *undo = NULL)
  {
    approved_or_rejected_info &inf(user_approved_or_rejected_versions[ver]);

    if(!inf.get_approved()->get_value())
      {
	LOG_TRACE(logger, "Mandating " << ver);

	if(undo != NULL)
	  undo->add_item(new undo_resolver_manipulation<PackageUniverse, version>(this, ver, &generic_problem_resolver<PackageUniverse>::unmandate_version));

	inf.get_approved()->set_value(true);
	unreject_version(ver, undo);
      }
  }

  void unmandate_version(const version &ver, undo_group *undo = NULL)
  {
    approved_or_rejected_info &inf(user_approved_or_rejected_versions[ver]);

    if(inf.get_approved()->get_value())
      {
	LOG_TRACE(logger, "Un-mandating " << ver);

	if(undo != NULL)
	  undo->add_item(new undo_resolver_manipulation<PackageUniverse, version>(this, ver, &generic_problem_resolver<PackageUniverse>::mandate_version));

	inf.get_approved()->set_value(false);
      }
  }

  /** Query whether the given version is rejected. */
  bool is_rejected(const version &ver) const
  {
    typename std::map<version, approved_or_rejected_info>::const_iterator found =
      user_approved_or_rejected_versions.find(ver);

    return
      found != user_approved_or_rejected_versions.end() &&
      found->second.get_rejected()->get_value();
  }

  /** Query whether the given version is mandated. */
  bool is_mandatory(const version &ver) const
  {
    typename std::map<version, approved_or_rejected_info>::const_iterator found =
      user_approved_or_rejected_versions.find(ver);

    return
      found != user_approved_or_rejected_versions.end() &&
      found->second.get_approved()->get_value();
  }

  /** Query whether the given dependency is hardened. */
  bool is_hardened(const dep &d) const
  {
    typename std::map<dep, approved_or_rejected_info>::const_iterator found =
      user_approved_or_rejected_broken_deps.find(d);

    return
      found != user_approved_or_rejected_broken_deps.end() &&
      found->second.get_rejected()->get_value();
  }

  /** Harden the given dependency. */
  void harden(const dep &d, undo_group *undo = NULL)
  {
    eassert(d.is_soft());

    approved_or_rejected_info &inf(user_approved_or_rejected_broken_deps[d]);

    if(!inf.get_rejected()->get_value())
      {
	LOG_TRACE(logger, "Hardening " << d);

	if(undo != NULL)
	  undo->add_item(new undo_resolver_manipulation<PackageUniverse, dep>(this, d, &generic_problem_resolver<PackageUniverse>::unharden));

	inf.get_rejected()->set_value(true);
	unapprove_break(d, undo);
      }
  }

  /** Un-harden (soften?) the given dependency. */
  void unharden(const dep &d, undo_group *undo = NULL)
  {
    approved_or_rejected_info &inf(user_approved_or_rejected_broken_deps[d]);

    if(inf.get_rejected()->get_value())
      {
	LOG_TRACE(logger, "Un-hardening " << d);

	if(undo != NULL)
	  undo->add_item(new undo_resolver_manipulation<PackageUniverse, dep>(this, d, &generic_problem_resolver<PackageUniverse>::harden));

	inf.get_rejected()->set_value(false);
      }
  }

  /** \return \b true if the given dependency is in the
   *  approved-broken state.
   */
  bool is_approved_broken(const dep &d) const
  {
    typename std::map<dep, approved_or_rejected_info>::const_iterator found =
      user_approved_or_rejected_broken_deps.find(d);

    return
      found != user_approved_or_rejected_broken_deps.end() &&
      found->second.get_approved()->get_value();
  }

  /** Approve the breaking of the given dependency. */
  void approve_break(const dep &d, undo_group *undo = NULL)
  {
    approved_or_rejected_info &inf(user_approved_or_rejected_broken_deps[d]);

    if(!inf.get_approved()->get_value())
      {
	LOG_TRACE(logger, "Approving breaking " << d);

	if(undo != NULL)
	  undo->add_item(new undo_resolver_manipulation<PackageUniverse, dep>(this, d, &generic_problem_resolver<PackageUniverse>::unapprove_break));

	inf.get_approved()->set_value(true);
	unharden(d, undo);
      }
  }

  /** Cancel the required breaking of the given dependency. */
  void unapprove_break(const dep &d, undo_group *undo = NULL)
  {
    approved_or_rejected_info &inf(user_approved_or_rejected_broken_deps[d]);

    if(inf.get_approved()->get_value())
      {
	LOG_TRACE(logger, "Un-approving breaking " << d);

	if(undo != NULL)
	  undo->add_item(new undo_resolver_manipulation<PackageUniverse, dep>(this, d, &generic_problem_resolver<PackageUniverse>::approve_break));

	inf.get_approved()->set_value(false);
      }
  }

  /** Cancel any find_next_solution call that is executing in the
   *  background.  If no such call is executing, then the next call
   *  will immediately be cancelled.
   */
  void cancel_solver()
  {
    cwidget::threads::mutex::lock l(execution_mutex);
    solver_cancelled = true;
  }

  /** Remove any pending find_next_solution cancellation. */
  void uncancel_solver()
  {
    cwidget::threads::mutex::lock l(execution_mutex);
    solver_cancelled = false;
  }

  /** Atomically read the current queue sizes of this resolver. */
  queue_counts get_counts()
  {
    maybe_update_deferred_and_counts();

    cwidget::threads::mutex::lock l(counts_mutex);
    return counts;
  }

  size_t get_num_deferred() const
  {
    return num_deferred;
  }

  /** Update the cached queue sizes. */
  void update_counts_cache()
  {
    cwidget::threads::mutex::lock l(counts_mutex);
    counts.open       = pending.size();
    counts.closed     = closed.size();
    counts.deferred   = get_num_deferred();
    counts.conflicts  = promotions.tier_size_above(tier_limits::conflict_tier);
    counts.promotions = promotions.size() - counts.conflicts;
    counts.finished   = finished;
    counts.current_tier = get_current_search_tier();
  }

  /** If no resolver is running, run through the deferred list and
   *  update the counts cache.  In particular, this allows the
   *  'are-we-out-of-solutions' state to be updated immediately when
   *  something like reject_version is called.
   */
  void maybe_update_deferred_and_counts()
  {
    cwidget::threads::mutex::lock l(execution_mutex);
    if(!solver_executing)
      {
	update_counts_cache();
      }
  }

  /** \brief Returns the "current" search tier, the tier of the next
   *  solution that would be considered (or minimum_search_tier if
   *  pending is empty).
   */
  const tier &get_current_search_tier() const
  {
    if(pending.empty())
      return tier_limits::minimum_tier;
    else
      return graph.get_step(*pending.begin()).step_tier;
  }

private:
  /** \brief Returns \b true if the open queue contains a step that
   *  can be processed.
   */
  bool pending_contains_candidate() const
  {
    return
      !pending.empty() &&
      graph.get_step(*pending.begin()).step_tier < tier_limits::defer_tier;
  }

  /** \brief Returns \b true if the pending future solutions queue
   *  contains a step that can be processed.
   */
  bool pending_future_solutions_contains_candidate() const
  {
    return
      !pending_future_solutions.empty() &&
      graph.get_step(*pending_future_solutions.begin()).step_tier < tier_limits::defer_tier;
  }

  // Counts how many action hits existed in a promotion, allowing up
  // to one mismatch (which it stores).
  class count_action_hits
  {
    unsigned int &action_hits;
    boost::optional<choice> &mismatch;
    const step &s;

  public:
    count_action_hits(unsigned int &_action_hits,
		      boost::optional<choice> &_mismatch,
		      const step &_s)
      : action_hits(_action_hits),
	mismatch(_mismatch),
	s(_s)
    {
      action_hits = 0;
      mismatch.reset();
    }

    bool operator()(const choice &c) const
    {
      if(s.actions.has_contained_choice(c))
	++action_hits;
      else if(!mismatch)
	mismatch = c;
      else
	return false;

      return true;
    }
  };

  /** \brief Apply a single promotion to a single step. */
  void apply_promotion(step &s, const promotion &p)
  {
    if(!(s.step_tier < p.get_tier()))
      LOG_TRACE(logger, "Not applying " << p
		<< " to step " << s.step_num << ": the step tier "
		<< s.step_tier << " is not below the promotion tier.");
    else
      {
	LOG_TRACE(logger, "Testing the promotion " << p
		  << " against step " << s.step_num);

	unsigned int action_hits;
	boost::optional<choice> mismatch;
	if(!p.get_choices().for_each(count_action_hits(action_hits,
						       mismatch,
						       s)))
	  LOG_TRACE(logger, "Too many mismatches against " << p
		    << ", not applying it.");
	else
	  {
	    const unsigned int p_size(p.get_choices().size());
	    if(action_hits == p_size)
	      {
		LOG_TRACE(logger, "Step " << s.step_num
			  << " contains " << p << " as an active promotion.");
		set_step_tier(s.step_num, p.get_tier());
	      }
	    else if(action_hits + 1 < p_size)
	      LOG_TRACE(logger, "Step " << s.step_num
			<< " does not contain " << p <<".");
	    else if(!mismatch)
	      LOG_ERROR(logger, "Internal error: found an incipient promotion with no mismatches!");
	    else if(!s.deps_solved_by_choice.contains_key(*mismatch))
	      LOG_TRACE(logger, "Step " << s.step_num
			<< " almost contains " << p
			<< " as an incipient promotion, but the choice "
			<< *mismatch << " is not a solver.");
	    else
	      {
		LOG_TRACE(logger, "Step " << s.step_num
			  << " contains " << p
			  << " as an incipient promotion for the choice "
			  << *mismatch << ".");
		increase_solver_tier(s, p, *mismatch);
	      }
	  }
      }
  }

  /** \brief Check for promotions that have been added
   *  since a step was generated, and apply them.
   *
   *  \param step_num  The index of the step to test.
   */
  void check_for_new_promotions(int step_num)
  {
    step &s = graph.get_step(step_num);

    eassert(promotion_queue_tail.get() != NULL);
    eassert(s.promotion_queue_location.get() != NULL);

    const promotion_queue_entry &current_tail = *promotion_queue_tail;
    const promotion_queue_entry &step_location = *s.promotion_queue_location;

    LOG_TRACE(logger, "The current promotion tail has index "
	      << current_tail.get_index() << " and action sum "
	      << current_tail.get_action_sum() << "; step "
	      << step_num << " points to a promotion cell with index "
	      << step_location.get_index() << " and action sum "
	      << step_location.get_action_sum() << ", for a difference of "
	      << (current_tail.get_index() - step_location.get_index())
	      << " steps and "
	      << (current_tail.get_action_sum() - step_location.get_action_sum())
	      << " actions.");

    if(promotion_queue_tail != s.promotion_queue_location)
      eassert(step_location.get_has_contents());

    const unsigned int extra_actions = current_tail.get_action_sum() - step_location.get_action_sum();
    if(extra_actions <= s.actions.size() + s.deps_solved_by_choice.size())
      {
	LOG_TRACE(logger, "Applying each new promotion to step "
		  << s.step_num << ".");
	for(const promotion_queue_entry *qEnt = &step_location;
	    qEnt->get_has_contents(); qEnt = qEnt->get_next().get())
	  apply_promotion(s, qEnt->get_promotion());

	s.promotion_queue_location = promotion_queue_tail;

	boost::unordered_map<choice, promotion> incipient_promotions;
	maybe<promotion> non_incipient_promotion;



	promotions.find_highest_incipient_promotions(s.actions,
						     s.deps_solved_by_choice,
						     incipient_promotions,
						     non_incipient_promotion);

	if(non_incipient_promotion.get_has_value())
	  {
	    const promotion &p(non_incipient_promotion.get_value());
	    if(s.step_tier < p.get_tier())
	      {
		LOG_ERROR(logger, "Found an unexpected promotion in the action set of step " << s.step_num
			  << ": " << p);
		increase_step_tier(s, p);
	      }
	  }
      }
    else
      {
	boost::unordered_map<choice, promotion> incipient_promotions;
	maybe<promotion> non_incipient_promotion;


	promotions.find_highest_incipient_promotions(s.actions,
						     s.deps_solved_by_choice,
						     incipient_promotions,
						     non_incipient_promotion);

	if(non_incipient_promotion.get_has_value())
	  {
	    const promotion &p(non_incipient_promotion.get_value());
	    LOG_TRACE(logger, "Found a new promotion in the action set of step "
		      << step_num << ": " << p);
	    increase_step_tier(s, p);
	  }

	// Note that some promotions might be generated as a result of
	// increasing tiers; to be perfectly correct we should account
	// for those.  On the other hand, this might not be necessary;
	// any promotions that are generated should by definition
	// already be included in the step, correct?
	s.promotion_queue_location = promotion_queue_tail;
	for(typename boost::unordered_map<choice, promotion>::const_iterator it =
	      incipient_promotions.begin();
	    it != incipient_promotions.end(); ++it)
	  increase_solver_tier(s, it->second, it->first);
      }
  }

  /** \brief Process the given step number and generate its
   *  successors.
   */
  void process_step(int step_num, std::set<package> *visited_packages)
  {
    bool done;

    do
      {
	done = true;

	step &s = graph.get_step(step_num);

	LOG_INFO(logger, "Examining step " << step_num
		 << ": " << s.actions << ";T" << s.step_tier
		 << "S" << s.score);

	if(s.step_tier >= tier_limits::defer_tier)
	  {
	    LOG_ERROR(logger, "Internal error: the tier of step "
		      << s.step_num
		      << " is an unprocessed tier, so why is it a candidate?");
	    // Bail out.
	    return;
	  }

	check_for_new_promotions(step_num);

	if(is_already_seen(step_num))
	  {
	    LOG_DEBUG(logger, "Dropping already visited search node in step " << s.step_num);
	  }
	else if(irrelevant(s))
	  {
	    LOG_DEBUG(logger, "Dropping irrelevant step " << s.step_num);
	  }
	// The step might have been promoted to the defer tier by
	// check_for_new_promotions.
	else if(s.step_tier >= tier_limits::defer_tier)
	  {
	    LOG_DEBUG(logger, "Skipping newly deferred step " << s.step_num);
	    // Stick it back into the open queue.
	    pending.insert(step_num);
	  }
	else
	  {
	    LOG_TRACE(logger, "Processing step " << step_num);

	    closed[step_contents(s.score, s.action_score, s.actions)] =
	      step_num;

	    // If all dependencies are satisfied, we found a solution.
	    if(s.unresolved_deps.empty())
	      {
		LOG_INFO(logger, " --- Found solution at step " << s.step_num
			 << ": " << s.actions << ";T" << s.step_tier
			 << "S" << s.score);

		// Remember this solution, so we don't try to return it
		// again in the future.
		choice_set generalized_actions;
		for(typename choice_set::const_iterator it = s.actions.begin();
		    it != s.actions.end(); ++it)
		  generalized_actions.insert_or_narrow(it->generalize());
		promotion already_generated_promotion(generalized_actions,
						      tier_limits::already_generated_tier);
		add_promotion(step_num, already_generated_promotion);

		s.is_blessed_solution = true;
		pending_future_solutions.insert(step_num);
	      }
	    // Nope, let's go enqueue successor nodes.
	    else
	      {
		generate_successors(step_num, visited_packages);
		// If we enqueued *exactly* one successor, then this
		// was a forced dependency and we should process that
		// successor before returning.
		if(s.first_child != -1 && graph.get_step(s.first_child).is_last_child)
		  {
		    LOG_TRACE(logger, "Following forced dependency resolution from step "
			      << step_num << " to step " << s.first_child);
		    step_num = s.first_child;
		    done = false;
		  }
	      }
	  }
      } while(!done);
  }

public:
  /** Try to find the "next" solution: remove partial solutions from
   *  the open queue and place them in the closed queue until one of
   *  the following occurs:
   *
   *   - The number of broken dependencies drops to 0, in which case
   *     there is much rejoicing and we return successfully.
   *
   *   - The upper limit on the number of steps to perform is exceeded,
   *     in which case we just give up and report failure.  (this is a
   *     guard against exponential blowup)
   *
   *   - We run out of potential solutions to try; failure.
   *
   *  \param max_steps the maximum number of solutions to test.
   *
   *  \param visited_packages
   *           if not NULL, each package that influences the
   *           resolver's choices will be placed here.
   *
   *  \return a solution that fixes all broken dependencies
   *
   * \throws NoMoreSolutions if the potential solution list is exhausted.
   * \throws NoMoreTime if no solution is found within max_steps steps.
   *
   *  \todo when throwing NoMoreSolutions or NoMoreTime, maybe we
   *        should include the "least broken" solution seen.
   */
  solution find_next_solution(int max_steps,
			      std::set<package> *visited_packages)
  {
    // This object is responsible for managing the instance variables
    // that control threaded operation: it sets solver_executing when
    // it is created and clears both solver_executing and
    // solver_cancelled when it is destroyed.
    instance_tracker t(*this);
 

    // Counter for checking how long we've been running and for
    // debugging (see below).
    int odometer = 0;

    // Counter for how many "future" steps are left.
    //
    // Because this is a local variable and not a class member, the
    // future horizon will restart each time this routine is called.
    // But since we always run it out when we find a solution, the
    // worst thing that can happen is that we search a little too much
    // if there are lots of solutions near each other.  If this
    // becomes a practical problem, it shouldn't be too hard to
    // implement better behavior; the most difficult thing will be
    // defining the best semantics for it.  Another possibility would
    // be to always return the first "future" solution that we find.
    int most_future_solution_steps = 0;

    if(finished)
      throw NoMoreSolutions();

    // If nothing has been processed, then we need to start a new
    // search.
    if(pending.empty() && pending_future_solutions.empty() && closed.empty())
      {
	LOG_INFO(logger, "Starting a new search.");

	step &root = graph.add_step();
	root.action_score = 0;
	root.score = initial_broken.size() * weights.broken_score;
	if(initial_broken.empty())
	  root.score += weights.full_solution_score;

	root.step_tier = tier_limits::minimum_tier;
	root.promotion_queue_location = promotion_queue_tail;

	for(typename imm::set<dep>::const_iterator it = initial_broken.begin();
	    it != initial_broken.end(); ++it)
	  add_unresolved_dep(root, *it);

	LOG_TRACE(logger, "Inserting the root at step " << root.step_num
		  << " with tier " << root.step_tier);
	pending.insert(root.step_num);
      }

    while(max_steps > 0 &&
	  pending_contains_candidate() &&
	  most_future_solution_steps <= future_horizon)
      {
	if(most_future_solution_steps > 0)
	  LOG_TRACE(logger, "Speculative \"future\" resolver tick ("
		    << most_future_solution_steps << "/" << future_horizon << ").");

	// Threaded operation: check whether we have been cancelled.
	{
	  cwidget::threads::mutex::lock l(execution_mutex);
	  if(solver_cancelled)
	    throw InterruptedException(odometer);
	}

	update_counts_cache();


	int curr_step_num = *pending.begin();
	pending.erase(pending.begin());

	++odometer;

	process_step(curr_step_num, visited_packages);

	// Keep track of the "future horizon".  If we haven't found a
	// solution, we aren't using those steps up at all.
	// Otherwise, we count up until we hit 50.
	if(pending_future_solutions_contains_candidate())
	  ++most_future_solution_steps;
	else
	  most_future_solution_steps = 0;

	LOG_TRACE(logger, "Done generating successors.");

	--max_steps;


	// Propagate any new promotions that we discovered up the
	// search tree.
	graph.run_scheduled_promotion_propagations(promotion_adder(*this));
	process_pending_promotions();
      }

    if(LOG4CXX_UNLIKELY(logger->isTraceEnabled()))
      {
	if(most_future_solution_steps > future_horizon)
	  LOG_TRACE(logger, "Done examining future steps for a better solution.");
	else if(!pending_contains_candidate())
	  LOG_TRACE(logger, "Exhausted all search branches.");
	else
	  LOG_TRACE(logger, "Ran out of time.");
      }

    if(pending_future_solutions_contains_candidate())
      {
	int best_future_solution = *pending_future_solutions.begin();
	step &best_future_solution_step = graph.get_step(best_future_solution);
	if(best_future_solution_step.step_tier < tier_limits::defer_tier)
	  {
	    solution rval(best_future_solution_step.actions,
			  initial_state,
			  best_future_solution_step.score,
			  best_future_solution_step.step_tier);

	    LOG_INFO(logger, " *** Converged after " << odometer << " steps.");

	    LOG_INFO(logger, " *** open: " << pending.size()
		     << "; closed: " << closed.size()
		     << "; promotions: " << promotions.size()
		     << "; deferred: " << get_num_deferred());

	    LOG_INFO(logger, "--- Returning the future solution "
		     << rval << " from step " << best_future_solution);

	    pending_future_solutions.erase(pending_future_solutions.begin());

	    return rval;
	  }
      }

    // Oh no, we either ran out of solutions or ran out of steps.

    if(pending_contains_candidate())
      {
	LOG_INFO(logger, " *** Out of time after " << odometer << " steps.");
	throw NoMoreTime();
      }

    finished=true;

    update_counts_cache();

    LOG_INFO(logger, " *** Out of solutions after " << odometer << " steps.");
    LOG_INFO(logger ,
	     " *** open: " << pending.size()
	     << "; closed: " << closed.size()
	     << "; promotions: " << promotions.size()
	     << "; deferred: " << get_num_deferred());

    throw NoMoreSolutions();
  }

  void dump_scores(std::ostream &out)
  {
    out << "{" << std::endl;
    for(typename PackageUniverse::package_iterator i=universe.packages_begin();
	!i.end(); ++i)
      {
	bool any_modified=false;

	for(typename PackageUniverse::package::version_iterator j=(*i).versions_begin();
	    !j.end(); ++j)
	  if(weights.version_scores[(*j).get_id()]!=0)
	    any_modified=true;

	if(any_modified)
	  {
	    out << "  SCORE " << (*i).get_name() << " <";

	    for(typename PackageUniverse::package::version_iterator j=(*i).versions_begin();
		!j.end(); ++j)
	      if(weights.version_scores[(*j).get_id()]!=0)
		out << " " << (*j).get_name() << " " << weights.version_scores[(*j).get_id()];

	    out << " >" << std::endl;
	  }
      }

    for(typename std::vector<std::pair<imm::set<version>, int> >::const_iterator it =
	  weights.get_joint_scores_list().begin();
	it != weights.get_joint_scores_list().end(); ++it)
      {
	out << "  SCORE { ";
	for(typename imm::set<version>::const_iterator vIt = it->first.begin();
	    vIt != it->first.end(); ++vIt)
	  {
	    out << *vIt << " ";
	  }
	out << "} " << it->second << std::endl;
      }

    out << "}" << std::endl;
  }
};

#endif
