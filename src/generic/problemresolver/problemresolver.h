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
#include "promotion_set.h"
#include "solution.h"
#include "resolver_undo.h"

#include "log4cxx/consoleappender.h"
#include "log4cxx/logger.h"
#include "log4cxx/patternlayout.h"

#include <loggers.h>

#include <cwidget/generic/threads/threads.h>
#include <cwidget/generic/util/eassert.h>

#include <generic/util/dense_setset.h>

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
class generic_problem_resolver
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

  static const int maximum_tier_num = INT_MAX;

  /** \brief The maximum tier; reserved for solutions that contain a
   *  logical conflict and thus are dead-ends.
   *
   *  Search nodes at this tier are discarded without being visited.
   */
  static const int conflict_tier_num = maximum_tier_num;

  /** \brief The second highest tier; reserved for solutions that were
   *  already generated (to prevent them from being generated again).
   *
   *  Search nodes at this tier are discarded without being visited.
   */
  static const int already_generated_tier_num = conflict_tier_num - 1;

  /** \brief The third highest tier; reserved for solutions that
   *  violate a user constraint and will be deferred until the
   *  constraints are changed.
   */
  static const int defer_tier_num = conflict_tier_num - 2;

  /** \brief The minimum tier; this is the initial tier of the empty
   *  solution.
   */
  static const int minimum_tier_num = INT_MIN;

  static const tier maximum_tier;
  static const tier conflict_tier;
  static const tier already_generated_tier;
  static const tier defer_tier;
  static const tier minimum_tier;

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

    queue_counts()
      : open(0), closed(0), deferred(0), conflicts(0), promotions(0), finished(false)
    {
    }
  };

private:
  log4cxx::LoggerPtr logger;
  log4cxx::AppenderPtr appender; // Used for the "default" appending behavior.

  // Structures that store the search graph.
  //
  // This information is used to backpropagate promotions/conflicts.
  // If all the children of a step ended up in a conflict, we can use
  // that information to infer a conflict for the step itself.  But
  // since aptitude has a flexible search order, the children might be
  // run after we've "finished" processing the parent step.  So it's
  // necessary to somehow store all the steps with children that are
  // pending evaluation.
  //
  // Actually, *all* steps are stored, not just ones with pending
  // children: this lets us handle situations where promotions are
  // generated more than once for the same step (for instance, if a
  // step eventually ends up at tier 40,000 but has one child that
  // passes through tier 30,000, it will first be promoted to tier
  // 30,000, then later to tier 40,000).
  //
  // The lifetime of a step is as follows:
  //  (1) Created with no children, an initial intrinsic promotion,
  //      and maybe a parent link.
  //  (2) Children added, along with successor constraints.
  //  (3) When each of its children has registered a promotion,
  //      the step's promotion is set and its parent is examined
  //      to see if it should get a promotion now.
  //
  // Regarding (3), what we do is this: whenever a new promotion is
  // computed for a step, it goes into the set of promotions for that
  // step and onto the list of promotions if it was really new.  Then,
  // we add the parent to a set of nodes that should be examined for
  // promotion propagation.  After the step finishes, the nodes in
  // this set are processed for propagation from their children.
  //
  // To save space and keep things compact, the tree is represented as
  // an array (actually a deque), with parent and child links stored
  // as indices into the array.  This made more sense when it had just
  // a few members; maybe now it should be allocated on the heap and
  // reference-counted?
  struct step
  {
    // If true, this is the last child in its parent's child list.
    // Meaningless if parent is -1 (meaning there is no parent node).
    bool is_last_child : 1;
    // Index of the parent step, or -1 if there is no parent.
    int parent;
    // Index of the first child step, or -1 if there are no children.
    // This is always -1 to start with, and is updated when the step's
    // successors are generated.
    int first_child;
    // The solution associated with this step.
    solution sol;
    // The choice associated with this step (meaningless if parent ==
    // -1).  Set when the step is created, and used when
    // backpropagating promotions: when the parent is computing its
    // promotion, it removes each child's reason from that child's
    // promotion's choice set.
    choice reason;
    // The choices that constrained the successors to this node.  This
    // is used, along with information from the successors, to compute
    // the promotion associated with this node (if any).
    //
    // This contains versions that either structurally knocked out
    // possible resolutions to the dependency that was selected for
    // expansion, *or* that caused a resolution to hit a conflict /
    // already-generated promotion.  NOTE: the entries in this set
    // might not be represented in every promotion at this step; some
    // promotions could be generated from dependencies that weren't
    // actually expanded.  This is used when accumulating this node's
    // sub-promotions and filling in new promotions; the parent
    // shouldn't examine it.
    //
    // This only has a meaningful value if first_child is not -1.
    choice_set successor_constraints;
    // All the promotions associated with this step; each promotion is
    // universally valid but was discovered in the context of this
    // step.  No attempt is made to eliminate redundant promotions at
    // the moment.
    std::set<promotion> promotions;

    // The same, but in the order that they were added; used to
    // quickly partition the list into "new" and "old" promotions.
    //
    // TODO: should be a list of const_iterators referencing the above
    // set.
    std::vector<promotion> promotions_list;

    // The number of "new" promotions added to the list since the
    // parent last scanned it.
    int promotions_list_new_promotions;

    /** \brief Remove the promotion information if it is in the
     *  deferral tier.
     */
    void blank_deferral_promotion()
    {
      eassert(false);
      // TODO: implement.
    }

    /** \brief Default step constructor; only exists for use
     *  by STL containers.
     */
    step()
      : is_last_child(true),
	parent(-1), first_child(-1), sol(), reason(),
	successor_constraints(), promotions(),
	promotions_list(), promotions_list_new_promotions(0)
    {
    }

    /** \brief Make a step suitable for use at the root.
     *
     *  The step has no parent, children, promotion, or successor
     *  constraints.
     */
    step(const solution &_sol)
      : is_last_child(true),
	parent(-1), first_child(-1), sol(_sol), reason(),
	successor_constraints(), promotions(),
	promotions_list(), promotions_list_new_promotions(0)
    {
    }

    /** \brief Make a step with the given parent.
     *
     *  The step initially has no children or successor constraints.
     */
    step(solution &_sol, int _parent,
	 const choice &_reason, bool _is_last_child)
      : is_last_child(_is_last_child),
	parent(_parent),
	first_child(-1), sol(_sol), reason(_reason),
	successor_constraints(), promotions(),
	promotions_list(), promotions_list_new_promotions(0)
    {
    }
  };

  /** \brief The maximum number of promotions to propagate
   *  through any one step.
   *
   *  This avoids an exponential growth in the size of promotion sets;
   *  I expect that in normal usage there will be only a few dozen
   *  promotions at the very most.  Throwing away promotions
   *  arbitrarily is OK -- it just means we won't have quite as much
   *  information available to prune the search tree.
   */
  static const int max_propagated_promotions = 100;
  std::deque<step> steps;
  // Steps whose children have pending propagation requests.  Stored
  // in reverse order, because we should handle later steps first
  // (since they might be children of earlier steps and thus add new
  // promotions to them).
  std::set<int, std::greater<int> > steps_pending_promotion_propagation;

  // Step-related routines.

  // Used to recursively build the set of all the promotions in the
  // Cartesian product of the sub-promotions that include at least one
  // promotion that's "new".
  //
  // Returns "true" if anything was generated.  We care because we
  // need to know whether to queue the parent of the parent for
  // propagation.
  bool add_child_promotions(int parentNum, int childNum, bool has_new_promotion,
			    const choice_set &choices, const tier &t)
  {
    step &parent = steps[parentNum];
    // Don't do anything if the parent has too many propagations
    // already.
    if(parent.promotions_list.size() >= max_propagated_promotions)
      return false;

    const step &child = steps[childNum];

    typename std::vector<promotion>::const_iterator begin, end = child.promotions_list.end();
    if(child.is_last_child && !has_new_promotion)
      // Only process new promotions if we don't have one yet.
      begin = child.promotions_list.begin() + child.promotions_list_new_promotions;
    else
      begin = child.promotions_list.begin();

    bool rval = false;
    for(typename std::vector<promotion>::const_iterator it = begin;
	it != end && parent.promotions_list_size() < max_propagated_promotions; ++it)
      {
	bool current_is_new_promotion =
	  (it - child.promotions_list.begin()) >= child.promotions_list_new_promotions;

	choice_set new_choices(choices);
	tier new_tier(t);

	const promotion &p(*it);
	choice_set p_choices(p.get_choices());
	// Strip out the child's link before merging with the existing
	// choice set.
	p_choices.remove(child.reason);
	const tier &p_tier(p.get_tier());
	// Augment the choice set with these new choices.  Narrowing
	// is appropriate: anything matching the promotion should
	// match all the choices we found.
	new_choices.insert_or_narrow(p_choices);
	if(p_tier < new_tier)
	  new_tier = p_tier;

	if(parent.sol.get_tier() >= new_tier)
	  // No point in generating a promotion whose tier is below
	  // the parent's tier.
	  continue;


	if(child.is_last_child)
	  {
	    promotion new_promotion(new_choices, new_tier);

	    LOG_DEBUG(logger, "Created backpropagated promotion at step "
		      << parentNum << ": " << new_promotion);

	    // Actually output a new promotion in the parent.
	    std::pair<std::set<promotion>, bool> insert_info =
	      parent.promotions.insert(new_promotion);

	    if(insert_info.second)
	      // Otherwise this promotion was already generated.
	      parent.promotions_list.push_back(new_promotion);

	    rval = true;
	  }
	else
	  {
	    bool new_has_new_promotion = has_new_promotion || current_is_new_promotion;
	    // Recur.
	    bool generated_anything =
	      add_child_promotions(parentNum, childNum + 1,
				   new_has_new_promotion,
				   new_choices, new_tier);

	    rval = rval && generated_anything;
	  }
      }

    return rval;
  }

  // TODO: log when we first fail to add a promotion because we hit
  // the maximum number -- that's actually not trivial to do without
  // complicating the code.
  void maybe_collect_child_promotions(int stepNum)
  {
    step &parentStep(steps[stepNum]);
    LOG_TRACE(logger, "Backpropagating promotions to step " << stepNum << ": " << parentStep.sol);

    if(parentStep.first_child == -1)
      {
	LOG_ERROR(logger, "Step " << stepNum << " has no children; no promotions to backpropagate.");
	return;
      }

    if(add_child_promotions(stepNum, parentStep.firstChild,
			     false, parentStep.successor_constraints,
			     maximum_tier))
      {
	if(parentStep.parent != -1)
	  {
	    LOG_TRACE(logger, "Scheduling step " << parentStep.parent
		      << " for promotion propagation.");
	    steps_pending_promotion_propagation.insert(parentStep.parent);
	  }
      }
    else
      LOG_TRACE(logger, "No new promotion at step " << stepNum);
  }

  void schedule_promotion_propagation(int stepNum,
				      const promotion &p)
  {
    step &targetStep(steps[stepNum]);

    // TODO: could do a slow check for redundant promotions here?

    std::pair<typename std::set<step>::iterator, bool>
      insert_info(targetStep.promotions.insert(p));
    if(insert_info.second)
      {
	LOG_TRACE(logger, "Adding the promotion " << p
		  << " to step " << stepNum
		  << " and scheduling it for propagation.");
	targetStep.promotions_list.push_back(p);
	if(targetStep.parent != -1)
	  steps_pending_promotion_propagation.insert(stepNum);
      }
  }

  /** \brief Execute any pending promotion propagations. */
  void run_scheduled_promotion_propagations()
  {
    while(!steps_pending_promotion_propagation.empty())
      {
	// Make a temporary copy to iterate over.
	std::set<int, std::greater<int> > tmp;
	tmp.swap(steps_pending_promotion_propagation);
	for(std::set<int, std::greater<int> >::const_iterator it = tmp.begin();
	    it != tmp.end(); ++it)
	  maybe_collect_child_promotions(*it);
      }
  }

  struct is_deferred
  {
    bool operator()(const promotion &p) const
    {
      const tier &p_tier(p.get_tier());

      return
	p_tier >= defer_tier &&
	p_tier < conflict_tier;
    }
  };

  /** \brief Remove any propagated promotions from the deferred
   *  tier.
   */
  void remove_deferred_propagations()
  {
    is_deferred is_deferred_f;

    for(typename std::deque<step>::iterator step_it = steps.begin();
	step_it != steps.end(); ++step_it)
      {
	step &curr_step(*step_it);

	for(typename std::vector<promotion>::const_iterator p_it =
	      curr_step.promotions_list.begin();
	    p_it != curr_step.promotions_list.end(); ++p_it)
	  {
	    const promotion &p(*p_it);

	    if(is_deferred_f(p))
	      curr_step.promotions.erase(p);
	  }

	typename std::vector<promotion>::iterator new_end =
	  std::remove_if(curr_step.promotions_list.begin(),
			 curr_step.promotions_list.end(),
			 is_deferred_f);


	curr_step.promotions_list.erase(new_end, curr_step.promotions_list.end());
      }
  }

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

  /** Compares steps according to their "goodness". */
  struct step_goodness_compare
  {
    const std::deque<step> &steps;

    step_goodness_compare(const std::deque<step> &_steps)
      : steps(_steps)
    {
    }

    bool operator()(int step_num1, int step_num2) const
    {
      const solution &s1(steps[step_num1].sol);
      const solution &s2(steps[step_num2].sol);

      // Note that *lower* tiers come "before" higher tiers, hence the
      // reversed comparison there.
      return s2.get_tier() < s1.get_tier() ||
	(s1.get_tier() == s2.get_tier() && s1.get_score() < s2.get_score());
    }
  };

  /** \brief Compares choices by their effects on the solution.
   *
   *  e.g., two choices that install the same version will always
   *  compare equal.  This is used to check for choices that the user
   *  has rejected or approved.
   */
  struct compare_choices_by_effects
  {
    bool operator()(const choice &c1, const choice &c2) const
    {
      if(c1.get_type() < c2.get_type())
	return true;
      else if(c2.get_type() < c1.get_type())
	return false;
      else
	switch(c1.get_type())
	  {
	  case choice::install_version:
	    return c1.get_ver() < c2.get_ver();

	  case choice::break_soft_dep:
	    return c1.get_dep() < c2.get_dep();
	  }
    }
  };

  /** Compares solutions according to their contents; used to
   *  manage "closed".
   *
   *  Note that this does not attempt to merge choices that have the
   *  same effect; it just ensures that a particular set of choices is
   *  only placed on the open queue once.
   */
  struct solution_contents_compare
  {
    bool operator()(const solution &s1, const solution &s2) const
    {
      // Variation on lexicographical comparison.
      //
      // S1 < S2 if
      //   - S1(p)=S2(p) for all p<p' and S1(p')<S2(p'), where
      //     p,p' \in \dom(S1) \cup dom(S2)


      // NB: the correctness of this code depends implicitly on the
      // invariant that the score is a function of the contents of the
      // solution.  Essentially we're using the score as a sort of
      // hash value on solutions!
      if(s1.get_score() < s2.get_score())
	return true;
      else if(s2.get_score() < s1.get_score())
	return false;
      else if(s1.get_action_score() < s2.get_action_score())
	return true;
      else if(s2.get_action_score() < s1.get_action_score())
	return false;

      const choice_set
	&cs1 = s1.get_choices(), &cs2 = s2.get_choices();


      // Speed hack: order by size first to avoid traversing the whole
      // tree.
      if(cs1.size() < cs2.size())
	return true;
      else if(cs2.size() < cs1.size())
	return false;
      else
	return cs1 < cs2;
    }
  };

  struct step_contents_compare
  {
    const std::deque<step> &steps;
    solution_contents_compare comparer;

    step_contents_compare(const std::deque<step> &_steps)
      : steps(_steps)
    {
    }

    bool operator()(int step_num1, int step_num2) const
    {
      const solution &s1(steps[step_num1].sol);
      const solution &s2(steps[step_num2].sol);

      return comparer(s1, s2);
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

  /** If \b true, it is possible that some deferred solutions are
   *  no longer "forbidden".
   */
  bool deferred_dirty:1;

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



  /** The working queue.
   *
   *  Each entry is a step number.
   */
  std::priority_queue<int, std::vector<int>, step_goodness_compare> open;

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
  std::priority_queue<int, std::vector<int>, step_goodness_compare> future_solutions;

  /** \brief Stores already-seen solutions that had their successors
   *  generated.
   */
  std::set<solution, solution_contents_compare> closed;

  /** \brief Stores solutions that are known to have a higher tier
   *  than the current search tier.
   *
   *  Note: conflict_tier will never have an entry in this set;
   *  solutions at that tier are thrown away rather than deferred.
   */
  std::map<tier, std::set<int, step_contents_compare> > deferred;

  /** Like deferred, but for future solutions.
   *
   *  Note: because we only generate a deferred future solution when
   *  we would otherwise have returned a solution, this doesn't need
   *  to be split by tier: the solutions are always all at the current
   *  tier.
   */
  std::set<int, step_contents_compare> deferred_future_solutions;


  /** Stores tier promotions: sets of installations that will force a
   *  solution to a higher tier of the search.
   */
  promotion_set promotions;

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

  /** Stores versions that have been rejected by the user; distinct
   *  from the per-solution reject sets that track changes on a single
   *  inference path.
   */
  std::set<version> user_rejected;

  /** Stores versions that have been mandated by the user; we should
   *  always pick these versions if the chance arises.
   */
  std::set<version> user_mandated;

  /** Stores dependencies that have been "hardened" by the user (that
   *  aren't allowed to default).
   */
  std::set<dep> user_hardened;

  /** Stores dependencies that the user has said should be broken if
   *  they come up.
   */
  std::set<dep> user_approved_broken;


  typedef std::set<std::pair<version, version> > stupid_table;

#if 0
  /** Generate a table listing the "stupid" pairs of actions in a
   *  solution.  If (a,b) is in the table, then b solves the
   *  dependency that triggered a's installation.
   */
  void initialize_stupid_pairs(const solution &s,
			       stupid_table &table)
  {
    for(typename std::map<package, action>::const_iterator i = s.get_actions().begin();
	i != s.get_actions().end(); ++i)
      {
	const dep &d = i->second.d;

	for(typename dep::solver_iterator j = d.solvers_begin();
	    !j.end(); ++j)
	  {
	    // Don't claim that (v,v) is a stupid pair.
	    if(*j == i->second.ver)
	      continue;

	    typename std::map<package, action>::const_iterator found
	      = s.get_actions().find((*j).get_package());

	    // Check whether j will be installed; if so, insert it.
	    if(found != s.get_actions().end() &&
	       found->second.ver == *j)
	      table.insert(std::pair<version, version>(i->second.ver, *j));
	  }
      }
  }

  /** A solution wrapper suitable for use with broken_under() that
   *  drops one mapping from the solution.
   */
  class drop_package
  {
    solution s;
    package drop;

  public:
    drop_package(const solution &_s, const package &_drop)
      :s(_s), drop(_drop)
    {
    }

    version version_of(const package &p) const
    {
      if(p == drop)
	return s.get_initial_state().version_of(p);
      else
	return s.version_of(p);
    }
  };

  /** An action iterator wrapper that replaces the action on a given
   *  package with the given action.
   */
  class swap_action
  {
    typename std::map<package, action>::const_iterator i;
    package replace;
    action with;
  public:
    swap_action(const typename std::map<package, action>::const_iterator &_i,
		const package &_replace, const action &_with)
      : i(_i), replace(_replace), with(_with)
    {
    }

    bool operator==(const swap_action &other) const
    {
      return i == other.i && replace == other.replace && with.ver == other.with.ver;
    }

    bool operator!=(const swap_action &other) const
    {
      return i != other.i || replace != other.replace || with.ver != other.with.ver;
    }

    swap_action &operator++()
    {
      ++i;
      return *this;
    }

    const action &operator*() const
    {
      if(i->first == replace)
	return with;
      else
	return i->second;
    }

    const action *operator->() const
    {
      if(i->first == replace)
	return &with;
      else
	return &i->second;
    }
  };

  /** Check whether dropping a version from a solution breaks
   *  dependencies.
   *
   *  \param s the "starting" solution; should not have broken deps
   *  \param v the version to drop
   *  \param d output parameter; if there is a broken dependency,
   *         it will be placed in this location.
   *
   *  \return \b true if a broken dependency exists.
   */
  bool broken_by_drop(const solution &s,
		      const version &v,
		      dep &d)
  {
    // Don't flag an error if a dep that's SUPPOSED to be unresolved
    // gets broken.
    const std::set<dep> &ignore_deps = s.get_unresolved_soft_deps();

    drop_package dropped(s, v.get_package());

    version curr = initial_state.version_of(v.get_package());
    for(typename version::revdep_iterator i = curr.revdeps_begin();
	!i.end(); ++i)
      if(ignore_deps.find(*i) == ignore_deps.end() &&
	 (*i).broken_under(dropped))
	{
	  d = *i;
	  return true;
	}

    for(typename version::revdep_iterator i = curr.revdeps_begin();
	!i.end(); ++i)
      if(ignore_deps.find(*i) == ignore_deps.end() &&
	 (*i).broken_under(dropped))
	{
	  d = *i;
	  return true;
	}

    for(typename version::dep_iterator i = curr.deps_begin();
	!i.end(); ++i)
      if(ignore_deps.find(*i) == ignore_deps.end() &&
	 (*i).broken_under(dropped))
	{
	  d = *i;
	  return true;
	}

    return false;
  }

  /** Represents a partial solution based directly (by reference) on a
   *  reference to a map.
   */
  class map_solution
  {
    const std::map<package, action> &actions;
    const resolver_initial_state &initial_state;
  public:
    map_solution(const std::map<package, action> &_actions,
		 const resolver_initial_state &_initial_state)
      : actions(_actions), initial_state(_initial_state)
    {
    }

    version version_of(const package &p) const
    {
      typename std::map<package, action>::const_iterator found = actions.find(p);

      if(found == actions.end())
	return initial_state.version_of(p);
      else
	return found->second.ver;
    }
  };

  /** Represents a possible successor to the given solution, created
   *  by installing a single 'new' version.
   */
  template<typename SolutionType>
  class partial_solution
  {
    /** The solution this is based on. */
    const SolutionType &s;
    /** More stuff to install */
    std::map<package, version> installations;
  public:
    /** Initialize a new solution.
     */
    partial_solution (const SolutionType &_s)
      :s(_s)
    {
    }

    /** Takes a range of versions and installs them in order. */
    template<typename InputIterator>
    void install(const InputIterator &begin, const InputIterator &end)
    {
      for(InputIterator i = begin; i != end; ++i)
	install(i->get_package(), *i);
    }

    void install(const package &p, const version &v)
    {
      eassert_on_pkg(installations.find(p) == installations.end(), p);

      installations[p]=v;
    }

    /** \return the currently installed version of the given
     *	package.
     */
    version version_of(const package &pkg) const
    {
      typename std::map<package, version>::const_iterator found=installations.find(pkg);

      if(found!=installations.end())
	return found->second;
      else
	return s.version_of(pkg);
    }

    /** Test whether the given version is installed in this solution.
     */
    bool ver_installed(const version &test_ver) const
    {
      return version_of(test_ver.get_package()) == test_ver;
    }
  };

  /** Wrap a solver list by generating pairs where the second value
   *  is a bound constant value.
   */
  class forbid_iter_builder
  {
    typename dep::solver_iterator i;
    dep d;
  public:
    forbid_iter_builder(const typename dep::solver_iterator &_i,
			const dep &_d)
      :i(_i), d(_d)
    {
    }

    typename std::pair<version, dep> operator*() const
    {
      return std::pair<version, dep>(*i, d);
    }

    forbid_iter_builder &operator++()
    {
      ++i;
      return *this;
    }

    bool end() const
    {
      return i.end();
    }
  };

  /** Wrap an iterator over pairs to an iterator over the second
   *  element of the pair.
   */
  template<typename Iter>
  class project_iter_2nd_impl
  {
    Iter i;
  public:
    project_iter_2nd_impl(const Iter &_i)
      :i(_i)
    {
    }

    typename Iter::value_type::second_type &operator*() const
    {
      return i->second;
    }

    typename Iter::value_type::second_type *operator->() const
    {
      return &i->second;
    }

    project_iter_2nd_impl &operator++()
    {
      ++i;
      return *this;
    }

    bool operator==(const project_iter_2nd_impl &other) const
    {
      return i == other.i;
    }

    bool operator!=(const project_iter_2nd_impl &other) const
    {
      return i != other.i;
    }
  };

  template<typename Iter>
  project_iter_2nd_impl<Iter> project_iter_2nd(const Iter &i)
  {
    return project_iter_2nd_impl<Iter>(i);
  }

  /** Wrap an action iterator to select its version element. */
  template<class Iter>
  class project_ver_impl
  {
    Iter i;
  public:
    project_ver_impl(const Iter &_i)
      :i(_i)
    {
    }

    bool operator==(const project_ver_impl &other)
    {
      return i == other.i;
    }

    bool operator!=(const project_ver_impl &other)
    {
      return i != other.i;
    }

    project_ver_impl &operator++()
    {
      ++i;
      return *this;
    }

    const version &operator*() const
    {
      return i->ver;
    }

    const version *operator->() const
    {
      return &i->ver;
    }
  };

  template<typename Iter>
  project_ver_impl<Iter> project_ver(const Iter &i)
  {
    return project_ver_impl<Iter>(i);
  }


  /** Convert a [begin,end) pair to a single APT-style iterator.
   */
  template<class iter>
  class apt_iter_wrapper_impl
  {
    iter curr, the_end;
  public:
    apt_iter_wrapper_impl(const iter &_begin, const iter &_end)
      :curr(_begin), the_end(_end)
    {
    }

    bool end() const
    {
      return curr==the_end;
    }

    apt_iter_wrapper_impl &operator++()
    {
      ++curr;
      return *this;
    }

    const typename std::iterator_traits<iter>::reference operator*() const
    {
      return *curr;
    }

    const typename std::iterator_traits<iter>::reference operator->() const
    {
      return &*curr;
    }
  };

  template<class Iter>
  apt_iter_wrapper_impl<Iter> apt_iter_wrapper(const Iter &begin,
					       const Iter &end)
  {
    return apt_iter_wrapper_impl<Iter>(begin, end);
  }


  /** Given a list of actions, remove "obviously" unnecessary actions;
   *  that is, actions which are not inspired by any broken
   *  dependency.  Corresponds to ResolveFrom, but is a bit more
   *  extreme in its approach.  As usual, no effort is made to avoid
   *  stupid pairs.
   *
   *  \param actions the set of actions to apply
   *
   *  \param unresolved_soft_deps a set of broken dependencies which
   *                             are permitted (indeed, required?)
   *                             to be broken at the end of the process.
   *
   *  \param forbidden_iter an iterator over the list of versions that
   *  should be forbidden in the new solution.
   *
   *  \return a solution containing only the actions which appear to
   *  be necessary.
   */
  template<typename forbid_iter>
  solution filter_unnecessary_actions(const std::vector<action> &actions,
				      const std::set<dep> &unresolved_soft_deps,
				      const forbid_iter &forbidden_iter)
  {
    // Versions from which to choose.
    std::set<version> avail_versions(project_ver(actions.begin()),
				     project_ver(actions.end()));

    // Currently broken deps.
    std::set<dep> broken_deps = initial_broken;

    // The solution being built.
    std::map<package, action> output_actions;

    // Score of this solution
    int action_score = 0;

    while(!broken_deps.empty())
      {
	eassert(!avail_versions.empty());

	// Pick an "arbitrary" broken dependency.
	dep d = *broken_deps.begin();
	broken_deps.erase(broken_deps.begin());

	// If it's to be ignored, just drop it on the floor.
	if(unresolved_soft_deps.find(d) != unresolved_soft_deps.end())
	  continue;

	// Now, pick an "arbitrary" resolver of this dep.
	//
	// (maybe pick the best-scored one that's available instead?)
	bool found_one = false;
	version solver;
	for(typename dep::solver_iterator si = d.solvers_begin();
	    !si.end(); ++si)
	  {
	    typename std::set<version>::const_iterator found
	      = avail_versions.find(*si);

	    if(found != avail_versions.end())
	      {
		found_one = true;
		solver = *si;
		break;
	      }
	  }

	// I have a proof that a solver should exist, but that doesn't
	// mean it does ;-)
	eassert_on_dep(found_one, d);
	eassert_on_ver(output_actions.find(solver.get_package()) == output_actions.end(), solver);
	eassert_on_ver(solver != initial_state.version_of(solver.get_package()), solver);

	LOG_TRACE("Filter: resolving " << d << " with "
		  << solver);

	action act(solver, d, output_actions.size());

	// Update the set of broken dependencies.  FIXME: it should be
	// possible to do this in-place, maybe using immutable sets?
	std::set<dep> new_broken;
	update_broken(partial_solution<map_solution>(output_actions),
		      project_ver(&act),
		      project_ver((&act)+1),
		      unresolved_soft_deps,
		      broken_deps,
		      new_broken);

	// Expensive copy.
	broken_deps = new_broken;

	// Finally, update the old solution with the new action.
	output_actions[solver.get_package()] = act;
	action_score += step_score;
	action_score += version_scores[solver.get_id()];
	action_score -= version_scores[initial_state.version_of(solver.get_package()).get_id()];
      }

    // By definition we have a solution now.
    int score = action_score + full_solution_score + unfixed_soft_score * unresolved_soft_deps.size();

    return solution(project_iter_2nd(output_actions.begin()),
		    project_iter_2nd(output_actions.end()),
		    unresolved_soft_deps,
		    broken_deps,
		    forbidden_iter,
		    score,
		    action_score);
  }

  /** Eliminate stupid pairs from the given solution.
   *
   *  \param s the solution from which stupid pairs should be removed.
   *
   *  \return the new solution
   */
  solution eliminate_stupid(const solution &s)
  {
    eassert_on_soln(s.get_broken().empty(), s);

    stupid_table stupid_pairs;

    initialize_stupid_pairs(s, stupid_pairs);

    solution rval = s;

    bool contained_stupid = !stupid_pairs.empty();

    while(!stupid_pairs.empty())
      {
	LOG_TRACE(logger, "Eliminating stupid pairs from " << tmp);

	// The pair to eliminate; picked (sorta) arbitrarily.
	const typename std::pair<version, version> &victim = *stupid_pairs.begin();
	stupid_pairs.erase(stupid_pairs.begin());

	// This is where we discard pairs that are no longer relevant
	// because one of the elements of the pair has been dropped.
	if(rval.get_actions().find(victim.first.get_package()) == rval.get_actions().end() ||
	   rval.get_actions().find(victim.second.get_package()) == rval.get_actions().end())
	  {
	    LOG_TRACE(logger,
		      "Dropping invalidated stupid pair("
		      << victim.first << ", "
		      << victim.second << ")");

	    continue;
	  }

	LOG_TRACE(logger,
		  "Examining stupid pair ("
		  << victim.first << ", "
		  << victim.second << ")");

	// Suppose we drop the second element in favor of the first.
	// Will that produce a valid solution?
	dep first_broken;

	// If something was broken, then we need to use the fallback
	// position: just change the dependency given as the "reason"
	// for the first action.
	if(broken_by_drop(rval, victim.first, first_broken))
	  {
	    LOG_TRACE(logger,
		      "Unable to drop "
		      << victim.first
		      << ", changing justification to "
		      << first_broken);

	    typename std::map<package, action>::const_iterator found = rval.get_actions().find(victim.first.get_package());
	    eassert(found != rval.get_actions().end());

	    action repl_action(victim.first, first_broken,
			       found->second.id);

	    // Note that the score is perfectly correct, as it depends
	    // only on the set of versions in rval (which doesn't
	    // change).  The set of forbidden versions is in some
	    // sense "wrong", but if we have a final solution this
	    // shouldn't be a problem.

	    swap_action swbegin(rval.get_actions().begin(),
				victim.first.get_package(),
				repl_action);
	    swap_action swend(rval.get_actions().end(),
			      victim.first.get_package(),
			      repl_action);

	    rval = solution(swbegin, swend,
			    rval.get_unresolved_soft_deps(),
			    rval.get_broken(),
			    apt_iter_wrapper(rval.get_forbidden_versions().begin(),
					     rval.get_forbidden_versions().end()),
			    rval.get_score(), rval.get_action_score());
	  }
	else
	  {
	    LOG_TRACE("Dropping "
		      << victim.first
		      << " and filtering unnecessary installations.");
	    // Ok, it's safe.
	    //
	    // Generate a node by dropping the second element and
	    // changing the version of the first:
	    std::vector<action> actions;

	    for(typename std::map<package, action>::const_iterator i =
		  s.get_actions().begin(); i != s.get_actions().end(); ++i)
	      {
		if(i->second.ver == victim.first)
		  actions.push_back(action(victim.second, i->second.d,
					   i->second.id));
		else if(i->second.ver != victim.second)
		  actions.push_back(i->second);
	      }

	    rval = filter_unnecessary_actions(actions,
					      rval.get_unresolved_soft_deps(),
					      apt_iter_wrapper(rval.get_forbidden_versions().begin(),
							       rval.get_forbidden_versions().end()));
	  }
      }

    LOG_TRACE(logger,
	      (contained_stupid
	       ? "Done eliminating stupid pairs, result is "
	       : "No stupid pairs in ")
	      << tmp);

    eassert(rval.get_broken().empty());

    return rval;
  }
#endif

  solution eliminate_stupid(const solution &s) const
  {
    LOG_TRACE(logger, "Would eliminate stupid, but stupid elimination is disabled.");

    return s;
  }

  /** \brief Used to convert a set of choices to a (possibly) more
   *  inclusive set that includes any choices which have the same
   *  effect on the package cache.
   *
   *  This is used to ensure that solutions which have been returned
   *  from the resolver are never produced again.
   */
  struct widen_choices_to_contents
  {
    choice_set &rval;

    widen_choices_to_contents(choice_set &_rval)
      : rval(_rval)
    {
    }

    bool operator()(const choice &c) const
    {
      switch(c.get_type())
	{
	case choice::install_version:
	  rval.insert_or_narrow(choice::make_install_version(c.get_ver(), c.get_dep(), c.get_id()));
	  break;

	case choice::break_soft_dep:
	  rval.insert_or_narrow(c);
	  break;
	}

      return true;
    }
  };

  static choice_set get_solution_choices_without_dep_info(const solution &s)
  {
    choice_set rval;
    widen_choices_to_contents populator(rval);

    s.get_choices().for_each(populator);

    return rval;
  }

  /** \brief Determine the tier of the given solution using only the
   *  promotions set.
   *
   *  For a completely correct calculation of the solution's tier,
   *  callers should ensure that tier information from each version
   *  contained in the solution is already "baked in" (this is true
   *  for anything we pull from the open queue).
   */
  const tier &get_solution_tier(const solution &s) const
  {
    const tier &s_tier(s.get_tier());
    choice_set choices = s.get_choices();
    typename promotion_set::const_iterator found =
      promotions.find_highest_promotion_for(choices);
    if(found != promotions.end())
      {
	const tier &found_tier(found->get_tier());
	if(s_tier < found_tier)
	  return found_tier;
	else
	  return s_tier;
      }
    else
      return s_tier;
  }

  /** \brief Determine the tier of the given solution, based only
   *  on promotions containing the given choice.
   */
  const tier &get_solution_tier_containing(const solution &s,
					   const choice &c) const
  {
    // Build a set of choices.  \todo Maybe solutions should be based
    // on the "choice" object instead of versions / broken deps?

    const tier &s_tier = s.get_tier();
    choice_set choices = s.get_choices();

    typename promotion_set::const_iterator found =
      promotions.find_highest_promotion_containing(choices, c);

    if(found != promotions.end())
      {
	const tier &found_tier = found->get_tier();
	if(found_tier > s_tier)
	  return found_tier;
	else
	  return s_tier;
      }
    else
      return s_tier;
  }

  // Note that these routines are slower than they could be: with some
  // extra indexing in choice_set and/or more generic stuff in immset,
  // they could use the fast set containment algorithm that immset
  // provides.  But they are only invoked when the user constraints
  // are changed, or when a solution is being added to or removed from
  // the queue.  This is a relatively infrequent operation, and it
  // wouldn't be worth the trouble to speed it up.

  struct choice_does_not_break_user_constraint
  {
    const std::set<version> &rejected_versions;
    const std::set<version> &mandated_versions;
    const std::set<dep> &hardened_deps;
    const std::set<dep> &approved_broken_deps;
    const resolver_initial_state<PackageUniverse> &initial_state;
    log4cxx::LoggerPtr logger;

    choice_does_not_break_user_constraint(const std::set<version> &_rejected_versions,
					  const std::set<version> &_mandated_versions,
					  const std::set<dep> &_hardened_deps,
					  const std::set<dep> &_approved_broken_deps,
					  const resolver_initial_state<PackageUniverse> &_initial_state,
					  const log4cxx::LoggerPtr &_logger)
      : rejected_versions(_rejected_versions),
	mandated_versions(_mandated_versions),
	hardened_deps(_hardened_deps),
	approved_broken_deps(_approved_broken_deps),
	initial_state(_initial_state),
	logger(_logger)
    {
    }

    /** \brief Test whether a choice violates a user-specified
     *  constraint.
     *
     *  \return \b true if the choice is OK, \b false if it breaks a
     *  user constraint.
     *
     *  Because this returns \b false if it breaks a user constraint,
     *  it is suitable for use with choice_set::for_each (it will
     *  short-circuit and cause for_each() to return true or false as
     *  appropriate).
     */
    bool operator()(const choice &c) const
    {
      // Check the easy cases first: it's rejected (in which case it
      // violates a constraint) or it's mandated (in which case it
      // definitely doesn't).  The only remaining case is that it
      // avoids a mandated choice; that's handled below.
      switch(c.get_type())
	{
	case choice::install_version:
	  {
	    version ver(c.get_ver());
	    if(rejected_versions.find(ver) != rejected_versions.end())
	      {
		LOG_TRACE(logger, c << " is rejected: it installs the rejected version " << ver);
		return false;
	      }

	    if(mandated_versions.find(ver) != mandated_versions.end())
	      return true;

	    break;
	  }

	case choice::break_soft_dep:
	  {
	    dep d(c.get_dep());

	    if(hardened_deps.find(d) != hardened_deps.end())
	      {
		LOG_TRACE(logger, c << " is rejected: it breaks the hardened soft dependency " << d);
		return false;
	      }

	    if(approved_broken_deps.find(d) != approved_broken_deps.end())
	      return true;
	  }

	  break;
	}

      // Testing whether the choice avoids a mandated choice is
      // trickier; we have to unpack it and look at what the
      // alternatives were.


      // \todo This preserves the old behavior, but I don't think it's
      // right.  If an approved alternative was thrown out because it
      // was illegal, that shouldn't cause all the other alternatives
      // to be deferred.  We should invoke is_legal() to double-check
      // that each alternative could actually be chosen (and thus this
      // routine should take a solution so that we have some context).
      switch(c.get_type())
	{
	case choice::install_version:
	  {
	    const dep &d(c.get_dep());
	    const version &chosen(c.get_ver());

	    // We could have avoided this choice by installing a
	    // version of the dependency's source that's not the
	    // version we chose or the source itself.
	    version source(d.get_source());
	    package source_p(source.get_package());

	    for(typename package::version_iterator vi = source_p.versions_begin();
		!vi.end(); ++vi)
	      {
		version alternate(*vi);

		if(alternate != source && alternate != chosen)
		  {
		    if(mandated_versions.find(alternate) != mandated_versions.end())
		      {
			LOG_TRACE(logger, c << " avoids installing the mandated version "
				  << alternate << " to solve the dependency " << d);
			return false;
		      }
		  }
	      }

	    // We could also have avoided this choice by installing a
	    // different solver of the same dependency.
	    for(typename dep::solver_iterator si = d.solvers_begin();
		!si.end(); ++si)
	      {
		version solver(*si);

		if(solver != chosen)
		  {
		    if(mandated_versions.find(solver) != mandated_versions.end())
		      {
			LOG_TRACE(logger, c << " avoids installing the mandated version "
				  << solver << " to solve the dependency " << d);
			return false;
		      }
		  }
	      }



	    // We could also have violated a user constraint by
	    // accidentally solving a soft dependency that the user
	    // told us to break.  Because of the possibly inconsistent
	    // reverse dependency links, we need to check the reverse
	    // dependencies of both the target and the initial
	    // versions, to ensure that all the dependencies are
	    // found.
	    //
	    // (note: in the apt case, checking both lists isn't
	    // necessary because soft dependencies always appear in
	    // the right place (only Conflicts are a problem and
	    // they're never soft); however, this double check makes
	    // the code more obviously correct)
	    if(!d.is_soft())
	      return true;

	    for(typename version::revdep_iterator rdi = chosen.revdeps_begin();
		!rdi.end(); ++rdi)
	      {
		dep rd(*rdi);

		if(rd.is_soft())
		  {
		    if(approved_broken_deps.find(rd) != approved_broken_deps.end())
		      {
			LOG_TRACE(logger, c << " solves the soft dependency " << rd
				  << " which was mandated to be broken.");
			return false;
		      }
		  }
	      }

	    version chosen_initial(initial_state.version_of(chosen.get_package()));
	    for(typename version::revdep_iterator rdi = chosen_initial.revdeps_begin();
		!rdi.end(); ++rdi)
	      {
		dep rd(*rdi);

		if(rd.is_soft())
		  {
		    if(approved_broken_deps.find(rd) != approved_broken_deps.end())
		      {
			LOG_TRACE(logger, c << " solves the soft dependency " << rd
				  << " which was mandated to be broken.");
			return false;
		      }
		  }
	      }
	  }

	  return true;

	case choice::break_soft_dep:
	  // This could only avoid a mandated choice if a move that
	  // would have solved the dependency was mandated.  That is:
	  // we need to check other versions of the source, and each
	  // solver of the dependency.
	  {
	    const dep &d(c.get_dep());
	    version source(d.get_source());
	    package source_p(source.get_package());

	    for(typename package::version_iterator vi = source_p.versions_begin();
		!vi.end(); ++vi)
	      {
		version alternate(*vi);

		if(alternate != source &&
		   mandated_versions.find(alternate) != mandated_versions.end())
		  {
		    LOG_TRACE(logger, c << " fails to install the mandated version " << alternate);
		    return false;
		  }
	      }

	    for(typename dep::solver_iterator si = d.solvers_begin();
		!si.end(); ++si)
	      {
		version solver(*si);

		if(mandated_versions.find(solver) != mandated_versions.end())
		  {
		    LOG_TRACE(logger, c << " fails to install the mandated version " << solver);
		    return false;
		  }
	      }
	  }

	  return true;
	}

      LOG_ERROR(logger, c << " is an unknown choice type!");
      return true;
    }
  };

  /** \return \b true if the given solution breaks a constraint
   *  imposed by the user.
   */
  bool breaks_user_constraint(const solution &s) const
  {
    choice_does_not_break_user_constraint f(user_rejected, user_mandated,
					    user_hardened, user_approved_broken,
					    initial_state, logger);

    bool does_not_break_user_constraint = s.get_choices().for_each(f);
    return !does_not_break_user_constraint;
  }

  /** Place any solutions that were deferred and are no longer
   *  rejected back on the open queue.
   */
  void reexamine_deferred()
  {
    // Throw out all the defer-tier promotions that were discovered
    // during promotion propagation.
    remove_deferred_propagations();

    // \todo If we un-defer a search node that has a lower tier than
    // the current maximum tier, should we reset the maximum tier to
    // be lower?  i.e., if we are at the "remove packages" tier, and
    // we un-defer some search nodes that only install packages,
    // should we put all the nodes at the "remove packages" tier aside
    // until we've examined all the nodes that were newly injected
    // into the "install packages" tier?


    // Throw out all higher-level knowledge about deferrals.  The only
    // way to ensure that this is done properly, but preserve
    // information that doesn't need to be discarded, is to somehow
    // (how?) "tag" each entry in the promotion set with the deferrals
    // that it is associated with.  Then only the promotions that have
    // actually become irrelevant need to be thrown away.
    promotions.remove_between_tiers(defer_tier, already_generated_tier);

    // Place any deferred entries that are no longer deferred back
    // into the "open" queue.
    const typename std::map<tier, std::set<int, step_contents_compare> >::iterator
      first_deferred = deferred.lower_bound(defer_tier),
      first_already_generated = deferred.lower_bound(already_generated_tier);

    if(first_deferred != first_already_generated)
      {
	LOG_TRACE(logger, "Re-examining the deferred tier.");

	for(typename std::map<tier, std::set<int, step_contents_compare> >::iterator
	      it = first_deferred; it != first_already_generated; ++it)
	  {
	    // Walk down the set associated with this tier and move
	    // anything that's not deferred any more back to the open
	    // queue.
	    typename std::set<int, step_contents_compare>::iterator
	      i = it->second.begin(), j = i;

	    while(i != it->second.end())
	      {
		LOG_TRACE(logger, "Re-examining step " << *i);

		++j;

		if(!breaks_user_constraint(steps[*i].sol))
		  {
		    LOG_DEBUG(logger, "Step " << *i << " is no longer deferred, placing it back on the open queue.");

		    open.push(*i);
		    it->second.erase(i);
		  }

		i = j;
	      }
	  }
      }
    else
      LOG_TRACE(logger, "Not re-examining the deferred tier: it does not exist.");

    LOG_TRACE(logger, "Re-examining the future deferred set.");

    typename std::set<int, step_contents_compare>::const_iterator
      i = deferred_future_solutions.begin(), j = i;

    while(i != deferred_future_solutions.end())
      {
	LOG_TRACE(logger, "Re-examining step " << *i);

	++j;

	if(!breaks_user_constraint(steps[*i].sol))
	  {
	    LOG_DEBUG(logger, "Step " << *i << " is no longer deferred, placing it back on the open queue.");

	    future_solutions.push(*i);
	    deferred_future_solutions.erase(i);
	  }

	i = j;
      }

    // Note that we might have to rescind the "finished" state: the
    // actions above can actually cause new solutions to be available
    // for processing!
    if(finished && (!open.empty() || !future_solutions.empty()))
      {
	LOG_DEBUG(logger, "More steps are available; clearing the \"finished\" flag on the dependency solver.");
	finished = false;
      }

    LOG_TRACE(logger, "Done re-examining the deferred step lists.");

    deferred_dirty = false;
  }

  /** \return \b true if the given solution is "irrelevant": that is,
   *  either it was already generated and placed in the closed queue,
   *  or it includes an already-generated solution as a proper subset.
   */
  bool irrelevant(const solution &s)
  {
    if(closed.find(s) != closed.end())
      {
	LOG_TRACE(logger, s << " is irrelevant: it was already encountered in this search.");
	return true;
      }

    const tier &s_tier = s.get_tier();
    if(s_tier >= conflict_tier)
      {
	LOG_TRACE(logger, s << " is irrelevant: it contains a conflict.");
	return true;
      }
    else if(s_tier >= already_generated_tier)
      {
	LOG_TRACE(logger, s << " is irrelevant: it was already produced as a result.");
	return true;
      }

    if(s.get_score() < minimum_score)
      {
	LOG_TRACE(logger, s << "is irrelevant: it has infinite badness " << s.get_score() << "<" << minimum_score);
	return true;
      }

    return false;
  }

  /** \brief Defer a step to the given tier. */
  void defer_step(const tier &t, int step_num)
  {
    typename std::map<tier, std::set<int, step_contents_compare> >::iterator
      found(deferred.find(t));
    if(found == deferred.end())
      found = deferred.insert(found, std::make_pair(t,
						    std::set<int, step_contents_compare>(step_contents_compare(steps))));

    found->second.insert(step_num);
  }

  /** Tries to enqueue the given step. */
  void try_enqueue(const int step_num)
  {
    const solution &s = steps[step_num].sol;
    const tier &s_tier = get_solution_tier(s);
    if(maximum_search_tier < s_tier)
      {
	if(s_tier >= conflict_tier)
	  LOG_TRACE(logger, "Dropping solution " << s << " (it is at the conflict tier).");
	else if(s_tier >= already_generated_tier)
	  LOG_TRACE(logger, "Dropping solution " << s << " (it was already generated as a solution).");
	else
	  {
	    if(s_tier >= defer_tier)
	      LOG_TRACE(logger, "Deferring rejected solution " << s << " (it breaks a user constraint).");
	    else
	      LOG_TRACE(logger, "Deferring " << s << " until tier " << s_tier);

	    defer_step(s_tier, step_num);
	  }
      }
    else if(irrelevant(s))
      // Shouldn't happen: we should have caught irrelevant solutions
      // earlier in the process.
      LOG_WARN(logger, "Unexpectedly irrelevant solution " << s);
    else if(breaks_user_constraint(s))
      {
	LOG_WARN(logger, "Deferring rejected solution " << s);
	defer_step(defer_tier, step_num);
      }
    else
      {
	LOG_TRACE(logger, "Enqueuing " << s);

	open.push(step_num);
      }
  }

  /** Try to enqueue the children of the given step. */
  void try_enqueue_children(int parent_step_num)
  {
    if(parent_step_num == -1)
      return;

    int child_step_num = steps[parent_step_num].first_child;
    while(child_step_num != -1)
      {
	try_enqueue(child_step_num);
	if(steps[child_step_num].is_last_child)
	  child_step_num = -1;
	else
	  ++child_step_num;
      }
  }

  /** Internal routine to check for the legality of a 'move' and
   *  generate a conflictor if it's not legal.
   *
   *  \param s   A solution giving the current context.
   *  \param v   The version to consider installing.
   *  \param out_choice   If v is illegal, this will be set
   *                      to an existing choice in s that
   *                      rendered v illegal.
   */
  bool is_legal(const solution &s,
		const version &v,
		choice &out_choice) const
  {
    package p = v.get_package();
    version cur = initial_state.version_of(p);
    version inst = s.version_of(p);

    if(inst != cur)
      {
	LOG_TRACE(logger,
		  "Discarding " << v
		  << ": monotonicity violation");

	out_choice = choice::make_install_version(inst, dep(), -1);
	return false;
      }
    else
      {
	eassert_on_ver(v != cur, s, v);

	typename imm::map<version, dep>::node found
	  = s.get_forbidden_versions().lookup(v);

	if(!found.isValid())
	  return true;
	else
	  {
	    // \todo Store choices instead of deps as the reason for
	    // forbidden versions; then we can also forbid resolving a
	    // broken soft dependency, which should cut down the
	    // branching factor.
	    const dep &found_d = found.getVal().second;

	    LOG_TRACE(logger,
		      "Discarding " << v
		      << ": forbidden by the resolution of " << found_d);

	    // The version that was installed to change the
	    // dependency's source.
	    version culprit_ver = s.version_of(found_d.get_source().get_package());
	    out_choice = choice::make_install_version_from_dep_source(culprit_ver, found_d, -1);

	    return false;
	  }
      }
  }

  /** Generate a solution and add a resolver step for it.
   *
   *  Note: correctness here relies on the fact that we don't
   *  interleave generation.  There's a sanity-check to ensure this.
   */
  struct real_generator
  {
    // The first time a step is generated, we update the parent's
    // child pointer.
    bool first;
    int parent_step_num;
    std::deque<step> &steps;
    typename std::deque<step>::size_type steps_size;
    std::set<package> *visited_packages;
    const log4cxx::LoggerPtr &logger;
  public:
    real_generator(int _parent_step_num,
		   std::deque<step> &_steps,
		   std::set<package> *_visited_packages,
		   const log4cxx::LoggerPtr &_logger)
      : first(true),
	parent_step_num(_parent_step_num),
	steps(_steps),
	steps_size(steps.size()),
	visited_packages(_visited_packages),
	logger(_logger)
    {
    }

    void make_successor(const solution &s,
			const choice &c,
			const tier &succ_tier,
			const PackageUniverse &universe,
			const solution_weights<PackageUniverse> &weights)
    {
      eassert(steps.size() == steps_size);

      // Update the parent's child link the first time through.
      if(first)
	{
	  if(parent_step_num != -1)
	    steps[parent_step_num].first_child = steps.size();
	  first = false;
	}
      else
	// On subsequent trips through this routine, move forward the
	// "last child" flag.
	steps[steps.size() - 1].is_last_child = false;

      solution sol(solution::successor(s,
				       &c, (&c) + 1,
				       succ_tier,
				       universe, weights));
      steps.push_back(step(sol, parent_step_num, c, true));

      steps_size = steps.size();

      LOG_TRACE(logger, "Generated successor (step " << steps_size - 1 << "): " << steps.back().sol);

      // Touch all the packages that are involved in broken dependencies
      if(visited_packages != NULL)
	{
	  const solution &generated(steps.back().sol);

	  for(typename imm::set<dep>::const_iterator bi =
		generated.get_broken().begin();
	      bi != generated.get_broken().end(); ++bi)
	    {
	      for(typename dep::solver_iterator si =
		    (*bi).solvers_begin(); !si.end(); ++si)
		visited_packages->insert((*si).get_package());

	      visited_packages->insert((*bi).get_source().get_package());
	    }
	}
    }
  };

  /** Don't actually generate solutions; instead, just count how many
   *  *would* be generated.  Each time a solution would be generated,
   *  the integer referenced by this object is incremented (it is
   *  never set to 0; if you need it to be initialized to 0, do that
   *  yourself).
   */
  class null_generator
  {
    int &count;
  public:
    null_generator(int &_count)
      :count(_count)
    {
    }

    void make_successor(const solution &s,
			const choice &c,
			const tier & succ_tier,
			const PackageUniverse &universe,
			const solution_weights<PackageUniverse> &weights) const
    {
      ++count;
    }

    int get_count() const
    {
      return count;
    }
  };

  /** \brief Function object that inserts each choice it is invoked on
   *  into a given choice set if the choice does not install a given
   *  version.
   *
   *  Used in generate_single_successor().
   */
  struct add_choices_not_for_version
  {
    choice_set &output;
    const version &ver;

    add_choices_not_for_version(choice_set &_output,
				const version &_ver)
      : output(_output), ver(_ver)
    {
    }

    bool operator()(const choice &c) const
    {
      bool ok = true;

      switch(c.get_type())
	{
	case choice::install_version:
	  ok = (ver != c.get_ver());
	  break;

	default:
	  break;
	}

      if(ok)
	output.insert_or_narrow(c);

      return true;
    }
  };

  /** Convenience routine for the below: try to generate a successor
   *  by installing a single package version.  NB: assumes that the
   *  solution's choices have dense identifiers (i.e., less than
   *  s.get_choices().size()).
   *
   *  \param s the solution for which a successor should be generated
   *  \param v the version to install
   *  \param d the dependency for which the successor is being generated.
   *  \param from_dep_source if \b true, this successor is the result
   *                         of an action on the source of a dependency
   *  \param result_promotion    set to an empty promotion to the lowest
   *                             tier if no promotion was found;
   *                             otherwise, set to the promotion that
   *                             was found for this particular successor.
   *
   *  \param generator an object supporting the make_successor() routine,
   *                   as real_generator and null_generator above.
   */
  template<typename SolutionGenerator>
  void generate_single_successor(const solution &s,
				 const dep &d,
				 const version &v,
				 bool from_dep_source,
				 promotion &result_promotion,
				 SolutionGenerator &generator,
				 std::set<package> *visited_packages) const
  {
    // Note: it's essential for correctness that the set of forcing
    // choices ends up containing every choice that "caused" the
    // dependency to be broken.  Currently this happens
    // coincidentally: if the source was modified so as to cause the
    // dependency to become relevant, then it can't be changed again
    // (so that installation is one reason), and if a solver was
    // modified from being installed to not, then it can't be changed
    // again (so that's another reason).  If the generation of
    // promotions becomes more intelligent, I'll need to think a
    // little more about whether this is right.
    if(visited_packages != NULL)
      visited_packages->insert(v.get_package());

    choice why_illegal;

    LOG_TRACE(logger,
	      "Trying to resolve " << d << " by installing "
	      << v.get_package().get_name() << " "
	      << v.get_name()
	      << (from_dep_source ? " from the dependency source" : ""));

    const int newid = s.get_choices().size();

    // The contents of the promotion to return.
    tier promotion_tier(minimum_tier);
    choice_set forcing_choices;

    if(!is_legal(s, v, why_illegal))
      {
	promotion_tier = conflict_tier;
	forcing_choices.insert_or_narrow(why_illegal);
      }
    else
      {
	choice new_choice;
	if(from_dep_source)
	  new_choice = choice::make_install_version_from_dep_source(v, d, newid);
	else
	  new_choice = choice::make_install_version(v, d, newid);

	// Speculatively form the new set of actions; doing this
	// rather than forming the whole solution allows us to avoid
	// several rather expensive steps in the successor routine
	// (most notably the formation of the new broken packages
	// set).
	choice_set new_choices = s.get_choices();
	new_choices.insert_or_narrow(new_choice);

 	typename promotion_set::const_iterator
	  found = promotions.find_highest_promotion_containing(new_choices,
							       new_choice);

	// If we didn't find a promotion, or the promotion didn't push
	// the solution to a level that would cause it to be thrown
	// away, go ahead and generate successors.  The caller will
	// defer them or put them into the open queue, as appropriate.
	if(!(found != promotions.end() &&
	     (found->get_tier() >= conflict_tier ||
	      found->get_tier() >= already_generated_tier)))
	  {
	    // Figure out the tier of the new solution.  It will be at
	    // least the tier of the current solution, but if we
	    // encountered a promotion or if the version that was
	    // selected has a higher tier, we need to promote the new
	    // solution to that tier (and generate promotion
	    // information).
	    const tier &current_tier = s.get_tier();
	    const tier &ver_tier = version_tiers[v.get_id()];

	    if(found != promotions.end())
	      {
		const tier &found_tier = found->get_tier();
		if(ver_tier < found_tier)
		  {
		    if(current_tier < found_tier)
		      {
			LOG_TRACE(logger,
				  v << " will promote this solution to tier " << found_tier
				  << " due to the promotion " << *found);
			promotion_tier = found_tier;
			forcing_choices = found->get_choices();
		      }
		    else
		      promotion_tier = current_tier;
		  }
		else
		  {
		    if(current_tier < ver_tier)
		      {
			LOG_TRACE(logger,
				  v << " will promote this solution to tier " << ver_tier);
			promotion_tier = ver_tier;
		      }
		    else
		      promotion_tier = current_tier;
		  }
	      }
	    else if(current_tier < ver_tier)
	      {
		LOG_TRACE(logger,
			  v << " will promote this solution to tier " << ver_tier);
		promotion_tier = ver_tier;
	      }
	    else
	      promotion_tier = current_tier;


	    generator.make_successor(s, new_choice,
				     promotion_tier,
				     universe, weights);
	  }
	else
	  LOG_TRACE(logger, "Discarding " << v << " due to conflict "
		    << found->get_choices());

	// Now add anything that we learned to forcing_choices and
	// extend the output promotion.

	// Note that the output promotion contains the reasons that
	// installing this version triggered a promotion; i.e., all
	// the choices in the promotion *except* the one under
	// consideration.  This means that if the version itself
	// triggered a promotion, we just ignore it: installing this
	// version triggers the promotion all by itself in that case.
	// We only need to "remember" choices that had to do with a
	// promotion that we hit.
	if(found != promotions.end())
	  {
	    const promotion &p(*found);
	    const choice_set &choices(p.get_choices());

	    choices.for_each(add_choices_not_for_version(forcing_choices, v));
	  }
      }

    // Note that we have to pass all the candidate promotions up to
    // the parent because the reasons for all the candidate
    // installations have to be combined to form the final promotion
    // for a solution.
    result_promotion = promotion(forcing_choices, promotion_tier);
  }

  /** Build the successors of a solution node for a particular
   *  dependency.
   *
   *  Any promotions / conflicts discovered in the course of this
   *  process will be immediately inserted into the global set of
   *  promotions.
   *
   *  \param visited_packages a set into which the packages used
   *                          to compute the successors will be
   *                          pushed.
   *
   *  \param out a vector onto which the successors should be pushed.
   */
  template<typename SolutionGenerator>
  void generate_successors(const solution &s,
			   const dep &d,
			   SolutionGenerator &generator,
			   std::set<package> *visited_packages)
  {
    version source = d.get_source();

    if(visited_packages != NULL)
      visited_packages->insert(source.get_package());

    // Check whether the source of the dependency was touched; if it
    // was, we can't modify it.
    version source_chosen;
    bool source_was_modified = s.get_choices().get_version_of(source.get_package(), source_chosen);

    // The tier and forcing reasons are updated until the tier is less
    // than or equal to the current tier (since at that point there's
    // no more information to be gleaned from them).

    // This is the least tier generated by installing any solver for
    // the dependency.
    tier succ_tier = maximum_tier;
    // The set of choices that explain the promotion that we're
    // calculating.
    choice_set forcing_reasons;

    // Try moving the source, if it is legal to do so.
    //
    // The source is not moved for soft deps: these model Recommends,
    // and experience has shown that users find it disturbing to have
    // packages upgraded or removed due to their recommendations.
    if(!d.is_soft())
      {
	if(source_was_modified)
	  forcing_reasons.insert_or_narrow(choice::make_install_version(source, dep(), -1));
	else
	  {
	    eassert_on_2objs_soln(source == initial_state.version_of(source.get_package()),
				  source,
				  initial_state.version_of(source.get_package()),
				  s);

	    for(typename package::version_iterator vi = source.get_package().versions_begin();
		!vi.end(); ++vi)
	      if(*vi != source)
		{
		  promotion p;
		  generate_single_successor(s, d, *vi, true, p, generator,
					    visited_packages);

		  if(s.get_tier() < succ_tier)
		    {
		      forcing_reasons.insert_or_narrow(p.get_choices());
		      if(p.get_tier() < succ_tier)
			succ_tier = p.get_tier();
		    }
		}
	  }
      }

    // Now try installing each target of the dependency.
    for(typename dep::solver_iterator si = d.solvers_begin();
	!si.end(); ++si)
      {
	if(visited_packages != NULL)
	  visited_packages->insert((*si).get_package());

	promotion p;
	generate_single_successor(s, d, *si, false, p, generator,
				  visited_packages);


	if(s.get_tier() < succ_tier)
	  {
	    forcing_reasons.insert_or_narrow(p.get_choices());
	    if(p.get_tier() < succ_tier)
	      succ_tier = p.get_tier();
	  }
      }

    // Finally, maybe we can leave this dependency unresolved.
    //
    // \todo Add a way to set tiers of broken soft deps.  (maybe for
    // now just have one tier that is always set for breaking them?)
    if(d.is_soft())
      {
	// Check whether adding this unresolved dep triggers a
	// promotion.
	choice_set choices(s.get_choices());
	choice break_d(choice::make_break_soft_dep(d, -1));
	choices.insert_or_narrow(break_d);
	typename promotion_set::const_iterator found =
	  promotions.find_highest_promotion_containing(choices, break_d);

	tier unresolved_tier = s.get_tier();
	if(found != promotions.end() && unresolved_tier < found->get_tier())
	  {
	    if(s.get_tier() < succ_tier)
	      forcing_reasons.insert_or_narrow(found->get_choices());
	    unresolved_tier = found->get_tier();
	    LOG_TRACE(logger, "Breaking " << d << " triggers a promotion to tier " << unresolved_tier);
	  }
	LOG_TRACE(logger, "Trying to leave " << d << " unresolved");
	generator.make_successor(s,
				 break_d,
				 unresolved_tier,
				 universe, weights);

        if(s.get_tier() < succ_tier)
	  {
	    if(unresolved_tier < succ_tier)
	      succ_tier = unresolved_tier;
	  }
      }

    // Now, if the minimum tier of any successor is still above the
    // previous solution's tier, we can promote solutions using the
    // set of forced reasons that we've been accumulating.
    if(s.get_tier() < succ_tier)
      {
	promotion p(forcing_reasons, succ_tier);
	LOG_DEBUG(logger, "Inserting new promotion: " << p);
	promotions.insert(p);
      }
  }

  /** Processes the given solution by enqueuing its successor nodes
   *  (if any are available).
   */
  void process_solution(int step_num,
			std::set<package> *visited_packages)
  {
    // Any forcings are immediately applied to 'curr', so that
    // forcings are performed ASAP.
    int curr_step_num = step_num;
    solution curr = steps[curr_step_num].sol;

    eassert_on_soln(!curr.get_broken().empty(), curr);

    // Set to \b true if this search node is untenable.
    bool dead_end = false;

    int num_least_successors = -1;
    dep least_successors;

    // This loop attempts to generate all the successors of a
    // solution.  However, if a "forced" dependency arises, it
    // re-verifies all the dependencies of the solution.
    bool done = false;
    while(!done)
      {
	LOG_DEBUG(logger, "Processing step " << curr_step_num << ": " << curr);

	done = true;

	// Remember the solution whose broken dependencies we're
	// iterating over.
	solution starting_solution = curr;

	for(typename imm::set<dep>::const_iterator bi=starting_solution.get_broken().begin();
	    bi!=starting_solution.get_broken().end(); ++bi)

	  {
	    if(visited_packages != NULL)
	      {
		for(typename dep::solver_iterator si = (*bi).solvers_begin();
		    !si.end(); ++si)
		  visited_packages->insert((*si).get_package());

		visited_packages->insert((*bi).get_source().get_package());
	      }

	    // Check for the case where this dependency has been
	    // fortuitously solved by forcing another broken
	    // dependency.
	    if(starting_solution != curr && !(*bi).broken_under(curr))
	      continue;

	    // Assert against impossible conditions (if this happens
	    // something is broken elsewhere).
	    if(starting_solution == curr && !(*bi).broken_under(curr))
	      {
		std::ostringstream msg;

		msg << "In solution ";
		curr.dump(msg);
		msg << ":" << std::endl;

		msg << "Unexpectedly non-broken dependency "
		    << *bi << "!";

		version source = (*bi).get_source();

		if(curr.version_of(source.get_package()) != source)
		  msg << "  (" << source.get_package().get_name()
		      << " " << source.get_name()
		      << " is not installed)";

		for(typename dep::solver_iterator si = (*bi).solvers_begin();
		    !si.end(); ++si)
		  if(curr.version_of((*si).get_package()) == *si)
		    msg << "  (" << (*si).get_package().get_name()
			<< " " << (*si).get_name()
			<< " is installed)";

		std::string msgstr;
		msg.str(msgstr);

		LOG_FATAL(logger, msgstr);

		throw ResolverInternalErrorException(msgstr);
	      }


	    int num_successors = 0;
	    {
	      null_generator g(num_successors);
	      generate_successors(curr, *bi, g,
				  visited_packages);
	    }

	    if(num_least_successors == -1 ||
	       num_successors < num_least_successors)
	      {
		num_least_successors = num_successors;
		least_successors = *bi;
	      }

	    if(num_successors == 0)
	      dead_end = true;
	    else if(num_successors == 1)
	      {
		LOG_TRACE(logger, "Forced resolution (step " << curr_step_num << ") of " << *bi);

		real_generator g(curr_step_num, steps, visited_packages, logger);
		generate_successors(curr, *bi,
				    g, visited_packages);

		curr_step_num = steps[curr_step_num].first_child;
		eassert_on_dep(curr_step_num != -1 &&
			       steps[curr_step_num].is_last_child,
			       curr, *bi);

		curr = steps[curr_step_num].sol;
		done = false;

		num_least_successors = -1;
	      }
	  }
      }

    // Discard this node if there is no solution to at least one
    // dependency.
    if(dead_end)
      return;

    // In the course of forcing dependencies, we might have actually
    // arrived at a solution, in which case we should just enqueue it
    // and stop (a full solution has no successors to generate).
    if(curr.is_full_solution())
      {
	try_enqueue(curr_step_num);
	return;
      }

    // Should never happen unless we have no broken dependencies.
    if(num_least_successors != -1)
      {
	LOG_TRACE(logger,
		  "Generating successors for step " << curr_step_num << " and dep "
		  << least_successors);

	real_generator g(curr_step_num, steps, visited_packages, logger);
	generate_successors(curr, least_successors, g,
			    visited_packages);
	try_enqueue_children(curr_step_num);
      }
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
     initial_state(_initial_state, _universe.get_package_count()),
     weights(_step_score, _broken_score, _unfixed_soft_score,
	     _full_solution_score, _universe.get_version_count(),
	     initial_state),
     minimum_score(-infinity),
     future_horizon(_future_horizon),
     universe(_universe), finished(false), deferred_dirty(false),
     remove_stupid(true),
     solver_executing(false), solver_cancelled(false),
     open(step_goodness_compare(steps)),
     minimum_search_tier(minimum_tier),
     maximum_search_tier(minimum_tier),
     future_solutions(step_goodness_compare(steps)),
     closed(solution_contents_compare()),
     deferred_future_solutions(step_contents_compare(steps)),
     promotions(_universe),
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
      version_tiers[i] = minimum_tier;

    // Used for sanity-checking below; create this only once for
    // efficiency's sake.
    solution empty_solution(solution::root_node(initial_broken,
						universe,
						weights,
						initial_state,
						minimum_tier));
    // Find all the broken deps.
    for(typename PackageUniverse::dep_iterator di = universe.deps_begin();
	!di.end(); ++di)
      {
	dep d(*di);

	if(d.broken_under(initial_state))
	  {
	    eassert_on_dep(d.broken_under(empty_solution), empty_solution, d);

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
	logger->setLevel(NULL);
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
    open.clear();
    closed.clear();

    for(size_t i=0; i<universe.get_version_count(); ++i)
      weights.version_scores[i]=0;
  }

  /** \return \b true if no solutions have been examined yet.
   *  This implies that it is safe to modify package scores.
   */
  bool fresh()
  {
    if(deferred_dirty)
      reexamine_deferred();

    return open.empty() && !finished;
  }

  /** \return \b true if the open queue is empty. */
  bool exhausted()
  {
    if(deferred_dirty)
      reexamine_deferred();

    return open.empty() && finished;
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
    promotions.insert(promotion(choices, promotion_tier));
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
    std::pair<typename std::set<version>::const_iterator, bool>
      insert_result = user_rejected.insert(ver);

    if(insert_result.second)
      {
	if(undo != NULL)
	  undo->add_item(new undo_resolver_manipulation<PackageUniverse, version>(this, ver, &generic_problem_resolver<PackageUniverse>::unreject_version));

	unmandate_version(ver, undo);
      }
  }

  /** Cancel any rejection of ver, allowing the resolver to once
   *  again generate solutions containing it.
   */
  void unreject_version(const version &ver, undo_group *undo = NULL)
  {
    typename std::set<version>::size_type
      erased_count = user_rejected.erase(ver);

    if(erased_count > 0)
      {
	if(undo != NULL)
	  undo->add_item(new undo_resolver_manipulation<PackageUniverse, version>(this, ver, &generic_problem_resolver<PackageUniverse>::reject_version));

	deferred_dirty = true;
      }
  }

  void mandate_version(const version &ver, undo_group *undo = NULL)
  {
    std::pair<typename std::set<version>::const_iterator, bool>
      insert_result = user_mandated.insert(ver);

    if(insert_result.second)
      {
	if(undo != NULL)
	  undo->add_item(new undo_resolver_manipulation<PackageUniverse, version>(this, ver, &generic_problem_resolver<PackageUniverse>::unmandate_version));

	unreject_version(ver, undo);
      }
  }

  void unmandate_version(const version &ver, undo_group *undo = NULL)
  {
    typename std::set<version>::size_type
      erased_count = user_mandated.erase(ver);

    if(erased_count > 0)
      {
	if(undo != NULL)
	  undo->add_item(new undo_resolver_manipulation<PackageUniverse, version>(this, ver, &generic_problem_resolver<PackageUniverse>::mandate_version));

	deferred_dirty = true;
      }
  }

  /** Query whether the given version is rejected. */
  bool is_rejected(const version &ver) const
  {
    return user_rejected.find(ver) != user_rejected.end();
  }

  /** Query whether the given version is mandated. */
  bool is_mandatory(const version &ver) const
  {
    return user_mandated.find(ver) != user_mandated.end();
  }

  /** Query whether the given dependency is hardened. */
  bool is_hardened(const dep &d) const
  {
    return user_hardened.find(d) != user_hardened.end();
  }

  /** Harden the given dependency. */
  void harden(const dep &d, undo_group *undo = NULL)
  {
    eassert(d.is_soft());

    std::pair<typename std::set<dep>::const_iterator, bool>
      insert_result = user_hardened.insert(d);

    if(insert_result.second)
      {
	if(undo != NULL)
	  undo->add_item(new undo_resolver_manipulation<PackageUniverse, dep>(this, d, &generic_problem_resolver<PackageUniverse>::unharden));

	unapprove_break(d, undo);
      }
  }

  /** Un-harden (soften?) the given dependency. */
  void unharden(const dep &d, undo_group *undo = NULL)
  {
    typename std::set<dep>::size_type
      erased_count = user_hardened.erase(d);

    if(erased_count > 0)
      {
	if(undo != NULL)
	  undo->add_item(new undo_resolver_manipulation<PackageUniverse, dep>(this, d, &generic_problem_resolver<PackageUniverse>::harden));

	deferred_dirty = true;
      }
  }

  /** \return \b true if the given dependency is in the
   *  approved-broken state.
   */
  bool is_approved_broken(const dep &d) const
  {
    return user_approved_broken.find(d) != user_approved_broken.end();
  }

  /** Approve the breaking of the given dependency. */
  void approve_break(const dep &d, undo_group *undo = NULL)
  {
    std::pair<typename std::set<dep>::const_iterator, bool>
      insert_result = user_approved_broken.insert(d);

    if(insert_result.second)
      {
	if(undo != NULL)
	  undo->add_item(new undo_resolver_manipulation<PackageUniverse, dep>(this, d, &generic_problem_resolver<PackageUniverse>::unapprove_break));

	unharden(d, undo);
      }
  }

  /** Cancel the required breaking of the given dependency. */
  void unapprove_break(const dep &d, undo_group *undo = NULL)
  {
    typename std::set<dep>::size_type erased_count
      = user_approved_broken.erase(d);

    if(erased_count > 0)
      {
	if(undo != NULL)
	  undo->add_item(new undo_resolver_manipulation<PackageUniverse, dep>(this, d, &generic_problem_resolver<PackageUniverse>::approve_break));

	deferred_dirty = true;
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

  size_t get_num_deferred()
  {
    // \todo Track this more efficiently (we just need to wrap up the
    // deferred map and keep a counter in the encapsulating class).
    size_t rval = 0;
    for(typename std::map<tier, std::set<int, step_contents_compare> >::const_iterator
	  it = deferred.begin(); it != deferred.end(); ++it)
      rval += it->second.size();

    return rval;
  }

  /** Update the cached queue sizes. */
  void update_counts_cache()
  {
    cwidget::threads::mutex::lock l(counts_mutex);
    counts.open       = open.size();
    counts.closed     = closed.size();
    counts.deferred   = get_num_deferred();
    counts.conflicts  = promotions.tier_size_above(conflict_tier);
    counts.promotions = promotions.size() - counts.conflicts;
    counts.finished   = finished;
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
	if(deferred_dirty)
	  reexamine_deferred();
	update_counts_cache();
      }
  }

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

    if(deferred_dirty)
      reexamine_deferred();

    if(finished)
      throw NoMoreSolutions();

    // If the open queue is empty, then we're between searches and
    // should enqueue a new root node.
    if(open.empty())
      {
	closed.clear();

	steps.push_back(step(solution::root_node(initial_broken,
						 universe,
						 weights,
						 initial_state,
						 minimum_tier)));
	open.push(0);
      }

    // Here the outer loop is used to ensure that we don't get
    // confused by supposedly existing solutions that are actually
    // deferred.  We don't check the solution list until we've already
    // found something, but if all the solutions on it are deferred,
    // we need to keep searching until max_steps is exhausted.
    while(max_steps > 0 && !open.empty())
      {
    while(max_steps > 0 && most_future_solution_steps <= future_horizon && !open.empty())
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


	int curr_step_num = open.top();
	solution s = steps[curr_step_num].sol;
	open.pop();

	++odometer;

	// Check whether the solution has been promoted since it was
	// inserted into the open queue.  If it has, defer it until we
	// get to the appropriate tier.
	const tier &s_tier = get_solution_tier(s);
	if(s_tier >= conflict_tier)
	  {
	    LOG_DEBUG(logger, "Dropping conflicted solution " << s);
	    continue;
	  }
	else if(s_tier >= already_generated_tier)
	  {
	    LOG_DEBUG(logger, "Dropping already generated solution " << s);
	    continue;
	  }
	else if(irrelevant(s))
	  {
	    LOG_DEBUG(logger, "Dropping irrelevant solution " << s);
	    continue;
	  }
	else if(maximum_search_tier < s_tier)
	  {
	    LOG_DEBUG(logger, "Deferring solution " << s << " to tier " << s_tier
		      << ": it is beyond the current horizon of " << maximum_search_tier);
	    defer_step(s_tier, curr_step_num);
	    continue;
	  }
	else if(breaks_user_constraint(s))
	  {
	    LOG_DEBUG(logger, "Deferring rejected solution " << s);

	    defer_step(defer_tier, curr_step_num);
	    continue;
	  }

	closed.insert(s);

	// If all dependencies are satisfied, we found a solution.
	if(s.is_full_solution())
	  {
	    LOG_INFO(logger, " --- Found solution " << s);

	    // Remember this solution, so we don't try to return it
	    // again in the future.  (note that we remove
	    // from-dep-source information so that we match any
	    // solution with the same version content)
	    promotions.insert(promotion(get_solution_choices_without_dep_info(s),
					already_generated_tier));

	    LOG_INFO(logger, " *** Converged after " << odometer << " steps.");

	    LOG_INFO(logger, " *** open: " << open.size()
		     << "; closed: " << closed.size()
		     << "; promotions: " << promotions.size()
		     << "; deferred: " << get_num_deferred());

	    future_solutions.push(curr_step_num);
	  }
	// Nope, let's go enqueue successor nodes.
	else
	  process_solution(curr_step_num, visited_packages);

	// Keep track of the "future horizon".  If we haven't found a
	// solution, we aren't using those steps up at all.
	// Otherwise, we count up until we hit 50.
	if(!future_solutions.empty())
	  ++most_future_solution_steps;

	LOG_TRACE(logger, "Done generating successors.");

	--max_steps;

	while(open.empty() && !deferred.empty() &&
	      deferred.begin()->first < defer_tier &&
	      deferred.begin()->first < already_generated_tier &&
	      deferred.begin()->first < conflict_tier)
	  {
	    const typename std::map<tier, std::set<int, step_contents_compare> >::iterator
	      deferred_begin = deferred.begin();
	    const std::pair<tier, std::set<int, step_contents_compare> > &first_pair = *deferred_begin;
	    const tier &first_tier = first_pair.first;
	    const std::set<int, step_contents_compare> &first_solutions = first_pair.second;

	    LOG_TRACE(logger, "Advancing to tier " << first_tier
		      << " and inserting " << first_solutions.size()
		      << " entries into the open list.");

	    maximum_search_tier = first_tier;

	    for(typename std::set<int, step_contents_compare>::const_iterator
		  it = first_solutions.begin(); it != first_solutions.end(); ++it)
	      // We can probably just do .push(), but doing this will
	      // avoid any nasty surprises due to not checking what
	      // try_enqueue usually checks.
	      try_enqueue(*it);

	    deferred.erase(deferred_begin);
	  }
      }

    if(!future_solutions.empty())
      {
	solution rval;

	while(!future_solutions.empty() && !rval.valid())
	  {
	    int tmp_step = future_solutions.top();
	    solution tmp = steps[tmp_step].sol;
	    future_solutions.pop();

	    if(breaks_user_constraint(tmp))
	      {
		LOG_TRACE(logger, "Deferring the future solution " << tmp);
		deferred_future_solutions.insert(tmp_step);
	      }
	    else
	      rval = tmp;
	  }

	if(rval.valid())
	  {
	    if(open.empty() && future_solutions.empty())
	      finished = true;

	    LOG_DEBUG(logger, "--- Returning the future solution " << rval);

	    return rval;
	  }
	else
	  most_future_solution_steps = 0;
      }
      }

    // Oh no, we either ran out of solutions or ran out of steps.

    if(max_steps==0)
      throw NoMoreTime();

    eassert(open.empty());

    finished=true;

    update_counts_cache();

    LOG_INFO(logger, " *** Out of solutions after " << odometer << " steps.");
    LOG_INFO(logger ,
	     " *** open: " << open.size()
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

// Appease the C++ gods by writing dummy definitions of these things.
// g++ happily accepts definitions of static constants inside a class,
// but they aren't actually defined unless you define them outside the
// class and don't give a definition in the definition, so without
// these dummy lines, the linker will complain that there are
// undefined references to the constants defined above because their
// definitions aren't really definitions.  Hopefully that's all
// obvious.
template<typename PackageUniverse>
const int generic_problem_resolver<PackageUniverse>::maximum_tier_num;
template<typename PackageUniverse>
const int generic_problem_resolver<PackageUniverse>::conflict_tier_num;
template<typename PackageUniverse>
const int generic_problem_resolver<PackageUniverse>::already_generated_tier_num;
template<typename PackageUniverse>
const int generic_problem_resolver<PackageUniverse>::defer_tier_num;
template<typename PackageUniverse>
const int generic_problem_resolver<PackageUniverse>::minimum_tier_num;

template<typename PackageUniverse>
const typename PackageUniverse::tier generic_problem_resolver<PackageUniverse>::maximum_tier(maximum_tier_num);

template<typename PackageUniverse>
const typename PackageUniverse::tier generic_problem_resolver<PackageUniverse>::conflict_tier(conflict_tier_num);

template<typename PackageUniverse>
const typename PackageUniverse::tier generic_problem_resolver<PackageUniverse>::already_generated_tier(already_generated_tier_num);

template<typename PackageUniverse>
const typename PackageUniverse::tier generic_problem_resolver<PackageUniverse>::defer_tier(defer_tier_num);

template<typename PackageUniverse>
const typename PackageUniverse::tier generic_problem_resolver<PackageUniverse>::minimum_tier(minimum_tier_num);

#endif
