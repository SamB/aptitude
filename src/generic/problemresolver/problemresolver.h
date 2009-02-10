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
#include <map>
#include <queue>
#include <set>
#include <vector>

#include <iostream>
#include <sstream>

#include "dump_universe.h"
#include "exceptions.h"
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

  typedef generic_solution<PackageUniverse> solution;

  typedef typename solution::action action;

  /** Information about the sizes of the various resolver queues. */
  struct queue_counts
  {
    size_t open;
    size_t closed;
    size_t deferred;
    size_t conflicts;

    /** \b true if the resolver has finished searching for solutions.
     *  If open is empty, this member distinguishes between the start
     *  and the end of a search.
     */
    bool finished;

    queue_counts()
      : open(0), closed(0), deferred(0), conflicts(0), finished(false)
    {
    }
  };

private:
  log4cxx::LoggerPtr logger;
  log4cxx::AppenderPtr appender; // Used for the "default" appending behavior.

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

  /** Compares solutions according to their "goodness". */
  struct solution_goodness_compare
  {
    bool operator()(const solution &s1, const solution &s2) const
    {
      return s1.get_score() < s2.get_score();
    }
  };

  /** Compares solutions according to their contents; used to
   *  manage "closed".
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

      const imm::map<package,action>
	&a1=s1.get_actions(), &a2=s2.get_actions();
      const imm::set<dep>
	&us1=s1.get_unresolved_soft_deps(), &us2=s2.get_unresolved_soft_deps();


      // Speed hack: order by size first to avoid traversing the whole
      // tree.
      if(a1.size() < a2.size())
	return true;
      else if(a2.size() < a1.size())
	return false;
      else if(us1.size() < us2.size())
	return true;
      else if(us2.size() < us1.size())
	return false;
      else
	return a1 < a2 || (a2 == a1 && us1 < us2);
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



  /** The working queue: */
  std::priority_queue<solution, std::vector<solution>, solution_goodness_compare> open;

  /** Solutions generated "in the future".
   *
   *  The main reason this is persistent at the moment is so we don't
   *  lose solutions if find_next_solution() throws an exception.
   */
  std::priority_queue<solution, std::vector<solution>, solution_goodness_compare> future_solutions;

  /** Stores already-seen solutions: */
  std::set<solution, solution_contents_compare> closed;

  /** Stores solutions that were ignored because of user constraints
   *  (but could be reinstated later).  Disjoint with closed.
   */
  std::set<solution, solution_contents_compare> deferred;

  typedef dense_mapset<package, action, ExtractPackageId> conflictset;


  /** Stores conflicts: sets of installations that have been
   *  determined to be mutually incompatible.
   */
  conflictset conflicts;

  /** The initial set of broken dependencies.  Kept here for use in
   *  the stupid-elimination algorithm.
   */
  imm::set<dep> initial_broken;

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

  /** Stores solutions that were already generated and that have
   *  broken soft dependencies.
   *
   *  \todo Merge this with conflicts so we can accumulate information
   *  about it?  Crazy idea: can we model breaking a soft dependency
   *  as a special package version with a very low weight in the
   *  model, and use that to unify the two?  Think about that.
   */
  std::vector<solution> generated_solutions;


  typedef std::set<std::pair<version, version> > stupid_table;

  std::ostream &dump_conflict(std::ostream &out, const imm::map<package, action> &conflict) const;
  std::ostream &dump_conflict(std::ostream &out, const action &act) const;


  /** \param conflictorpair a (package, action) pair contained in a conflict.
   *  \param apair a (package, action) pair from a solution or a conflict.
   *
   *  \return \b true if the given conflictor matches a.  This relation
   *          is transitive, so if c1 matches c2 and c2 matches a,
   *          then c1 matches a.
   */
  static bool conflictor_matches(const std::pair<package, action> &conflictorpair,
				 const std::pair<package, action> &apair)
  {
    const action &conflictor = conflictorpair.second;
    const action &a = apair.second;

    if(a.ver != conflictor.ver)
      return false;

    if(!conflictor.from_dep_source)
      return true;
    else
      return a.from_dep_source && a.d == conflictor.d;
  }

  /** \return \b true if each element of pc2 is matched by an element
   *  in pc1.
   */
  static bool conflict_matches(const typename imm::map<package, action> &c,
			       const typename imm::map<package, action> &acts)
  {
    typename imm::map<package, action>::const_iterator ci = c.begin();
    typename imm::map<package, action>::const_iterator ai = acts.begin();

    while(ci != c.end() &&
	  ai != acts.end())
      {
	if(ai->first < ci->first)
	  ++ai;
	else if(ci->first < ai->first)
	  return false;
	else if(!(conflictor_matches(ci->second, ai->second)))
	  return false;
	else
	  {
	    ++ci;
	    ++ai;
	  }
      }

    return (ci == c.end());
  }

  /** Test whether the given partial conflict subsumes an existing
   *  conflict.
   *
   *  \param m the conflict or action to match
   *
   *  \return a conflict matched by m, or conflicts.end() if no such
   *  conflict exists.
   */
  typename conflictset::const_iterator
  find_matching_conflict(const imm::map<package, action> &m) const
  {
    return conflicts.find_submap(m, &conflictor_matches);
  }

  /** Test whether the given partial conflict subsumes an existing
   *  conflict.
   *
   *  \param m the conflict or action to match
   *  \param actpair a single element that should be present in the
   *                 returned conflict.  For instance, if you have a
   *                 solution that you know is conflict-free and you
   *                 perform a single action, passing this action
   *                 as actpair may speed up the search for a
   *                 conflict.
   *
   *  \return a conflict matched by m, or conflicts.end() if no such
   *  conflict exists.
   */
  typename conflictset::const_iterator
  find_matching_conflict(const imm::map<package, action> &m,
			 const std::pair<package, action> &actpair) const
  {
    return conflicts.find_submap_containing(m, actpair, &conflictor_matches);
  }

  /** Test whether the given solution contains a conflict. */
  bool contains_conflict(const solution &s) const
  {
    typename conflictset::const_iterator
      found = find_matching_conflict(s.get_actions());
    bool rval = (found != conflicts.end());

    if(rval)
      LOG_TRACE(logger,
		"The conflict "
		<< *found
		<< " is triggered by the solution "
		<< s);

    return rval;
  }

#if 0
  /** Test whether the given solution contains a conflict when the
   *  given action is taken.
   *
   *  \return such a conflict if it exists; otherwise conflicts.end().
   */
  typename std::set<std::map<package, act_conflict> >::const_iterator
  will_conflict(const solution &s,
		const act_conflict &a) const
  {
    // For now I'm being lazy and actually generating a full mapping.
    // However, this could be done much more efficiently if optimizing
    // this code is important.
    std::map<package, act_conflict> m;

    populate_partial_conflict(s, m);

    package p = a.ver.get_package();

    m[p] = a;

    typename std::set<std::map<package, act_conflict> >::const_iterator
      result = subsumes_any_conflict(m);

    if(result != conflicts.end())
      {
	if(a.from_dep_source)
	  LOG_TRACE(logger,
		    "Adding " << a.ver
		    << " due to " << a.d
		    << " will trigger conflict "
		    << *result);
	else
	  LOG_TRACE(logger,
		    "Adding " << a.ver
		    << " will trigger conflict "
		    << *result);
      }

    return result;
  }

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

  /** Calculate whether the solution is rejected based on
   *  user_rejected by testing whether the intersection of the
   *  solution domain and the rejected set is nonempty.
   */
  bool contains_rejected(const solution &s) const
  {
    for(typename std::set<version>::const_iterator uri
	  = user_rejected.begin(); uri != user_rejected.end(); ++uri)
      {
	typename imm::map<package, action>::node found = s.get_actions().lookup(uri->get_package());

	if(found.isValid() && found.getVal().second.ver == *uri)
	  {
	    LOG_TRACE(logger,
		      "Rejected version " << found.getVal().second.ver
		      << " detected.");

	    return true;
	  }
      }

    return false;
  }

  bool breaks_hardened(const solution &s) const
  {
    typename std::set<dep>::const_iterator uh_iter
      = user_hardened.begin();

    if(uh_iter == user_hardened.end())
      return false;

    typename imm::set<dep>::const_iterator su_iter
      = s.get_unresolved_soft_deps().begin();

    while(uh_iter != user_hardened.end() &&
	  su_iter != s.get_unresolved_soft_deps().end())
      {
	if(*uh_iter == *su_iter)
	  {
	    LOG_TRACE(logger,
		      "Broken hardened dependency " << *uh_iter
		      << " detected.");

	    return true;
	  }
	else if(*uh_iter < *su_iter)
	  ++uh_iter;
	else
	  ++su_iter;
      }

    return false;
  }

  bool solves_approved_broken(const solution &s) const
  {
    const imm::map<package, action> &actions = s.get_actions();

    for(typename imm::map<package, action>::const_iterator
	  ai = actions.begin(); ai != actions.end(); ++ai)
      if(user_approved_broken.find(ai->second.d) != user_approved_broken.end())
	return true;

    return false;
  }

  /** \return \b true if the given solution passed up an opportunity
   *          to include an 'mandated' version.
   */
  bool avoids_mandated(const solution &s) const
  {
    // NB: The current algorithm is not terribly efficient.
    for(typename std::set<version>::const_iterator ai = user_mandated.begin();
	ai != user_mandated.end(); ++ai)
      {
	version v = s.version_of(ai->get_package());
	// If it's already being installed, then we're fine.
	if(v == *ai)
	  continue;

	// Check (very slowly) whether we made a decision where we had
	// the opportunity to use this version (and of course didn't).
	for(typename imm::map<package, action>::const_iterator si = s.get_actions().begin();
	    si != s.get_actions().end(); ++si)
	  if(si->second.d.solved_by(*ai) &&
	     user_mandated.find(si->second.ver) == user_mandated.end())
	    {
	      LOG_TRACE(logger,
			*ai << " is avoided (when resolving "
			<< si->second.d << ") by the solution: " << s);

	      return true;
	    }

	// Check whether a soft dependency is being left unresolved
	// that this version could have satisfied and that's not
	// approved-broken.
	for(typename imm::set<dep>::const_iterator udi =
	      s.get_unresolved_soft_deps().begin();
	    udi != s.get_unresolved_soft_deps().end(); ++udi)
	  {
	    const dep ud(*udi);
	    const bool is_approved_broken =
	      user_approved_broken.find(ud) != user_approved_broken.end();
	    if(!is_approved_broken && ud.solved_by(*ai))
	      {
		LOG_TRACE(logger,
			  *ai << " is avoided (by leaving " << ud
			  << " unresolved) by the solution: " << s);

		return true;
	      }
	  }
      }

    return false;
  }

  /** \return \b true if the resolution of the given dependency might
   *                  be affected by a user constraint.
   */
  bool impinges_user_constraint(const dep &d) const
  {
    if(user_hardened.find(d) != user_hardened.end() ||
       user_approved_broken.find(d) != user_approved_broken.end())
      return true;

    for(typename dep::solver_iterator si = d.solvers_begin();
	!si.end(); ++si)
      {
	if(user_rejected.find(*si) != user_rejected.end())
	  return true;

	for(typename std::set<version>::iterator mi = user_mandated.begin();
	    mi != user_mandated.end(); ++mi)
	  {
	    if(mi->get_package() == (*si).get_package())
	      return true;
	  }
      }

    return false;
  }


  /** \return \b true if the given solution should be deferred. */
  bool should_defer(const solution &s) const
  {
    return contains_rejected(s) || breaks_hardened(s) ||
      avoids_mandated(s) || solves_approved_broken(s);
  }

  /** Place any solutions that were deferred and are no longer
   *  rejected back on the open queue.
   */
  void reexamine_deferred()
  {
    // NB: the STL guarantees that erasing elements from a set does
    // not invalidate existing iterators.  Hence this very careful
    // iteration:

    typename std::set<solution, solution_contents_compare>::const_iterator
      i = deferred.begin(), j = i;

    while(i != deferred.end())
      {
	++j;

	if(!should_defer(*i))
	  {
	    open.push(*i);
	    deferred.erase(i);
	  }

	i = j;
      }

    // Note that we might have to rescind the "finished" state: the
    // actions above can actually cause new solutions to be available
    // for processing!
    if(finished && !open.empty())
      finished = false;

    deferred_dirty = false;
  }

  /** \return \b true if the given solution is "irrelevant": that is,
   *  either it was already generated and placed in the closed queue,
   *  or it includes an already-generated solution as a proper subset.
   */
  bool irrelevant(const solution &s)
  {
    if(closed.find(s) != closed.end())
      return true;

    if(contains_conflict(s))
      return true;

    // The efficiency of this step hinges on the *assumption* that the
    // number of solutions that will be generated is small relative to
    // the number of potential solutions.
    for(typename std::vector<solution>::const_iterator i=generated_solutions.begin();
	i!=generated_solutions.end(); ++i)
      if(std::includes(s.get_actions().begin(), s.get_actions().end(),
		       i->get_actions().begin(), i->get_actions().end()) &&
	 std::includes(s.get_unresolved_soft_deps().begin(), s.get_unresolved_soft_deps().end(),
		       i->get_unresolved_soft_deps().begin(), i->get_unresolved_soft_deps().end()))
	return true;

    if(s.get_score() < minimum_score)
      {
	LOG_TRACE(logger, "Not generating solution (infinite badness " << s.get_score() << "<" << minimum_score << ")");
	return true;
      }

    return false;
  }

  /** Tries to enqueue the given package.
   *
   *  \return \b true if the solution was not irrelevant.
   */
  bool try_enqueue(const solution &s)
  {
    if(irrelevant(s))
      {
	LOG_TRACE(logger, "Dropping irrelevant solution " << s);

	return false;
      }
    else if(should_defer(s))
      {
	LOG_TRACE(logger, "Deferring rejected solution " << s);

	deferred.insert(s);
	return false;
      }
    else
      {
	LOG_TRACE(logger, "Enqueuing " << s);

	open.push(s);

	return true;
      }
  }

  /** Try to enqueue the given collection of packages. */
  void try_enqueue(const std::vector<solution> &sols)
  {
    for(typename std::vector<solution>::const_iterator
	  succi = sols.begin();	succi != sols.end(); ++succi)
      try_enqueue(*succi);
  }

  /** Internal routine to check for the legality of a 'move' and
   *  generate a conflictor if it's not legal.
   */
  bool is_legal(const solution &s,
		const version &v,
		action &out_act) const
  {
    package p = v.get_package();
    version cur = initial_state.version_of(p);
    version inst = s.version_of(p);

    if(inst != cur)
      {
	LOG_TRACE(logger,
		  "Discarding " << v
		  << ": monotonicity violation");

	out_act.ver = inst;
	out_act.from_dep_source = false;
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
	    const dep &found_d = found.getVal().second;

	    LOG_TRACE(logger,
		      "Discarding " << v
		      << ": forbidden by the resolution of " << found_d);

	    out_act.ver = s.version_of(found_d.get_source().get_package());
	    out_act.d   = found_d;
	    out_act.from_dep_source = true;

	    return false;
	  }
      }
  }

  /** Internal routine to insert a new conflictor into a conflict set.
   *  Handles the case in which the conflictor subsumes an existing
   *  element of the set.
   */
  void insert_conflictor(imm::map<package, action> &conflict,
			 const action &act) const
  {
    package p = act.ver.get_package();
    typename imm::map<package, action>::node found
      = conflict.lookup(p);

    if(!found.isValid())
      conflict.put(p, act);
    else
      {
	const action &found_act = found.getVal().second;

	action a2 = act;

	eassert_on_2objs(found_act.ver == act.ver, found_act.ver, act.ver);
	if(a2.from_dep_source)
	  {
	    if(found_act.from_dep_source)
	      eassert_on_2objs(a2.d == found_act.d, a2.d, found_act.d);

	    else
	      a2.from_dep_source = false;
	  }
	conflict.put(p, a2);
      }
  }

  /** Generate a solution and push it onto an encapsulated vector of
   *  solutions.
   */
  class real_generator
  {
    std::vector<solution> &target;
    std::set<package> *visited_packages;
  public:
    real_generator(std::vector<solution> &_target,
		   std::set<package> *_visited_packages)
      :target(_target), visited_packages(_visited_packages)
    {
    }

    template<typename a_iter, class u_iter>
    void make_successor(const solution &s,
			const a_iter &abegin, const a_iter &aend,
			const u_iter &ubegin, const u_iter &uend,
			const PackageUniverse &universe,
			const solution_weights<PackageUniverse> &weights) const
    {
      target.push_back(solution::successor(s,
					   abegin, aend,
					   ubegin, uend,
					   universe, weights));

      // Touch all the packages that are involved in broken dependencies
      if(visited_packages != NULL)
	{
	  const solution &generated(target.back());

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

    template<typename a_iter, class u_iter>
    void make_successor(const solution &s,
			const a_iter &abegin, const a_iter &aend,
			const u_iter &ubegin, const u_iter &uend,
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

  /** Convenience routine for the below: try to generate a successor
   *  by installing a single package version.  NB: assumes that the
   *  solution's actions have dense identifiers (i.e., less than
   *  s.get_actions().size()).
   *
   *  \param s the solution for which a successor should be generated
   *  \param v the version to install
   *  \param d the dependency for which the successor is being generated.
   *  \param from_dep_source if \b true, this successor is the result
   *                         of an action on the source of a dependency
   *  \param conflict a map to which conflictors for this version, if
   *                  any, will be added.
   *
   *  \param generator an object supporting the make_successor() routine,
   *                   as real_generator and null_generator above.
   */
  template<typename SolutionGenerator>
  void generate_single_successor(const solution &s,
				 const dep &d,
				 const version &v,
				 bool from_dep_source,
				 imm::map<package, action> &conflict,
				 const SolutionGenerator &generator,
				 std::set<package> *visited_packages) const
  {
    if(visited_packages != NULL)
      visited_packages->insert(v.get_package());

    action conflictor;

    LOG_TRACE(logger,
	      "Trying to resolve " << d << " by installing "
	      << v.get_package().get_name() << " "
	      << v.get_name()
	      << (from_dep_source ? " from the dependency source" : ""));

    const int newid = s.get_actions().size();

    if(!is_legal(s, v, conflictor))
      insert_conflictor(conflict, conflictor);
    else
      {
	action act(v, d, from_dep_source, newid);

	// Speculatively form the new set of actions; doing this
	// rather than forming the whole solution allows us to avoid
	// several rather expensive steps in the successor routine
	// (most notably the formation of the new broken packages
	// set).
	imm::map<package, action> new_acts = s.get_actions();
	new_acts.put(v.get_package(), act);

	typename conflictset::const_iterator
	  found = find_matching_conflict(new_acts,
					 std::pair<package, action>(v.get_package(), act));

	if(found == conflicts.end())
	  generator.make_successor(s, &act, &act+1,
				   (dep *) 0, (dep *) 0,
				   universe, weights);
	else
	  {
	    LOG_TRACE(logger,
		      "Discarding " << v << " due to conflict "
		      << *found);

	    imm::map<package, action> m = *found;

	    for(typename imm::map<package, action>::const_iterator ci
		  = m.begin(); ci != m.end(); ++ci)
	      {
		// Discard the version that we were trying to install,
		// so that the caller can use this to form a more
		// general conflict if all its resolutions fail.
		if(ci->first != v.get_package())
		  insert_conflictor(conflict, ci->second);
		else
		  eassert_on_2objs_soln(ci->second.ver == v,
					ci->second.ver,
					v,
					s);
	      }
	  }
      }
  }

  /** Build the successors of a solution node for a particular
   *  dependency.
   *
   *  \param conflict a map which will be initialized with a conflict
   *  explaining the non-appearance of some solvers (if there are no
   *  solvers, this will be a full conflict explaining the lack of
   *  solvers).
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
			   imm::map<package, action> &conflict,
			   const SolutionGenerator &generator,
			   std::set<package> *visited_packages) const
  {
    version source = d.get_source();

    if(visited_packages != NULL)
      visited_packages->insert(source.get_package());

    typename imm::map<package, action>::node
      source_found = s.get_actions().lookup(source.get_package());

    // Try moving the source, if it is legal to do so.
    //
    // The source is not moved for soft deps: these model Recommends,
    // and experience has shown that users find it disturbing to have
    // packages upgraded or removed due to their recommendations.
    //
    // Note that a conflictor is never generated for the source if the
    // dep is soft, since there's no logical dependency in that case
    // between the source being in the working solution and the
    // dependency being unsatisfiable.
    if(!d.is_soft())
      {
	if(source_found.isValid())
	  insert_conflictor(conflict, action(source, d, false, -1));
	else
	  {
	    eassert_on_2objs_soln(source == initial_state.version_of(source.get_package()),
				  source,
				  initial_state.version_of(source.get_package()),
				  s);

	    for(typename package::version_iterator vi = source.get_package().versions_begin();
		!vi.end(); ++vi)
	      if(*vi != source)
		generate_single_successor(s, d, *vi, true, conflict, generator,
					  visited_packages);
	  }
      }

    // Now try installing each target of the dependency.
    for(typename dep::solver_iterator si = d.solvers_begin();
	!si.end(); ++si)
      {
	if(visited_packages != NULL)
	  visited_packages->insert((*si).get_package());
	generate_single_successor(s, d, *si, false, conflict, generator,
				  visited_packages);
      }

    // Finally, maybe we can leave this dependency unresolved.
    if(d.is_soft())
      generator.make_successor(s, (action *) 0, (action *) 0,
			       &d, &d+1, universe, weights);
  }

  /** Processes the given solution by enqueuing its successor nodes
   *  (if any are available).  Note that for clarity, we now generate
   *  *all* successors before examining *any*.
   */
  void process_solution(const solution &s,
			std::set<package> *visited_packages)
  {
    // Any forcings are immediately applied to 'curr', so that
    // forcings are performed ASAP.
    solution curr = s;

    eassert_on_soln(!s.get_broken().empty(), s);

    // Set to \b true if this search node is untenable.
    bool dead_end = false;

    int num_least_successors_user_impinging = -1;
    dep least_successors_user_impinging;
    int num_least_successors = -1;
    dep least_successors;

    // This loop attempts to generate all the successors of a
    // solution.  However, if a "forced" dependency arises, it
    // re-verifies all the dependencies of the solution.
    bool done = false;
    while(!done)
      {
	LOG_DEBUG(logger, "Processing " << curr);

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

	    imm::map<package, action> conflict;


	    int num_successors = 0;
	    generate_successors(curr, *bi, conflict,
				null_generator(num_successors),
				visited_packages);

	    if(impinges_user_constraint(*bi))
	      {
		if(num_least_successors_user_impinging == -1 ||
		   num_successors < num_least_successors_user_impinging)
		  {
		    num_least_successors_user_impinging = num_successors;
		    least_successors_user_impinging = *bi;
		  }
	      }

	    if(num_least_successors == -1 ||
	       num_successors < num_least_successors)
	      {
		num_least_successors = num_successors;
		least_successors = *bi;
	      }


	    if(num_successors == 0)
	      {
		LOG_DEBUG(logger,
			  "Discarding solution; unresolvable dependency "
			  << *bi << " with conflict "
			  << conflict);

		add_conflict(conflict);

		dead_end = true;
	      }
	    else if(num_successors == 1)
	      {
		LOG_TRACE(logger, "Forced resolution of " << *bi);

		std::vector<solution> v;
		real_generator g(v, visited_packages);
		// NB: this may do redundant work adding to 'conflict'.
		// Use a generator object to avoid that?
		generate_successors(curr, *bi, conflict,
				    g, visited_packages);

		eassert_on_dep(v.size() == 1, s, *bi);

		curr = v.back();
		done = false;

		num_least_successors_user_impinging = -1;
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
	try_enqueue(curr);
	return;
      }

    unsigned int nsols = 0;

    // First try to enqueue stuff related to a dependency that the
    // user constrained; then just go for a free-for-all.
    if(num_least_successors_user_impinging != -1)
      {
	imm::map<package, action> conflict;

	LOG_TRACE(logger,
		  "Generating successors for "
		  << least_successors_user_impinging);

	std::vector<solution> v;
	generate_successors(curr, least_successors_user_impinging,
			    conflict, real_generator(v, visited_packages),
			    visited_packages);
	try_enqueue(v);
	nsols += v.size();
      }

    // Should never happen unless we have no broken dependencies.
    if(num_least_successors != -1)
      {
	imm::map<package, action> conflict;

	LOG_TRACE(logger,
		  "Generating successors for "
		  << least_successors);

	std::vector<solution> v;
	generate_successors(curr, least_successors,
			    conflict, real_generator(v, visited_packages),
			    visited_packages);
	try_enqueue(v);
	nsols += v.size();
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
    :logger(aptitude::Loggers::getAptitudeResolver()),
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
     conflicts(_universe.get_package_count())
  {
    // Find all the broken deps.
    for(typename PackageUniverse::dep_iterator di = universe.deps_begin();
	!di.end(); ++di)
      {
	dep d(*di);

	if(d.broken_under(initial_state))
	  {
	    solution empty_solution(solution::root_node(initial_broken,
							universe,
							weights,
							initial_state));
	    eassert_on_dep(d.broken_under(empty_solution), empty_solution, d);

	    initial_broken.insert(d);
	  }
      }
  }

  ~generic_problem_resolver()
  {
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

  /** Add a set of actions on packages to the set of 'conflicts'.  No
   *  solution containing these actions will be generated or
   *  contemplated.  This routine is public primarily to allow the
   *  frontend to discard the 'do-nothing' solution (eg, the aptitude
   *  resolver will ignore solutions that cancel all pending user
   *  actions).
   */
  void add_conflict(const imm::map<package, action> &conflict)
  {
    typename conflictset::const_iterator
      found = find_matching_conflict(conflict);

    if(found != conflicts.end())
      LOG_TRACE(logger,
		"Dropping conflict " << conflict
		<< " because it is redundant with " << *found);
    else
      {
	LOG_TRACE(logger, "Inserting conflict " << conflict);
	// TODO: drop conflicts of which this is a subset.  Needs work
	// at the setset level.
	conflicts.insert(conflict);
      }
  }

  /** Remove all dependency-related information from a set of actions;
   *  used to insert conflicts that shouldn't have dependency tags.
   */
  static imm::map<package, action>
  strip_dep_info(const imm::map<package, action> &actmap)
  {
    imm::map<package, action> rval;

    for(typename imm::map<package, action>::const_iterator i = actmap.begin();
	i != actmap.end(); ++i)
      rval.put(i->first, action(i->second.ver,
				dep(), false,
				i->second.id));

    return rval;
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

  /** Update the cached queue sizes. */
  void update_counts_cache()
  {
    cwidget::threads::mutex::lock l(counts_mutex);
    counts.open      = open.size();
    counts.closed    = closed.size();
    counts.deferred  = deferred.size();
    counts.conflicts = conflicts.size();
    counts.finished  = finished;
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

	open.push(solution::root_node(initial_broken,
				      universe,
				      weights,
				      initial_state));
      }

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


	solution s=open.top();
	open.pop();

	++odometer;

	if(irrelevant(s))
	  {
	    LOG_DEBUG(logger, "Dropping irrelevant solution " << s);
	    continue;
	  }

	if(should_defer(s))
	  {
	    LOG_DEBUG(logger, "Deferring rejected solution " << s);

	    deferred.insert(s);
	    continue;
	  }

	closed.insert(s);

	// If all dependencies are satisfied, we found a solution.
	if(s.is_full_solution())
	  {
	    LOG_INFO(logger, " --- Found solution " << s);

	    solution minimized = remove_stupid ? eliminate_stupid(s) : s;

	    // We only want to re-enqueue the solution if it changed
	    // in version content, meaning that it changed in *size*
	    // (recall that the new version content is always a subset
	    // of the old).  If we do this whenever the solution
	    // *changed*, we sometimes end up re-inserting something
	    // with the same version content and hence something
	    // that's already on the closed queue.
	    if(minimized.get_actions().size() != s.get_actions().size())
	      {
		// Make it fend for itself! (although I expect it to
		// come up again immediately; hrm)
		LOG_TRACE(logger, " --- Placing de-stupidified solution back on the open queue");
		finished = false;
		open.push(minimized);
	      }
	    else
	      {
		closed.insert(minimized);
		// In the cases where the solution can be completely
		// represented by a conflict, add one (so we get the
		// benefits of conflict tracking).
		//
		// \todo extend conflicts so they can represent
		// everything or extend everything so a conflict can
		// represent it.
		if(minimized.get_unresolved_soft_deps().empty())
		  add_conflict(strip_dep_info(minimized.get_actions()));
		else
		  generated_solutions.push_back(minimized);

		LOG_INFO(logger, " *** Converged after " << odometer << " steps.");
		LOG_INFO(logger, " *** open: " << open.size()
			 << "; closed: " << closed.size()
			 << "; conflicts: " << conflicts.size()
			 << "; deferred: " << deferred.size()
			 << "; generated solutions: " << generated_solutions.size());

		update_counts_cache();

		future_solutions.push(minimized);
	      }
	  }
	// Nope, let's go enqueue successor nodes.
	else
	  process_solution(s, visited_packages);

	// Keep track of the "future horizon".  If we haven't found a
	// solution, we aren't using those steps up at all.
	// Otherwise, we count up until we hit 50.
	if(!future_solutions.empty())
	  ++most_future_solution_steps;

	LOG_TRACE(logger, "Done generating successors.");

	--max_steps;
      }

    if(!future_solutions.empty())
      {
	solution rval(future_solutions.top());
	future_solutions.pop();

	if(open.empty() && future_solutions.empty())
	  finished = true;

	return rval;
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
	     << "; conflicts: " << conflicts.size()
	     << "; deferred: " << deferred.size()
	     << "; generated solutions: " << generated_solutions.size());

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

template<class PackageUniverse>
std::ostream &generic_problem_resolver<PackageUniverse>::dump_conflict(std::ostream &out, const typename generic_problem_resolver<PackageUniverse>::action &act) const
{
  out << act.ver.get_package().get_name() << " "
      << act.ver.get_name();

  if(act.from_dep_source)
    out << " [" << act.d << "]";

  return out;
}

template<class PackageUniverse>
std::ostream &generic_problem_resolver<PackageUniverse>::dump_conflict(std::ostream &out, const typename imm::map<typename PackageUniverse::package, typename generic_solution<PackageUniverse>::action> &act) const
{
  out << "(";

  for(typename imm::map<typename PackageUniverse::package, typename generic_solution<PackageUniverse>::action>::const_iterator ci
	= act.begin(); ci != act.end(); ++ci)
    {
      if(ci != act.begin())
	out << ", ";

      dump_conflict(out, ci->second);
    }

  out << ")";

  return out;
}

#endif
