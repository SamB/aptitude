// problemresolver.h                  -*-c++-*-
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
// Generic problem resolver (generic because I want to be able to do
// regression-testing for once, if I can figure out how, and Packages
// files make lousy regression-tests).

#ifndef PROBLEMRESOLVER_H
#define PROBLEMRESOLVER_H

#include <assert.h>

#include <algorithm>
#include <map>
#include <queue>
#include <set>
#include <vector>

#include <iostream>

#include "dump_universe.h"
#include "exceptions.h"
#include "solution.h"
#include "resolver_undo.h"

#include <generic/util/dense_setset.h>
#include <generic/util/threads.h>

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
 *  This is a replacement for the generic apt problem resolver.  It
 *  uses a different algorithm which, while it is less efficient,
 *  allows the program to have more direct control over the results,
 *  allows the user to pick and choose from multiple solutions, and
 *  can (eventually) spit out explanations of its actions in a
 *  user-friendly way.
 *
 *  The problem resolver class is templated on abstract packages.
 *  Normally this will be apt packages, but for the purpose of testing
 *  the algorithm a simpler package system is used.  Each package has
 *  an associated "score" which indicates, essentially, how hard the
 *  problem resolver will try to keep it on the system (or, for
 *  negative scores, to remove it from the system).  The sum of all
 *  installed package's scores is used as a heuristic in a directed
 *  search of the solution space (see generic_problem_resolver's
 *  documentation for the interface and its documentation for the gory
 *  details).
 */

/** A generic package problem resolver.  The class PackageUniverse
 *  contains information on how to access packages, package versions,
 *  dependencies (in the specialized form this needs), etc.  It should
 *  export classes that are suitable for use as immediate values (eg,
 *  thin wrappers around pointers/PkgIterators).
 *
 *  The problem resolver views the world in terms of packages,
 *  versions of packages, and dependencies.  A dependency states that
 *  a version of a particular package requires one or more other
 *  packages/versions.  The resolver works by repeatedly fixing single
 *  dependencies; its work queue is priority-ordered according to the
 *  relative "goodness" of the solutions.  The "goodness" of a
 *  solution is affected by:
 *
 *    - the penalty/bonus applied to each package version, which
 *      indicates (roughly) how hard aptitude will try to fix the
 *      problem.
 *
 *    - a penalty is applied as the length of a solution (the number
 *      of packages to be installed/removed) grows.  The idea is to
 *      prioritize simpler solutions first.
 *
 *    - a penalty is applied for broken (strong) dependencies.
 *      This aims at directing the search towards "less broken"
 *      situations.
 *
 *  PackageUniverse::package is the representation of a package; the
 *  main interesting thing about packages is that they have lists of
 *  versions.  Packages can be compared with == and <, they have
 *  unique numerical IDs (retrieved via p.get_id()), and
 *  PackageUniverse::package::version_iterator lets you iterate from
 *  p.versions_begin() to p.versions_end(); dereferencing the iterator
 *  results in a PackageUniverse::version.
 *
 *  A PackageUniverse::version is the object upon which dependencies
 *  operate.  Given a PackageUniverse::version, you can compare it
 *  with == and <, retrieve its corresponding package, or get a
 *  PU::version::revdep_iterator to iterate from v.revdeps_begin() to
 *  v.revdeps_end().  Dereferencing this iterator gives you a PU::dep.
 *
 *  A PU::dep is a dependency of the form "A -> B | C | ...".  Given a
 *  dep, you can compare it with ==, retrieve A with
 *  d.get_source(), and retrieve a PU::dep::solvers_iterator
 *  with d.solvers_begin()/d.solvers_end().  \todo add a function to
 *  efficiently test whether a dep is satisfied using information
 *  available at the concrete package system level.
 *
 *  A PU::dep::solvers_iterator iterates over the list B,C,... above.
 *  Dereferencing it returns a ver_iterator.
 *
 *  \note revdeps need not iterate over \b all reverse dependencies of
 *  a version; it is sufficient that for versions v1,v2, if a
 *  dependency is satisfied by one and not the other, it appears in
 *  the revdeps list of at least one of the versions.  (this allows
 *  apt's conflicts to be handled efficiently)
 *
 *  The client can request that particular package versions not appear
 *  in generated solutions, or cancel such requests, by using the
 *  "reject_version" and "unreject_version" methods.  Solutions that
 *  are held up by a "reject_version" invocation will be returned to
 *  the search queue when a corresponding "unreject_version" is
 *  issued.
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
      threads::mutex::lock l(r.execution_mutex);
      if(r.solver_executing)
	throw DoubleRunException();
      else
	r.solver_executing = true;
    }

    ~instance_tracker()
    {
      threads::mutex::lock l(r.execution_mutex);
      assert(r.solver_executing);

      r.solver_executing = false;
      r.solver_cancelled = false;
    }
  };

  // Information regarding the weight given to various parameters;
  // packaged up in a struct so it can be easily used by the solution
  // constructors.
  solution_weights weights;

  /** Solutions whose score is smaller than this value will be
   *  discarded rather than being enqueued.
   */
  int minimum_score;

  /** The "maximum" number of successors to generate for any given
   *  node.  Note, however, that a full set of successors will always
   *  be generated for each broken dependency, so this is not exact.
   *
   *  The theoretical justification is that in order to cover all
   *  solutions, it's sufficient to generate all successors for a
   *  single broken dependency.  This avoids the problem that when a
   *  large number of packages are broken, or when a single package
   *  version with a large number of reverse dependencies is broken,
   *  you can end up searching way too many successor nodes, even if
   *  all the successors are discarded outright.
   *
   *  Unfortunately, only generating one successor has its own
   *  problem: solutions might be generated out-of-order (i.e., worse
   *  solutions before better).  This variable allows you to seek a
   *  "happy medium" by generating a reasonable number of successors,
   *  but not too many.  If a single package breaks a large number of
   *  other packages, then any attempt to fix those packages should
   *  generate the correct successor (which will then pop to the top
   *  of the queue and likely stay there), while if a large number of
   *  unrelated packages are broken, it doesn't matter which successor
   *  goes first.
   *
   *  Note that in practice, turning this up is very likely to result
   *  in dreadful performance; the option to do so may very well be
   *  removed in the future.
   */
  unsigned int max_successors;

  /** The universe in which we are solving problems. */
  const PackageUniverse universe;

  /** If \b true, we have exhausted the list of solutions. */
  bool finished:1;

  /** If \b true, it is possible that some deferred solutions are
   *  no longer "forbidden".
   */
  bool deferred_dirty:1;

  /** If \b true, debugging messages will be sent to stdout. */
  bool debug:1;

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
  threads::mutex execution_mutex;



  queue_counts counts;

  /** Mutex guarding the cache of resolver status information. */
  threads::mutex counts_mutex;



  /** The working queue: */
  std::priority_queue<solution, std::vector<solution>, solution_goodness_compare> open;

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

  /** Stores generated solutions: */
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

    if(debug && rval)
      {
	std::cout << "The conflict ";
	dump_conflict(std::cout, *found);
	std::cout << " is triggered by the solution ";
	s.dump(std::cout);
	std::cout << std::endl;
      }

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

    if(debug && result != conflicts.end())
      {
	std::cout << "Adding " << p.get_name() << " "
		  << a.ver.get_name();

	if(a.from_dep_source)
	  std::cout << " due to " << a.d;

	std::cout << " will trigger conflict ";
	dump(std::cout, *result);
	std::cout << std::endl;
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
	return p.current_version();
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

    version curr = v.get_package().current_version();
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

  /** Represents an empty solution. */
  class null_solution
  {
  public:
    version version_of(const package &p) const
    {
      return p.current_version();
    }
  };

  /** Represents a partial solution based directly (by reference) on a
   *  reference to a map.
   */
  class map_solution
  {
    const std::map<package, action> &actions;
  public:
    map_solution(const std::map<package, action> &_actions)
      : actions(_actions)
    {
    }

    version version_of(const package &p) const
    {
      typename std::map<package, action>::const_iterator found = actions.find(p);

      if(found == actions.end())
	return p.current_version();
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
      assert(installations.find(p) == installations.end());

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
	assert(!avail_versions.empty());

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
	assert(found_one);
	assert(output_actions.find(solver.get_package()) == output_actions.end());
	assert(solver != solver.get_package().current_version());

	if(debug)
	  std::cout << "Filter: resolving " << d << " with "
		    << solver.get_package().get_name() << ":"
		    << solver.get_name() << std::endl; 

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
	action_score -= version_scores[solver.get_package().current_version().get_id()];
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
    assert(s.get_broken().empty());

    stupid_table stupid_pairs;

    initialize_stupid_pairs(s, stupid_pairs);

    solution rval = s;

    bool contained_stupid = !stupid_pairs.empty();

    while(!stupid_pairs.empty())
      {
	if(debug)
	  {
	    std::cout << "Eliminating stupid pairs from ";
	    rval.dump(std::cout);
	    std::cout << std::endl;
	  }

	// The pair to eliminate; picked (sorta) arbitrarily.
	const typename std::pair<version, version> &victim = *stupid_pairs.begin();
	stupid_pairs.erase(stupid_pairs.begin());

	// This is where we discard pairs that are no longer relevant
	// because one of the elements of the pair has been dropped.
	if(rval.get_actions().find(victim.first.get_package()) == rval.get_actions().end() ||
	   rval.get_actions().find(victim.second.get_package()) == rval.get_actions().end())
	  {
	    if(debug)
	      std::cout << "Dropping invalidated stupid pair("
			<< victim.first.get_package().get_name()
			<< ":" << victim.first.get_name() << ","
			<< victim.second.get_package().get_name()
			<< ":" << victim.second.get_name() << ")"
			<< std::endl;

	    continue;
	  }

	if(debug)
	  std::cout << "Examining stupid pair ("
		    << victim.first.get_package().get_name()
		    << ":" << victim.first.get_name() << ","
		    << victim.second.get_package().get_name()
		    << ":" << victim.second.get_name() << ")" << std::endl;

	// Suppose we drop the second element in favor of the first.
	// Will that produce a valid solution?
	dep first_broken;

	// If something was broken, then we need to use the fallback
	// position: just change the dependency given as the "reason"
	// for the first action.
	if(broken_by_drop(rval, victim.first, first_broken))
	  {
	    if(debug)
	      std::cout << "Unable to drop "
			<< victim.first.get_package().get_name()
			<< ":" << victim.first.get_name()
			<< ", changing justification to "
			<< first_broken << std::endl;

	    typename std::map<package, action>::const_iterator found = rval.get_actions().find(victim.first.get_package());
	    assert(found != rval.get_actions().end());

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
	    if(debug)
	      std::cout << "Dropping "
			<< victim.first.get_package().get_name()
			<< ":" << victim.first.get_name()
			<< " and filtering unnecessary installations."
			<< std::endl;
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

    if(debug)
      {
	if(contained_stupid)
	  {
	    std::cout << "Done eliminating stupid pairs, result is ";
	    rval.dump(std::cout);
	    std::cout << std::endl;
	  }
	else
	  {
	    std::cout << "No stupid pairs in ";
	    rval.dump(std::cout);
	    std::cout << std::endl;
	  }
      }

    assert(rval.get_broken().empty());

    return rval;
  }
#endif

  solution eliminate_stupid(const solution &s) const
  {
    if(debug)
      std::cout << "Would eliminate stupid, but stupid elimination is disabled." << std::endl;

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
	    if(debug)
	      std::cout << "Rejected version " << found.getVal().first.get_name()
			<< " " << found.getVal().second.ver.get_name()
			<< " detected." << std::endl;

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
	    if(debug)
	      std::cout << "Broken hardened dependency " << *uh_iter
			<< " detected." << std::endl;

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
	      if(debug)
		{
		  std::cout << ai->get_package().get_name() << " version " << ai->get_name() << " is avoided (when resolving " << si->second.d << ") by the solution:" << std::endl;
		  s.dump(std::cout);
		  std::cout << std::endl;
		}

	      return true;
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
	if(debug)
	  std::cout << "Not generating solution (infinite badness " << s.get_score() << "<" << minimum_score << ")" << std::endl;
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
	if(debug)
	  {
	    std::cout << "Dropping irrelevant solution ";
	    s.dump(std::cout);
	    std::cout << std::endl;
	  }

	return false;
      }
    else if(should_defer(s))
      {
	if(debug)
	  {
	    std::cout << "Deferring rejected solution ";
	    s.dump(std::cout);
	    std::cout << std::endl;
	  }

	deferred.insert(s);
	return false;
      }
    else
      {
	if(debug)
	  {
	    std::cout << "Enqueuing ";
	    s.dump(std::cout);
	    std::cout << std::endl;
	  }

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
    version cur = p.current_version();
    version inst = s.version_of(p);

    if(inst != cur)
      {
	if(debug)
	  std::cout << "Discarding " << p.get_name() << " "
		    << v.get_name() << ": monotonicity violation"
		    << std::endl;

	out_act.ver = inst;
	out_act.from_dep_source = false;
	return false;
      }
    else
      {
	assert(v != cur);

	typename imm::map<version, dep>::node found
	  = s.get_forbidden_versions().lookup(v);

	if(!found.isValid())
	  return true;
	else
	  {
	    const dep &found_d = found.getVal().second;

	    if(debug)
	      std::cout << "Discarding " << p.get_name() << " "
			<< v.get_name() << ": forbidden by the resolution of "
			<< found_d << std::endl;

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

	assert(found_act.ver == act.ver);
	if(a2.from_dep_source)
	  {
	    if(found_act.from_dep_source)
	      assert(a2.d == found_act.d);

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
  public:
    real_generator(std::vector<solution> &_target)
      :target(_target)
    {
    }

    template<typename a_iter, class u_iter>
    void make_successor(const solution &s,
			const a_iter &abegin, const a_iter &aend,
			const u_iter &ubegin, const u_iter &uend,
			const PackageUniverse &universe,
			const solution_weights &weights) const
    {
      target.push_back(solution::successor(s,
					   abegin, aend,
					   ubegin, uend,
					   universe, weights));
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
			const solution_weights &weights) const
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
				 const SolutionGenerator &generator) const
  {
    action conflictor;

    if(debug)
      {
	std::cout << "Trying to resolve " << d << " by installing "
		  << v.get_package().get_name() << " "
		  << v.get_name();
	if(from_dep_source)
	  std::cout << " from the dependency source";

	std::cout << std::endl;
      }

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
	    if(debug)
	      {
		std::cout << "Discarding " << v.get_package().get_name()
			  << " " << v.get_name() << " due to conflict ";
		dump_conflict(std::cout, *found);
		std::cout << std::endl;
	      }

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
		  assert(ci->second.ver == v);
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
   *  \param out a vector onto which the successors should be pushed.
   */
  template<typename SolutionGenerator>
  void generate_successors(const solution &s,
			   const dep &d,
			   imm::map<package, action> &conflict,
			   const SolutionGenerator &generator) const
  {
    version source = d.get_source();
    typename imm::map<package, action>::node
      source_found = s.get_actions().lookup(source.get_package());

    // Try moving the source, if it is legal to do so
    if(source_found.isValid())
      insert_conflictor(conflict, action(source, d, false, -1));
    else
      {
	assert(source == source.get_package().current_version());

	for(typename package::version_iterator vi = source.get_package().versions_begin();
	    !vi.end(); ++vi)
	  if(*vi != source)
	    generate_single_successor(s, d, *vi, true, conflict, generator);
      }

    // Now try installing each target of the dependency.
    for(typename dep::solver_iterator si = d.solvers_begin();
	!si.end(); ++si)
      generate_single_successor(s, d, *si, false, conflict, generator);

    // Finally, maybe we can leave this dependency unresolved.
    if(d.is_soft())
      generator.make_successor(s, (action *) 0, (action *) 0,
			       &d, &d+1, universe, weights);
  }

  /** Processes the given solution by enqueuing its successor nodes
   *  (if any are available).  Note that for clarity, we now generate
   *  *all* successors before examining *any*.
   */
  void process_solution(const solution &s)
  {
    // Any forcings are immediately applied to 'curr', so that
    // forcings are performed ASAP.
    solution curr = s;

    assert(!s.get_broken().empty());

    // This loop attempts to generate all the successors of a
    // solution.  However, if a "forced" dependency arises, it
    // re-verifies all the dependencies of the solution.
    bool done = false;
    while(!done)
      {
	if(debug)
	  {
	    std::cout << "Processing ";
	    curr.dump(std::cout);
	    std::cout << std::endl;
	  }

	done = true;

	// Remember the solution whose broken dependencies we're
	// iterating over.
	solution starting_solution = curr;

	for(typename imm::set<dep>::const_iterator bi=starting_solution.get_broken().begin();
	    bi!=starting_solution.get_broken().end(); ++bi)

	  {
	    // Check for the case where this dependency has been
	    // fortuitously solved by forcing another broken
	    // dependency.
	    if(starting_solution != curr && !(*bi).broken_under(curr))
	      continue;

	    // Assert against impossible conditions (if this happens
	    // something is broken elsewhere).
	    if(starting_solution == curr && !(*bi).broken_under(curr))
	      {
		std::cerr << "Unexpectedly non-broken dependency "
			  << *bi << "!" << std::endl;

		version source = (*bi).get_source();

		if(curr.version_of(source.get_package()) != source)
		  std::cerr << "  (" << source.get_package().get_name()
			    << " " << source.get_name()
			    << " is not installed)" << std::endl;

		for(typename dep::solver_iterator si = (*bi).solvers_begin();
		    !si.end(); ++si)
		  if(curr.version_of((*si).get_package()) == *si)
		    std::cerr << "  (" << source.get_package().get_name()
			      << " " << source.get_name()
			      << " is not installed)" << std::endl;

		abort();
	      }

	    imm::map<package, action> conflict;


	    int num_successors = 0;
	    generate_successors(curr, *bi, conflict,
				null_generator(num_successors));



	    if(num_successors == 0)
	      {
		if(debug)
		  {
		    std::cout << "Discarding solution; unresolvable dependency "
			      << *bi << " with conflict ";

		    dump_conflict(std::cout, conflict);

		    std::cout << std::endl;
		  }

		add_conflict(conflict);

		return;
	      }
	    else if(num_successors == 1)
	      {
		if(debug)
		  std::cout << "Forced resolution of " << *bi << std::endl;

		std::vector<solution> v;
		real_generator g(v);
		// NB: this may do redundant work adding to 'conflict'.
		// Use a generator object to avoid that?
		generate_successors(curr, *bi, conflict,
				    real_generator(v));

		assert(v.size() == 1);

		curr = v.back();
		done = false;
	      }
	  }
      }

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
    for(typename imm::set<dep>::const_iterator bi=curr.get_broken().begin();
	bi!=curr.get_broken().end() && (nsols == 0 ||
					nsols < max_successors); ++bi)

      if(impinges_user_constraint(*bi))
	{
	  // Is it possible to take this out somehow?
	  imm::map<package, action> conflict;

	  if(debug)
	    std::cout << "Generating successors for " << *bi
		      << std::endl;

	  std::vector<solution> v;
	  generate_successors(curr, *bi, conflict, real_generator(v));
	  try_enqueue(v);
	  nsols += v.size();
	}

    for(typename imm::set<dep>::const_iterator bi=curr.get_broken().begin();
	bi!=curr.get_broken().end() && (nsols == 0 ||
					nsols < max_successors); ++bi)
      {
	// Is it possible to take this out somehow?
	imm::map<package, action> conflict;

	if(debug)
	  std::cout << "Generating successors for " << *bi
		    << std::endl;

	std::vector<solution> v;
	generate_successors(curr, *bi, conflict, real_generator(v));
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
   *  \param _universe the universe in which we are working.
   */
  generic_problem_resolver(int _step_score, int _broken_score,
			   int _unfixed_soft_score,
			   int infinity, unsigned int _max_successors,
			   int _full_solution_score,
			   const PackageUniverse &_universe)
    :weights(_step_score, _broken_score, _unfixed_soft_score,
	     _full_solution_score, _universe.get_version_count()),
     minimum_score(-infinity), max_successors(_max_successors),
     universe(_universe), finished(false), deferred_dirty(false),
     debug(false), remove_stupid(true),
     solver_executing(false), solver_cancelled(false),
     conflicts(_universe.get_package_count())
  {
    // Find all the broken deps.
    for(typename PackageUniverse::broken_dep_iterator bi=universe.broken_begin();
	!bi.end(); ++bi)
      initial_broken.insert(*bi);
  }

  ~generic_problem_resolver()
  {
  }

  const PackageUniverse &get_universe()
  {
    return universe;
  }

  int get_step_score() {return weights.step_score;}
  int get_broken_score() {return weights.broken_score;}
  int get_unresolved_soft_dep_score() {return weights.unfixed_soft_score;}
  int get_infinity() {return -minimum_score;}
  int get_max_successors() {return max_successors;}
  int get_full_solution_score() {return weights.full_solution_score;}

  /** Enables or disables debugging.  Debugging is initially
   *  disabled.
   */
  void set_debug(bool new_debug)
  {
    debug=new_debug;
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

  /** Tells the resolver how highly to value a particular package
   *  version.  All scores are relative, and a higher score will
   *  result in a bias towards that version appearing in the final
   *  solution.
   */
  void set_version_score(const version &ver, int score)
  {
    assert(ver.get_id()<universe.get_version_count());
    weights.version_scores[ver.get_id()]=score;
  }

  /** As set_version_score, but instead of replacing the current score
   *  increment it.
   */
  void add_version_score(const version &ver, int score)
  {
    assert(ver.get_id()<universe.get_version_count());
    weights.version_scores[ver.get_id()]+=score;
  }

  /** \return the score of the version ver. */
  int get_version_score(const version &ver)
  {
    assert(ver.get_id()<universe.get_version_count());
    return weights.version_scores[ver.get_id()];
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
    assert(d.is_soft());

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
      {
	if(debug)
	  {
	    std::cout << "Dropping conflict ";
	    dump_conflict(std::cout, conflict);
	    std::cout << " because it is redundant with ";
	    dump_conflict(std::cout, *found);
	    std::cout << std::endl;
	  }
      }
    else
      // TODO: drop conflicts of which this is a subset.  Needs work
      // at the setset level.
      conflicts.insert(conflict);
  }

  /** Cancel any find_next_solution call that is executing in the
   *  background.  If no such call is executing, then the next call
   *  will immediately be cancelled.
   */
  void cancel_solver()
  {
    threads::mutex::lock l(execution_mutex);
    solver_cancelled = true;
  }

  /** Remove any pending find_next_solution cancellation. */
  void uncancel_solver()
  {
    threads::mutex::lock l(execution_mutex);
    solver_cancelled = false;
  }

  /** Atomically read the current queue sizes of this resolver. */
  queue_counts get_counts()
  {
    maybe_update_deferred_and_counts();

    threads::mutex::lock l(counts_mutex);
    return counts;
  }

  /** Update the cached queue sizes. */
  void update_counts_cache()
  {
    threads::mutex::lock l(counts_mutex);
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
    threads::mutex::lock l(execution_mutex);
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
   *  \return a solution that fixes all broken dependencies
   *
   * \throws NoMoreSolutions if the potential solution list is exhausted.
   * \throws NoMoreTime if no solution is found within max_steps steps.
   *
   *  \todo when throwing NoMoreSolutions or NoMoreTime, maybe we
   *        should include the "least broken" solution seen.
   */
  solution find_next_solution(int max_steps)
  {
    // This object is responsible for managing the instance variables
    // that control threaded operation: it sets solver_executing when
    // it is created and clears both solver_executing and
    // solver_cancelled when it is destroyed.
    instance_tracker t(*this);
 

    // Counter for debugging (see below)
    int odometer = 0;

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
				      weights));
      }

    while(max_steps>0 && !open.empty())
      {
	// Threaded operation: check whether we have been cancelled.
	{
	  threads::mutex::lock l(execution_mutex);
	  if(solver_cancelled)
	    throw InterruptedException();
	}

	update_counts_cache();


	solution s=open.top();
	open.pop();

	++odometer;

	if(irrelevant(s))
	  {
	    if(debug)
	      {
		std::cout << "Dropping irrelevant solution ";
		s.dump(std::cout);
		std::cout << std::endl;
	      }
	    continue;
	  }

	if(should_defer(s))
	  {
	    if(debug)
	      {
		std::cout << "Deferring rejected solution ";
		s.dump(std::cout);
		std::cout << std::endl;
	      }

	    deferred.insert(s);
	    continue;
	  }

	closed.insert(s);

	// If all dependencies are satisfied, we found a solution.
	if(s.is_full_solution())
	  {
	    if(debug)
	      {
		std::cout << " --- Found solution ";
		s.dump(std::cout);
		std::cout << std::endl;
	      }

	    // Set it here too in case the last tentative solution is
	    // really a solution.
	    if(open.empty())
	      finished=true;

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
		if(debug)
		  std::cout << " --- Placing de-stupidified solution back on the open queue" << std::endl;
		finished = false;
		open.push(minimized);
	      }
	    else
	      {
		closed.insert(minimized);
		generated_solutions.push_back(minimized);

		if(debug)
		  {
		    std::cout << " *** Converged after " << odometer << " steps." << std::endl;
		    std::cout << " *** open: " << open.size()
			      << "; closed: " << closed.size()
			      << "; conflicts: " << conflicts.size()
			      << "; deferred: " << deferred.size()
			      << "; generated solutions: " << generated_solutions.size()
			      << std::endl;
		  }

		update_counts_cache();

		return minimized;
	      }
	  }
	// Nope, let's go enqueue successor nodes.
	else
	  process_solution(s);

	if(debug)
	  std::cout << "Done generating successors." << std::endl;

	--max_steps;
      }

    // Oh no, we either ran out of solutions or ran out of steps.

    if(max_steps==0)
      throw NoMoreTime();

    assert(open.empty());

    finished=true;

    update_counts_cache();

    if(debug)
      {
	std::cout << " *** Out of solutions after " << odometer << " steps." << std::endl;
	std::cout << " *** open: " << open.size()
		  << "; closed: " << closed.size()
		  << "; conflicts: " << conflicts.size()
		  << "; deferred: " << deferred.size()
		  << "; generated solutions: " << generated_solutions.size()
		  << std::endl;
      }

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
