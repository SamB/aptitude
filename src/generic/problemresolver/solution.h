// solution.h                                             -*-c++-*-
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
//   General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with this program; see the file COPYING.  If not, write to
//   the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//   Boston, MA 02111-1307, USA.
//
// The solution class for the problem resolver.

#ifndef SOLUTION_H
#define SOLUTION_H

#include <iostream>
#include <map>
#include <set>

#include <cwidget/generic/util/ref_ptr.h>
#include <generic/util/immset.h>
#include <generic/util/refcounted_base.h>

/** \brief The solution class for the problem resolver.
 * 
 *  \file solution.h
 */

template<typename PackageUniverse>
class solution_weights;

/** \brief Represents the initial state of a dependency search.
 *
 *  This is optimized under the assumption that overrides of package
 *  versions will be rare, but not unheard of.  (note: this is O(1),
 *  but in reality it might not be optimized due to poor locality and
 *  the low number of overrides; if it's really an issue some
 *  profiling of different approaches -- sparse binary tree, sparse
 *  array, etc -- would be handy)
 */
template<typename PackageUniverse>
class resolver_initial_state
{
  class impl : public aptitude::util::refcounted_base_threadsafe
  {
    // A collection of indices into the vector of overridden versions,
    // indexed by package ID; -1 means to use the real current
    // version.  This lets us avoid allocating space for
    // non-overridden versions while keeping good locality for the
    // list of versions.
    //
    // If NULL, all packages have their real current version.
    int *overridden_versions;
    // The size that the version array has / will have (depending on
    // whether the array is NULL).
    int num_overridden_versions;

    // Stores the versions that have been overridden;
    // overridden_versions indexes into this list.
    std::vector<typename PackageUniverse::version> version_store;

    /** \brief Override p (if necessary) to the version v. */
    void map_package(const typename PackageUniverse::package &p,
		     const typename PackageUniverse::version &v)
    {
      if(overridden_versions == NULL)
	{
	  if(p.current_version() == v)
	    return;

	  overridden_versions = new int[num_overridden_versions];
	  for(int i = 0; i < num_overridden_versions; ++i)
	    overridden_versions[i] = -1;
	}

      const int p_id = p.get_id();
      if(overridden_versions[p_id] == -1)
	{
	  if(p.current_version() == v)
	    return;

	  // Allocate a new slot.
	  const int slot = (int)version_store.size();
	  version_store.push_back(v);
	  overridden_versions[p_id] = slot;
	}
      else
	{
	  const int slot = overridden_versions[p_id];
	  version_store[slot] = v;
	}
    }

    struct do_map_package
    {
      impl &state;

      do_map_package(impl &_state)
	: state(_state)
      {
      }

      bool operator()(const std::pair<typename PackageUniverse::package, typename PackageUniverse::version> &pair) const
      {
	state.map_package(pair.first, pair.second);
	return true;
      }
    };

  public:
    /** \brief Create a new initial state that sets the given packages
     *  to the given versions.
     *
     *  \param mappings   Each package in this map will be treated as
     *                    "starting at" the version it is mapped to.
     *
     *  \param package_count The number of packages in the universe;
     *                          used to allocate space for internal
     *                          structures.
     */
    impl(const imm::map<typename PackageUniverse::package, typename PackageUniverse::version> &mappings,
	 int package_count)
      : overridden_versions(NULL),
	num_overridden_versions(package_count)
    {
      mappings.for_each(do_map_package(*this));
    }

    bool empty() const
    {
      if(overridden_versions == NULL)
	return true;

      for(int i = 0; i < num_overridden_versions; ++i)
	if(overridden_versions[i] != -1)
	  return false;

      return true;
    }

    ~impl()
    {
      delete[] overridden_versions;
    }

    typename PackageUniverse::version version_of(const typename PackageUniverse::package &p) const
    {
      int slot;
      int p_id;
      if(overridden_versions == NULL)
	{
	  slot = -1;
	  p_id = -1;
	}
      else
	{
	  p_id = p.get_id();
	  slot = overridden_versions[p.get_id()];
	}

      if(slot == -1)
	return p.current_version();
      else
	return version_store[slot];
    }

    void get_initial_versions(std::set<typename PackageUniverse::version> &out) const
    {
      out.insert(version_store.begin(), version_store.end());
    }
  };

  // If invalid, this represents an empty set of initial versions;
  // otherwise, a pointer to the real object that contains the initial
  // version set.
  cwidget::util::ref_ptr<impl> the_impl;

public:
  resolver_initial_state()
  {
  }

  resolver_initial_state(const resolver_initial_state &state)
    : the_impl(state.the_impl)
  {
  }

  resolver_initial_state(const imm::map<typename PackageUniverse::package, typename PackageUniverse::version> &mappings,
			 int package_count)
    : the_impl(mappings.empty()
	       ? cwidget::util::ref_ptr<impl>()
	       : new impl(mappings, package_count))
  {
  }

  resolver_initial_state &operator=(const resolver_initial_state &other)
  {
    the_impl = other.the_impl;
    return *this;
  }

  bool empty() const
  {
    return !the_impl || the_impl->empty();
  }

  typename PackageUniverse::version version_of(const typename PackageUniverse::package &p) const
  {
    if(the_impl.valid())
      return the_impl->version_of(p);
    else
      return p.current_version();
  }

  /** \brief Retrieve the initial installations stored in this object.
   *
   *  \param out  A set into which the initially installed versions
   *              are placed; it is guaranteed that no two output
   *              versions have the same package.
   */
  void get_initial_versions(std::set<typename PackageUniverse::version> &out) const
  {
    return the_impl->get_initial_versions(out);
  }
};

/** Represents a single action taken by the resolver: the
 *  installation of a particular version of a package.  The
 *  *identity* of an action (in terms of operator< and operator==)
 *  is based solely on the version it installs, although additional
 *  information is provided to "tag" it.
 */
template<typename PackageUniverse>
struct generic_action
{
  typedef typename PackageUniverse::package package;
  typedef typename PackageUniverse::version version;
  typedef typename PackageUniverse::dep dep;

  version ver;

  /** The dependency that triggered this action. */
  dep d;

  /** If \b true, this action was triggered by removing the source
   *	of the dependency d.
   */
  bool from_dep_source:1;

  /** The order in which this action should be placed.  Used when
   *  presenting a "story" about a solution.
   */
  int id:31;

  generic_action() {}

  generic_action(const version &_ver,
		 const dep &_d,
		 bool _from_dep_source,
		 int _id)
    : ver(_ver), d(_d),
      from_dep_source(_from_dep_source), id(_id)
  {
  }

  bool operator<(const generic_action &other) const {return ver<other.ver;}

  bool operator==(const generic_action &other) const
  {
    return ver == other.ver;
  }

  bool operator!=(const generic_action &other) const
  {
    return ver != other.ver;
  }
};

/** Represents a partial or complete solution to a dependency
 *  problem.  Solutions are transparently refcounted to save on
 *  memory and avoid copies.
 *
 *  Solution identity is based on both the mapping stored in the
 *  solution and on the set of unfixed soft dependencies stored in it.
 *  Dependencies in unfixed_soft_deps are removed from broken_deps so
 *  it's easy to check whether a solution is complete.
 */
template<class PackageUniverse>
class generic_solution
{
public:
  // Let the resolver tester poke around in our internals.
  friend class ResolverTest;

  typedef typename PackageUniverse::package package;
  typedef typename PackageUniverse::version version;
  typedef typename PackageUniverse::dep dep;
  typedef generic_action<PackageUniverse> action;

private:
  /** Hide this, it's meaningless. */
  bool operator<(const generic_solution &other) const;

  class solution_rep
  {
    /** \brief The initial state of this solution.
     *
     *  This is currently assumed to be the same for all solutions in
     *  a given resolver run (i.e., operator== and friends don't check
     *  it); if it's not, you have some serious weirdness.  The
     *  pointer goes away after the resolver is done with its
     *  computations, but nothing else should use it anyway.
     *
     *  We need this mainly so that version_of works properly with no
     *  extra arguments.  (maybe instead I should just accept having
     *  to wrap it all the time?)
     */
    const resolver_initial_state<PackageUniverse> initial_state;

    /** The actions performed by this solution.
     *
     *  Originally the plan was to read this off by tracing to the
     *  root -- but it seems likely that this will be read many more
     *  times than it's created, so I'm swallowing the space/time
     *  hit to build a lookup table at each node in the hope that it
     *  pays off later..
     *
     *  Note that this also means you can't find out chronological
     *  ordering, which might help make sense of everything; that
     *  can be added in later one way or another, though.  (eg,
     *  storing the DAG structure after all?)
     */
    imm::map<package, action> actions;

    /** The full list of currently broken dependencies. */
    imm::set<dep> broken_deps;

    /** The set of soft dependencies being left unresolved. */
    imm::set<dep> unresolved_soft_deps;

    /** A set of versions that have been "locked out" by this
     *	solution.  Primarily used to optimize the common case of
     *	having to "climb a hill" as removals propagate up the
     *	dependency chain.  (this allows us to easily force the search
     *	in the direction it has to go)
     */
    imm::map<version, dep> forbidden_versions;

    /** The score of this solution. */
    int score;

    /** The combined score due to package version installations
     *  and distance from the root -- "score" is calculated by
     *  adding the broken-dependency count to this.
     */
    int action_score;

    /** The reference count of this solution. */
    mutable unsigned int refcount;

  public:
    void incref() const {++refcount;}
    void decref() const {eassert(refcount>0); if(--refcount==0) delete this;}

    /** Construct a new solution_rep directly. */
    solution_rep(const imm::map<package, action> &_actions,
		 const imm::set<dep> &_broken_deps,
		 const imm::set<dep> &_unresolved_soft_deps,
		 const imm::map<version, dep> &_forbidden_versions,
		 const resolver_initial_state<PackageUniverse> &_initial_state,
		 int _score,
		 int _action_score)
      : initial_state(_initial_state), actions(_actions),
	broken_deps(_broken_deps),
	unresolved_soft_deps(_unresolved_soft_deps),
	forbidden_versions(_forbidden_versions),
	score(_score),
	action_score(_action_score),
	refcount(1)
    {
    }

    const resolver_initial_state<PackageUniverse> &get_initial_state() const
    {
      return initial_state;
    }

    const imm::set<dep> &get_broken_deps() const
    {
      return broken_deps;
    }

    const imm::set<dep> &get_unresolved_soft_deps() const
    {
      return unresolved_soft_deps;
    }

    const imm::map<package, action> &get_actions() const
    {
      return actions;
    }

    const imm::map<version, dep> &get_forbidden_versions() const
    {
      return forbidden_versions;
    }

    int get_score() const {return score;}
    int get_action_score() const {return action_score;}

    version version_of(const package &pkg) const
    {
      typename imm::map<package, action>::node found
	= actions.lookup(pkg);
      if(!found.isValid())
	return initial_state.version_of(pkg);
      else
	return found.getVal().second.ver;
    }

    /** \return true iff this solution touches the given package. */
    bool package_modified(const package &pkg) const
    {
      return actions.find(pkg) != actions.end();
    }
  }; // End solution representation.

  solution_rep *real_soln;

  /** Create a solution directly from a rep; assumes control of the
   *  reference passed in as an argument (i.e., doesn't incref() it)
   */
  generic_solution(solution_rep *r)
    :real_soln(r)
  {
  }

  /** Wrapper structure used to pass a raw map into broken_under().
   *  Used to determine the set of packages broken by a solution
   *  before the solution is actually created.
   */
  struct solution_map_wrapper
  {
    const imm::map<package, action> &actions;
    const resolver_initial_state<PackageUniverse> &initial_state;
  public:
    solution_map_wrapper(const imm::map<package, action> &_actions,
			 const resolver_initial_state<PackageUniverse> &_initial_state)
      : actions(_actions),
	initial_state(_initial_state)
    {
    }

    version version_of(const package &p) const
    {
      typename imm::map<package, action>::node found
	= actions.lookup(p);

      if(found.isValid())
	return found.getVal().second.ver;
      else
	return initial_state.version_of(p);
    }
  };

public:
  generic_solution():real_soln(0) {}

  generic_solution(const generic_solution &other)
    :real_soln(other.real_soln)
  {
    if(real_soln)
      real_soln->incref();
  }

  /** Generate a new, identical solution that shares no memory with
   *  this solution and hence is safe to hand to another thread.  Note
   *  that as with other refcounted structures, the handover should be
   *  done "at one remove" by allocating a generic_solution object on
   *  the heap.
   */
  generic_solution clone() const
  {
    return generic_solution(new solution_rep(get_actions().clone(),
					     get_broken().clone(),
					     get_unresolved_soft_deps().clone(),
					     get_forbidden_versions().clone(),
					     get_initial_state(),
					     get_score(),
					     get_action_score()));
  }


  /** Generate the root node for a search in the given universe. */
  static generic_solution root_node(const imm::set<dep> &initial_broken,
				    const PackageUniverse &universe,
				    const solution_weights<PackageUniverse> &weights,
				    const resolver_initial_state<PackageUniverse> &initial_state);

  /** Generate a successor to the given solution.
   *
   *  \param [abegin, aend) a range of actions to perform
   *  \param [ubegin, uend) a range of dependencies to leave unresolved
   */
  template<typename a_iter, typename u_iter>
  static generic_solution successor(const generic_solution &s,
				    const a_iter &abegin,
				    const a_iter &aend,
				    const u_iter &ubegin,
				    const u_iter &uend,
				    const PackageUniverse &universe,
				    const solution_weights<PackageUniverse> &weights);

  ~generic_solution()
  {
    if(real_soln)
      real_soln->decref();
  }

  /** Make this an invalid reference. */
  void nullify()
  {
    if(real_soln)
      real_soln->decref();
    real_soln=0;
  }

  generic_solution &operator=(const generic_solution &other)
  {
    if(other.real_soln)
      other.real_soln->incref();
    if(real_soln)
      real_soln->decref();
    real_soln=other.real_soln;

    return *this;
  }

  /** \note solutions are compared by pointer identity, not by
   *  value.
   *
   *  \return \b true if the solutions are the same solution.
   */
  bool operator==(const generic_solution &other) const
  {
    return real_soln == other.real_soln;
  }

  /** Without this method, some random function that produces
   *  incorrect results is used.
   *
   *  \return \b true if the solutions are not the same solution.
   */
  bool operator!=(const generic_solution &other) const
  {
    return real_soln != other.real_soln;
  }

  operator bool() const
  {
    return real_soln != 0;
  }

  const action &operator*() const
  {
    return real_soln->get_action();
  }

  const action *operator->() const
  {
    return &real_soln->get_action();
  }

  const generic_solution &get_parent() const
  {
    return real_soln->get_parent();
  }

  /** \return an immutable list of all the broken dependencies
   *  in this solution.  The list will live at least as long as
   *  the reference from which it was retrieved.
   */
  const imm::set<dep> &get_broken() const
  {
    return real_soln->get_broken_deps();
  }

  const imm::set<dep> &get_unresolved_soft_deps() const
  {
    return real_soln->get_unresolved_soft_deps();
  }

  const imm::map<package, action> &get_actions() const
  {
    return real_soln->get_actions();
  }

  const imm::map<version, dep> &get_forbidden_versions() const
  {
    return real_soln->get_forbidden_versions();
  }

  bool is_full_solution() const
  {
    return get_broken().empty();
  }

  /** \return the initial state of the solution. */
  const resolver_initial_state<PackageUniverse> &get_initial_state() const
  {
    return real_soln->get_initial_state();
  }

  /** \return the score of the scolution */
  int get_score() const
  {
    return real_soln->get_score();
  }

  /** \return the score of the solution's actions. */
  int get_action_score() const
  {
    return real_soln->get_action_score();
  }

  version version_of(const package &pkg) const
  {
    return real_soln->version_of(pkg);
  }

  bool package_modified(const package &pkg) const
  {
    return real_soln->package_modified(pkg);
  }

  // The following operators are used to place the solution components
  // in order by name, to better permit comparison of debugging output
  // between versions.
  struct package_action_pair_name_lt
  {
  public:
    bool operator()(const std::pair<package, action> &p1,
		    const std::pair<package, action> &p2) const
    {
      return std::string(p1.first.get_name()) < p2.first.get_name();
    }
  };

  struct ver_name_lt
  {
  public:
    int cmp(const version &v1, const version &v2) const
    {
      // EW: I don't have a formal standard on what get_name()
      // returns, so force it to be a string here:
      int pcmp = std::string(v1.get_package().get_name()).compare(v2.get_package().get_name());

      if(pcmp != 0)
	return pcmp;
      else
	return std::string(v1.get_name()).compare(v2.get_name());
    }

    bool operator()(const version &v1, const version &v2) const
    {
      return cmp(v1, v2) < 0;
    }
  };

  struct dep_name_lt
  {
  public:
    bool operator()(const dep &d1, const dep &d2) const
    {
      ver_name_lt vlt;

      int scmp = vlt.cmp(d1.get_source(), d2.get_source());

      if(scmp != 0)
	return scmp < 0;
      else
	{
	  typename dep::solver_iterator si1 = d1.solvers_begin();
	  typename dep::solver_iterator si2 = d2.solvers_begin();

	  while(!si1.end() && !si2.end())
	    {
	      scmp = vlt.cmp(*si1, *si2);

	      if(scmp != 0)
		return scmp < 0;

	      ++si1;
	      ++si2;
	    }

	  if(si1.end())
	    {
	      if(si2.end())
		return false;
	      else
		return true;
	    }
	  else
	    return false;
	}
    }
  };

  void dump(std::ostream &out, bool show_order = false) const
  {
    std::vector<std::pair<package, action> > actions;
    for(typename imm::map<package, action>::const_iterator i = get_actions().begin();
	i != get_actions().end(); ++i)
      actions.push_back(*i);
    sort(actions.begin(), actions.end(), package_action_pair_name_lt());


    out << "<";
    for(typename std::vector<std::pair<package, action> >::const_iterator i = actions.begin();
	i != actions.end(); ++i)
      {
	if(i != actions.begin())
	  out << ", ";
	out << i->first.get_name() << ":=" << i->second.ver.get_name();
	if(show_order)
	  out << "[#" << i->second.id << "]";
      }
    out << ">;";


    std::vector<dep> unresolved_deps;
    for(typename imm::set<dep>::const_iterator i = get_unresolved_soft_deps().begin();
	i != get_unresolved_soft_deps().end(); ++i)
      unresolved_deps.push_back(*i);
    sort(unresolved_deps.begin(), unresolved_deps.end(), dep_name_lt());


    if(!get_unresolved_soft_deps().empty())
      {
	out << "<!";
	for(typename std::vector<dep>::const_iterator i
	      = unresolved_deps.begin();
	    i != unresolved_deps.end(); ++i)
	  {
	    if(i != unresolved_deps.begin())
	      out << ", ";
	    out << *i;
	  }

	out << "!>;";
      }

    std::vector<dep> broken_deps;
    for(typename imm::set<dep>::const_iterator i = get_broken().begin();
	i != get_broken().end(); ++i)
      broken_deps.push_back(*i);
    sort(broken_deps.begin(), broken_deps.end(), dep_name_lt());

    out << "[";

    for(typename std::vector<dep>::const_iterator i = broken_deps.begin();
	i != broken_deps.end(); ++i)
      {
	if(i != broken_deps.begin())
	  out << ", ";
	out << *i;
      }

    out<< "];";

    if(!get_forbidden_versions().empty())
      {
	std::vector<version> forbidden_vers;
	for(typename imm::map<version, dep>::const_iterator i = get_forbidden_versions().begin();
	    i != get_forbidden_versions().end(); ++i)
	  forbidden_vers.push_back(i->first);
	sort(forbidden_vers.begin(), forbidden_vers.end(), ver_name_lt());

	out << "!!";
	for(typename std::vector<version>::const_iterator i = forbidden_vers.begin();
	    i != forbidden_vers.end(); ++i)
	  {
	    if(i != forbidden_vers.begin())
	      out << ", ";
	    out << i->get_package().get_name() << " " << i->get_name();
	  }
	out << "!!;";
      }

    out << get_score();
  }

  /** Compare actions by their ID */
  struct action_id_compare
  {
  public:
    bool operator()(const action &a1,
		    const action &a2) const
    {
      return a1.id < a2.id;
    }
  };
}; // End solution wrapper

/** \brief Dump a solution without showing the order of the entries. */
template<typename PackageUniverse>
std::ostream &operator<<(std::ostream &out, const generic_solution<PackageUniverse> &sol)
{
  sol.dump(out);
  return out;
}

/** \brief Write out a single action by writing out the version that
 *  is to be installed, and the associated dep if any.
 */
template<typename PackageUniverse>
std::ostream &operator<<(std::ostream &out, const generic_action<PackageUniverse> &act)
{
  out << act.ver;
  if(act.from_dep_source)
    out << " [" << act.d << "]";

  return out;
}


/** Represents the current score weights for a resolver.  Used to
 *  calculate scores at the time a solution is instantiated.
 */
template<typename  PackageUniverse>
struct solution_weights
{
  typedef typename PackageUniverse::package package;
  typedef typename PackageUniverse::version version;
  typedef typename PackageUniverse::dep dep;
  typedef typename generic_solution<PackageUniverse>::action action;

  /** \brief Represents a score assigned to a collection of actions. */
  class joint_score
  {
    imm::map<package, action> actions;
    int score;

  public:
    joint_score(const imm::map<package, action> &_actions, int _score)
      : actions(_actions), score(_score)
    {
    }

    const imm::map<package, action> &get_actions() const { return actions; }
    int get_score() const { return score; }
  };

  typedef std::map<action, std::vector<joint_score> > joint_score_set;

  /** How much to reward long and/or broken solutions.  Typically
   *  negative to penalize such things, or 0 to ignore them.
   */
  int step_score, broken_score, unfixed_soft_score;


  /** How much to reward real solutions -- solutions that fix all
   *  dependencies.  Make this big to immediately pick them up, or
   *  small to ignore "bad" solutions (at the risk of running out of
   *  time if no better solution pops up).
   */
  int full_solution_score;

  /** The scores to apply to individual package versions.
   */
  int *version_scores;

private:
  /** \brief Scores that apply to simultaneous collections of
   *  versions.
   */
  joint_score_set joint_scores;

  /** \brief The initial state of the resolver.
   *
   *  This is currently assumed to be the same for all solutions in
   *  a given resolver run (i.e., operator== and friends don't check
   *  it); if it's not, you have some serious weirdness.  The
   *  pointer goes away after the resolver is done with its
   *  computations, but nothing else should use it anyway.
   *
   *  We need this mainly so that version_of works properly with no
   *  extra arguments.  (maybe instead I should just accept having
   *  to wrap it all the time?)
   */
  const resolver_initial_state<PackageUniverse> initial_state;

  /** \brief A list of the joint scores added to this
   *  set of weights, in order.
   *
   *  Each entry is a pair of the versions that are affected to and
   *  the score to add.
   */
  std::vector<std::pair<imm::set<version>, int> > joint_scores_list;

public:
  solution_weights(int _step_score, int _broken_score,
		   int _unfixed_soft_score, int _full_solution_score,
		   unsigned long num_versions,
		   const resolver_initial_state<PackageUniverse> &_initial_state)
    :step_score(_step_score), broken_score(_broken_score),
     unfixed_soft_score(_unfixed_soft_score),
     full_solution_score(_full_solution_score),
     version_scores(new int[num_versions]),
     initial_state(_initial_state)
  {
    for(unsigned long i = 0; i < num_versions; ++i)
      version_scores[i] = 0;
  }

  ~solution_weights()
  {
    delete[] version_scores;
  }

private:
  class build_joint_score_action_set
  {
    imm::map<package, action> &output;
    const resolver_initial_state<PackageUniverse> &initial_state;
    bool &any_is_current;
  public:
    build_joint_score_action_set(imm::map<package, action> &_output,
				 bool &_any_is_current,
				 const resolver_initial_state<PackageUniverse> &_initial_state)
      : output(_output),
	initial_state(_initial_state),
	any_is_current(_any_is_current)
    {
    }

    bool operator()(const version &version) const
    {
      if(version == initial_state.version_of(version.get_package()))
	any_is_current = true;

      output.put(version.get_package(),
		 action(version, dep(), false, 0));

      return true;
    }
  };

  class add_to_joint_scores
  {
    typedef typename solution_weights<PackageUniverse>::joint_score joint_score;
    typedef typename solution_weights<PackageUniverse>::joint_score_set joint_score_set;
    typedef typename generic_solution<PackageUniverse>::action action;

    joint_score_set &s;
    joint_score score;
  public:
    add_to_joint_scores(joint_score_set &_s, const joint_score &_score)
      : s(_s), score(_score)
    {
    }

    bool operator()(const std::pair<package, action> &entry) const
    {
      const typename joint_score_set::iterator found =
	s.find(entry.second);

      if(found == s.end())
	s[entry.second].push_back(score);
      else
	found->second.push_back(score);

      return true;
    }
  };

public:
  void add_joint_score(const imm::set<version> &versions, int score)
  {
    // Build a map internally: it's easier to compare a map to another
    // map than to compare it to a set.  (could be fixed by allowing
    // disjoint sets to be compared under inclusion with an
    // appropriate cross-compare)
    imm::map<package, action> actions_map;
    bool any_is_current = false;
    versions.for_each(build_joint_score_action_set(actions_map,
						   any_is_current,
						   initial_state));

    if(any_is_current)
      return;

    joint_scores_list.push_back(std::make_pair(versions, score));

    actions_map.for_each(add_to_joint_scores(joint_scores,
					     typename solution_weights<PackageUniverse>::joint_score(actions_map, score)));
  }

  const joint_score_set &get_joint_scores() const { return joint_scores; }
  const std::vector<std::pair<imm::set<version>, int> > &
  get_joint_scores_list() const { return joint_scores_list; }
};

template<typename PackageUniverse>
inline generic_solution<PackageUniverse>
generic_solution<PackageUniverse>::root_node(const imm::set<dep> &initial_broken,
					     const PackageUniverse &universe,
					     const solution_weights<PackageUniverse> &weights,
					     const resolver_initial_state<PackageUniverse> &initial_state)
{
  int score = initial_broken.size() * weights.broken_score;

  if(initial_broken.empty())
    score += weights.full_solution_score;

  return generic_solution(new solution_rep(imm::map<package, action>(),
					   initial_broken,
					   imm::set<dep>(),
					   imm::map<version, dep>(),
					   initial_state,
					   score,
					   0));
}


template<typename PackageUniverse>
template<typename a_iter, typename u_iter>
inline generic_solution<PackageUniverse>
generic_solution<PackageUniverse>::successor(const generic_solution &s,
					     const a_iter &abegin,
					     const a_iter &aend,
					     const u_iter &ubegin,
					     const u_iter &uend,
					     const PackageUniverse &universe,
					     const solution_weights<PackageUniverse> &weights)
{
  const resolver_initial_state<PackageUniverse>
    &initial_state(s.get_initial_state());

  imm::set<dep> broken_deps = s.get_broken();
  imm::map<package, action> actions = s.get_actions();
  imm::map<version, dep> forbidden_versions = s.get_forbidden_versions();
  imm::set<dep> unresolved_soft_deps = s.get_unresolved_soft_deps();
  int action_score = s.get_action_score();

  // Add notes about unresolved dependencies
  for(u_iter ui = ubegin; ui != uend; ++ui)
    {
      const dep &d = *ui;

      eassert(broken_deps.contains(d));
      broken_deps.erase(d);
      unresolved_soft_deps.insert(d);
    }

  for(a_iter ai = abegin; ai != aend; ++ai)
    {
      const action &a = *ai;
      eassert(!actions.domain_contains(a.ver.get_package()));
      eassert(a.ver != initial_state.version_of(a.ver.get_package()));

      actions.put(a.ver.get_package(), a);

      action_score += weights.step_score;
      action_score += weights.version_scores[a.ver.get_id()];
      action_score -= weights.version_scores[initial_state.version_of(a.ver.get_package()).get_id()];

      // Look for joint score constraints triggered by adding this
      // action.
      const typename solution_weights<PackageUniverse>::joint_score_set::const_iterator
	joint_scores_found = weights.get_joint_scores().find(a);
      if(joint_scores_found != weights.get_joint_scores().end())
	{
	  typedef typename solution_weights<PackageUniverse>::joint_score joint_score;
	  for(typename std::vector<joint_score>::const_iterator it =
		joint_scores_found->second.begin();
	      it != joint_scores_found->second.end(); ++it)
	    {
	      if(actions.is_supermap_of(it->get_actions()))
		action_score += it->get_score();
	    }
	}

      if(a.from_dep_source)
	{
	  for(typename dep::solver_iterator si = a.d.solvers_begin();
	      !si.end(); ++si)
	    forbidden_versions.put(*si, a.d);
	}



      // Update the set of broken dependencies, trying to re-use as
      // much of the former set as possible.
      version old_version=s.version_of(a.ver.get_package());
      solution_map_wrapper tmpsol(actions, initial_state);

      // Check reverse deps of the old version
      for(typename version::revdep_iterator rdi=old_version.revdeps_begin();
	  !rdi.end(); ++rdi)
	{
	  dep rd = *rdi;

	  if((rd).broken_under(tmpsol))
	    {
	      if(!unresolved_soft_deps.contains(rd))
		broken_deps.insert(rd);
	    }
	  else
	    {
	      broken_deps.erase(rd);
	      unresolved_soft_deps.erase(rd);
	    }
	}

      // Check reverse deps of the new version
      //
      // Because reverse deps of a version might be fixed by its
      // removal, we need to check brokenness and insert or erase as
      // appropriate.
      for(typename version::revdep_iterator rdi = a.ver.revdeps_begin();
	  !rdi.end(); ++rdi)
	{
	  dep rd = *rdi;

	  if((rd).broken_under(tmpsol))
	    {
	      if(!unresolved_soft_deps.contains(rd))
		broken_deps.insert(rd);
	    }
	  else
	    {
	      broken_deps.erase(rd);
	      unresolved_soft_deps.erase(rd);
	    }
	}

      // Remove all forward deps of the old version (they're
      // automagically fixed)
      for(typename version::dep_iterator di=old_version.deps_begin();
	  !di.end(); ++di)
	{
	  dep d = *di;

	  broken_deps.erase(d);
	  unresolved_soft_deps.erase(d);
	}

      // Check forward deps of the new version (no need to erase
      // non-broken dependencies since they're automatically
      // non-broken at the start)
      for(typename version::dep_iterator di=a.ver.deps_begin();
	  !di.end(); ++di)
	{
	  dep d = *di;

	  if(!unresolved_soft_deps.contains(d) &&
	     (d).broken_under(tmpsol))
	    broken_deps.insert(d);
	}
    }

  int score
    = action_score + broken_deps.size()*weights.broken_score
    + unresolved_soft_deps.size()*weights.unfixed_soft_score;

  if(broken_deps.empty())
    score += weights.full_solution_score;

  return generic_solution(new solution_rep(actions,
					   broken_deps,
					   unresolved_soft_deps,
					   forbidden_versions,
					   initial_state,
					   score,
					   action_score));
}

#endif // SOLUTION_H
