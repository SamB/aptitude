// solution.h                                             -*-c++-*-
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
#include <generic/util/immset.h>

/** Represents the current score weights for a resolver.  Used to
 *  calculate scores at the time a solution is instantiated.
 */
struct solution_weights
{
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

  solution_weights(int _step_score, int _broken_score,
		   int _unfixed_soft_score, int _full_solution_score,
		   unsigned long num_versions)
    :step_score(_step_score), broken_score(_broken_score),
     unfixed_soft_score(_unfixed_soft_score),
     full_solution_score(_full_solution_score),
     version_scores(new int[num_versions])
  {
    for(unsigned long i = 0; i < num_versions; ++i)
      version_scores[i] = 0;
  }

  ~solution_weights()
  {
    delete[] version_scores;
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

  /** Represents a single action taken by the resolver: the
   *  installation of a particular version of a package.  The
   *  *identity* of an action (in terms of operator< and operator==)
   *  is based solely on the version it installs, although additional
   *  information is provided to "tag" it.
   */
  struct action
  {
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

    action() {}

    action(const version &_ver,
	   const dep &_d,
	   bool _from_dep_source,
	   int _id)
      : ver(_ver), d(_d),
	from_dep_source(_from_dep_source), id(_id)
    {
    }

    bool operator<(const action &other) const {return ver<other.ver;}

    bool operator==(const action &other) const
    {
      return ver == other.ver;
    }

    bool operator!=(const action &other) const
    {
      return ver != other.ver;
    }
  };

private:
  /** Hide this, it's meaningless. */
  bool operator<(const generic_solution &other) const;

  class solution_rep
  {
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
		 int _score,
		 int _action_score)
      : actions(_actions),
	broken_deps(_broken_deps),
	unresolved_soft_deps(_unresolved_soft_deps),
	forbidden_versions(_forbidden_versions),
	score(_score),
	action_score(_action_score),
	refcount(1)
    {
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
	return pkg.current_version();
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
  public:
    solution_map_wrapper(const imm::map<package, action> &_actions)
      :actions(_actions)
    {
    }

    version version_of(const package &p) const
    {
      typename imm::map<package, action>::node found
	= actions.lookup(p);

      if(found.isValid())
	return found.getVal().second.ver;
      else
	return p.current_version();
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
					     get_score(),
					     get_action_score()));
  }


  /** Generate the root node for a search in the given universe. */
  static generic_solution root_node(const imm::set<dep> &initial_broken,
				    const PackageUniverse &universe,
				    const solution_weights &weights)
  {
    int score = initial_broken.size() * weights.broken_score;

    if(initial_broken.empty())
      score += weights.full_solution_score;

    return generic_solution(new solution_rep(imm::map<package, action>(),
					     initial_broken,
					     imm::set<dep>(),
					     imm::map<version, dep>(),
					     score,
					     0));
  }

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
				    const solution_weights &weights)
  {
    // NB: as I fully expect to move to a scheme of shared-memory
    // sets/maps, the implicit copies here will go away eventually.
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
	eassert(a.ver != a.ver.get_package().current_version());

	actions.put(a.ver.get_package(), a);

	action_score += weights.step_score;
	action_score += weights.version_scores[a.ver.get_id()];
	action_score -= weights.version_scores[a.ver.get_package().current_version().get_id()];

	if(a.from_dep_source)
	  {
	    for(typename dep::solver_iterator si = a.d.solvers_begin();
		!si.end(); ++si)
	      forbidden_versions.put(*si, a.d);
	  }



	// Update the set of broken dependencies, trying to re-use as
	// much of the former set as possible.
	version old_version=s.version_of(a.ver.get_package());
	solution_map_wrapper tmpsol(actions);

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
					     score,
					     action_score));
  }

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

  void dump(std::ostream &out) const
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

#endif // SOLUTION_H
