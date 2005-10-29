// aptitude_resolver_universe.cc                       -*-c++-*-

#include "aptitude_resolver_universe.h"

#include <generic/problemresolver/solution.h>

#include <sstream>

using namespace std;

static inline
bool ver_disappeared(const pkgCache::VerIterator ver)
{
  return
    !ver.Downloadable() &&
    (ver != ver.ParentPkg().CurrentVer() ||
     ver.ParentPkg()->CurrentState == pkgCache::State::ConfigFiles);
}


string aptitude_resolver_version::get_name() const
{
  if(!ver.end())
    {
      // If there are two distinct package files with the same
      // Version, apt will give them the same VerStr.  Detect and
      // compensate for this.
      int count=0;
      int idx=-1;

      for(pkgCache::VerIterator i=pkg.VersionList(); !i.end(); ++i)
	{
	  if(i==ver)
	    {
	      idx=count;
	      ++count;
	    }
	  else if(!strcmp(i.VerStr(), ver.VerStr()))
	    ++count;
	}

      // if not, we have a "version" of this package that's not in
      // its list of versions!
      assert(idx>=0);

      if(count>1)
	{
	  ostringstream s;
	  s << ver.VerStr() << "<" << idx+1 << ">";
	  return s.str();
	}
      else
	return ver.VerStr();
    }
  else
    // Note that this is an invalid version string for apt, so we
    // can't clash with real versions.
    return "[UNINST]";
}

bool aptitude_resolver_version::revdep_iterator::applicable() const
{
  if(!is_interesting_dep(dep_lst, cache))
    return false;

  // Unversioned deps always apply.
  if(!dep_lst.TargetVer())
    return true;

  if(provides_open)
    return _system->VS->CheckDep(prv_lst.ProvideVersion(),
				 dep_lst->CompareOp, dep_lst.TargetVer());
  else
    return _system->VS->CheckDep(ver.VerStr(),
				 dep_lst->CompareOp, dep_lst.TargetVer());
}

void aptitude_resolver_version::revdep_iterator::normalize()
{
  while(!dep_lst.end() && !applicable())
    ++dep_lst;

  if(dep_lst.end() && !provides_open)
    {
      assert(prv_lst.end());
      prv_lst=ver.ProvidesList();
      provides_open=true;
      if(!prv_lst.end())
	{
	  dep_lst=prv_lst.ParentPkg().RevDependsList();
	  while(!dep_lst.end() && !applicable())
	    ++dep_lst;
	}
    }

  // When we've run out of provides, give up..
  while(dep_lst.end() && !prv_lst.end())
    {
      assert(provides_open);
      ++prv_lst;

      if(!prv_lst.end())
	{
	  assert(!prv_lst.ParentPkg().end());
	  dep_lst=prv_lst.ParentPkg().RevDependsList();

	  while(!dep_lst.end() && !applicable())
	    ++dep_lst;
	}
    }
}

void aptitude_resolver_version::dep_iterator::normalize()
{
  if(prv_open)
    {
      assert(!dep.end());
      assert(dep->Type == pkgCache::Dep::Conflicts);

      while(!prv.end() && prv.OwnerPkg()==dep.ParentPkg())
	++prv;

      if(prv.end())
	{
	  prv_open=false;
	  ++dep;
	}
      else
	return;
    }

  assert(!prv_open);


  // Skip non-critical and self dependencies.  Need to do this here
  // as well as below in case dep already points to such a
  // dependency.
  while(!dep.end() &&
	(dep.ParentPkg() == dep.TargetPkg() ||
	 !is_interesting_dep(dep, cache)))
    ++dep;

  // If we ran out of deps, we're done!
}



void aptitude_resolver_dep::solver_iterator::normalize()
{
  if(dep_lst->Type != pkgCache::Dep::Conflicts)
    {
      while(!end())
	{
	  while(!ver_lst.end())
	    {
	      bool ver_matches =
		!dep_lst.TargetVer() ||
		_system->VS->CheckDep(ver_lst.VerStr(),
				      dep_lst->CompareOp,
				      dep_lst.TargetVer());

	      if(ver_matches && ver_disappeared(ver_lst))
		ver_matches = false;

	      if(ver_matches)
		// Found the next entry; drop out.
		return;
	      else
		++ver_lst;
	    }

	  // If we ran out of versions, try provides instead.
	  while(!prv_lst.end())
	    {
	      bool prv_matches=(!dep_lst.TargetVer()) ||
		(prv_lst.ProvideVersion() &&
		 _system->VS->CheckDep(prv_lst.ProvideVersion(),
				       dep_lst->CompareOp,
				       dep_lst.TargetVer()));

	      if(prv_matches &&
		 !ver_disappeared(prv_lst.OwnerVer()))
		return;
	      else
		++prv_lst;
	    }

	  // No more target versions or providers of the target;
	  // increment the dependency list if we aren't yet at the
	  // end of the OR group.

	  if(!(dep_lst->CompareOp & pkgCache::Dep::Or))
	    finished=true;
	  else
	    {
	      ++dep_lst;
	      // Since we aren't finished, dep_lst should still be
	      // valid.
	      assert(!dep_lst.end());
	      ver_lst=dep_lst.TargetPkg().VersionList();

	      // Only set the prv_lst to non-end if there is no target
	      // version.
	      prv_lst=dep_lst.TargetPkg().ProvidesList();
	    }
	}
    }
  else
    {
      // For Conflicts, we're iterating over all the versions of
      // *one* package for *one* dep, either the owner of the
      // dep or a provided package.  (prv_lst is mostly
      // unnecessary, but it makes it simple to remember whether
      // we have a provides).  Note that the efficiency of this
      // stanza is based on the *assumption* that most packages
      // only Provide a few things.

      // For provided packages, return exactly those packages
      // that *don't* have a matching Provides.
      if(!prv_lst.end())
	{
	  while(!ver_lst.end())
	    {
	      if(ver_lst != prv_lst.OwnerVer() &&
		 !ver_disappeared(ver_lst))
		return;

	      ++ver_lst;
	    }
	  // Important point: end version iterators always match
	  // a Conflicts! (i.e., any Conflicts can be resolved
	  // by removing the conflicted package)
	  return;
	}
      else
	{
	  while(!ver_lst.end())
	    {
	      bool ver_matches=(!dep_lst.TargetVer()) ||
		(ver_lst.VerStr() &&
		 _system->VS->CheckDep(ver_lst.VerStr(),
				       dep_lst->CompareOp,
				       dep_lst.TargetVer()));

	      if(!ver_matches && !ver_disappeared(ver_lst))
		// This version resolves the conflict.
		return;
	      else
		++ver_lst;
	    }

	  // Ignore provides; as above, end versions are A-OK.
	  return;
	}
    }
}

bool aptitude_resolver_dep::solved_by(const aptitude_resolver_version &v) const
{
  // First check for moving the source.
  if(v.get_pkg() == const_cast<pkgCache::DepIterator &>(start).ParentPkg() && v.get_ver() != const_cast<pkgCache::DepIterator &>(start).ParentVer())
    return true;

  // Now check each of the members of the OR group.
  pkgCache::DepIterator d = start;

  if(start->Type != pkgCache::Dep::Conflicts)
    {
      // Of course, installing an end version never fixes a
      // non-conflict unless it removes the source (tested for above).
      if(v.get_ver().end())
	return false;

      while(1)
	{
	  if(d.TargetPkg() == v.get_pkg() &&
	     (!d.TargetVer() || _system->VS->CheckDep(v.get_ver().VerStr(),
						      d->CompareOp,
						      d.TargetVer())))
	    return true;

	  // Check for a resolution via Provides.
	  if(!d.TargetVer())
	    for(pkgCache::PrvIterator p2 = v.get_ver().ProvidesList();
		!p2.end(); ++p2)
	      if(const_cast<pkgCache::PrvIterator &>(p2).ParentPkg() == d.TargetPkg())
		return true;

	  if((d->CompareOp & pkgCache::Dep::Or) != 0)
	    ++d;
	  else
	    return false;
	}
    }
  else if(prv.end())
    {
      if(d.TargetPkg() != v.get_pkg())
	return false;

      if(v.get_ver().end())
	return true;

      // Check the non-virtual part of the conflict: the package is
      // the same and the version **doesn't** match.
      return !(!d.TargetVer() || _system->VS->CheckDep(v.get_ver().VerStr(),
						       d->CompareOp,
						       d.TargetVer()));
    }
  else
    {
      // Only other versions of the provider can solve this.
      if(v.get_pkg() != const_cast<pkgCache::PrvIterator &>(prv).OwnerPkg())
	return false;
      else
	return v.get_ver() != const_cast<pkgCache::PrvIterator &>(prv).OwnerVer();
    }
}

aptitude_resolver_dep::solver_iterator &aptitude_resolver_dep::solver_iterator::operator++()
{
  assert(!end());

  // Advance whatever needs to be advanced next in the
  // sub-list.

  if(!ver_lst.end())
    ++ver_lst;
  else if(dep_lst->Type != pkgCache::Dep::Conflicts)
    {
      if(!prv_lst.end())
	++prv_lst;
    }
  else
    finished=true;

  normalize();

  return *this;
}

aptitude_resolver_version::dep_iterator &aptitude_resolver_version::dep_iterator::operator++()
{
  assert(!dep.end());

  // If the Provides list is nonempty, advance it.
  if(!prv.end())
    ++prv;
  // If we weren't trying to iterate over a Provides list *and* the
  // current dep is a non-versioned Conflicts, start such an
  // iteration.
  else if(!prv_open && dep->Type == pkgCache::Dep::Conflicts &&
	  !dep.TargetVer())
    {
      prv_open=true;
      prv=dep.TargetPkg().ProvidesList();
    }
  // Otherwise push on to the next top-level dep.
  else
    {
      if(!dep.end() && dep->Type == pkgCache::Dep::Conflicts)
	++dep;
      else
	{
	  // If it's not a conflict, skip a whole OR group.
	  while(!dep.end() && (dep->CompareOp & pkgCache::Dep::Or))
	    ++dep;

	  // Now we're on the last element of the OR group, push
	  // forward.
	  if(!dep.end())
	    ++dep;
	}
    }

  normalize();

  return *this;
}

aptitude_resolver_version aptitude_resolver_dep::solver_iterator::operator*() const
{
  assert(!end());

  if(!ver_lst.end())
    return aptitude_resolver_version(ver_lst.ParentPkg(),ver_lst,cache);
  else // In this case we're trying to remove some package or other.
    {
      if(dep_lst->Type != pkgCache::Dep::Conflicts)
	{
	  // Assume this because otherwise end() should be true.
	  assert(!prv_lst.end());

	  return aptitude_resolver_version(const_cast<pkgCache::PrvIterator &>(prv_lst).OwnerPkg(),const_cast<pkgCache::PrvIterator &>(prv_lst).OwnerVer(),cache);
	}
      else if(!prv_lst.end())
	return aptitude_resolver_version(const_cast<pkgCache::PrvIterator &>(prv_lst).OwnerPkg(),ver_lst,cache);
      else
	return aptitude_resolver_version(const_cast<pkgCache::DepIterator &>(dep_lst).TargetPkg(),ver_lst,cache);
    }
}

void aptitude_universe::dep_iterator::normalize()
{
  while(dep.end() && !pkg.end())
    {
      while(dep.end() && !ver.end())
	{
	  ++ver;
	  if(!ver.end())
	    dep=aptitude_resolver_version::dep_iterator(ver, cache);
	}

      if(dep.end())
	{
	  ++pkg;
	  if(!pkg.end())
	    {
	      ver=pkg.VersionList();
	      if(!ver.end())
		dep=aptitude_resolver_version::dep_iterator(ver, cache);
	    }
	}
    }
}

bool aptitude_universe::broken_dep_iterator::dep_is_inst_broken(const pkgCache::DepIterator &d) const
{
  pkgCache::DepIterator d2=d;

  while(d2->CompareOp & pkgCache::Dep::Or)
    ++d2;

  return ((*cache)[d2] & pkgDepCache::DepGInstall)==0;
}

void aptitude_universe::broken_dep_iterator::normalize()
{
  while(!the_dep.end() &&
	!(is_interesting_dep(the_dep, cache) &&
	  dep_is_inst_broken(the_dep)))
    ++the_dep;

  while(the_dep.end() && !pkg.end())
    {
      // Make sure we move at least one package forward!
      // Otherwise we just spin on the same package over and over,
      // since it's still broken..
      ++pkg;

      if(!pkg.end())
	{
	  // Examine just the InstVer of the package.
	  pkgCache::VerIterator ver=(*cache)[pkg].InstVerIter(*cache);

	  if(!ver.end())
	    the_dep=ver.DependsList();

	  while(!the_dep.end() &&
		!(is_interesting_dep(the_dep, cache) &&
		  dep_is_inst_broken(the_dep)))
	    ++the_dep;

	  if(the_dep.end())
	    ++ver;
	}
    }

  assert(the_dep.end() || is_interesting_dep(the_dep, cache));

  // Now dep is a broken critical dep or an end dep.  If it is a
  // conflicts, we might need to push down into Provides...
  if(!the_dep.end() && the_dep->Type == pkgCache::Dep::Conflicts)
    {
      // If we aren't in provides, check whether the dep is
      // trivially broken (i.e., without following provides).
      if(!prv_open)
	{
	  // If it's a direct self-conflict, jump into provides
	  // right away.
	  if(the_dep.TargetPkg() != the_dep.ParentPkg())
	    {
	      pkgCache::VerIterator ver=(*cache)[the_dep.TargetPkg()].InstVerIter(*cache);

	      if(!ver.end() &&
		 !ver_disappeared(ver) &&
		 (!the_dep.TargetVer() ||
		  (ver.VerStr() &&
		   _system->VS->CheckDep(ver.VerStr(),
					 the_dep->CompareOp,
					 the_dep.TargetVer()))))
		// OK, the dep is broken without provides, no need
		// to descend.
		return;
	    }

	  prv_open=true;
	  prv=the_dep.TargetPkg().ProvidesList();
	}

      // Ok, we have found something that causes breakage.  The
      // provides-list is a list of all the package versions that
      // provide this package name; move forward until we find one
      // that matches.
      while(!prv.end())
	{
	  // Ignore indirect self-conflicts.
	  if(prv.OwnerPkg() != the_dep.ParentPkg())
	    {
	      // First, is the providing version going to be
	      // installed?
	      if((*cache)[prv.OwnerPkg()].InstVerIter(*cache) == prv.OwnerVer())
		{
		  // Ok, does it match the version string?
		  bool matches =
		    !the_dep.TargetVer() ||
		    (prv.ProvideVersion() &&
		     _system->VS->CheckDep(prv.ProvideVersion(),
					   the_dep->CompareOp,
					   the_dep.TargetVer()));

		  if(ver_disappeared(prv.OwnerVer()))
		    matches = false;

		  if(matches)
		    return;
		}
	    }

	  ++prv;
	}

      // if all provides are exhausted, increment the dep and try
      // again.  (probably this was only a self-conflict and
      // nothing else)
      ++the_dep;
      prv_open=false;
      normalize();
      // hopefully g++ is smart enough to optimize this into a
      // tail call.
      return;
    }
}

aptitude_universe::broken_dep_iterator &aptitude_universe::broken_dep_iterator::operator++()
{
  assert(!pkg.end());
  // If the_dep.end() we have pkg.end().
  assert(!the_dep.end());

  if(!prv_open && the_dep->Type == pkgCache::Dep::Conflicts)
    {
      prv_open = true;
      prv = the_dep.TargetPkg().ProvidesList();
    }
  else if(prv_open && !prv.end())
    ++prv;
  else
    ++the_dep;

  normalize();
  return *this;
}

std::ostream &operator<<(ostream &out, aptitude_resolver_dep d)
{
  std::vector<aptitude_resolver_version> solvers;
  for(aptitude_resolver_dep::solver_iterator i=d.solvers_begin(); !i.end(); ++i)
    solvers.push_back(*i);

  generic_solution<aptitude_universe>::ver_name_lt lt;
  sort(solvers.begin(), solvers.end(), lt);

  out << d.get_source().get_package().get_name()
      << " "
      << d.get_source().get_name()
      << " -> {";

  for(std::vector<aptitude_resolver_version>::const_iterator i = solvers.begin();
      i != solvers.end(); ++i)
    {
      if(i != solvers.begin())
	out << " ";
      out << (*i).get_package().get_name() << " "
	  << (*i).get_name();
    }
  out << "}";

  return out;
}
