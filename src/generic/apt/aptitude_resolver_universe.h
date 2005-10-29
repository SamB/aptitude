// aptitude_resolver_universe.h                     -*-c++-*-
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

#ifndef APTITUDE_RESOLVER_UNIVERSE_H
#define APTITUDE_RESOLVER_UNIVERSE_H

#include <apt-pkg/pkgcache.h>
#include <apt-pkg/pkgsystem.h>
#include <apt-pkg/version.h>

#include "apt.h"
#include "aptcache.h"

class aptitude_resolver_version;

/** Wraps a PkgIterator for the resolver. */
class aptitude_resolver_package
{
  pkgDepCache *cache;
  pkgCache::PkgIterator pkg;
public:
  aptitude_resolver_package()
    :cache(0)
  {
  }

  aptitude_resolver_package(const pkgCache::PkgIterator &_pkg,
			    pkgDepCache *_cache)
    :cache(_cache), pkg(_pkg)
  {
    assert(cache!=0);
    assert(pkg.Cache()!=0);
  }

  unsigned int get_id() const
  {
    return pkg->ID;
  }

  const char *get_name() const
  {
    return pkg.Name();
  }

  pkgCache::PkgIterator get_pkg() const
  {
    return pkg;
  }

  bool operator==(const aptitude_resolver_package &other) const
  {
    return pkg==other.pkg;
  }

  bool operator!=(const aptitude_resolver_package &other) const
  {
    return pkg!=other.pkg;
  }

  bool operator<(const aptitude_resolver_package &other) const
  {
    return ((const pkgCache::Package *) pkg) < ((const pkgCache::Package *) other.pkg);
  }

  aptitude_resolver_version current_version() const;

  class version_iterator;

  version_iterator versions_begin() const;
};

/** Wraps a package/version pair for the resolver. */
class aptitude_resolver_version
{
  pkgDepCache *cache;
  pkgCache::PkgIterator pkg;
  pkgCache::VerIterator ver;
public:
  aptitude_resolver_version()
    :cache(0)
  {
  }

  aptitude_resolver_version(const pkgCache::PkgIterator &_pkg,
			    const pkgCache::VerIterator &_ver,
			    pkgDepCache *_cache)
    :cache(_cache), pkg(_pkg), ver(_ver)
  {
    assert(cache!=0);
    assert(pkg.Cache()!=0);
    assert(ver.Cache()!=0);
  }

  pkgCache::PkgIterator get_pkg() const
  {
    return pkg;
  }

  pkgCache::VerIterator get_ver() const
  {
    return ver;
  }

  unsigned int get_id() const
  {
    if(!ver.end())
      return ver->ID;
    else
      // non-installed versions are faked.
      //
      // If this eats a lot of time, it could be memoized..but then
      // there's more to copy.  I could also teach the resolver about
      // "null" versions...but that would mean a bunch of pointless
      // special-casing caller-side anyway.
      return cache->Head().VersionCount+pkg->ID;
  }

  std::string get_name() const;

  aptitude_resolver_package get_package() const
  {
    return aptitude_resolver_package(pkg, cache);
  }

  bool operator==(const aptitude_resolver_version &other) const
  {
    return pkg == other.pkg && ver == other.ver;
  }

  bool operator!=(const aptitude_resolver_version &other) const
  {
    return pkg != other.pkg || ver != other.ver;
  }

  bool operator<(const aptitude_resolver_version &other) const
  {
    if(((const pkgCache::Package *) pkg) < ((const pkgCache::Package *) other.pkg))
      return true;
    else if(((const pkgCache::Package *) other.pkg) < ((const pkgCache::Package *) pkg))
      return false;
    else if(((const pkgCache::Version *) ver) < ((const pkgCache::Version *) other.ver))
      return true;
    else // if(((pkgCache::Version *) other.ver) < ((pkgCache::Version *) ver))
      return false;
  }

  class revdep_iterator;
  class dep_iterator;

  revdep_iterator revdeps_begin() const;
  dep_iterator deps_begin() const;
};

inline aptitude_resolver_version aptitude_resolver_package::current_version() const
{
  // Transmute removed-with-config-files packages into not-installed
  // packages.
  if((*cache)[pkg].Keep() &&
     pkg->CurrentState == pkgCache::State::ConfigFiles)
    return aptitude_resolver_version(pkg, pkgCache::VerIterator(*cache, 0), cache);
  else
    return aptitude_resolver_version(pkg, (*cache)[pkg].InstVerIter(*cache),
				     cache);
}

class aptitude_resolver_dep
{
  pkgDepCache *cache;
  pkgCache::DepIterator start;
  /** If start is a Conflicts and prv is not an end iterator, then the
   *  object represents "V -> {V'_1 V'_2 ..} where the V'-s are
   *  versions of prv.OwnerPkg() that do *not* provide V.ParentPkg().
   *  Otherwise, if start is a Conflicts and prv is an end iterator,
   *  the object represents the non-virtual part of the Conflicts; if
   *  start is not a Conflicts, prv is unused.
   *
   *  All that discussion is mainly important when checking if the dep
   *  is broken and/or when finding its solvers.
   */
  pkgCache::PrvIterator prv;
public:
  /** \todo this is a dreadful hack, but it's needed to avoid crashing
   *        until PrvIterators get a sensible default constructor.
   */
  aptitude_resolver_dep()
    :cache(0), prv(*apt_cache_file, (pkgCache::Provides *) 1, (pkgCache::Package *) 0)
  {
  }

  aptitude_resolver_dep(const pkgCache::DepIterator dep,
			const pkgCache::PrvIterator _prv,
			pkgDepCache *_cache)
    :cache(_cache), prv(_prv)
  {
    assert(cache!=0);
    assert(const_cast<pkgCache::DepIterator &>(dep).Cache()!=0);
    assert(prv.Cache()!=0);
    assert(!dep.end());
    if(dep->Type != pkgCache::Dep::Conflicts)
      {
	// Throw away the end, since it's not necessary.
	pkgCache::DepIterator end;
	surrounding_or(dep, start, end, &cache->GetCache());
      }
    else
      // Ignore ORs and just use the selected conflict.
      //
      //  NOTE: as of this writing, no known package does something as
      // stupid as ORed conflicts.
      start=dep;
  }

  bool is_soft() const
  {
    return start->Type == pkgCache::Dep::Recommends;
  }

  bool operator==(const aptitude_resolver_dep &other) const
  {
    return start == other.start &&
      (start->Type != pkgCache::Dep::Conflicts || prv == other.prv);
  }

  bool operator!=(const aptitude_resolver_dep &other) const
  {
    return start != other.start ||
      (start->Type == pkgCache::Dep::Conflicts && prv != other.prv);
  }

  bool operator<(const aptitude_resolver_dep &other) const
  {
    if(((const pkgCache::Dependency *) start) < ((const pkgCache::Dependency *) other.start))
      return true;
    else if(((const pkgCache::Dependency *) start) > ((const pkgCache::Dependency *) other.start))
      return false;
    else if(start->Type != pkgCache::Dep::Conflicts)
      return false;
    else if(((const pkgCache::Provides *) prv) < ((const pkgCache::Provides *) other.prv))
      return true;
    else
      return false;
  }

  /** SolutionType is a class defining the method ST.version_of(pkg),
   *  which returns the installed version of pkg according to ST.
   */
  template<class InstallationType>
  bool broken_under(const InstallationType &I) const;

  pkgCache::DepIterator get_dep() const
  {
    return start;
  }

  pkgCache::PrvIterator get_prv() const
  {
    return prv;
  }

  /** Return \b true if the given version will resolve this dependency. */
  bool solved_by(const aptitude_resolver_version &v) const;

  aptitude_resolver_version get_source() const
  {
    assert(!start.end());
    assert(!const_cast<pkgCache::DepIterator &>(start).ParentPkg().end());
    return aptitude_resolver_version(const_cast<pkgCache::DepIterator &>(start).ParentPkg(),
				     const_cast<pkgCache::DepIterator &>(start).ParentVer(),
				     cache);
  }

  class solver_iterator;

  solver_iterator solvers_begin() const;
};

class aptitude_resolver_package::version_iterator
{
  pkgDepCache *cache;
  pkgCache::PkgIterator pkg;
  pkgCache::VerIterator ver;

  void normalize()
  {
    // This loop should only trigger once.
    while(!ver.end() &&
	  !ver.Downloadable() &&
	  (ver != pkg.CurrentVer() || pkg->CurrentState == pkgCache::State::ConfigFiles))
    ++ver;
  }
public:
  version_iterator()
    :cache(0)
  {
  }

  version_iterator(pkgCache::PkgIterator _pkg,
		   pkgCache::VerIterator _ver,
		   pkgDepCache *_cache)
    :cache(_cache), pkg(_pkg), ver(_ver)
  {
  }

  // For aptitude-specific code:
  pkgCache::PkgIterator get_pkg() {return pkg;}
  pkgCache::VerIterator get_ver() {return ver;}

  bool operator==(const version_iterator &other) const
  {
    return pkg == other.pkg && ver == other.ver;
  }

  bool operator!=(const version_iterator &other) const
  {
    return pkg != other.pkg || ver != other.ver;
  }

  aptitude_resolver_version operator *() const
  {
    return aptitude_resolver_version(pkg, ver, cache);
  }

  version_iterator &operator++()
  {
    if(!ver.end())
      {
	++ver;
	normalize();
      }
    else
      pkg=pkgCache::PkgIterator();
    return *this;
  }

  bool end() const
  {
    return pkg.end();
  }
};

inline aptitude_resolver_package::version_iterator aptitude_resolver_package::versions_begin() const
{
  return version_iterator(pkg, pkg.VersionList(), cache);
}

/** \note For our purposes, conflicts are revdeps of the conflicted
 *  package version(s): this is much easier to calculate than the
 *  alternative and it is sufficient (since we only need to make sure
 *  that newly broken deps due to p:v1->p:v2 show up in either v1's
 *  revdep list or v2's revdep list).
 */
class aptitude_resolver_version::revdep_iterator
{
  pkgDepCache *cache;
  /** The Depends which is currently being tried. */
  pkgCache::DepIterator dep_lst;
  /** The Provides which is currently being tried. */
  pkgCache::PrvIterator prv_lst;
  /** The package version to which this dep should apply (used
   *  to check versioned deps).
   */
  pkgCache::VerIterator ver;
  /** Whether we've started looking at provides yet. */
  bool provides_open;

  /** Advance to the next valid iterator. */
  void normalize();

  /** \return true if dep_lst applies to ver: this is, if it is
   *               a strong dependency on ver.
   */
  bool applicable() const;
public:
#if 0
  revdep_iterator()
    :cache(0)
  {
  }
#endif

  /** Generate a revdep_iterator to cover the reverse deps of
   *  the given version. (note that v may be an end iterator..)
   */
  revdep_iterator(const pkgCache::VerIterator &v,
		  pkgDepCache *_cache)
    :cache(_cache),
     prv_lst(*_cache, 0, (pkgCache::Package *) 0), ver(v),
     provides_open(false)
  {
    // Note that if v is an end iterator, we present an empty list and
    // hence don't need to know its package.  This is safe because the
    // [UNINST] version has no reverse dependencies (except conflicts,
    // but those are handled in the usual way).
    if(!v.end())
      dep_lst=v.ParentPkg().RevDependsList();
    else
      // Immediately flag this as an end iterator, and avoid crashing
      // in normalize() when we look at v.ProvidesList().
      provides_open=true;
    normalize();
  }

//   bool operator==(const revdep_iterator &other) const
//   {
//     return dep == other.dep && ver == other.ver;
//   }

//   bool operator!=(const revdep_iterator &other) const
//   {
//     return dep != other.dep || ver != other.ver;
//   }

  bool end() const
  {
    return dep_lst.end();
  }

  aptitude_resolver_dep operator*() const
  {
    return aptitude_resolver_dep(dep_lst, prv_lst, cache);
  }

  revdep_iterator &operator++()
  {
    ++dep_lst;
    normalize();

    return *this;
  }
};

/** Iterates over the distinct dependencies of a version.  These
 *  include its direct/indirect dependencies (separated into OR
 *  groups) and its direct/indirect conflicts (one apiece).
 */
class aptitude_resolver_version::dep_iterator
{
  pkgDepCache *cache;
  pkgCache::DepIterator dep;
  pkgCache::PrvIterator prv;
  /** If \b true, then dep is a Conflicts and we are iterating over
   *  the packages providing its target.
   */
  bool prv_open;

  void normalize();

public:
  dep_iterator(pkgDepCache *_cache)
    :cache(_cache),
     prv(*_cache, 0, (pkgCache::Package *) 0),
     prv_open(false)
  {
  }


  dep_iterator(const pkgCache::DepIterator &start,
	       pkgDepCache *_cache)
    :cache(_cache),
     dep(start),
     prv(*_cache, 0, (pkgCache::Package *) 0),
     prv_open(false)
  {
    normalize();
  }

  dep_iterator &operator=(const dep_iterator &other)
  {
    cache=other.cache;
    dep=other.dep;
    prv=other.prv;
    prv_open=other.prv_open;

    return *this;
  }

  aptitude_resolver_dep operator*() const
  {
    return aptitude_resolver_dep(dep, prv, cache);
  }

  bool end() const
  {
    return dep.end();
  }

  dep_iterator &operator++();
};

inline aptitude_resolver_version::revdep_iterator aptitude_resolver_version::revdeps_begin() const
{
  return revdep_iterator(ver, cache);
}

inline aptitude_resolver_version::dep_iterator aptitude_resolver_version::deps_begin() const
{
  if(ver.end())
    return dep_iterator(cache);
  else
    return dep_iterator(ver.DependsList(), cache);
}

/** This class uses a technique similar to rev_dep_lst.  It assumes
 *  that the dependency is critical (noncritical deps are weeded out
 *  by the universe's broken_dep_iterator and hidden from the
 *  resolver).
 *
 *  For Conflicts, at most one Provides is followed.
 *
 *  Ugh, too many members :(.
 */
class aptitude_resolver_dep::solver_iterator
{
  pkgDepCache *cache;

  pkgCache::DepIterator dep_lst;
  pkgCache::VerIterator ver_lst;
  pkgCache::PrvIterator prv_lst;
  /** \b true if we exhausted all options; needed because
   *          dep_lst might not be an end iterator (since it'll
   *          move to the next OR group)
   */
  bool finished;

  /** Advance to the next interesting version/provides -- i.e., skip
   *  uninteresting ones.
   */
  void normalize();

public:
  solver_iterator(const pkgCache::DepIterator &start,
		  pkgDepCache *_cache)
    :cache(_cache),
     dep_lst(start),
     prv_lst(*cache, 0, (pkgCache::Package *) 0),
     finished(start.end())
  {
    if(!dep_lst.end())
      {
	assert(dep_lst->Type != pkgCache::Dep::Conflicts);

	ver_lst=const_cast<pkgCache::DepIterator &>(start).TargetPkg().VersionList();
	prv_lst=const_cast<pkgCache::DepIterator &>(start).TargetPkg().ProvidesList();
      }

    normalize();
  }

  /** Initialize a Conflicts solution iterator. */
  solver_iterator(const pkgCache::DepIterator &d,
		  const pkgCache::PrvIterator &p,
		  pkgDepCache *_cache)
    :cache(_cache), dep_lst(d), prv_lst(p), finished(d.end())
  {
    if(!dep_lst.end())
      {
	assert(d->Type == pkgCache::Dep::Conflicts);
	// Either we're looking at all versions of the named dep, or
	// at all versions of the providing package.
	if(prv_lst.end())
	  ver_lst=const_cast<pkgCache::DepIterator &>(dep_lst).TargetPkg().VersionList();
	else
	  ver_lst=const_cast<pkgCache::PrvIterator &>(p).OwnerPkg().VersionList();
      }

    normalize();
  }

#if 0
  solver_iterator()
    :cache(0),
     // shouldn't do this, but otherwise we crash!!
     prv_lst(*apt_cache_file, 0, (pkgCache::Package *) 0), finished(true)
  {
  }
#endif

  bool operator==(const solver_iterator &other) const
  {
    return dep_lst == other.dep_lst &&
      ver_lst == other.ver_lst &&
      prv_lst == other.prv_lst &&
      finished == other.finished;
  }

  bool operator!=(const solver_iterator &other) const
  {
    return dep_lst != other.dep_lst ||
      ver_lst != other.ver_lst ||
      prv_lst != other.prv_lst ||
      finished != other.finished;
  }

  solver_iterator &operator++();

  aptitude_resolver_version operator*() const;

  bool end() const
  {
    return finished;
  }
};

inline aptitude_resolver_dep::solver_iterator aptitude_resolver_dep::solvers_begin() const
{
  if(start->Type != pkgCache::Dep::Conflicts)
    return solver_iterator(start, cache);
  else
    return solver_iterator(start, prv, cache);
}

template<class InstallationType>
bool aptitude_resolver_dep::broken_under(const InstallationType &I) const
{
  // First, check that the solution actually installs the source.
  if(const_cast<pkgCache::DepIterator &>(start).ParentVer() != I.version_of(aptitude_resolver_package(const_cast<pkgCache::DepIterator &>(start).ParentPkg(), cache)).get_ver())
    return false;

  if(start->Type != pkgCache::Dep::Conflicts)
    {
      pkgCache::DepIterator dep=start;

      while(!dep.end())
	{
	  pkgCache::VerIterator direct_ver=I.version_of(aptitude_resolver_package(dep.TargetPkg(), cache)).get_ver();
	  if(!direct_ver.end() &&
	     _system->VS->CheckDep(direct_ver.VerStr(),
				   dep->CompareOp,
				   dep.TargetVer()))
	    return false;

	  if(!dep.TargetVer())
	    {
	      for(pkgCache::PrvIterator prv=dep.TargetPkg().ProvidesList();
		  !prv.end(); ++prv)
		if(prv.OwnerVer() == I.version_of(aptitude_resolver_package(prv.OwnerPkg(), cache)).get_ver())
		  return false;
	    }

	  if(!(dep->CompareOp & pkgCache::Dep::Or))
	    break;
	  ++dep;
	}

      return true;
    }
  else
    {
      // Recall that a Conflicts dep iterator is looking at a single
      // element of the Conflicts: either a direct conflict or an
      // indirect conflict (i.e., via a virtual pkg).

      if(prv.end())
	{
	  if(const_cast<pkgCache::DepIterator &>(start).TargetPkg() == const_cast<pkgCache::DepIterator &>(start).ParentPkg())
	    return false;

	  pkgCache::VerIterator direct_ver=I.version_of(aptitude_resolver_package(const_cast<pkgCache::DepIterator &>(start).TargetPkg(), cache)).get_ver();

	  if(!direct_ver.end() &&
	     _system->VS->CheckDep(direct_ver.VerStr(),
				   start->CompareOp,
				   start.TargetVer()))
	    return true;
	  else
	    return false;
	}
      else
	{
	  if(const_cast<pkgCache::PrvIterator &>(prv).OwnerPkg() == const_cast<pkgCache::DepIterator &>(start).ParentPkg())
	    return false;

	  if(start.TargetVer())
	    return false;

	  return I.version_of(aptitude_resolver_package(const_cast<pkgCache::PrvIterator &>(prv).OwnerPkg(), cache)).get_ver()==const_cast<pkgCache::PrvIterator &>(prv).OwnerVer();
	}
    }
}

/** \brief This class translates an APT package system into the
 *  abstract package system as described in \ref abstract_universe.
 *
 *  \sa \ref universe_universe
 */
class aptitude_universe
{
  aptitudeDepCache *cache;

  aptitude_universe();
public:
  typedef aptitude_resolver_package package;
  typedef aptitude_resolver_version version;
  typedef aptitude_resolver_dep dep;

  aptitude_universe(aptitudeDepCache *_cache)
    :cache(_cache)
  {
  }

  aptitudeDepCache *get_cache() const {return cache;}

  class package_iterator
  {
    pkgDepCache *cache;
    pkgCache::PkgIterator realiter;
  public:
    package_iterator()
      :cache(0)
    {
    }

    package_iterator(pkgCache::PkgIterator _realiter,
		     pkgDepCache *_cache)
      :cache(_cache), realiter(_realiter)
    {
    }

    bool operator==(const package_iterator &other) const
    {
      return realiter==other.realiter;
    }

    bool operator!=(const package_iterator &other) const
    {
      return realiter!=other.realiter;
    }

    // Override for aptitude-specific code.
    pkgCache::PkgIterator get_pkg() const
    {
      return realiter;
    }

    package operator*() const
    {
      return package(realiter, cache);
    }

    package_iterator &operator++()
    {
      ++realiter;
      return *this;
    }

    bool end() const
    {
      return realiter.end();
    }
  };

  /** \brief Iterate over all the interesting dependencies in the apt
   *  cache.
   */
  class dep_iterator
  {
    pkgDepCache *cache;

    pkgCache::PkgIterator pkg;
    pkgCache::VerIterator ver;
    aptitude_resolver_version::dep_iterator dep;

    /** \brief Advance to the earliest interesting dependency that is
     *  no earlier than the current dependency.
     */
    void normalize();

  public:
    /** \brief Create a dep_iterator for the given cache.
     *
     *  \param _cache The package cache whose dependencies should be
     *  iterated over.
     */
    dep_iterator(pkgDepCache *_cache)
      :cache(_cache),
       pkg(_cache->PkgBegin()),
       ver(),
       dep(_cache)
    {
      if(!pkg.end())
	ver=pkg.VersionList();
      if(!ver.end())
	dep=aptitude_resolver_version::dep_iterator(ver.DependsList(),
						    _cache);

      normalize();
    }

    /** \return the dependency at which this iterator currently points. */
    aptitude_universe::dep operator*() const
    {
      return *dep;
    }

    /** \brief Advance to the next dependency in the cache.
     *
     *  \return a reference to this iterator.
     */
    dep_iterator &operator++()
    {
      assert(!dep.end());

      ++dep;

      normalize();

      return *this;
    }

    /** \brief Test whether this is an end iterator. */
    bool end() const
    {
      return pkg.end();
    }
  };

  /** \brief Iterate over the broken interesting dependencies in an
   *  apt cache.
   *
   *  A bit like dep_iterator, but skips non-broken packages and deps.
   *  Since the "exposed version" of a package is its InstVersion, we
   *  need to test at most one version per package, so no need to keep
   *  a version iterator here.
   *
   *  Note on OR groups: DepGInstall is only set on the last entry in
   *  an OR group.  But Conflicts should be handled individually.  As
   *  I'm not even sure ORed conflicts are valid, none exist in the
   *  wild, and ORed conflicts are a Pointless Idea[tm] anyway, THIS
   *  WILL NOT PRODUCE CORRECT OUTPUT for ORed conflicts.  \todo try
   *  to find a way to get correct output without compromising in the
   *  main codepath.
   */
  class broken_dep_iterator
  {
    pkgDepCache *cache;

    class pkgCache::PkgIterator pkg;
    class pkgCache::DepIterator the_dep;
    /** If the_dep is a Conflicts, then the following keep track
     *  of which sub-relationship is being examined.
     */
    class pkgCache::PrvIterator prv;
    bool prv_open;

    /** \return \b true if the given non-end dep is InstBroken. */
    bool dep_is_inst_broken(const pkgCache::DepIterator &d) const;

    // Push forward to the next interesting point.
    void normalize();

  public:
#if 0
    broken_dep_iterator()
      :cache(0)
    {
    }
#endif

    /** \brief Create a broken_dep_iterator for the given package cache. */
    broken_dep_iterator(pkgDepCache *_cache)
      :cache(_cache),
       pkg(_cache->PkgBegin()), prv(*_cache, 0, (pkgCache::Package *) 0),
       prv_open(false)
    {
      if(!pkg.end())
	{
	  pkgCache::VerIterator ver=(*cache)[pkg].InstVerIter(*cache);

	  if(!ver.end())
	    the_dep=ver.DependsList();
	}

      normalize();
    }

    /** \return the dependency at which this iterator currently points. */
    aptitude_universe::dep operator*() const
    {
      return aptitude_universe::dep(the_dep, prv, cache);
    }

    /** \brief Advance to the next broken dependency.
     *
     *  \return a reference to this iterator.
     */
    broken_dep_iterator &operator++();

    /** \brief Test whether this is an end iterator. */
    bool end() const
    {
      return pkg.end();
    }
  };

  package_iterator packages_begin() const
  {
    return package_iterator(cache->PkgBegin(), cache);
  }

  dep_iterator deps_begin() const
  {
    return dep_iterator(cache);
  }

  broken_dep_iterator broken_begin() const
  {
    return broken_dep_iterator(cache);
  }

  unsigned long get_version_count() const
  {
    // PackageCount is added to make room for the UNINST versions.
    return cache->Head().VersionCount+cache->Head().PackageCount;
  }

  unsigned long get_package_count() const
  {
    return cache->Head().PackageCount;
  }
};

/** \brief Write an aptitude_resolver_dep to the given stream. */
std::ostream &operator<<(ostream &out, aptitude_resolver_dep d);

#endif
