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

/** \brief Translates a version of an apt package into the abstract
 *  realm.
 *
 *  This class is a model of the \ref universe_version "Version concept".
 *
 *  The version in question may be either a real version of the
 *  package, or the "not-installed" version, which indicates that the
 *  package is removed.
 *
 *  \sa \ref universe_version
 */
class aptitude_resolver_version
{
  pkgDepCache *cache;
  pkgCache::PkgIterator pkg;
  pkgCache::VerIterator ver;
public:
  /** \brief Create an invalid version object. */
  aptitude_resolver_version()
    :cache(0)
  {
  }

  /** \brief Create a version wrapper for the given version of the
   *  given package.
   *
   *  \param _pkg The package of which this is a version.  Must not be
   *  an end iterator.
   *
   *  \param _ver The version to be wrapped.  If an end iterator, the
   *  new object will represent the removal of _pkg.
   */
  aptitude_resolver_version(const pkgCache::PkgIterator &_pkg,
			    const pkgCache::VerIterator &_ver,
			    pkgDepCache *_cache)
    :cache(_cache), pkg(_pkg), ver(_ver)
  {
    assert(cache!=0);
    assert(pkg.Cache()!=0);
    assert(ver.Cache()!=0);
  }

  /** \return The APT package of which this is a version.
   *
   *  \sa get_package()
   */
  pkgCache::PkgIterator get_pkg() const
  {
    return pkg;
  }

  /** \return The APT version wrapped by this object, or an end
   *  iterator if this is a "removal version".
   */
  pkgCache::VerIterator get_ver() const
  {
    return ver;
  }

  /** \return The APT ID of this version if it is a real version,
   *  or a fake ID if it is a "removal version".
   */
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

  /** \return The version string of this version, mangled if multiple
   *  distinct APT versions exist with identical version strings.
   */
  std::string get_name() const;

  /** \return An abstract wrapper of the package with which this
   *  version is associated.
   *
   *  \sa get_pkg()
   */
  aptitude_resolver_package get_package() const
  {
    return aptitude_resolver_package(pkg, cache);
  }

  /** \return \b true if this is the same version as other. */
  bool operator==(const aptitude_resolver_version &other) const
  {
    return pkg == other.pkg && ver == other.ver;
  }

  /** \return \b true if this is not the same version as other. */
  bool operator!=(const aptitude_resolver_version &other) const
  {
    return pkg != other.pkg || ver != other.ver;
  }

  /** \brief Order versions according to their memory location. */
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

  /** \brief Return the first entry in the list of reverse
   *  dependencies for this version.
   */
  revdep_iterator revdeps_begin() const;

  /** \brief Return the first entry in the list of forward
   *  dependencies for this version.
   */
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

/** \brief Translates an apt dependency into the abstract realm.
 *
 *  This class is a model of the \ref universe_dep "Dependency concept".
 *
 *  Dependency relationships other than Conflicts are translated in a
 *  very straightforward manner: unversioned dependencies collect all
 *  the versions of the target package and are pushed backwards
 *  through Provides, while versioned dependencies collect all
 *  matching versions.  ORed dependencies collect all the versions
 *  targeted by their subcomponents.
 *
 *  Conflicts relationships are handled by generating one abstract
 *  dependency for the immediate conflict, and then a separate one for
 *  \e each provider of the conflicted name (if the conflict is
 *  unversioned, of course).  The solvers of these conflicts are the
 *  non-conflicted versions of the conflicted package (including the
 *  non-installed version), or the versions of the providing package
 *  other than the immediate provider, respectively.
 *
 *  \sa \ref universe_dep
 */
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
  /** \brief Generate an invalid dependency object.
   *
   *  \todo how prv is instantiated is a dreadful hack, but it's
   *  needed to avoid crashing until PrvIterators get a sensible
   *  default constructor.
   */
  aptitude_resolver_dep()
    :cache(0), prv(*apt_cache_file, (pkgCache::Provides *) 1, (pkgCache::Package *) 0)
  {
  }

  /** \brief Generate a new dependency.
   *
   *  \param dep The APT dependency to represent.
   *
   *  \param _prv If dep is a Conflicts, then this is either an end
   *  iterator (indicating that this object represents the conflict on
   *  the real target package), or the Provides through which the
   *  conflict should be projected.
   *
   *  \param _cache The package cache in which this dependency exists.
   */
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

  /** \brief Test whether the encapsulated dependency is a
   *   Recommends.
   */
  bool is_soft() const
  {
    return start->Type == pkgCache::Dep::Recommends;
  }

  /** \brief Compare two dependencies for equality. */
  bool operator==(const aptitude_resolver_dep &other) const
  {
    return start == other.start &&
      (start->Type != pkgCache::Dep::Conflicts || prv == other.prv);
  }

  /** \brief Compare two dependencies for equality. */
  bool operator!=(const aptitude_resolver_dep &other) const
  {
    return start != other.start ||
      (start->Type == pkgCache::Dep::Conflicts && prv != other.prv);
  }

  /** \brief Orders dependencies according to their memory
   *  location.
   */
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

  /** \brief Test whether a given solution breaks this dependency.
   *
   *  \param InstallationType A model of \ref universe_installation Installation.
   *
   *  \param I An installation to test against.
   *
   *  \return \b true if this dependency is not satisfied by I.
   */
  template<typename InstallationType>
  bool broken_under(const InstallationType &I) const;

  /** \return The APT dependency associated with this abstract dependency. */
  pkgCache::DepIterator get_dep() const
  {
    return start;
  }

  /** \return The APT Provides relationship associated with this
   *  abstract dependency.
   */
  pkgCache::PrvIterator get_prv() const
  {
    return prv;
  }

  /** \return \b true if the given version will resolve this dependency. */
  bool solved_by(const aptitude_resolver_version &v) const;

  /** \return The source version of this dependency. */
  aptitude_resolver_version get_source() const
  {
    assert(!start.end());
    assert(!const_cast<pkgCache::DepIterator &>(start).ParentPkg().end());
    return aptitude_resolver_version(const_cast<pkgCache::DepIterator &>(start).ParentPkg(),
				     const_cast<pkgCache::DepIterator &>(start).ParentVer(),
				     cache);
  }

  class solver_iterator;

  /** \return The head of the target list for this dependency. */
  solver_iterator solvers_begin() const;
};

/** \brief Iterate over the versions of a package.
 *
 *  \sa aptitude_resolver_package, aptitude_resolver_version
 */
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
  /** \brief Create an invalid version_iterator. */
  version_iterator()
    :cache(0)
  {
  }

  /** \brief Create a version_iterator pointing at the first version of the given package.
   */
  version_iterator(pkgCache::PkgIterator _pkg,
		   pkgDepCache *_cache)
    :cache(_cache), pkg(_pkg), ver(_pkg.VersionList())
  {
  }

  /** \return The APT package corresponding to this abstract package. */
  pkgCache::PkgIterator get_pkg() {return pkg;}

  /** \return The APT version corresponding to this abstract version.
   *  If this is an end iterator, then this version corresponds to
   *  removing the package.
   */
  pkgCache::VerIterator get_ver() {return ver;}

  /** \return \b true if this iterator is identical to other. */
  bool operator==(const version_iterator &other) const
  {
    return pkg == other.pkg && ver == other.ver;
  }

  /** \return \b true if this iterator differs from other. */
  bool operator!=(const version_iterator &other) const
  {
    return pkg != other.pkg || ver != other.ver;
  }

  /** \return The version at which this iterator currently points. */
  aptitude_resolver_version operator *() const
  {
    return aptitude_resolver_version(pkg, ver, cache);
  }

  /** \brief Advance to the next version in the list.
   *
   *  \return A reference to this iterator.
   */
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

  /** \return \b true if this is an end iterator. */
  bool end() const
  {
    return pkg.end();
  }
};

inline aptitude_resolver_package::version_iterator aptitude_resolver_package::versions_begin() const
{
  return version_iterator(pkg, cache);
}

/** \brief Iterates over the reverse dependencies of a version.
 *
 *  As explained in the definition of the \ref universe_version
 *  "Version concept", this is not necessarily the set of dependencies
 *  that impinge on the version.
 *
 *  \sa aptitude_resolver_version, aptitude_resolver_dep
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

  /** \brief Generate a revdep_iterator to cover the reverse deps of
   *  the given version.
   *
   *  \param v The version whose reverse dependencies are to be
   *  enumerated.  If this is an end iterator, the resulting list will
   *  be empty.
   *
   *  \param _cache The cache in which to operate.
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

  /** \brief Test whether this is an end iterator. */
  bool end() const
  {
    return dep_lst.end();
  }

  /** \return The dependency at which this iterator currently
   *  points.
   */
  aptitude_resolver_dep operator*() const
  {
    return aptitude_resolver_dep(dep_lst, prv_lst, cache);
  }

  /** \brief Advance to the next entry in the list.
   *
   *  \return A reference to this iterator.
   */
  revdep_iterator &operator++()
  {
    ++dep_lst;
    normalize();

    return *this;
  }
};

/** \brief Iterates over the distinct dependencies of a version.
 *
 *  \sa aptitude_resolver_version, aptitude_resolver_dep
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
  /** \brief Create an invalid dep iterator for the given cache.
   *
   *  \param cache The cache in which to create a dep iterator.
   */
  dep_iterator(pkgDepCache *_cache)
    :cache(_cache),
     prv(*_cache, 0, (pkgCache::Package *) 0),
     prv_open(false)
  {
  }

  /** \brief Create an iterator for the given version's dependencies in the given cache.
   *
   *  \param ver The version whose dependencies should be iterated over.
   *
   *  \param _cache The cache in which to operate.
   */
  dep_iterator(const pkgCache::VerIterator &ver,
	       pkgDepCache *_cache)
    :cache(_cache),
     dep(ver.DependsList()),
     prv(*_cache, 0, (pkgCache::Package *) 0),
     prv_open(false)
  {
    normalize();
  }

  /** \brief Assignment operator. */
  dep_iterator &operator=(const dep_iterator &other)
  {
    cache=other.cache;
    dep=other.dep;
    prv=other.prv;
    prv_open=other.prv_open;

    return *this;
  }

  /** \return The dependency at which this iterator currently points. */
  aptitude_resolver_dep operator*() const
  {
    return aptitude_resolver_dep(dep, prv, cache);
  }

  /** \brief Test whether this is an end iterator. */
  bool end() const
  {
    return dep.end();
  }

  /** \brief Advance to the next dependency of this version.
   *
   *  \return A reference to this iterator.
   */
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
    return dep_iterator(ver, cache);
}

/** \brief Iterates over the targets of a dependency.
 *
 *  \sa aptitude_resolver_dep
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
  /** \brief Initialize a solution iterator for a dependency that is
   *  not a Conflicts.
   *
   *  \param start The dependency whose targets should be enumerated.
   *
   *  \param _cache The package cache in which this dependency is
   *  located.
   */
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

  /** \brief Initialize a solution iterator for a Conflicts.
   *
   *  \param d The conflict that we should iterate over solutions to.
   *
   *  \param p The Provides through which the Conflicts is being
   *  projected, or an end iterator if we are handling a straight
   *  Conflicts.
   *
   *  \param _cache The package cache in which to work.
   */
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

  /** \brief Compare two solver iterators for equality. */
  bool operator==(const solver_iterator &other) const
  {
    return dep_lst == other.dep_lst &&
      ver_lst == other.ver_lst &&
      prv_lst == other.prv_lst &&
      finished == other.finished;
  }

  /** \brief Compare two solver iterators for equality. */
  bool operator!=(const solver_iterator &other) const
  {
    return dep_lst != other.dep_lst ||
      ver_lst != other.ver_lst ||
      prv_lst != other.prv_lst ||
      finished != other.finished;
  }

  /** \brief Advance to the next solution.
   *
   *  \return a reference to this iterator.
   */
  solver_iterator &operator++();

  /** \return The version at which this iterator currently points. */
  aptitude_resolver_version operator*() const;

  /** \brief Test whether this is an end iterator. */
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

template<typename InstallationType>
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

  /** \brief Iterate over all the packages in the universe. */
  class package_iterator
  {
    pkgDepCache *cache;
    pkgCache::PkgIterator realiter;
  public:
    /** \brief Create an invalid package iterator. */
    package_iterator()
      :cache(0)
    {
    }

    /** \brief Create an iterator pointing at the first package in the
     *  cache.
     *
     *  \param _cache The package cache to iterate over.
     */
    package_iterator(pkgDepCache *_cache)
      :cache(_cache), realiter(_cache->PkgBegin())
    {
    }

    /** \brief Compare two package iterators for equality. */
    bool operator==(const package_iterator &other) const
    {
      return realiter==other.realiter;
    }

    /** \brief Compare two package iterators for equality. */
    bool operator!=(const package_iterator &other) const
    {
      return realiter!=other.realiter;
    }

    /** \brief Retrieve the underlying apt iterator. */
    pkgCache::PkgIterator get_pkg() const
    {
      return realiter;
    }

    /** \brief Extract the package at which this iterator currently points. */
    package operator*() const
    {
      return package(realiter, cache);
    }

    /** \brief Advance to the next package.
     *
     *  \return A reference to this iterator.
     */
    package_iterator &operator++()
    {
      ++realiter;
      return *this;
    }

    /** \brief Test whether this is an end iterator. */
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
	dep=aptitude_resolver_version::dep_iterator(ver, _cache);

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
    return package_iterator(cache);
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
