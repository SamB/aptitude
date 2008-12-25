// cmdline_why.h                            -*-c++-*-
//
//   Copyright (C) 2007-2008 Daniel Burrows
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

#ifndef CMDLINE_WHY_H
#define CMDLINE_WHY_H

#include <deque>
#include <string>
#include <vector>

#include <apt-pkg/depcache.h>
#include <apt-pkg/error.h>
#include <apt-pkg/pkgcache.h>

#include <cwidget/fragment.h>

#include <generic/apt/apt.h>
#include <generic/apt/aptcache.h>
#include <generic/apt/matching/pattern.h>

#include <generic/util/immset.h>

/** \file cmdline_why.h
 */

/** \brief Explain why a package is installed or conflicted against.
 *
 *  aptitude why A1 [A2 ...] B
 *     --> show the shortest strongest justification from (some) A to B.
 *          A and B may NOT have version/archive tags; any A may be a pattern,
 *          but B must not be.
 *  aptitude why-not A1 [A2 ...] B
 *     --> show the shortest strongest justification from (some) A1
 *         to a conflict on B. A and B may NOT have version/archive tags;
 *         any A may be a pattern, but B must not be.
 *  aptitude why B
 *    --> Equivalent to aptitude why ~i!~M B
 *  aptitude why-not B
 *    --> Equivalent to aptitude why-not ~i!~M B
 *
 *  If -v is passed on the command-line, aptitude displays all the
 *  justifications it can find, rather than stopping at the shortest
 *  one.
 *
 *  \return 0 if a justification was found, -1 if an error occurred, and
 *  1 if no justification could be found.
 */
int cmdline_why(int argc, char *argv[],
		const char *status_fname, int verbosity,
		bool why_not);


// Direct access to the "why" algorithm.
namespace cwidget
{
  class fragment;
}
namespace aptitude
{
  namespace why
  {
    class justification;

    class search_params
    {
    public:
      /** \brief Indicates which package version should be
       *  considered.
       *
       *  To reduce confusion, it is assumed that all packages are
       *  kept at their current versions, that all candidate versions
       *  are being installed, or that packages are at their install
       *  versions.
       */
      enum VersionSelection
	{
	  Current,
	  Candidate,
	  Install,
	  /** \brief When following dependencies, only follow
	   *  dependencies that apply to the to-be-installed version
	   *  of the package and not to the current version (if any).
	   *
	   *  In other words: if a package is being upgraded, only
	   *  follow versioned dependencies that would have forced the
	   *  upgrade; otherwise behave as normal.  This is used by
	   *  the command-line prompt code to produce reasonable
	   *  output for auto-upgraded packages.
	   */
	  InstallNotCurrent
	};
      /** \brief Indicates which dependencies should be considered.
       *
       *  All dependenies of the given type and all stronger
       *  dependencies will be followed by the search.
       */
      enum DepLevel { DependsOnly, Recommends, Suggests };

    private:
      VersionSelection version_selection;
      DepLevel dep_level;
      bool allow_choices;

    public:
      /** \brief Set up the parameters for a search.
       *
       *  \param _version_selection  which versions should
       *                             be included in the search.
       *  \param _dep_level          which dependencies should
       *                             be followed.
       *  \param _allow_choices      if \b false, ORed dependencies
       *                             and Provides will not be followed.
       */
      search_params(VersionSelection _version_selection,
		    DepLevel _dep_level,
		    bool _allow_choices)
	: version_selection(_version_selection),
	  dep_level(_dep_level),
	  allow_choices(_allow_choices)
      {
      }

      /** \return \b true if a search should follow the given
       *  dependency according to these parameters.
       */
      bool should_follow_dep(const pkgCache::DepIterator &dep) const
      {
	switch(dep->Type)
	  {
	  case pkgCache::Dep::Depends:
	  case pkgCache::Dep::PreDepends:
	  case pkgCache::Dep::Conflicts:
	  case pkgCache::Dep::DpkgBreaks:
	    return true;
	  case pkgCache::Dep::Recommends:
	    return dep_level == Recommends || dep_level == Suggests;
	  case pkgCache::Dep::Suggests:
	    return dep_level == Suggests;
	  default:
	    return false;
	  }
      }

      pkgCache::VerIterator selected_version(const pkgCache::PkgIterator &pkg) const
      {
	switch(version_selection)
	  {
	  case Current:
	    return pkg.CurrentVer();
	  case Candidate:
	    return (*apt_cache_file)[pkg].CandidateVerIter(*apt_cache_file);
	  case Install:
	  case InstallNotCurrent:
	    return (*apt_cache_file)[pkg].InstVerIter(*apt_cache_file);
	  default:
	    _error->Error("Unknown version selection, something is very wrong.");
	    return pkg.CurrentVer();
	  }
      }

      bool get_only_not_current() const
      {
	switch(version_selection)
	  {
	  case InstallNotCurrent:
	    return true;
	  default:
	    return false;
	  }
      }

      bool get_allow_choices() const { return allow_choices; }

      std::wstring description() const;
    };

    /** Represents something that the "why" algorithm can explain.
     *
     *  This is the low-level way to tell the "why" algorithm what to
     *  search for.
     */
    class target
    {
      // An Install node represents the installation of a package.
      //
      // A Remove node represents the removal of a package.
      //
      // A ProvidesInstall node indicates that something that provides
      // the package name is being installed.
      //
      // A ProvidesRemove node indicates that something that provides
      // the package name is being removed.

      /** \brief What type of target this is. */
      enum Type
	{
	  /** \brief This target represents installing a package. */
	  InstallType,
	  /** \brief This target represents removing a package. */
	  RemoveType,
	  /** \brief This target represents installing anything that
	   *  provides a package name.
	   */
	  ProvidesInstall,
	  /** \brief This target represents removing anything that
	   *  provides a package name.
	   */
	  ProvidesRemove
	};

      // The package on which to operate; since we uniformly take one of
      // the three package targets, there isn't a need to store a
      // version.
      pkgCache::PkgIterator pkg;
      // The provides is stored so we can use the ProvideVersion.
      pkgCache::PrvIterator prv;
      Type type;

      target(const pkgCache::PkgIterator &_pkg,
	     const pkgCache::PrvIterator &_prv,
	     Type _type)
	: pkg(_pkg), prv(_prv), type(_type)
      {
      }

      target(const pkgCache::PkgIterator &_pkg,
	     Type _type)
	: pkg(_pkg), type(_type)
      {
      }

    public:
      /** Returns the package that should be marked as "visited"
       *  to restrict future searches. (since the search never visits
       *  two different versions of the same package, this is OK)
       */
      pkgCache::PkgIterator get_visited_package() const
      {
	return pkg;
      }

      pkgCache::PrvIterator get_provides() const
      {
	return prv;
      }

      /** \brief Return true if this is a ProvidesInstall or ProvidesRemove node.
       *
       *  Provides nodes are only created internally, as a side effect
       *  of the search process.
       */
      bool is_provides() const
      {
	return type == ProvidesInstall || type == ProvidesRemove;
      }

      /** \brief Return true if this is a Remove or ProvidesRemove node. */
      bool is_remove() const
      {
	return type == RemoveType || type == ProvidesRemove;
      }

      /** \brief Return the package that this target represents. */
      pkgCache::PkgIterator get_pkg() const { return pkg; }

      /** \brief Return the provides, if any, that this target represents. */

      // Use the implicit operator==.
      //
      // bool operator==(const target &other) const;

      static target Provide(const pkgCache::PkgIterator &pkg,
			    const pkgCache::PrvIterator &prv,
			    bool provided_is_removed)
      {
	Type tp = provided_is_removed ? ProvidesRemove : ProvidesInstall;
	return target(pkg, prv, tp);
      }

      static target Install(const pkgCache::PkgIterator &pkg)
      {
	return target(pkg, InstallType);
      }

      static target Remove(const pkgCache::PkgIterator &pkg)
      {
	return target(pkg, RemoveType);
      }

      cwidget::fragment *description() const;

      /** \brief Push the successors of this target onto the head of a queue.
       *
       *  This is mainly used by the "why" algorithm itself.
       *
       *  The successors of a node are the actions that could have generated
       *  it (installing this package, installing this provides).
       *
       *  \param parent the parent of any new search nodes that are generated.
       *  \param output the queue onto which the successors should be loaded.
       *  \param params the parameters of the search (these control
       *                which dependencies get followed).
       */
      void generate_successors(const justification &parent,
			       std::deque<justification> &output,
			       const search_params &params,
			       int verbosity) const;
    };

    /** \brief Represents one step in an explanation formed by the
     *  "why" algorithm.
     *
     *  This includes an integer ID that counts forward from the root
     *  of the search (i.e., the final target being justified).
     */
    class action
    {
      pkgCache::DepIterator dep;
      pkgCache::PrvIterator prv;
      int id;

    public:
      /** \brief Create an action that represents following the given
       *  dependency.
       */
      action(const pkgCache::DepIterator &_dep,
	     int _id)
	: dep(_dep), id(_id)
      {
      }

      /** \brief Create an action that represents following the given
       *  Provides.
       */
      action(const pkgCache::PrvIterator &_prv,
	     int _id)
	: prv(_prv), id(_id)
      {
      }

      /** \return the dependency followed by this action, or an
       *  end iterator if this action follows a Provides.
       */
      pkgCache::DepIterator get_dep() const { return dep; }
      /** \return the Provides followed by this action, or an
       *  end iterator if this action follows a depends.
       */
      pkgCache::PrvIterator get_prv() const { return prv; }

      cwidget::style get_style() const;

      cwidget::fragment *description_column1_fragment() const;

      cwidget::fragment *description_column2_fragment() const;

      cwidget::fragment *description_column3_fragment() const;

      cwidget::fragment *description_fragment() const;

      bool operator<(const action &other) const;
    };

    /** \brief Search for a justification for an action.
     *
     *  \param target    the action to justify.
     *  \param leaves    patterns selecting the packages to build a
     *                   justification from.
     *  \param params    the parameters of the search.
     *  \param output    where the justification is stored.
     *
     *  \return \b true if a justification could be constructed,
     *          \b false otherwise.
     */
    bool find_justification(const target &target,
			    const std::vector<cwidget::util::ref_ptr<aptitude::matching::pattern> > leaves,
			    const search_params &params,
			    bool find_all,
			    std::vector<std::vector<action> > &output);

    /** \brief Find the shortest strongest justification for the given
     *  goal starting at the given set of leaves.
     *
     *  \param leaves     The packages at which the search should terminate.
     *  \param goal       The target of the search.
     *  \param find_all   If \b true, return all the possible justifications
     *                    rather than just the first one.
     *  \param verbosity  How verbose the search should be (if set to a value
     *                    greater than zero, various trace information will be

     *                    written to standard output).
     *  \param output     A vector in which to store the results of the search.
     */
    void find_best_justification(const std::vector<cwidget::util::ref_ptr<aptitude::matching::pattern> > &leaves,
				 const target &goal,
				 bool find_all,
				 int verbosity,
				 std::vector<std::vector<action> > &output);
  }
}

cwidget::fragment *do_why(const std::vector<cwidget::util::ref_ptr<aptitude::matching::pattern> > &leaves,
			  const pkgCache::PkgIterator &root,
			  bool find_all,
			  bool root_is_removal,
			  bool &success);

// Parses the leaves as if they were command-line arguments.
cwidget::fragment *do_why(const std::vector<std::string> &arguments,
			  const std::string &root,
			  bool find_all,
			  bool root_is_removal,
			  bool &success);


#endif
