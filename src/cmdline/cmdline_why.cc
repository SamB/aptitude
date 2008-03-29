// cmdline_why.cc                                -*-c++-*-
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


#include "cmdline_why.h"

#include "cmdline_common.h"
#include "cmdline_show.h"
#include "cmdline_util.h"

#include <aptitude.h>

#include <pkg_ver_item.h> // For column formats.
#include <solution_fragment.h> // For dep_targets()

#include <algorithm>
#include <deque>

#include <apt-pkg/depcache.h>
#include <apt-pkg/error.h>
#include <apt-pkg/pkgcache.h>
#include <apt-pkg/pkgsystem.h>
#include <apt-pkg/version.h>

#include <generic/apt/apt.h>
#include <generic/apt/matchers.h>

#include <generic/util/immset.h>
#include <generic/util/util.h>

#include <cwidget/fragment.h>

#include <../pkg_item.h>

#include <set>

namespace cw = cwidget;
using cwidget::fragf;
using cwidget::fragment;

using namespace aptitude::matching;

namespace
{
  struct compare_pkgs
  {
    bool operator()(const pkgCache::PkgIterator &p1,
		    const pkgCache::PkgIterator &p2)
    {
      return p1->ID < p2->ID;
    }
  };
}

namespace aptitude
{
  namespace why
  {
    // Represents a full forward or reverse justification for the
    // installation of the given package/version.  The justification
    // may terminate on either a package version, a provided package
    // name, or the removal of a package.
    class justification
    {
      target the_target;
      imm::set<action> actions;

      justification(const target &_the_target,
		    const imm::set<action> &_actions)
	: the_target(_the_target), actions(_actions)
      {
      }
    public:
      // Create a node with an empty history rooted at the given target.
      justification(const target &_the_target)
	: the_target(_the_target)
      {
      }

      const target &get_target() const
      {
	return the_target;
      }

      const imm::set<action> &get_actions() const
      {
	return actions;
      }

      // Generate all the successors of this node (Q: could I lift
      // justify_target::generate_successors into this class?  It feels
      // like it makes more sense to have the smarts in here)
      void generate_successors(std::deque<justification> &output,
			       const search_params &params,
			       int verbosity) const
      {
	the_target.generate_successors(*this, output, params, verbosity);
      }

      // Build a successor of this node.
      justification successor(const target &new_target,
			      const pkgCache::DepIterator &dep) const
      {
	imm::set<action> new_actions(actions);
	new_actions.insert(action(dep, new_actions.size()));

	return justification(new_target, new_actions);
      }

      justification successor(const target &new_target,
			      const pkgCache::PrvIterator &prv) const
      {
	imm::set<action> new_actions(actions);
	new_actions.insert(action(prv, new_actions.size()));

	return justification(new_target, new_actions);
      }

      cwidget::fragment *description() const;
    };

    cw::style action::get_style() const
    {
      pkgCache::PkgIterator pkg;
      if(!dep.end())
	pkg = const_cast<pkgCache::DepIterator &>(dep).ParentPkg();
      else
	pkg = const_cast<pkgCache::PrvIterator &>(prv).OwnerPkg();

      return pkg_item::pkg_style(pkg, false);
    }

    cw::fragment *action::description_column1_fragment() const
    {
      pkgCache::VerIterator ver;
      if(!dep.end())
	ver = const_cast<pkgCache::DepIterator &>(dep).ParentVer();
      else
	ver = const_cast<pkgCache::PrvIterator &>(prv).OwnerVer();

      pkgCache::PkgIterator pkg = ver.ParentPkg();

      // Copy flags from "aptitude search" et al.
      //
      // This is a little crufty; the column formatting needs an overhaul.
      cw::column_disposition flag1 =
	pkg_ver_columnizer::setup_column(ver, true, 0, pkg_item::pkg_columnizer::stateflag);
      cw::column_disposition flag2 =
	pkg_ver_columnizer::setup_column(ver, true, 0, pkg_item::pkg_columnizer::actionflag);
      cw::column_disposition flag3 =
	pkg_ver_columnizer::setup_column(ver, true, 0, pkg_item::pkg_columnizer::autoset);

      std::wstring rval;
      if(flag1.text.size() < 1)
	rval += L' ';
      else
	rval += flag1.text;
      if(flag2.text.size() < 1)
	rval += L' ';
      else
	rval += flag2.text;
      if(flag3.text.size() < 1)
	rval += L' ';
      else
	rval += flag3.text;
      rval += L' ';
      rval += cw::util::transcode(pkg.Name());
      return cw::text_fragment(rval);
    }

    cw::fragment *action::description_column2_fragment() const
    {
      if(!dep.end())
	return cw::text_fragment(const_cast<pkgCache::DepIterator &>(dep).DepType());
      else
	return cw::text_fragment(_("Provides"));
    }

    cw::fragment *action::description_column3_fragment() const
    {
      // Q: can I use a std::string and cw::util::transcode on the way out instead?
      if(!dep.end())
	{
	  pkgCache::DepIterator start, end;
	  surrounding_or(dep, start, end);
	  return cw::text_fragment(dep_targets(start));
	}
      else
	return cw::text_fragment(const_cast<pkgCache::PrvIterator &>(prv).ParentPkg().Name());
    }

    cw::fragment *action::description_fragment() const
    {
      return cw::fragf("%F %F %F",
		       description_column1_fragment(),
		       description_column2_fragment(),
		       description_column3_fragment());
    }

    bool action::operator<(const action &other) const
    {
      typedef pkgCache::Dependency Dependency;
      typedef pkgCache::Provides Provides;

      if(id > other.id)
	return true;
      else if(other.id > id)
	return false;
      else if(dep.end() && !other.dep.end())
	return true;
      else if(other.dep.end() && !dep.end())
	return false;
      else if(!dep.end() && !other.dep.end() &&
	      (const Dependency *)dep < (const Dependency *)other.dep)
	return true;
      else if(!dep.end() && !other.dep.end() &&
	      (const Dependency *)other.dep < (const Dependency *)dep)
	return false;
      else if(prv.end() && !other.prv.end())
	return true;
      else if(other.prv.end() && !prv.end())
	return false;
      else if(!prv.end() && !other.prv.end() &&
	      (const Provides *)prv < (const Provides *)other.prv)
	return true;
      else if(!prv.end() && !other.prv.end() &&
	      (const Provides *)other.prv < (const Provides *)prv)
	return false;
      else
	return false;
    }

    std::wstring search_params::description() const
    {
      std::wstring rval(L"{ ");
      rval += W_("dep_level");
      rval += L" = ";

      switch(dep_level)
	{
	case DependsOnly:
	  rval += W_("DependsOnly");
	  break;
	case Recommends:
	  rval += W_("Recommends");
	  break;
	case Suggests:
	  rval += W_("Suggests");
	  break;
	default:
	  rval += L"???";
	  break;
	}

      rval += L", ";
      rval += W_("version_selection");
      rval += L" = ";
      switch(version_selection)
	{
	case Current:
	  rval += W_("Current");
	  break;
	case Candidate:
	  rval += W_("Candidate");
	  break;
	case Install:
	  rval += W_("Install");
	  break;
	case InstallNotCurrent:
	  rval += W_("InstallNotCurrent");
	  break;
	default:
	  rval += L"???";
	  break;
	}

      rval += L", ";
      rval += W_("allow_choices");
      rval += L" = ";
      if(allow_choices)
	rval += W_("true");
      else
	rval += W_("false");

      rval += L" }";
      return rval;
    }

    cw::fragment *justification::description() const
    {
      std::vector<cw::fragment *> rval;
      rval.push_back(cw::fragf("%F\n", the_target.description()));
      std::vector<cw::fragment *> col1_entries, col2_entries, col3_entries;
      for(imm::set<action>::const_iterator it = actions.begin();
	  it != actions.end(); ++it)
	{
	  col1_entries.push_back(cw::hardwrapbox(cw::fragf("%F | \n", it->description_column1_fragment())));
	  col2_entries.push_back(cw::hardwrapbox(cw::fragf("%F\n", it->description_column2_fragment())));
	  col3_entries.push_back(cw::hardwrapbox(cw::fragf("%F\n", it->description_column3_fragment())));
	}

      using cw::fragment_column_entry;

      std::vector<fragment_column_entry> columns;
      columns.push_back(fragment_column_entry(false, true, 0, fragment_column_entry::top, col1_entries));
      columns.push_back(fragment_column_entry(false, false, 1, fragment_column_entry::top, NULL)),
	columns.push_back(fragment_column_entry(false, true, 0, fragment_column_entry::top, col2_entries));
      columns.push_back(fragment_column_entry(false, false, 1, fragment_column_entry::top, NULL)),
	columns.push_back(fragment_column_entry(false, true, 0, fragment_column_entry::top, col3_entries));
      rval.push_back(fragment_columns(columns));
      return cw::sequence_fragment(rval);
    }

  cw::fragment *print_dep(pkgCache::DepIterator dep)
  {
    if(dep.TargetVer() != NULL)
      return cw::fragf("%s %s %s (%s %s)",
		   dep.ParentPkg().Name(),
		   dep.DepType(),
		   dep.TargetPkg().Name(),
		   dep.CompType(),
		   dep.TargetVer());
    else
      return cw::fragf("%s %s %s",
		   dep.ParentPkg().Name(),
		   dep.DepType(),
		   dep.TargetPkg().Name());
  }

  cw::fragment *target::description() const
  {
    pkgCache::PkgIterator &mpkg = const_cast<pkgCache::PkgIterator &>(pkg);

    switch(type)
      {
      case InstallType:
	return cw::fragf(_("Install(%s)"), mpkg.Name());
      case RemoveType:
	return cw::fragf(_("Remove(%s)"), mpkg.Name());
      case ProvidesInstall:
	return cw::fragf(_("Install(%s provides %s)"),
			 const_cast<pkgCache::PrvIterator &>(prv).OwnerPkg().Name(),
			 mpkg.Name());
      case ProvidesRemove:
	return cw::fragf(_("Remove(%s provides %s)"),
			 const_cast<pkgCache::PrvIterator &>(prv).OwnerPkg().Name(),
			 mpkg.Name());
      }
  }

  void target::generate_successors(const justification &parent,
				   std::deque<justification> &output,
				   const search_params &params,
				   int verbosity) const
  {
    // The reverse successors of an install node are all the revdeps
    // of the package, minus conflicts and deps from versions that
    // aren't selected by the params, plus paths passing through
    // Provides nodes.
    //
    // The successors of a provides node are the same as for an
    // install node, except that we don't look for provided names.
    //
    // The successors of a remove node are the same as for an install
    // node, except that we ONLY take conflicts and we use the
    // candidate version regardless of what params says.
    //
    // Note that there's no need to broaden ORs here; I just care
    // about reaching backwards until I find a leaf node.
    for(pkgCache::DepIterator dep = pkg.RevDependsList(); !dep.end(); ++dep)
      {
	// If we walked through a Provides, we can only look at conflicts.
	if(!params.get_allow_choices() &&
	   !is_conflict(dep->Type) &&
	   is_provides())
	  continue;

	{
	  pkgCache::DepIterator start, end;
	  // Drop ORs if choices are disallowed.  Note that ORs are
	  // meaningless for conflicts, so we ignore them there.
	  if(!params.get_allow_choices() &&
	     !is_conflict(dep->Type))
	    {
	      // Check if we're in an OR by checking whether either
	      // (a) the OR flag is set, or (b) this isn't the first
	      // element of its OR group.  (if the OR flag isn't set,
	      // then either we're the last element of an OR group, or
	      // we aren't part of an OR group)
	      if(dep->CompareOp & pkgCache::Dep::Or)
		continue;

	      surrounding_or(dep, start, end);
	      if(start != dep)
		continue;
	    }
	}

	if(verbosity > 1)
	  {
	    std::auto_ptr<cw::fragment> tmp(cw::fragf(_("    ++ Examining %F\n"), print_dep(dep)));
	    std::cout << tmp->layout(screen_width, screen_width, cw::style());
	  }

	if(is_remove())
	  {
	    // Remove, ProvidesRemove nodes take this.
	    if(!is_conflict(dep->Type))
	      {
		if(verbosity > 1)
		  std::cout << _("    ++   --> skipping, not a conflict\n");
		continue;
	      }
	  }
	else
	  {
	    // Install, ProvidesInstall nodes take this.
	    if(is_conflict(dep->Type))
	      {
		if(verbosity > 1)
		  std::cout << _("    ++   --> skipping conflict\n");
		continue;
	      }
	  }

	if(!params.should_follow_dep(dep))
	  {
	    if(verbosity > 1)
	      std::cout << _("    ++   --> skipping, not relevant according to params\n");
	    continue;
	  }

	if(dep.ParentVer() != params.selected_version(dep.ParentPkg()))
	  {
	    if(verbosity > 1)
	      std::cout << _("    ++   --> skipping, parent is not the selected version\n");
	    continue;
	  }

	if(params.get_only_not_current())
	  {
	    bool satisfied_by_current = false;

	    // Skip this dep if it's satisfied by the package's
	    // current version.
	    if(is_provides())
	      {
		pkgCache::VerIterator provider_current = get_provides().OwnerPkg().CurrentVer();
		for(pkgCache::PrvIterator prv = provider_current.ProvidesList();
		    !satisfied_by_current && !prv.end(); ++prv)
		  {
		    if(dep.TargetVer() == NULL ||
		       (prv.ProvideVersion() != NULL &&
			_system->VS->CheckDep(prv.ProvideVersion(),
					      dep->CompareOp,
					      dep.TargetVer())))
		      satisfied_by_current = true;
		  }
	      }
	    else
	      {
		pkgCache::VerIterator current = dep.TargetPkg().CurrentVer();
		if(!current.end())
		  {
		    if(dep.TargetVer() == NULL ||
		       _system->VS->CheckDep(current.VerStr(),
					     dep->CompareOp,
					     dep.TargetVer()))
		      satisfied_by_current = true;
		  }
	      }

	    if(satisfied_by_current)
	      {
		if(verbosity > 1)
		  std::cout << _("    ++   --> skipping, the dep is satisfied by the current version\n");
		continue;
	      }
	  }

	const char *ver_to_check;
	if(is_provides())
	  ver_to_check = get_provides().ProvideVersion();
	else if(is_remove())
	  {
	    pkgCache::VerIterator candver =
	      (*apt_cache_file)[pkg].CandidateVerIter(*apt_cache_file);
	    ver_to_check = candver.VerStr();
	  }
	else
	  {
	    pkgCache::VerIterator ver = params.selected_version(pkg);
	    if(ver.end())
	      ver_to_check = "";
	    else
	      ver_to_check = params.selected_version(pkg).VerStr();
	  }

	if(dep.TargetVer() == NULL ||
	   (ver_to_check != NULL &&
	    _system->VS->CheckDep(ver_to_check,
				  dep->CompareOp,
				  dep.TargetVer())))
	  {
	    if(verbosity > 1)
	      std::cout << _("    ++   --> ENQUEUING\n");
	    target the_target(Install(dep.ParentPkg()));
	    output.push_back(parent.successor(the_target, dep));
	  }
	else
	  {
	    if(verbosity > 1)
	      std::cout << _("    ++   --> skipping, version check failed\n");
	  }
      }

    if(!is_provides())
      {
	pkgCache::VerIterator ver;
	if(is_remove())
	  ver = (*apt_cache_file)[pkg].CandidateVerIter(*apt_cache_file);
	else
	  ver = params.selected_version(pkg);

	// Walk over the provides declared by the version wrapped by this node.
	if(!ver.end())
	  {
	    for(pkgCache::PrvIterator prv = ver.ProvidesList(); !prv.end(); ++prv)
	      {
		if(verbosity > 1)
		  std::cout << ssprintf(_("    ++   --> ENQUEUING %s Provides %s\n"),
					prv.OwnerPkg().Name(),
					prv.ParentPkg().Name());
		target the_target(Provide(prv.ParentPkg(), prv, is_remove()));
		output.push_back(parent.successor(the_target, prv));
	      }
	  }
      }
  }

    namespace
    {
  class justification_search
  {
    // The central queue.  Nodes are inserted at the back and removed
    // from the front.
    std::deque<justification> q;

    std::vector<pkg_matcher *> leaves;

    search_params params;

    // Array of flags indicating which packages have been visited.
    bool *seen_packages;

    // Used for debug output.
    bool first_iteration;

    int verbosity;

  public:
    /** \brief Initialize a search for justifications.
     *
     *  \param leaves the point at which to stop searching and signal
     *                success.
     *                These pointers are NOT owner by the search; they
     *                must live as long as the search does, and will
     *                not be freed when it is destroyed.
     *
     *  \param root the root package of the search.
     *
     *  \param search_for_removal if true, the root note is the removal
     *                            of root; otherwise, it is the installation
     *                            of root.
     *
     *  \param params the search parameters: whether to use the current
     *                or the inst ver, and whether to consider
     *                suggests/recommends to be important.
     */
    justification_search(const std::vector<pkg_matcher *> &_leaves,
			 const target &root,
			 const search_params &_params,
			 int _verbosity)
      : leaves(_leaves),
	params(_params),
	seen_packages(NULL),
	first_iteration(true),
	verbosity(_verbosity)
    {
      // Prime the pump.
      q.push_back(justification(root));
    }

    justification_search(const justification_search &other)
      : q(other.q),
	leaves(other.leaves),
	params(other.params),
	first_iteration(other.first_iteration)
    {
      if(other.seen_packages == NULL)
	seen_packages = NULL;
      else
	{
	  int count = (*apt_cache_file)->Head().PackageCount;
	  seen_packages = new bool[count];
	  for(int i = 0; i < count; ++i)
	    seen_packages[i] = other.seen_packages[i];
	}
    }

    justification_search &operator=(const justification_search &other)
    {
      q = other.q;
      leaves = other.leaves;
      params = other.params;
      if(other.seen_packages == NULL)
	seen_packages = NULL;
      else
	{
	  int count = (*apt_cache_file)->Head().PackageCount;
	  seen_packages = new bool[count];
	  for(int i = 0; i < count; ++i)
	    seen_packages[i] = other.seen_packages[i];
	}
      first_iteration = other.first_iteration;
    }

    ~justification_search()
    {
      delete[] seen_packages;
    }

    /** \brief Compute the next output of this search.
     *
     *  \param output a vector whose contents will be replaced with the
     *                results of the search (expressed as a sequence
     *                of actions in the order in which they were
     *                performed).  If no justification is found,
     *                output will be set to an empty list.
     *
     *  \return true if a justification was found, false otherwise.
     */
    bool next(std::vector<action> &output)
    {
      std::vector<action> tmp;
      bool reached_leaf = false;

      if(seen_packages == NULL)
	{
	  int count = (*apt_cache_file)->Head().PackageCount;
	  seen_packages = new bool[count];
	  for(int i = 0; i < count; ++i)
	    seen_packages[i] = false;
	}

      if(first_iteration)
	{
	  if(verbosity > 1)
	    std::cout << ssprintf(_("Starting search with parameters %ls\n"),
				  params.description().c_str());
	  first_iteration = false;
	}

      while(!q.empty() && !reached_leaf)
	{
	  // NB: could avoid this copy, but I'd have to move the
	  // pop_front() into all execution branches.  Not worth it.
	  const justification front(q.front());
	  q.pop_front();


	  if(verbosity > 1)
	    {
	      std::auto_ptr<cw::fragment> f(cw::fragf("Search for %F\n",
					      front.description()));
	      std::cout << f->layout(screen_width,
				     screen_width,
				     cw::style());
	    }


	  // If we visited this package already, skip it.  Otherwise,
	  // flag it as visited.
	  pkgCache::PkgIterator frontpkg = front.get_target().get_visited_package();
	  int frontid = frontpkg->ID;
	  bool &package_is_seen = seen_packages[frontid];
	  if(package_is_seen)
	    continue;
	  package_is_seen = true;

	  // Test whether the front node is a leaf; if it is, return
	  // it and quit.
	  pkgCache::VerIterator frontver = params.selected_version(frontpkg);
	  if(!frontver.end())
	    for(std::vector<pkg_matcher *>::const_iterator it = leaves.begin();
		!reached_leaf && it != leaves.end(); ++it)
	      {
		if(apply_matcher((*it),
				 frontpkg, frontver,
				 *apt_cache_file,
				 *apt_package_records))
		  reached_leaf = true;
	      }
	  if(reached_leaf)
	    {
	      tmp.insert(tmp.begin(),
			 front.get_actions().begin(),
			 front.get_actions().end());
	    }
	  else
	    // Since this isn't a leaf, stick its successors on the
	    // queue and carry on.
	    front.generate_successors(q, params, verbosity);
	}

      output.swap(tmp);
      return reached_leaf;
    }
  };
    }

    bool find_justification(const target &target,
			    const std::vector<aptitude::matching::pkg_matcher *> leaves,
			    const search_params &params,
			    bool find_all,
			    std::vector<std::vector<action> > &output)
    {
      justification_search search(leaves, target, params, 0);

      std::vector<std::vector<action> > rval;
      std::vector<action> tmp;

      int i = 0;

      while((i == 0 || find_all) && search.next(tmp))
	{
	  rval.push_back(std::vector<action>());
	  rval.back().swap(tmp);
	}

      if(rval.size() > 0)
	{
	  output.swap(rval);
	  return true;
	}
      else
	return false;
    }
  }
}

cw::fragment *do_why(const std::vector<pkg_matcher *> &leaves,
		 const pkgCache::PkgIterator &root,
		 int verbosity,
		 bool root_is_removal,
		 bool &success)
{
  using namespace aptitude::why;

  std::vector<cw::fragment *> rval;
  success = true;

  std::vector<search_params> searches;

  // The priority of searches goes like this:
  // (1) install version, depends only
  // (2) current version, depends only
  // (3) install version, recommends or depends
  // (4) current version, recommends or depends
  // (5) install version, recommends or depends or suggests
  // (6) current version, recommends or depends or suggests
  searches.push_back(search_params(search_params::Install,
				   search_params::DependsOnly,
				   false));
  searches.push_back(search_params(search_params::Current,
				   search_params::DependsOnly,
				   false));

  searches.push_back(search_params(search_params::Install,
				   search_params::DependsOnly,
				   true));
  searches.push_back(search_params(search_params::Current,
				   search_params::DependsOnly,
				   true));



  searches.push_back(search_params(search_params::Install,
				   search_params::Recommends,
				   false));
  searches.push_back(search_params(search_params::Current,
				   search_params::Recommends,
				   false));

  searches.push_back(search_params(search_params::Install,
				   search_params::Recommends,
				   true));
  searches.push_back(search_params(search_params::Current,
				   search_params::Recommends,
				   true));





  searches.push_back(search_params(search_params::Install,
				   search_params::Suggests,
				   false));

  searches.push_back(search_params(search_params::Current,
				   search_params::Suggests,
				   false));

  searches.push_back(search_params(search_params::Install,
				   search_params::Suggests,
				   true));

  searches.push_back(search_params(search_params::Current,
				   search_params::Suggests,
				   true));




  // As a last-ditch thing, run searches against candidate versions.
  // We prefer *any* match that sticks to current/future installed versions
  // to this, though.
  searches.push_back(search_params(search_params::Candidate,
				   search_params::DependsOnly,
				   false));
  searches.push_back(search_params(search_params::Candidate,
				   search_params::DependsOnly,
				   true));


  searches.push_back(search_params(search_params::Candidate,
				   search_params::Recommends,
				   false));
  searches.push_back(search_params(search_params::Candidate,
				   search_params::Recommends,
				   true));

  searches.push_back(search_params(search_params::Candidate,
				   search_params::Suggests,
				   false));
  searches.push_back(search_params(search_params::Candidate,
				   search_params::Suggests,
				   true));


  // Throw out completely identical search results.  (note that this
  // might not perfectly eliminate results that appear identical if
  // multiple versions of something are available; needs more work to
  // do that)
  std::set<std::vector<action> > seen_results;
  std::vector<action> results;
  bool first = true;

  for(std::vector<search_params>::const_iterator it = searches.begin();
      it != searches.end(); ++it)
    {
      target goal = root_is_removal ? target::Remove(root) : target::Install(root);
      justification_search search(leaves, goal, *it, verbosity);

      while(search.next(results))
	{
	  if(seen_results.find(results) != seen_results.end())
	    {
	      if(verbosity > 1)
		std::cout << ssprintf(_("Skipping this solution, I've already seen it.\n"));
	      continue;
	    }
	  else
	    seen_results.insert(results);

	  if(first)
	    first = false;
	  else
	    rval.push_back(cw::newline_fragment());

	  if(results.empty())
	    return cw::fragf(_("The package \"%s\" is a starting point of the search.\n"),
			 root.Name());
	  else
	    {
	      std::vector<cw::fragment *> col1_entries, col2_entries, col3_entries;
	      for(std::vector<action>::const_iterator it = results.begin();
		  it != results.end(); ++it)
		{
		  col1_entries.push_back(cw::hardwrapbox(cw::style_fragment(cw::fragf("%F\n", it->description_column1_fragment()),
									it->get_style())));
		  col2_entries.push_back(cw::hardwrapbox(cw::style_fragment(cw::fragf("%F\n", it->description_column2_fragment()),
									it->get_style())));
		  col3_entries.push_back(cw::hardwrapbox(cw::style_fragment(cw::fragf("%F\n", it->description_column3_fragment()),
									it->get_style())));
		}

	      using cw::fragment_column_entry;

	      std::vector<fragment_column_entry> columns;
	      columns.push_back(fragment_column_entry(false,
						      true,
						      0,
						      fragment_column_entry::top,
						      col1_entries));
	      columns.push_back(fragment_column_entry(false,
						      false,
						      1,
						      fragment_column_entry::top,
						      NULL));
	      columns.push_back(fragment_column_entry(false,
						      true,
						      0,
						      fragment_column_entry::top,
						      col2_entries));
	      columns.push_back(fragment_column_entry(false,
						      false,
						      1,
						      fragment_column_entry::top,
						      NULL));
	      columns.push_back(fragment_column_entry(false,
						      true,
						      0,
						      fragment_column_entry::top,
						      col3_entries));
	      cw::fragment *solution_fragment(cw::fragment_columns(columns));

	      if(verbosity < 1)
		return solution_fragment;
	      else
		rval.push_back(solution_fragment);
	    }
	}
    }

  if(first)
    {
      success = false;

      if(root_is_removal)
	return cw::fragf(_("No justification for removing %s could be constructed.\n"), root.Name());
      else
	return cw::fragf(_("No justification for %s could be constructed.\n"), root.Name());
    }
  else
    return cw::sequence_fragment(rval);
}

int do_why(const std::vector<pkg_matcher *> &leaves,
	   const pkgCache::PkgIterator &root,
	   int verbosity,
	   bool root_is_removal)
{
  bool success = false;
  std::auto_ptr<cw::fragment> f(do_why(leaves, root, verbosity, root_is_removal,
				   success));
  update_screen_width();
  // TODO: display each result as we find it.
  std::cout << f->layout(screen_width, screen_width, cw::style());

  return success ? 0 : 1;
}

cw::fragment *do_why(const std::vector<pkg_matcher *> &leaves,
		 const pkgCache::PkgIterator &root,
		 bool find_all,
		 bool root_is_removal,
		 bool &success)
{
  const int verbosity = find_all ? 1 : 0;
  return do_why(leaves, root, verbosity, root_is_removal, success);
}

bool interpret_why_args(const std::vector<std::string> &args,
			std::vector<pkg_matcher *> &output)
{
  bool parsing_arguments_failed = false;

  for(std::vector<std::string>::const_iterator it = args.begin();
      it != args.end(); ++it)
    {
      // If there isn't a tilde, treat it as an exact package name.
      pkg_matcher *m = NULL;
      if(!cmdline_is_search_pattern(*it))
	{
	  pkgCache::PkgIterator pkg = (*apt_cache_file)->FindPkg(*it);
	  if(pkg.end())
	    _error->Error(_("No package named \"%s\" exists."), it->c_str());
	  else
	    m = make_const_matcher(pkg);
	}
      else
	m = parse_pattern(*it);

      if(m == NULL)
	parsing_arguments_failed = true;
      else
	output.push_back(m);
    }

  return !parsing_arguments_failed;
}

cw::fragment *do_why(const std::vector<std::string> &arguments,
		 const std::string &root,
		 int verbosity,
		 bool root_is_removal,
		 bool &success)
{
  success = false;

  pkgCache::PkgIterator pkg((*apt_cache_file)->FindPkg(root.c_str()));
  if(pkg.end())
    return cw::fragf(_("No package named \"%s\" exists."), root.c_str());

  std::vector<pkg_matcher *> matchers;
  if(!interpret_why_args(arguments, matchers))
    {
      for(std::vector<pkg_matcher *>::const_iterator it = matchers.begin();
	  it != matchers.end(); ++it)
	delete *it;
      return cw::text_fragment(_("Unable to parse some match patterns."));
    }

  cw::fragment *rval = do_why(matchers, pkg, verbosity, root_is_removal, success);
  for(std::vector<pkg_matcher *>::const_iterator it = matchers.begin();
      it != matchers.end(); ++it)
    delete *it;
  return rval;
}

cw::fragment *do_why(const std::vector<std::string> &leaves,
		 const std::string &root,
		 bool find_all,
		 bool root_is_removal,
		 bool &success)
{
  return do_why(leaves, root, find_all ? 1 : 0, root_is_removal, success);
}

int cmdline_why(int argc, char *argv[],
		const char *status_fname, int verbosity,
		bool is_why_not)
{
  _error->DumpErrors();

  if(argc < 2)
    {
      fprintf(stderr, _("%s: this command requires at least one argument (the package to query)."),
	      argv[0]);
      return -1;
    }

  OpProgress progress;

  apt_init(&progress, true, status_fname);

  if(_error->PendingError())
    {
      _error->DumpErrors();
      return -1;
    }

  update_screen_width();

  // Keep track of whether any argument couldn't be parsed, but
  // don't bail until we finish parsing, so we can display all
  // the errors we find.
  bool parsing_arguments_failed = false;

  const char *pkgname = argv[argc - 1];
  bool is_removal = is_why_not;
  pkgCache::PkgIterator pkg = (*apt_cache_file)->FindPkg(argv[argc - 1]);
  if(pkg.end())
    {
      _error->Error(_("No package named \"%s\" exists."), pkgname);
      parsing_arguments_failed = true;
    }

  std::vector<std::string> arguments;
  for(int i = 1; i + 1 < argc; ++i)
    arguments.push_back(argv[i]);
  std::vector<pkg_matcher *> matchers;
  if(!interpret_why_args(arguments, matchers))
    parsing_arguments_failed = true;

  if(matchers.empty())
    {
      pkg_matcher *m = parse_pattern("~i!~M");
      if(m == NULL)
	parsing_arguments_failed = true;
      else
	{
	  matchers.push_back(m);
	  if(!pkg.end() && apply_matcher(m, pkg, pkg.CurrentVer(),
					 *apt_cache_file, *apt_package_records))
	    {
	      printf(_("The package \"%s\" is manually installed.\n"),
		     pkg.Name());
	      delete m;
	      return 0;
	    }
	}
    }



  _error->DumpErrors();

  int rval;
  if(parsing_arguments_failed)
    rval = -1;
  else
    rval = do_why(matchers, pkg, verbosity, is_removal);

  for(std::vector<pkg_matcher *>::const_iterator it = matchers.begin();
      it != matchers.end(); ++it)
    delete *it;
  return rval;
}
