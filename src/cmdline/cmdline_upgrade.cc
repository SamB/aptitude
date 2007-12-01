// cmdline_upgrade.cc
//
//  Copyright 2004 Daniel Burrows

#include "cmdline_upgrade.h"

#include "cmdline_progress.h"
#include "cmdline_prompt.h"
#include "cmdline_resolver.h" // for cmdline_dump_resolver.
#include "cmdline_show_broken.h"
#include "cmdline_simulate.h"
#include "cmdline_util.h"

#include <aptitude.h>

#include <generic/apt/apt.h>
#include <generic/apt/aptitude_resolver_universe.h>
#include <generic/apt/config_signal.h>
#include <generic/apt/download_install_manager.h>
#include <generic/apt/resolver_manager.h>

#include <generic/problemresolver/exceptions.h>

#include <apt-pkg/depcache.h>
#include <apt-pkg/error.h>
#include <apt-pkg/packagemanager.h>
#include <apt-pkg/progress.h>

#include <cwidget/generic/util/ssprintf.h>

namespace cw = cwidget;

namespace
{
  // Set up aptitude's internal resolver for a 'safe' upgrade.
  // Essentialy this means (1) forbid it from removing any package,
  // and (2) only install the default candidate version of a package
  // or the current version.
  void setup_resolver_for_safe_upgrade()
  {
    if(!resman->resolver_exists())
      return;

    resman->reset_resolver();

    resman->set_debug(aptcfg->FindB(PACKAGE "::CmdLine::Resolver-Debug", false));

    for(pkgCache::PkgIterator p = (*apt_cache_file)->PkgBegin();
	!p.end(); ++p)
      {
	// Forbid the resolver from removing installed packages.
	if(!p.CurrentVer().end())
	  {
	    aptitude_resolver_version
	      remove_p(p,
		       pkgCache::VerIterator(*apt_cache_file),
		       *apt_cache_file);

	    resman->reject_version(remove_p);
	  }

	// Forbid all real versions that aren't the current version or
	// the candidate version.
	for(pkgCache::VerIterator v = p.VersionList();
	    !v.end(); ++v)
	  {
	    if(v != p.CurrentVer() &&
	       v != (*apt_cache_file)[p].CandidateVerIter(*apt_cache_file))
	      {
		aptitude_resolver_version
		  p_v(p, v, *apt_cache_file);
		resman->reject_version(p_v);
	      }
	  }
      }

    cmdline_dump_resolver();
  }

  // Take the first solution we can compute, returning false if we
  // failed to find a solution.
  bool safe_upgrade_resolve_deps(int verbose)
  {
    if(!resman->resolver_exists())
      return true;

    setup_resolver_for_safe_upgrade();

    try
      {
	generic_solution<aptitude_universe> sol = calculate_current_solution();
	(*apt_cache_file)->apply_solution(sol, NULL);
      }
    // If anything goes wrong, we give up (silently if verbosity is disabled).
    catch(NoMoreTime)
      {
	if(verbose > 0)
	  std::cout << _("Unable to resolve dependencies for the upgrade (resolver timed out).");
	return false;
      }
    catch(NoMoreSolutions)
      {
	if(verbose > 0)
	  std::cout << _("Unable to resolve dependencies for the upgrade (no solution found).");
	return false;
      }
    catch(const cw::util::Exception &e)
      {
	if(verbose > 0)
	  std::cout << cw::util::ssprintf(_("Unable to resolve dependencies for the upgrade (%s)."), e.errmsg().c_str());
	return false;
      }

    return true;
  }
}

int cmdline_upgrade(int argc, char *argv[],
		    const char *status_fname, bool simulate,
		    bool assume_yes, bool download_only,
		    bool showvers, bool showdeps, bool showsize,
		    bool visual_preview,
		    bool always_prompt, bool queue_only,
		    int verbose)
{
  pkgset to_install, to_hold, to_remove, to_purge;

  if(!strcasecmp(argv[0], "upgrade"))
    _error->Warning("The \"upgrade\" command is deprecated; use \"safe-upgrade\" instead.");

  _error->DumpErrors();

  if(argc != 1)
    {
      fprintf(stderr, _("E: The %s command takes no arguments\n"), argv[0]);
      return -1;
    }

  OpTextProgress progress(aptcfg->FindI("Quiet", 0));
  apt_init(&progress, false, status_fname);

  if(_error->PendingError())
    {
      _error->DumpErrors();
      return -1;
    }

  if(!simulate)
    apt_cache_file->GainLock();
  else
    apt_cache_file->ReleaseLock();

  if(_error->PendingError())
    {
      _error->DumpErrors();
      return -1;
    }

  // Build to_install to avoid a big printout
  for(pkgCache::PkgIterator i=(*apt_cache_file)->PkgBegin(); !i.end(); ++i)
    {
      pkgDepCache::StateCache &state=(*apt_cache_file)[i];

      if(!i.CurrentVer().end() &&
	 state.Upgradable() && !(*apt_cache_file)->is_held(i))
	to_install.insert(i);
    }

  if(verbose > 0)
    show_broken();

  // First try the built-in resolver; if it fails (which it shouldn't
  // if an upgrade is possible), cancel all changes and try apt's
  // resolver.
  (*apt_cache_file)->mark_all_upgradable(false, true, NULL);
  if(!safe_upgrade_resolve_deps(verbose))
    {
      // Reset all the package states.
      for(pkgCache::PkgIterator i=(*apt_cache_file)->PkgBegin();
	  !i.end(); ++i)
	(*apt_cache_file)->mark_keep(i, false, false, NULL);

      // Use the apt 'upgrade' algorithm as a fallback against, e.g.,
      // bugs in the aptitude resolver.
      if(!(*apt_cache_file)->all_upgrade(false, NULL))
	{
	  show_broken();

	  _error->DumpErrors();
	  return -1;
	}
    }

  if(!show_broken())
    return -1;

  if(visual_preview)
    {
      // we will exit here:
      ui_preview();
      // but just in case
      return 0;
    }
  else if(simulate)
    return cmdline_simulate(true, to_install, to_hold, to_remove, to_purge,
			    showvers, showdeps, showsize,
			    always_prompt, verbose, assume_yes,
			    false);
  else if(queue_only)
    {
      if(!(*apt_cache_file)->save_selection_list(progress))
	return -1;
      else
	return 0;
    }
  else
    {

      if(!cmdline_do_prompt(true, to_install, to_hold, to_remove,
			    to_purge, showvers, showdeps, showsize,
			    always_prompt, verbose,
			    assume_yes, false))
	{
	  printf(_("Abort.\n"));
	  return 0;
	}

      download_install_manager m(download_only);
      int rval =
	(cmdline_do_download(&m) == download_manager::success ? 0 : -1);

      if(_error->PendingError())
	rval = -1;

      _error->DumpErrors();

      return rval;
    }
}

