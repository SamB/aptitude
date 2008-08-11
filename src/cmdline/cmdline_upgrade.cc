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
#include <apt-pkg/policy.h>
#include <apt-pkg/progress.h>

int cmdline_upgrade(int argc, char *argv[],
		    const char *status_fname, bool simulate,
		    bool no_new_installs, bool show_resolver_actions,
		    bool assume_yes, bool download_only,
		    bool showvers, bool showdeps,
		    bool showsize, bool showwhy,
		    const std::vector<aptitude::cmdline::tag_application> &user_tags,
		    bool visual_preview,
		    bool always_prompt, bool arch_only,
		    bool queue_only, int verbose)
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

  pkgPolicy policy(&(*apt_cache_file)->GetCache());
  ReadPinFile(policy);

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

  // First try the built-in resolver; if it fails (which it shouldn't
  // if an upgrade is possible), cancel all changes and try apt's
  // resolver.
  (*apt_cache_file)->mark_all_upgradable(false, true, NULL);

  if(verbose > 0 && (*apt_cache_file)->BrokenCount() > 0)
    {
      pkgset to_install, to_hold, to_remove;
      cmdline_show_preview(true,
			   to_install,
			   to_hold,
			   to_remove,
			   showvers,
			   showdeps,
			   showsize,
			   showwhy,
			   verbose);
      show_broken();
    }

  if(!aptitude::cmdline::safe_resolve_deps(verbose, no_new_installs, true, show_resolver_actions))
    {
      {
	aptitudeDepCache::action_group action_group(*apt_cache_file);

	// Reset all the package states.
	for(pkgCache::PkgIterator i=(*apt_cache_file)->PkgBegin();
	    !i.end(); ++i)
	  {
	    bool held = (*apt_cache_file)->get_ext_state(i).selection_state == pkgCache::State::Hold;
	    (*apt_cache_file)->mark_keep(i, false, held, NULL);
	  }
      }

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
			    showvers, showdeps, showsize, showwhy,
			    always_prompt, verbose, assume_yes,
			    false,
			    policy, arch_only);
  else if(queue_only)
    {
      aptitude::cmdline::apply_user_tags(user_tags);

      if(!(*apt_cache_file)->save_selection_list(progress))
	return -1;
      else
	return 0;
    }
  else
    {
      if(!cmdline_do_prompt(true, to_install, to_hold, to_remove,
			    to_purge, showvers, showdeps,
			    showsize, showwhy,
			    always_prompt, verbose,
			    assume_yes, false,
			    policy, arch_only))
	{
	  printf(_("Abort.\n"));
	  return 0;
	}

      aptitude::cmdline::apply_user_tags(user_tags);

      download_install_manager m(download_only);
      int rval =
	(cmdline_do_download(&m, verbose) == download_manager::success ? 0 : -1);

      if(_error->PendingError())
	rval = -1;

      _error->DumpErrors();

      return rval;
    }
}

