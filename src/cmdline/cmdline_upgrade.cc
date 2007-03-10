// cmdline_upgrade.cc
//
//  Copyright 2004 Daniel Burrows

#include "cmdline_upgrade.h"

#include "cmdline_progress.h"
#include "cmdline_prompt.h"
#include "cmdline_show_broken.h"
#include "cmdline_simulate.h"
#include "cmdline_util.h"

#include <aptitude.h>

#include <generic/apt/apt.h>
#include <generic/apt/config_signal.h>
#include <generic/apt/download_install_manager.h>

#include <apt-pkg/depcache.h>
#include <apt-pkg/error.h>
#include <apt-pkg/packagemanager.h>
#include <apt-pkg/progress.h>

int cmdline_upgrade(int argc, char *argv[],
		    const char *status_fname, bool simulate,
		    bool assume_yes, bool download_only,
		    bool showvers, bool showdeps, bool showsize,
		    bool visual_preview,
		    bool always_prompt, bool queue_only,
		    int verbose)
{
  pkgset to_install, to_hold, to_remove, to_purge;

  _error->DumpErrors();

  if(argc != 1)
    {
      fprintf(stderr, _("E: The upgrade command takes no arguments\n"));
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

  if(!(*apt_cache_file)->all_upgrade(false, NULL))
    {
      show_broken();

      _error->DumpErrors();
      return -1;
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

