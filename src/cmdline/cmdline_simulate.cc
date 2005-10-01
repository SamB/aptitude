// cmdline_simulate.cc
//
//  Copyright 2004 Daniel Burrows

#include "cmdline_simulate.h"

#include "cmdline_common.h"
#include "cmdline_prompt.h"

#include <aptitude.h>

#include <generic/apt/apt.h>

#include <apt-pkg/algorithms.h>
#include <apt-pkg/error.h>

#include <stdio.h>

int cmdline_simulate(bool as_upgrade,
		     pkgset &to_install, pkgset &to_hold, pkgset &to_remove,
		     pkgset &to_purge,
		     bool showvers, bool showdeps, bool showsize,
		     bool always_prompt, int verbose,
		     bool assume_yes, bool force_no_change)
{
  if(!cmdline_do_prompt(as_upgrade,
			to_install, to_hold, to_remove, to_purge,
			showvers, showdeps, showsize, always_prompt, verbose,
			assume_yes, force_no_change))
    {
      printf(_("Abort.\n"));
      return 0;
    }

  if(verbose==0)
    {
      printf(_("Would download/install/remove packages.\n"));
      return 0;
    }

  pkgSimulate PM(*apt_cache_file);
  pkgPackageManager::OrderResult Res=PM.DoInstall();

  if(Res==pkgPackageManager::Failed)
    return -1;
  else if(Res!=pkgPackageManager::Completed)
    {
      _error->Error(_("Internal Error, Ordering didn't finish"));
      return -1;
    }
  else
    return 0;
}
