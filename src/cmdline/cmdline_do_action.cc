// cmdline_do_action.cc
//
//  Copyright 2004 Daniel Burrows

#include "cmdline_do_action.h"

#include "cmdline_action.h"
#include "cmdline_common.h"
#include "cmdline_prompt.h"
#include "cmdline_resolver.h"
#include "cmdline_show_broken.h"
#include "cmdline_simulate.h"
#include "cmdline_util.h"

#include <generic/apt/apt.h>
#include <generic/apt/config_signal.h>
#include <generic/apt/download_install_manager.h>

#include <aptitude.h>

#include <apt-pkg/algorithms.h>
#include <apt-pkg/error.h>
#include <apt-pkg/progress.h>

using namespace std;

// TODO: add an option to obey sticky states even if it wasn't
//      explicitly requested.
//
// TODO: perhaps when trying to find a list of possible candidates for
// installation, we should use a formatted display?
int cmdline_do_action(int argc, char *argv[],
		      const char *status_fname, bool simulate,
		      bool assume_yes, bool download_only, bool fix_broken,
		      bool showvers, bool showdeps, bool showsize,
		      bool visual_preview, bool always_prompt,
		      bool queue_only, int verbose)
{
  _error->DumpErrors();

  cmdline_pkgaction_type default_action=cmdline_install;
  bool dist_upgrade=false;

  // Parse the action.
  if(!strcasecmp(argv[0], "install"))
    default_action=cmdline_install;
  else if(!strcasecmp(argv[0], "reinstall"))
    default_action=cmdline_reinstall;
  else if(!strcasecmp(argv[0], "dist-upgrade"))
    {
      default_action=cmdline_install;
      dist_upgrade=true;
    }
  else if(!strcasecmp(argv[0], "remove"))
    default_action=cmdline_remove;
  else if(!strcasecmp(argv[0], "purge"))
    default_action=cmdline_purge;
  else if(!strcasecmp(argv[0], "hold"))
    default_action=cmdline_hold;
  else if(!strcasecmp(argv[0], "keep") || !strcasecmp(argv[0], "keep-all"))
    default_action=cmdline_keep;
  else if(!strcasecmp(argv[0], "unhold"))
    default_action=cmdline_unhold;
  else if(!strcasecmp(argv[0], "markauto"))
    default_action=cmdline_markauto;
  else if(!strcasecmp(argv[0], "unmarkauto"))
    default_action=cmdline_unmarkauto;
  else if(!strcasecmp(argv[0], "forbid-version"))
    default_action=cmdline_forbid_version;
  else
    {
      // Should never happen.
      _error->Error(_("Invalid operation %s"), argv[0]);
      _error->DumpErrors();
      return -1;
    }

  OpTextProgress progress(aptcfg->FindI("Quiet", 0));

  aptcfg->SetNoUser(PACKAGE "::Auto-Upgrade", "false");

  // If there are no arguments and the default action is "install", act
  // on stickies.
  //
  // This way, "aptitude install" will just perform any pending
  // installations.
  apt_init(&progress, (argc==1 && default_action==cmdline_install &&
		       !dist_upgrade), status_fname);

  if(_error->PendingError())
    {
      _error->DumpErrors();
      return -1;
    }

  // In case we aren't root.
  if(!simulate)
    apt_cache_file->GainLock();
  else
    apt_cache_file->ReleaseLock();

  if(_error->PendingError())
    {
      _error->DumpErrors();
      return -1;
    }

  pkgset to_upgrade, to_install, to_hold, to_remove, to_purge;

  if(dist_upgrade)
    {
      // Build to_install to avoid a big printout
      for(pkgCache::PkgIterator i=(*apt_cache_file)->PkgBegin(); !i.end(); ++i)
	{
	  pkgDepCache::StateCache &state=(*apt_cache_file)[i];

	  if(!i.CurrentVer().end() &&
	     state.Upgradable() && !(*apt_cache_file)->is_held(i))
	    to_upgrade.insert(i);

	}

      (*apt_cache_file)->mark_all_upgradable(true, false, NULL);
    }
  /*else if(argc==1 && default_action==cmdline_install)
    {
      // FIXME: Build to_install to avoid a big printout
      for(pkgCache::PkgIterator i=(*apt_cache_file)->PkgBegin(); !i.end(); ++i)
	{

	}
	}*/

  // TODO: look for filenames and call dpkg directly if that's the case.

  (*apt_cache_file)->begin_action_group();
  // If keep-all is the argument, we expect no patterns and keep all
  // packages back.
  if(!strcasecmp(argv[0], "keep-all"))
    {
      if(argc != 1)
	{
	  cerr << _("Unexpected pattern argument following \"keep-all\"") << endl;
	  return -1;
	}

      for(pkgCache::PkgIterator i=(*apt_cache_file)->PkgBegin();
	  !i.end(); ++i)
	(*apt_cache_file)->mark_keep(i, true, false, NULL);
    }
  else
    // If we didn't take the keep-all path, mark according to the
    // action requested.
    for(int i=1; i<argc; ++i)
      {
	cmdline_pkgaction_type action=default_action;
	int tmp=strlen(argv[i])-1;

	// HACK: disable interpreting of escapes if it's an existing
	//      package name.
	if((*apt_cache_file)->FindPkg(argv[i]).end())
	  switch(argv[i][tmp])
	    {
	    case '-':
	      action=cmdline_remove;
	      argv[i][tmp]=0;
	      break;
	    case '=':
	      action=cmdline_hold;
	      argv[i][tmp]=0;
	      break;
	    case '+':
	      action=cmdline_install;
	      argv[i][tmp]=0;
	      break;
	    case '_':
	      action=cmdline_purge;
	      argv[i][tmp]=0;

	      break;
	    case ':':
	      action=cmdline_keep;
	      argv[i][tmp]=0;
	      break;
	    case 'm':
	    case 'M':
	      if(tmp>0 && argv[i][tmp-1]=='&')
		{
		  argv[i][tmp-1]=0;
		  if(argv[i][tmp]=='m')
		    action=cmdline_unmarkauto;
		  else
		    action=cmdline_markauto;
		}
	      else if(tmp>0 && argv[i][tmp-1]=='+' && argv[i][tmp]=='M')
		{
		  argv[i][tmp-1]=0;
		  action=cmdline_installauto;
		}
	    }

	cmdline_applyaction(argv[i], action,
			    to_install, to_hold, to_remove, to_purge,
			    verbose, false);
      }
  if(aptcfg->FindB(PACKAGE "::Auto-Install", true))
    {
      for(pkgset::const_iterator i = to_install.begin(); i != to_install.end(); ++i)
	(*apt_cache_file)->mark_install(*i, true, (*apt_cache_file)->get_ext_state(*i).reinstall, NULL);
    }
  (*apt_cache_file)->end_action_group(NULL);

  if(visual_preview)
    {
      ui_preview();
      return 0;
    }
  else if(simulate)
    return cmdline_simulate(dist_upgrade, to_install, to_hold, to_remove, to_purge,
			    showvers, showdeps, showsize,
			    always_prompt, verbose, assume_yes,
			    !fix_broken);
  else if(queue_only)
    {
      if(!(*apt_cache_file)->save_selection_list(progress))
	{
	  _error->DumpErrors();
	  return -1;
	}
      else
	return 0;
    }
  else
    {
      if(!cmdline_do_prompt(dist_upgrade,
			    to_install, to_hold, to_remove, to_purge,
			    showvers, showdeps, showsize,
			    always_prompt, verbose, assume_yes,
			    !fix_broken))
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
