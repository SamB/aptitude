// cmdline_forget_new.cc
//
//   Copyright 2004 Daniel Burrows

#include "cmdline_forget_new.h"

#include <aptitude.h>

#include <generic/apt/apt.h>
#include <generic/apt/config_signal.h>

#include <apt-pkg/error.h>

using namespace std;

int cmdline_forget_new(int argc, char *argv[],
		       const char *status_fname, bool simulate)
{
  _error->DumpErrors();

  OpTextProgress progress(aptcfg->FindI("Quiet", 0));

  apt_init(&progress, false, status_fname);

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

  if(simulate)
    printf(_("Would forget what packages are new\n"));
  else
    {
      (*apt_cache_file)->forget_new(NULL);

      (*apt_cache_file)->save_selection_list(progress);

      if(_error->PendingError())
	{
	  _error->DumpErrors();

	  return -1;
	}
    }

  return 0;
}

