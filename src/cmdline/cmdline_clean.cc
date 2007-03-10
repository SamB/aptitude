// cmdline_clean.cc
//
//  Copyright 2004 Daniel Burrows

#include "cmdline_clean.h"

#include <aptitude.h>

#include <generic/apt/apt.h>
#include <generic/apt/config_signal.h>

#include <apt-pkg/acquire.h>
#include <apt-pkg/clean.h>
#include <apt-pkg/error.h>
#include <apt-pkg/strutl.h>

#include <iostream>

#include <sys/stat.h>

using namespace std;

int cmdline_clean(int argc, char *argv[], bool simulate)
{
  _error->DumpErrors();

  if(argc != 1)
    {
      fprintf(stderr, _("E: The clean command takes no arguments\n"));
      return -1;
    }  

  OpTextProgress progress(aptcfg->FindI("Quiet", 0));

  apt_init(&progress, false);

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

  if(aptcfg)
    {
      if(simulate)
	printf(_("Del %s* %spartial/*\n"),
	       aptcfg->FindDir("Dir::Cache::archives").c_str(),
	       aptcfg->FindDir("Dir::Cache::archives").c_str());
      else
	{
	  pkgAcquire fetcher;
	  fetcher.Clean(aptcfg->FindDir("Dir::Cache::archives"));
	  fetcher.Clean(aptcfg->FindDir("Dir::Cache::archives")+"partial/");
	}
    }

  int rval=_error->PendingError() ? -1 : 0;

  _error->DumpErrors();

  return rval;
}

// Shamelessly stolen from apt-get:
class LogCleaner : public pkgArchiveCleaner
{
  bool simulate;

  long total_size;

protected:
  virtual void Erase(const char *File,string Pkg,string Ver,struct stat &St) 
  {
    printf(_("Del %s %s [%sB]\n"),
	   Pkg.c_str(),
	   Ver.c_str(),
	   SizeToStr(St.st_size).c_str());

    if (!simulate)
      {
	if(unlink(File)==0)
	  total_size+=St.st_size;
      }
    else
      total_size+=St.st_size;
  };
public:
  LogCleaner(bool _simulate):simulate(_simulate), total_size(0) { }

  long get_total_size() {return total_size;}
};

int cmdline_autoclean(int argc, char *argv[], bool simulate)
{
  _error->DumpErrors();

  if(argc != 1)
    {
      fprintf(stderr, _("E: The autoclean command takes no arguments\n"));
      return -1;
    }  

  OpTextProgress progress(aptcfg->FindI("Quiet", 0));

  apt_init(&progress, false);

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

  LogCleaner cleaner(simulate);
  int rval=0;
  if(!(cleaner.Go(aptcfg->FindDir("Dir::Cache::archives"), *apt_cache_file) &&
       cleaner.Go(aptcfg->FindDir("Dir::Cache::archives")+"partial/",
		  *apt_cache_file)) ||
     _error->PendingError())
    rval=-1;

  _error->DumpErrors();

  if(simulate)
    printf(_("Would free %sB of disk space\n"),
	   SizeToStr(cleaner.get_total_size()).c_str());
  else
    printf(_("Freed %sB of disk space\n"),
	   SizeToStr(cleaner.get_total_size()).c_str());

  return rval;
}

