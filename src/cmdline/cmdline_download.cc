// cmdline_download.cc
//
//  Copyright 2004 Daniel Burrows

#include "cmdline_download.h"

#include "cmdline_common.h"
#include "cmdline_progress.h"
#include "cmdline_util.h"

#include <aptitude.h>

#include <generic/apt/apt.h>
#include <generic/apt/config_signal.h>
#include <generic/apt/download_signal_log.h>
#include <generic/apt/pkg_acqfile.h>

#include <apt-pkg/acquire.h>
#include <apt-pkg/error.h>
#include <apt-pkg/progress.h>
#include <apt-pkg/sourcelist.h>

#include <stdio.h>

// Download stuff to the current directory
int cmdline_download(int argc, char *argv[])
{
  if(argc<=1)
    {
      printf(_("download: you must specify at least one package to download\n"));
      return -1;
    }

  _error->DumpErrors();

  OpTextProgress progress(aptcfg->FindI("Quiet", 0));
  apt_init(&progress, false);

  pkgSourceList list;
  if(!list.ReadMainList())
    {
      _error->Error(_("Couldn't read source list"));

      _error->DumpErrors();
      return -1;
    }
  pkgAcquire fetcher(gen_cmdline_download_progress());
  string filenames[(*apt_cache_file)->Head().PackageCount];
  string default_release = aptcfg->Find("APT::Default-Release");

  for(int i=1; i<argc; ++i)
    // FIXME: use the same logic as pkgaction here.
    if(!strchr(argv[i], '~'))
      {
	cmdline_version_source source;
	string name, sourcestr;

	if(!cmdline_parse_source(argv[i], source, name, sourcestr))
	  continue;

	if(source == cmdline_version_cand && !default_release.empty())
	  {
	    source = cmdline_version_archive;
	    sourcestr = default_release;
	  }

	pkgCache::PkgIterator pkg=(*apt_cache_file)->FindPkg(name);
	if(pkg.end())
	  {
	    _error->Error(_("Can't find a package named \"%s\""), name.c_str());
	    continue;
	  }

	pkgCache::VerIterator ver=cmdline_find_ver(pkg, source, sourcestr);

	if(ver.end())
	  continue;

	if(!ver.Downloadable())
	  _error->Error(_("No downloadable files for %s version %s; perhaps it is a local or obsolete package?"),
			name.c_str(), ver.VerStr());

	get_archive(&fetcher, &list, apt_package_records,
		    ver, ".", filenames[pkg->ID]);
      }

  if(fetcher.Run()!=pkgAcquire::Continue)
    // We failed or were cancelled
    {
      _error->DumpErrors();
      return -1;
    }

  return 0;
}
