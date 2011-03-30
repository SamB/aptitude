// cmdline_download.cc
//
//  Copyright 2004 Daniel Burrows

#include "cmdline_download.h"

#include "cmdline_common.h"
#include "cmdline_progress.h"
#include "cmdline_util.h"
#include "terminal.h"
#include "text_progress.h"

#include <aptitude.h>

#include <generic/apt/apt.h>
#include <generic/apt/config_signal.h>
#include <generic/apt/download_signal_log.h>
#include <generic/apt/matching/match.h>
#include <generic/apt/matching/parse.h>
#include <generic/apt/matching/pattern.h>
#include <generic/apt/pkg_acqfile.h>

#include <apt-pkg/acquire.h>
#include <apt-pkg/error.h>
#include <apt-pkg/progress.h>
#include <apt-pkg/sourcelist.h>

#include <stdio.h>

using aptitude::controllers::acquire_download_progress;
using aptitude::cmdline::create_cmdline_download_progress;
using aptitude::cmdline::create_terminal;
using aptitude::cmdline::make_text_progress;
using aptitude::cmdline::terminal_io;
using aptitude::cmdline::terminal_locale;
using boost::shared_ptr;

// Download stuff to the current directory
int cmdline_download(int argc, char *argv[])
{
  shared_ptr<terminal_io> term = create_terminal();

  if(argc<=1)
    {
      printf(_("download: you must specify at least one package to download\n"));
      return -1;
    }

  _error->DumpErrors();

  shared_ptr<OpProgress> progress = make_text_progress(false, term, term, term);
  apt_init(progress.get(), false);

  pkgSourceList list;
  if(!list.ReadMainList())
    {
      _error->Error(_("Couldn't read source list"));

      _error->DumpErrors();
      return -1;
    }

  std::pair<download_signal_log *, boost::shared_ptr<acquire_download_progress> >
    progress_display = create_cmdline_download_progress(term, term, term, term);

  pkgAcquire fetcher;
  fetcher.Setup(progress_display.first);
  string filenames[(*apt_cache_file)->Head().PackageCount];
  string default_release = aptcfg->Find("APT::Default-Release");

  for(int i=1; i<argc; ++i)
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

      std::vector<pkgCache::PkgIterator> packages;

      if(!aptitude::matching::is_pattern(name))
	{
	  pkgCache::PkgIterator pkg=(*apt_cache_file)->FindPkg(name);
	  if(pkg.end())
	    {
	      _error->Error(_("Can't find a package named \"%s\""), name.c_str());
	      continue;
	    }

	  packages.push_back(pkg);
	}
      else
	{
	  using namespace aptitude::matching;
	  using cwidget::util::ref_ptr;
	  ref_ptr<pattern> p(parse(name.c_str()));
	  if(!p.valid())
	    {
	      _error->DumpErrors();
	      return false;
	    }

	  std::vector<std::pair<pkgCache::PkgIterator, ref_ptr<structural_match> > > matches;
	  ref_ptr<search_cache> search_info(search_cache::create());
	  search(p, search_info,
		 matches,
		 *apt_cache_file,
		 *apt_package_records);

	  for(std::vector<std::pair<pkgCache::PkgIterator, ref_ptr<structural_match> > >::const_iterator
		it = matches.begin(); it != matches.end(); ++it)
	    packages.push_back(it->first);

	  // Maybe there should be a warning here if packages is
	  // empty?  TODO: think about it again when the string freeze
	  // is lifted post-lenny.
	}

      for(std::vector<pkgCache::PkgIterator>::const_iterator it =
	    packages.begin(); it != packages.end(); ++it)
	{
	  const pkgCache::PkgIterator pkg = *it;

	  pkgCache::VerIterator ver=cmdline_find_ver(pkg, source, sourcestr);

	  if(ver.end())
	    continue;

	  if(!ver.Downloadable())
	    _error->Error(_("No downloadable files for %s version %s; perhaps it is a local or obsolete package?"),
			  name.c_str(), ver.VerStr());

	  get_archive(&fetcher, &list, apt_package_records,
		      ver, ".", filenames[pkg->ID]);
	}
    }

  if(fetcher.Run()!=pkgAcquire::Continue)
    // We failed or were cancelled
    {
      _error->DumpErrors();
      return -1;
    }

  return 0;
}
