// cmdline_extract_cache_subset.cc
//
//   Copyright (C) 2008-2009 Daniel Burrows
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

#include "cmdline_extract_cache_subset.h"

#include "cmdline_util.h"

#include <aptitude.h>

#include <generic/apt/apt.h>
#include <generic/apt/dump_packages.h>
#include <generic/apt/matching/match.h>
#include <generic/apt/matching/parse.h>
#include <generic/apt/matching/pattern.h>

#include <stdio.h>

#include <apt-pkg/error.h>
#include <apt-pkg/progress.h>

namespace aptitude
{
  namespace cmdline
  {
    int extract_cache_subset(int argc, char *argv[])
    {
      if(argc < 2)
	{
	  fprintf(stderr, _("extract-cache-entries: at least one argument is required (the directory\nto which to write files).\n"));
	  return -1;
	}

      std::string out_dir = argv[1];

      OpTextProgress progress;

      apt_init(&progress, true);
      if(_error->PendingError())
	{
	  _error->DumpErrors();
	  return -1;
	}

      bool ok = true;
      std::set<pkgCache::PkgIterator> packages;
      if(argc == 2)
	{
	  for(pkgCache::PkgIterator pIt = (*apt_cache_file)->PkgBegin();
	      !pIt.end(); ++pIt)
	    packages.insert(pIt);
	}
      else
	{
	  for(int i = 2; i < argc; ++i)
	    {
	      std::string arg(argv[i]);

	      if(!aptitude::matching::is_pattern(arg))
		{
		  pkgCache::PkgIterator pIt = (*apt_cache_file)->FindPkg(arg);
		  if(pIt.end())
		    {
		      fprintf(stderr, _("No such package \"%s\".\n"),
			      arg.c_str());
		      ok = false;
		    }
		  else
		    packages.insert(pIt);
		}
	      else
		{
		  using namespace aptitude::matching;
		  using cwidget::util::ref_ptr;

		  ref_ptr<pattern> p = parse(arg);

		  if(p.valid())
		    {
		      _error->DumpErrors();
		    }
		  else
		    {
		      std::vector<std::pair<pkgCache::PkgIterator, ref_ptr<structural_match> > > matches;
		      ref_ptr<search_cache> search_info(search_cache::create());
		      search(p, search_info,
			     matches,
			     *apt_cache_file,
			     *apt_package_records);

		      for(std::vector<std::pair<pkgCache::PkgIterator, ref_ptr<structural_match> > >::const_iterator
			    it = matches.begin(); it != matches.end(); ++it)
			packages.insert(it->first);
		    }
		}
	    }
	}

      if(!ok)
	return 2;

      if(packages.size() == 0)
	{
	  printf(_("No packages were selected by the given search pattern; nothing to do.\n"));
	  return 0;
	}

      aptitude::apt::make_truncated_state_copy(out_dir, packages);

      bool copy_ok = !_error->PendingError();

      _error->DumpErrors();

      return copy_ok ? 0 : 1;
    }
  }
}
