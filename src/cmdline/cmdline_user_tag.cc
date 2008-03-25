// cmdline_user_tag.cc
//
//   Copyright (C) 2008 Daniel Burrows
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

#include "cmdline_user_tag.h"

#include "cmdline_util.h"

#include <aptitude.h>

#include <apt-pkg/error.h>

#include <generic/apt/apt.h>
#include <generic/apt/aptcache.h>
#include <generic/apt/matchers.h>

#include <string.h>

namespace aptitude
{
  namespace cmdline
  {
    namespace
    {
      enum user_tag_action { action_add, action_remove };

      void do_user_tag(user_tag_action act,
		       const std::string &tag,
		       const pkgCache::PkgIterator &pkg,
		       int verbose)
      {
	switch(act)
	  {
	  case action_add:
	    if(verbose > 0)
				// Sometimes also "user-tag" is used!
	      printf(_("Adding user tag \"%s\" to the package \"%s\".\n"),
		     tag.c_str(), pkg.Name());

	    (*apt_cache_file)->attach_user_tag(pkg, tag, NULL);
	    break;
	  case action_remove:
	    if(verbose > 0)
	      printf(_("Removing user tag \"%s\" from the package \"%s\".\n"),
		     tag.c_str(), pkg.Name());

	    (*apt_cache_file)->detach_user_tag(pkg, tag, NULL);
	    break;
	  default:
	    fprintf(stderr, "Internal error: bad user tag action %d.", act);
	    break;
	  }
      }
    }

    int cmdline_user_tag(int argc, char *argv[], int quiet, int verbose)
    {
      user_tag_action action = (user_tag_action)-1;

      if(strcmp(argv[0], "add-user-tag") == 0)
	action = action_add;
      else if(strcmp(argv[0], "remove-user-tag") == 0)
	action = action_remove;
      else
	{
	  fprintf(stderr, "Internal error: cmdline_user_tag encountered an unknown command name \"%s\"\n",
		  argv[0]);
	  return -1;
	}

      if(argc < 3)
	{
	  fprintf(stderr,
		  _("%s: too few arguments; expected at least a tag name and a package.\n"),
		  argv[0]);
	  return -1;
	}

      _error->DumpErrors();

      OpProgress progress;

      apt_init(&progress, true);
      if(_error->PendingError())
	{
	  _error->DumpErrors();
	  return -1;
	}

      std::string tag(argv[1]);

      bool all_ok = true;
      for(int i = 2; i < argc; ++i)
	{
	  if(!cmdline_is_search_pattern(argv[i]))
	    {
	      pkgCache::PkgIterator pkg = (*apt_cache_file)->FindPkg(argv[i]);
	      if(pkg.end())
		{
		  if(quiet == 0)
		    fprintf(stderr, _("No such package \"%s\".\n"), argv[i]);
		  all_ok = false;
		}
	      else
		do_user_tag(action, tag, pkg, verbose);
	    }
	  else
	    {
	      matching::pkg_matcher *m = matching::parse_pattern(argv[i]);
	      if(m == NULL)
		{
		  _error->DumpErrors();
		  all_ok = false;
		}
	      else
		{
		  for(pkgCache::PkgIterator pkg = (*apt_cache_file)->PkgBegin();
		      !pkg.end(); ++pkg)
		    {
		      if(matching::apply_matcher(m, pkg, *apt_cache_file, *apt_package_records))
			do_user_tag(action, tag, pkg, verbose);
		    }
		}
	    }
	}

      OpTextProgress text_progress;
      if(!(*apt_cache_file)->save_selection_list(text_progress))
	return 1;

      if(!all_ok)
	return 2;
    }
  }
}
