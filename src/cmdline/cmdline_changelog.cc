// cmdline_changelog.cc
//
//  Copyright 2004 Daniel Burrows

#include "cmdline_changelog.h"

#include "cmdline_common.h"
#include "cmdline_progress.h"
#include "cmdline_util.h"

#include <aptitude.h>

#include <generic/apt/apt.h>
#include <generic/apt/config_signal.h>
#include <generic/apt/pkg_changelog.h>

#include <apt-pkg/error.h>
#include <apt-pkg/metaindex.h>
#include <apt-pkg/progress.h>
#include <apt-pkg/sourcelist.h>
#include <apt-pkg/srcrecords.h>

#include <sigc++/adaptors/bind.h>

#include <stdlib.h>

using namespace std;

namespace
{
void set_name(temp::name n, temp::name *target)
{
  *target = n;
}

/** Try to find a particular package version without knowing the
 *  section that it occurs in.  The resulting name will be invalid if
 *  no changelog could be found.
 */
temp::name changelog_by_version(const std::string &pkg,
				const std::string &ver)
{
  // Try forcing the particular version that was
  // selected, using various sections.  FIXME: relies
  // on specialized knowledge about how get_changelog
  // works; in particular, that it only cares whether
  // "section" has a first component.

  temp::name rval;
  download_manager::result res = download_manager::failure;

  download_manager *m = get_changelog_from_source(pkg, ver, "", pkg,
						  sigc::bind(sigc::ptr_fun(set_name),
							     &rval));
  if(m != NULL)
    {
      res = cmdline_do_download(m, 0);
      delete m;
    }

  if(res != download_manager::success || !rval.valid())
    {
      m = get_changelog_from_source(pkg, ver, "contrib/foo", pkg,
				    sigc::bind(sigc::ptr_fun(set_name), &rval));
      if(m != NULL)
	{
	  res = cmdline_do_download(m, 0);
	  delete m;
	}
    }

  if(res != download_manager::success || !rval.valid())
    {
      m = get_changelog_from_source(pkg, ver, "non-free/foo", pkg,
				    sigc::bind(sigc::ptr_fun(set_name), &rval));
      if(m != NULL)
	{
	  res = cmdline_do_download(m, 0);
	  delete m;
	}
    }

  if(res != download_manager::success)
    return temp::name();
  else
    return rval;
}
}

bool do_cmdline_changelog(const vector<string> &packages)
{
  const char *pager="/usr/bin/sensible-pager";

  if(access("/usr/bin/sensible-pager", X_OK)!=0)
    {
      _error->Warning(_("Can't execute sensible-pager, is this a working Debian system?"));

      pager=getenv("PAGER");

      if(pager==NULL)
	pager="more";
    }

  string default_release = aptcfg->Find("APT::Default-Release");

  for(vector<string>::const_iterator i=packages.begin(); i!=packages.end(); ++i)
    {
      // We need to do this because some code (see above) checks
      // PendingError to see whether everything is OK.  In addition,
      // dumping errors means we get sensible error message output
      // (this will be true even if the PendingError check is removed
      // ... which it arguably should be).
      _error->DumpErrors();
      string input=*i;

      cmdline_version_source source;
      string package, sourcestr;

      if(!cmdline_parse_source(input, source, package, sourcestr))
	continue;

      if(source == cmdline_version_cand && !default_release.empty())
	{
	  source    = cmdline_version_archive;
	  sourcestr = default_release;
	}

      pkgCache::PkgIterator pkg=(*apt_cache_file)->FindPkg(package);

      temp::name filename;

      // For real packages/versions, we can do a sanity check on the
      // version and warn the user if it looks like it doesn't have a
      // corresponding source package.
      if(!pkg.end())
	{
	  pkgCache::VerIterator ver=cmdline_find_ver(pkg,
						     source, sourcestr);

	  if(!ver.end())
	    {
	      // Move this to a central location and just display an
	      // apt error?
	      bool in_debian=false;

	      for(pkgCache::VerFileIterator vf=ver.FileList();
		  !vf.end() && !in_debian; ++vf)
		if(!vf.File().end() && vf.File().Origin()!=NULL &&
		   strcmp(vf.File().Origin(), "Debian")==0)
		  in_debian=true;

	      if(!in_debian)
		{
		  _error->Error(_("%s is not an official Debian package, cannot display its changelog."), input.c_str());
		  continue;
		}
	    }

	  aptitude::cmdline::source_package p =
	    aptitude::cmdline::find_source_package(package,
						   source,
						   sourcestr);

	  // Use the source package if one was found; otherwise try to
	  // use an explicit version.
	  download_manager *m = NULL;
	  if(p.valid())
	    {
	      m = get_changelog_from_source(p.get_package(),
					    p.get_version(),
					    p.get_section(),
					    pkg.Name(),
					    sigc::bind(sigc::ptr_fun(&set_name), &filename));
	    }
	  else
	    {
	      // Fall back to string-based guessing.
	      if(ver.end() && source == cmdline_version_version)
		filename = changelog_by_version(package, sourcestr);
	      else
		m = get_changelog(ver,
				  sigc::bind(sigc::ptr_fun(&set_name), &filename));
	    }

	  if(m != NULL)
	    {
	      cmdline_do_download(m, 0);
	      delete m;
	    }
	}
      else
	{
	  aptitude::cmdline::source_package p =
	    aptitude::cmdline::find_source_package(package,
						   source,
						   sourcestr);

	  download_manager *m = NULL;

	  if(p.valid())
	    m = get_changelog_from_source(p.get_package(),
					  p.get_version(),
					  p.get_section(),
					  p.get_package(),
					  sigc::bind(sigc::ptr_fun(&set_name), &filename));
	  else
	    {
	      // If the user didn't specify a version or selected a
	      // candidate and we couldn't find anything, we have no
	      // recourse.  But if they passed a version number, we
	      // can fall back to just blindly guessing that the
	      // version exists.

	      switch(source)
		{
		case cmdline_version_cand:
		  break;

		case cmdline_version_archive:
		  break;

		case cmdline_version_version:
		  filename = changelog_by_version(package, sourcestr);
		  break;
		}
	    }

	  if(!filename.valid() && m != NULL)
	    {
	      cmdline_do_download(m, 0);

	      delete m;
	    }
	}

      if(!filename.valid())
	_error->Error(_("Couldn't find a changelog for %s"), input.c_str());
      else
	// Run the user's pager.
	system((string(pager) + " " + filename.get_name()).c_str());
    }

  _error->DumpErrors();
}

// TODO: fetch them all in one go.
int cmdline_changelog(int argc, char *argv[])
{
  _error->DumpErrors();

  OpProgress progress;
  apt_init(&progress, false);

  if(_error->PendingError())
    {
      _error->DumpErrors();
      return -1;
    }

  vector<string> packages;
  for(int i=1; i<argc; ++i)
    packages.push_back(argv[i]);

  do_cmdline_changelog(packages);

  _error->DumpErrors();

  return 0;
}
