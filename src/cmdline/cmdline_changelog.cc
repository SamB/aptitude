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

using namespace std;

/** Represents the information needed to retrieve a changelog. */
struct changelog_entity
{
  /** The name of the source package. */
  std::string pkg;

  /** The name of the source version. */
  std::string ver;

  /** The section of the package. */
  std::string section;

  /** Initializes the empty entity: all the members are 0-length
   *  strings.
   */
  changelog_entity():pkg(), ver(), section()
  {
  }

  changelog_entity(const std::string &_pkg,
		   const std::string &_ver,
		   const std::string &_section)
    :pkg(_pkg), ver(_ver), section(_section)
  {
  }

  changelog_entity &operator=(const changelog_entity &other)
  {
    pkg = other.pkg;
    ver = other.ver;
    section = other.section;
  }
};

/** Find a source record in the given set of source records
 *  corresponding to the package pkg, version ver.
 *
 *  \param records the source records object
 *  \param pkg the package name to match on
 *  \param ver the version string to match on
 *
 *  \return a matching changelog entity, or the empty entity if no
 *  such entity exists.
 */
changelog_entity find_src_ver(pkgSourceList &list,
			      const std::string &pkg,
			      const std::string &ver)
{
  pkgSrcRecords records(list);
  records.Restart();

  pkgSrcRecords :: Parser *parser = records.Find(pkg.c_str());

  while(parser != NULL && parser->Version() != ver)
    parser = records.Find(pkg.c_str());

  if(parser == NULL)
    return changelog_entity();
  else
    return changelog_entity(pkg, ver, parser->Section());
}

static void set_name(temp::name n, temp::name *target)
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
      res = cmdline_do_download(m);
      delete m;
    }

  if(res != download_manager::success || !rval.valid())
    {
      m = get_changelog_from_source(pkg, ver, "contrib/foo", pkg,
				    sigc::bind(sigc::ptr_fun(set_name), &rval));
      if(m != NULL)
	{
	  res = cmdline_do_download(m);
	  delete m;
	}
    }

  if(res != download_manager::success || !rval.valid())
    {
      m = get_changelog_from_source(pkg, ver, "non-free/foo", pkg,
				    sigc::bind(sigc::ptr_fun(set_name), &rval));
      if(m != NULL)
	{
	  res = cmdline_do_download(m);
	  delete m;
	}
    }

  if(res != download_manager::success)
    return temp::name();
  else
    return rval;
}

/** Find a source record in the given set of source records
 *  corresponding to the given package and archive.  Expects no
 *  pending errors when it starts.
 *
 *  \param records the source records object
 *  \param pkg the package name to match on
 *  \param ver the version string to match on
 *
 *  \return a matching changelog entity, or the empty entity
 *  ("","","") if no such entity exists.
 */

// Based heavily on pkgSrcRecords.
changelog_entity find_src_archive(pkgSourceList &list,
				  const std::string &pkg,
				  const std::string &archive)
{
  for(pkgSourceList::const_iterator i = list.begin(); i!=list.end(); ++i)
    {
      if((*i)->GetDist() != archive)
	continue;

      vector<pkgIndexFile *> *indexes = (*i)->GetIndexFiles();

      for(vector<pkgIndexFile *> :: const_iterator j = indexes->begin();
	  j != indexes->end(); ++j)
	{
	  auto_ptr<pkgSrcRecords :: Parser> p((*j)->CreateSrcParser());

	  if(_error->PendingError())
	    return changelog_entity();
	  if(p.get() != 0)
	    {
	      // Step through the file until we reach the end or find
	      // the package:
	      while(p.get()->Step() == true)
		{
		  if(_error->PendingError() == true)
		    return changelog_entity();

		  if(p.get()->Package() == pkg)
		    return changelog_entity(pkg,
					    p.get()->Version(),
					    p.get()->Section());
		}
	    }
	}
    }

  return changelog_entity();
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

      // For real packages/versions, we can do a sanity check.
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

	  if(ver.end() && source == cmdline_version_version)
	    filename = changelog_by_version(package, sourcestr);
	  else
	    {
	      download_manager *m = get_changelog(ver,
						  sigc::bind(sigc::ptr_fun(&set_name), &filename));
	      if(m != NULL)
		{
		  cmdline_do_download(m);
		  delete m;
		}
	    }
	}
      else
	{
	  changelog_entity ent;

	  switch(source)
	    {
	    case cmdline_version_cand:
	      // In this case, pull the first one we see (not very
	      // elegant, but finding the actual candidate is a bit
	      // hard)
	      {
		pkgSrcRecords r(*apt_source_list);

		pkgSrcRecords :: Parser *p(r.Find(package.c_str()));
		while(p != NULL && p->Package() != package)
		  p = r.Find(package.c_str());

		if(p != NULL)
		  ent = changelog_entity(package, p->Version(),
					 p->Section());
	      }

	      break;

	    case cmdline_version_archive:
	      ent = find_src_archive(*apt_source_list,
				     package, sourcestr);

	      break;

	    case cmdline_version_version:
	      ent = find_src_ver(*apt_source_list, package, sourcestr);

	      if(ent.pkg.empty())
		filename = changelog_by_version(package, sourcestr);

	      break;
	    }


	  if(!filename.valid() && !ent.pkg.empty())
	    {
	      download_manager *m
		= get_changelog_from_source(ent.pkg,
					    ent.ver,
					    ent.section,
					    ent.pkg,
					    sigc::bind(sigc::ptr_fun(&set_name), &filename));

	      if(m != NULL)
		{
		  cmdline_do_download(m);

		  delete m;
		}
	    }
	}

      if(!filename.valid())
	_error->Error(_("Couldn't find a changelog for %s"), input.c_str());
      else
	// Run the user's pager.
	system((string(pager) + " " + filename.get_name()).c_str());
    }
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
