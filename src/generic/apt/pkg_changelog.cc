// pkg_changelog.cc
//
//  Copyright 2000, 2004-2005 Daniel Burrows
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; see the file COPYING.  If not, write to
//  the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//  Boston, MA 02111-1307, USA.

#include "pkg_changelog.h"

#include "apt.h"
#include "download_manager.h"
#include "pkg_acqfile.h"

#include <aptitude.h>
#include <config.h>

#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#include <signal.h>
#include <sys/stat.h>

#include <generic/util/util.h>

#include <apt-pkg/error.h>
#include <apt-pkg/configuration.h>
#include <apt-pkg/acquire-item.h>
#include <apt-pkg/strutl.h>

using namespace std;

class download_changelog_manager : public download_manager
{
  string srcpkg;
  string ver;
  string section;
  string name;
  string uri;

  sigc::slot1<void, temp::name> k;

  download_signal_log *log;

  /** The name of the file into which the changelog is downloaded. */
  temp::name tempname;

  bool failed;


  /** HACK: This just overrides the failure method to catch 404s. */
  class AcqWithFail:public pkgAcqFile
  {
    bool &failed;
  public:
    AcqWithFail(pkgAcquire *Owner,
		const string &URI,
		const string &MD5,
		unsigned long Size,
		const string &Description,
		const string &ShortDesc,
		const string &filename,
		bool &_failed):
      pkgAcqFile(Owner, URI, MD5, Size, Description, ShortDesc, "", filename),
      failed(_failed)
    {
      failed=false;
    }

    void Failed(string Message, pkgAcquire::MethodConfig *Cnf)
    {
      failed=true;
    }

    void get_failed();
  };

public:
  download_changelog_manager(const string &_srcpkg,
			     const string &_ver,
			     const string &_section,
			     const string &_name,
			     const sigc::slot1<void, temp::name> &_k)
    : srcpkg(_srcpkg), ver(_ver), section(_section), name(_name),
      k(_k), log(NULL), failed(false)
  {
  }

  bool prepare(OpProgress &progress,
	       pkgAcquireStatus &acqlog,
	       download_signal_log *signallog)
  {
    log = signallog;

    temp::dir tempdir;

    try
      {
	tempdir = temp::dir("aptitude");
	tempname = temp::name(tempdir, "changelog");
      }
    catch(temp::TemporaryCreationFailure e)
      {
	_error->Error("%s", e.errmsg().c_str());
	return false;
      }

    string realsection;

    if(section.find('/') != section.npos)
      realsection.assign(section, 0, section.find('/'));
    else
      realsection.assign("main");

    string prefix;

    if(srcpkg.size() > 3 &&
       srcpkg[0] == 'l' && srcpkg[1] == 'i' && srcpkg[2] == 'b')
      prefix = std::string("lib") + srcpkg[3];
    else
      prefix = srcpkg[0];

    string realver;

    if(ver.find(':') != ver.npos)
      realver.assign(ver, ver.find(':')+1, ver.npos);
    else
      realver = ver;

    uri = ssprintf("http://packages.debian.org/changelogs/pool/%s/%s/%s/%s_%s/changelog",
		   realsection.c_str(),
		   prefix.c_str(),
		   srcpkg.c_str(),
		   srcpkg.c_str(),
		   realver.c_str());

    fetcher = new pkgAcquire(&acqlog);

    string title = ssprintf(_("ChangeLog of %s"), name.c_str());

    failed = false;
    new AcqWithFail(fetcher,
		    uri,
		    "",
		    0,
		    title,
		    title,
		    tempname.get_name().c_str(),
		    failed);

    return true;
  }

  result finish(pkgAcquire::RunResult res,
		OpProgress &progress)
  {
    if(res != pkgAcquire::Continue || failed)
      {
	_error->Error("Couldn't fetch URL %s", uri.c_str());

	return failure;
      }
    else
      {
	k(tempname);

	return success;
      }
  }
};

download_manager *get_changelog(pkgCache::VerIterator ver,
				const sigc::slot1<void, temp::name> &k)
{
  if(ver.end())
    return NULL;

  if(ver.FileList().end())
    return NULL;

  // Look up the source package.
  pkgRecords::Parser &rec =
    apt_package_records->Lookup(ver.FileList());
  string srcpkg =
    rec.SourcePkg().empty() ? ver.ParentPkg().Name() : rec.SourcePkg();

  return get_changelog_from_source(srcpkg, ver.VerStr(),
				   ver.Section(),
				   ver.ParentPkg().Name(),
				   k);
}

download_manager *get_changelog_from_source(const string &srcpkg,
					    const string &ver,
					    const string &section,
					    const string &name,
					    const sigc::slot1<void, temp::name> &k)
{
  return new download_changelog_manager(srcpkg, ver, section, name, k);
}
