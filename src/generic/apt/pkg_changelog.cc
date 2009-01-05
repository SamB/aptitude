// pkg_changelog.cc
//
//  Copyright 2000, 2004-2005, 2008 Daniel Burrows
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

#include <sigc++/bind.h>

using namespace std;

namespace aptitude
{
  namespace apt
  {
    changelog_cache::changelog_cache()
    {
    }

class download_changelog_manager : public download_manager
{
public:
  class entry
  {
    string srcpkg;
    string ver;
    string section;
    string name;
    changelog_cache::download_callbacks callbacks;
    temp::name tempname;

  public:
    entry(const string &_srcpkg,
	  const string &_ver,
	  const string &_section,
	  const string &_name,
	  const temp::name &_tempname,
	  const changelog_cache::download_callbacks &_callbacks)
      : srcpkg(_srcpkg),
	ver(_ver),
	section(_section),
	name(_name),
	callbacks(_callbacks),
	tempname(_tempname)
    {
    }

    const string &get_srcpkg() const { return srcpkg; }
    const string &get_ver() const { return ver; }
    const string &get_section() const { return section; }
    const string &get_name() const { return name; }
    const changelog_cache::download_callbacks &get_callbacks() const { return callbacks; }
    const temp::name &get_tempname() const { return tempname; }
  };

private:
  std::vector<entry> entries;

  download_signal_log *log;

  /** \brief An acquire item that fetches a file for a changelog entry,
   *  invoking the entry's callback when it's finished.
   *
   *  \todo Instead of just one URI, use a list of fallback URIs (so we
   *  can try fetching a local copy, but grab a network copy instead if
   *  available).
   */
  class AcqForEntry : public pkgAcqFile
  {
    entry ent;
    vector<string> uris;
    vector<string>::const_iterator current_uri;
    // Stores the description and short description of this item; used
    // to requeue it after a failure. (the pkgAcqFile constructor
    // enqueues it to start with)
    pkgAcquire::ItemDesc desc;

    int Retries;
  public:
    AcqForEntry(pkgAcquire *Owner,
		// Must contain at least one entry.
		const vector<string> URIs,
		const string &MD5,
		unsigned long Size,
		const string &Description,
		const string &ShortDesc,
		const string &filename,
		const entry &_ent):
      pkgAcqFile(Owner, URIs.front(), MD5, Size, Description, ShortDesc, "", filename),
      ent(_ent),
      uris(URIs),
      current_uri(uris.begin())
    {
      Retries = _config->FindI("Acquire::Retries", 0);

      desc.URI = URIs.front();
      desc.Description = Description;
      desc.Owner = this;
      desc.ShortDesc = ShortDesc;
    }

    const entry &get_entry() const { return ent; }
    // The URI that is representative of all the URIs in this entry
    // (\todo this is used to display error messages, but we should do
    // something more complete).
    const string &get_real_uri() const { return uris.back(); }

    void Failed(string Message, pkgAcquire::MethodConfig *Cnf)
    {
      // We do what pkgAcqFile would do, except that we only give up
      // for good if we've run out of URIs.

      ErrorText = LookupTag(Message,"Message");

      // This is the retry counter
      if (Retries != 0 &&
	  Cnf->LocalOnly == false &&
	  StringToBool(LookupTag(Message,"Transient-Failure"),false) == true)
	{
	  Retries--;
	  QueueURI(desc);
	  return;
	}

      if(current_uri < uris.end())
	{
	  ++current_uri;
	  if(current_uri < uris.end())
	    {
	      Retries = _config->FindI("Acquire::Retries",0);
	      desc.URI = *current_uri;
	      QueueURI(desc);
	      return;
	    }
	}

      Item::Failed(Message, Cnf);
      ent.get_callbacks().get_failure()(ErrorText);
    }

    void Done(std::string Message,
	      unsigned long Size,
	      std::string CalcHash,
	      pkgAcquire::MethodConfig *Cnf)
    {
      pkgAcqFile::Done(Message, Size, CalcHash, Cnf);

      if(Status != pkgAcquire::Item::StatDone)
	_error->Error("Failed to fetch the description of %s from the URI %s: %s",
		      ent.get_srcpkg().c_str(),
		      desc.URI.c_str(),
		      ErrorText.c_str());
      else
	ent.get_callbacks().get_success()(ent.get_tempname());
    }
  };

public:
  template<typename Iter>
  download_changelog_manager(Iter begin, Iter end)
    : entries(begin, end), log(NULL)
  {
  }

  bool prepare(OpProgress &progress,
	       pkgAcquireStatus &acqlog,
	       download_signal_log *signallog)
  {
    log = signallog;

    fetcher = new pkgAcquire(&acqlog);

    for(std::vector<entry>::const_iterator it = entries.begin();
	it != entries.end(); ++it)
      {
	const string srcpkg(it->get_srcpkg());
	const string ver(it->get_ver());
	const string section(it->get_section());
	const string name(it->get_name());
	const string title = ssprintf(_("ChangeLog of %s"), name.c_str());

	// Try to find a changelog that's already on the system,
	// first.  Check each binary package in the source package;
	// for any package that's unpacked, check that the version on
	// the system corresponds to the requested source version, and
	// if it passes look for a changelog.
	pkgSrcRecords source_records(*apt_source_list);
	source_records.Restart();
	pkgSrcRecords::Parser *source_rec = source_records.Find(srcpkg.c_str());

	std::vector<std::string> changelog_uris;

	if(source_rec != NULL)
	  for(const char **binaryIt = source_rec->Binaries();
	      *binaryIt != NULL; ++binaryIt)
	    {
	      pkgCache::PkgIterator pkg = (*apt_cache_file)->FindPkg(*binaryIt);
	      if(!pkg.end() &&
		 !pkg.CurrentVer().end() &&
		 !pkg.CurrentVer().FileList().end() &&
		 pkg->CurrentState != pkgCache::State::NotInstalled &&
		 pkg->CurrentState != pkgCache::State::ConfigFiles)
		{
		  pkgRecords::Parser &rec(apt_package_records->Lookup(pkg.CurrentVer().FileList()));
		  std::string rec_sourcepkg = rec.SourcePkg();
		  if(rec_sourcepkg.empty())
		    rec_sourcepkg = pkg.Name();
		  std::string rec_sourcever = rec.SourceVer();
		  if(rec_sourcever.empty())
		    rec_sourcever = pkg.CurrentVer().VerStr();

		  if(rec_sourcepkg == srcpkg &&
		     rec_sourcever == ver)
		    {
		      // Everything passed.  Now test to see whether
		      // the changelog exists by trying to stat it.
		      struct stat buf;

		      std::string changelog_file = "/usr/share/doc/";
		      changelog_file += pkg.Name();
		      changelog_file += "/changelog.Debian";

		      if(stat(changelog_file.c_str(), &buf) == 0)
			changelog_uris.push_back("file://" + changelog_file);

		      changelog_file += ".gz";

		      if(stat(changelog_file.c_str(), &buf) == 0)
			changelog_uris.push_back("gzip://" + changelog_file);

		      // Beware the races here -- ideally we should
		      // parse the returned changelog and check that
		      // the first version it contains is what we
		      // expect.  This should be reliable in *most*
		      // cases, though.
		    }
		}
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

	string uri = ssprintf("http://packages.debian.org/changelogs/pool/%s/%s/%s/%s_%s/changelog",
			      realsection.c_str(),
			      prefix.c_str(),
			      srcpkg.c_str(),
			      srcpkg.c_str(),
			      realver.c_str());

	changelog_uris.push_back(uri);

	new AcqForEntry(fetcher,
			changelog_uris,
			"",
			0,
			title,
			title,
			it->get_tempname().get_name().c_str(),
			*it);
      }

    return true;
  }

  void finish(pkgAcquire::RunResult res,
	      OpProgress *progress,
	      const sigc::slot1<void, result> &k)
  {
    if(fetcher != NULL)
      {
	result rval = success;

	if(res != pkgAcquire::Continue)
	  rval = failure;
	else
	  for(pkgAcquire::ItemIterator it = fetcher->ItemsBegin();
	      it != fetcher->ItemsEnd(); ++it)
	    {
	      AcqForEntry *item(dynamic_cast<AcqForEntry *>(*it));
	      if(item != NULL)
		{
		  if(item->Status != pkgAcquire::Item::StatDone)
		    {
		      _error->Error("Failed to fetch the description of %s from the URI %s: %s",
				    item->get_entry().get_srcpkg().c_str(),
				    item->get_real_uri().c_str(),
				    item->ErrorText.c_str());

		      rval = failure;
		    }
		  else
		    {
		      const entry &ent(item->get_entry());

		      ent.get_callbacks().get_success()(ent.get_tempname());
		    }
		}
	    }

	k(rval);
	return;
      }
    else
      {
	k(failure);
	return;
      }
  }
};

void changelog_cache::register_changelog(const temp::name &n,
					 const std::string &package,
					 const std::string &version)
{
  cache[std::make_pair(package, version)] = n;
  pending_downloads[std::make_pair(package, version)].get_success()(n);
  pending_downloads.erase(std::make_pair(package, version));
}

void changelog_cache::changelog_failed(const std::string &errmsg,
				       const std::string &package,
				       const std::string &version)
{
  const std::pair<std::string, std::string> key(package, version);

  sigc::slot<void, std::string> failure_slot =
    pending_downloads[key].get_failure();

  pending_downloads.erase(key);

  failure_slot(errmsg);
}

download_manager *changelog_cache::get_changelogs(const std::vector<std::pair<pkgCache::VerIterator, changelog_cache::download_callbacks> > &versions)
{
  std::vector<download_changelog_manager::entry> entries;

  for(std::vector<std::pair<pkgCache::VerIterator, download_callbacks> >::const_iterator
	it = versions.begin(); it != versions.end(); ++it)
    {
      pkgCache::VerIterator ver(it->first);

      if(ver.end())
	continue;

      if(ver.FileList().end())
	continue;

      // Look up the source package.
      pkgRecords::Parser &rec =
	apt_package_records->Lookup(ver.FileList());
      string srcpkg =
	rec.SourcePkg().empty() ? ver.ParentPkg().Name() : rec.SourcePkg();

      const string sourcever =
	rec.SourceVer().empty() ? ver.VerStr() : rec.SourceVer();

      temp::dir tempdir;
      temp::name tempname;

      try
	{
	  tempdir = temp::dir("aptitude");
	  tempname = temp::name(tempdir, "changelog");
	}
      catch(temp::TemporaryCreationFailure e)
	{
	  _error->Error("%s", e.errmsg().c_str());
	  continue;
	}

      const download_callbacks &callbacks(it->second);

      const std::map<std::pair<std::string, std::string>, download_signals>::iterator
	pending_found = pending_downloads.find(std::make_pair(srcpkg, sourcever));

      if(pending_found != pending_downloads.end())
	{
	  pending_found->second.get_success().connect(callbacks.get_success());
	  pending_found->second.get_failure().connect(callbacks.get_failure());
	}
      else
	{
	  const std::map<std::pair<std::string, std::string>, temp::name>::const_iterator
	    found = cache.find(std::make_pair(srcpkg, sourcever));

	  if(found != cache.end())
	    callbacks.get_success()(found->second);
	  else
	    {
	      download_signals &signals(pending_downloads[std::make_pair(srcpkg, sourcever)]);
	      signals.get_success().connect(callbacks.get_success());
	      signals.get_failure().connect(callbacks.get_failure());

	      const sigc::slot1<void, temp::name> register_slot =
		sigc::bind(sigc::mem_fun(*this, &changelog_cache::register_changelog),
			   srcpkg,
			   sourcever);
	      const sigc::slot1<void, std::string> failure_slot =
		sigc::bind(sigc::mem_fun(*this, &changelog_cache::changelog_failed),
			   srcpkg, sourcever);

	      changelog_cache::download_callbacks
		callbacks(register_slot, failure_slot);

	      entries.push_back(download_changelog_manager::entry(srcpkg, sourcever, ver.Section(), ver.ParentPkg().Name(),
								  tempname, callbacks));
	    }
	}
    }

  return new download_changelog_manager(entries.begin(), entries.end());
}

download_manager *changelog_cache::get_changelog(const pkgCache::VerIterator &ver,
						 const sigc::slot<void, temp::name> &success,
						 const sigc::slot<void, std::string> &failure)
{
  std::vector<std::pair<pkgCache::VerIterator, download_callbacks> > versions;
  versions.push_back(std::make_pair(ver, download_callbacks(success, failure)));

  return get_changelogs(versions);
}

download_manager *changelog_cache::get_changelog_from_source(const string &srcpkg,
							     const string &ver,
							     const string &section,
							     const string &name,
							     const sigc::slot<void, temp::name> &success,
							     const sigc::slot<void, std::string> &failure)
{
  temp::dir tempdir;
  temp::name tempname;

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

  const std::map<std::pair<std::string, std::string>, download_signals>::iterator
    pending_found = pending_downloads.find(std::make_pair(srcpkg, ver));

  std::vector<download_changelog_manager::entry> entries;

  if(pending_found != pending_downloads.end())
    {
      pending_found->second.get_success().connect(success);
      pending_found->second.get_failure().connect(failure);
    }
  else
    {
      const std::map<std::pair<std::string, std::string>, temp::name>::const_iterator
	found = cache.find(std::make_pair(srcpkg, ver));

      if(found != cache.end())
	success(found->second);
      else
	{
	  download_signals &signals(pending_downloads[std::make_pair(srcpkg, ver)]);
	  signals.get_success().connect(success);
	  signals.get_failure().connect(failure);

	  sigc::slot1<void, temp::name>
	    register_and_return(sigc::bind(sigc::mem_fun(*this, &changelog_cache::register_changelog),
					   srcpkg,
					   ver));
	  const sigc::slot1<void, std::string>
	    fail_and_return(sigc::bind(sigc::mem_fun(*this, &changelog_cache::changelog_failed),
				       srcpkg, ver));

	  changelog_cache::download_callbacks
	    callbacks(register_and_return, fail_and_return);

	  entries.push_back(download_changelog_manager::entry(srcpkg, ver, section, name, tempname, callbacks));
	}
    }

  return new download_changelog_manager(entries.begin(), entries.end());
}

    changelog_cache global_changelog_cache;
  }
}
