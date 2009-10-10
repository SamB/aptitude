// pkg_changelog.cc
//
//  Copyright 2000, 2004-2005, 2008-2009 Daniel Burrows
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

#include <generic/util/file_cache.h>
#include <generic/util/util.h>

#include <apt-pkg/error.h>
#include <apt-pkg/configuration.h>
#include <apt-pkg/acquire-item.h>
#include <apt-pkg/strutl.h>

#include <sigc++/bind.h>

#include <cwidget/generic/util/ssprintf.h>

#include <boost/make_shared.hpp>

#include <loggers.h>

using namespace std;
namespace cw = cwidget;

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
  // An entry that's ready for induction into the download queue.
  class processed_entry
  {
    // The original input entry.
    entry ent;

    std::vector<std::string> changelog_uris;

  public:
    processed_entry(const entry &_entry,
		    const std::vector<std::string> &_changelog_uris)
      : ent(_entry),
	changelog_uris(_changelog_uris)
    {
    }

    const entry &get_entry() const { return ent; }
    const std::vector<std::string> &get_changelog_uris() const
    {
      return changelog_uris;
    }
  };

  boost::shared_ptr<std::vector<processed_entry> > entries;

  download_signal_log *log;

  download_changelog_manager(const boost::shared_ptr<std::vector<processed_entry> > &_entries)
    : entries(_entries),
      log(NULL)
  {
  }

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

    static std::string showURIs(const std::vector<std::string> &URIs)
    {
      std::string rval;
      for(std::vector<std::string>::const_iterator it = URIs.begin();
	  it != URIs.end(); ++it)
	{
	  if(!rval.empty())
	    rval += ", ";

	  rval += *it;
	}

      return rval;
    }

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
      LOG_TRACE(Loggers::getAptitudeChangelog(),
		"Setting up a download of the changelog for " << _ent.get_name() << " from "
		<< showURIs(URIs) << "; MD5=" << MD5 << "; filename=" << filename);

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
      LOG_WARN(Loggers::getAptitudeChangelog(),
	       "Failed to download the changelog for " << ent.get_name()
	       << ": " << Message);

      // We do what pkgAcqFile would do, except that we only give up
      // for good if we've run out of URIs.

      ErrorText = LookupTag(Message,"Message");

      // This is the retry counter
      if (Retries != 0 &&
	  Cnf->LocalOnly == false &&
	  StringToBool(LookupTag(Message,"Transient-Failure"),false) == true)
	{
	  Retries--;

	  LOG_TRACE(Loggers::getAptitudeChangelog(),
		    "Trying again to download the changelog for " << ent.get_name()
		    << " from " << desc.URI << ", " << Retries << " attempts remaining.");

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
	      LOG_TRACE(Loggers::getAptitudeChangelog(),
			"Falling back to download the changelog for " << ent.get_name()
			<< " from " << desc.URI);
	      QueueURI(desc);
	      return;
	    }
	}

      LOG_TRACE(Loggers::getAptitudeChangelog(),
		"Failed to download the changelog for " << ent.get_name());

      Item::Failed(Message, Cnf);
      ent.get_callbacks().get_failure().get_slot()(ErrorText);
    }

    void Done(std::string Message,
	      unsigned long Size,
	      std::string CalcHash,
	      pkgAcquire::MethodConfig *Cnf)
    {
      pkgAcqFile::Done(Message, Size, CalcHash, Cnf);

      if(Status != pkgAcquire::Item::StatDone)
	{
	  _error->Error("Failed to fetch the description of %s from the URI %s: %s",
			ent.get_srcpkg().c_str(),
			desc.URI.c_str(),
			ErrorText.c_str());

	  LOG_ERROR(Loggers::getAptitudeChangelog(),
		    "Failed to download the changelog for " << ent.get_name()
		    << ": " << Message << "[" << ErrorText << "]");
	}
      else
	{
	  LOG_INFO(Loggers::getAptitudeChangelog(),
		   "Successfully downloaded the changelog for " << ent.get_name()
		   << ": " << Message);

	  download_cache->putItem(desc.URI, ent.get_tempname().get_name());
	  ent.get_callbacks().get_success().get_slot()(ent.get_tempname());
	}
    }
  };

  class create_background_thread
  {
    boost::shared_ptr<std::vector<entry> > entries;
    safe_slot1<void, boost::shared_ptr<download_manager> > k;

  public:
    create_background_thread(const boost::shared_ptr<std::vector<entry> > &_entries,
			     const safe_slot1<void, boost::shared_ptr<download_manager> > &_k)
        : entries(_entries)
    {
    }

    void operator()() const
    {
      log4cxx::LoggerPtr logger(Loggers::getAptitudeChangelog());

      try
	{
	  boost::shared_ptr<std::vector<processed_entry> >
	    processed_entries(boost::make_shared<std::vector<processed_entry> >());

	  for(std::vector<entry>::const_iterator it = entries->begin();
	      it != entries->end(); ++it)
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
		    binaryIt != NULL && *binaryIt != NULL; ++binaryIt)
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
			      {
				LOG_TRACE(logger,
					  "The changelog for " << name << " " << ver
					  << " exists on the system as " << changelog_file);
				changelog_uris.push_back("file://" + changelog_file);
			      }

			    changelog_file += ".gz";

			    if(stat(changelog_file.c_str(), &buf) == 0)
			      {
				LOG_TRACE(logger,
					  "The changelog for " << name << " " << ver
					  << " exists on the system as " << changelog_file);
				changelog_uris.push_back("gzip://" + changelog_file);
			      }

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



	      // Check the file cache to see if the HTTP URI is cached (we
	      // don't cache local files).
	      temp::name cached_result = download_cache->getItem(uri);
	      if(cached_result.valid())
		{
		  LOG_INFO(logger, "Fetched the changelog of " << name << " " << ver
			   << " from the download cache.");
		  it->get_callbacks().get_success().get_slot()(cached_result);
		}
	      else
		{
		  LOG_TRACE(logger, "Will download the changelog of " << name << " " << ver);
		  processed_entries->push_back(processed_entry(*it, changelog_uris));
		}
	    }

	  LOG_TRACE(logger, "Starting to download changelogs.");
	  k.get_slot()(boost::shared_ptr<download_changelog_manager>(new download_changelog_manager(processed_entries)));
	}
      catch(cw::util::Exception &ex)
	{
	  LOG_FATAL(logger, "Failed to download changelogs: " << ex.errmsg());
	  k.get_slot()(boost::shared_ptr<download_changelog_manager>());
	}
      catch(std::exception &ex)
	{
	  LOG_FATAL(logger, "Failed to download changelogs: " << ex.what());
	  k.get_slot()(boost::shared_ptr<download_changelog_manager>());
	}
      catch(...)
	{
	  LOG_FATAL(logger, "Failed to download changelogs: unexpected exception type");
	  k.get_slot()(boost::shared_ptr<download_changelog_manager>());
	}
    }
  };

public:
  static void create(std::vector<entry>::const_iterator begin, std::vector<entry>::const_iterator end,
		     const safe_slot1<void, boost::shared_ptr<download_manager> > &k)
  {
    create_background_thread bootstrap(boost::make_shared<std::vector<entry> >(begin, end), k);
    cw::threads::thread t(bootstrap);
    // NB: When this object leaves scope, the thread will be detached
    // into the background.
  }

  static boost::shared_ptr<download_manager>
  create_blocking(std::vector<entry>::const_iterator begin, std::vector<entry>::const_iterator end)
  {
    cw::threads::box<boost::shared_ptr<download_manager> > rval;
    sigc::slot1<void, boost::shared_ptr<download_manager> >
      put_rval_slot(sigc::mem_fun(rval, &cw::threads::box<boost::shared_ptr<download_manager> >::put));
    create(begin, end, make_safe_slot(put_rval_slot));

    return rval.take();
  }

  bool prepare(OpProgress &progress,
	       pkgAcquireStatus &acqlog,
	       download_signal_log *signallog)
  {
    LOG_INFO(Loggers::getAptitudeChangelog(), "Beginning to download changelogs.");

    log = signallog;

    fetcher = new pkgAcquire(&acqlog);

    for(std::vector<processed_entry>::const_iterator it = entries->begin();
	it != entries->end(); ++it)
      {
	LOG_TRACE(Loggers::getAptitudeChangelog(),
		  "Adding the changelog of " << it->get_entry().get_name() << " to the queue.");

	const std::string title(ssprintf(_("Changelog of %s"), it->get_entry().get_name().c_str()));
	new AcqForEntry(fetcher,
			it->get_changelog_uris(),
			"",
			0,
			title,
			title,
			it->get_entry().get_tempname().get_name().c_str(),
			it->get_entry());
      }

    return true;
  }

private:
  static std::string resultText(pkgAcquire::RunResult result)
  {
    switch(result)
      {
      case pkgAcquire::Continue: return "continue";
      case pkgAcquire::Failed: return "failed";
      case pkgAcquire::Cancelled: return "cancelled";
      default: return cw::util::ssprintf("result%d", result);
      }
  }

public:
  void finish(pkgAcquire::RunResult res,
	      OpProgress *progress,
	      const sigc::slot1<void, result> &k)
  {
    LOG_INFO(Loggers::getAptitudeChangelog(),
	     "Done downloading changelogs: " << resultText(res) << ".");

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
		      _error->Error("Failed to fetch the changelog of %s from the URI %s: %s",
				    item->get_entry().get_srcpkg().c_str(),
				    item->get_real_uri().c_str(),
				    item->ErrorText.c_str());

		      LOG_ERROR(Loggers::getAptitudeChangelog(),
				"Failed to fetch the changelog of "
				<< item->get_entry().get_srcpkg()
				<< ": " << item->ErrorText);

		      rval = failure;
		    }
		  else
		    {
		      const entry &ent(item->get_entry());

		      LOG_TRACE(Loggers::getAptitudeChangelog(),
				"Succeeded in fetching the changelog of " << item->get_entry().get_srcpkg());

		      download_cache->putItem(item->get_real_uri(),
					      ent.get_tempname().get_name());
		      ent.get_callbacks().get_success().get_slot()(ent.get_tempname());
		    }
		}
	      else
		LOG_WARN(Loggers::getAptitudeChangelog(),
			 "Wrong object type in the changelog download queue.");
	    }

	k(rval);
	return;
      }
    else
      {
	LOG_WARN(Loggers::getAptitudeChangelog(),
		 "No fetcher object was created; assuming the changelog download failed.");

	k(failure);
	return;
      }
  }
};

void changelog_cache::register_changelog(const temp::name &n,
					 const std::string &package,
					 const std::string &version)
{
  LOG_TRACE(Loggers::getAptitudeChangelog(),
	    "Sending success to all listeners on the changelog of " << package << " " << version);

  pending_downloads[std::make_pair(package, version)].get_success()(n);
  pending_downloads.erase(std::make_pair(package, version));
}

void changelog_cache::changelog_failed(const std::string &errmsg,
				       const std::string &package,
				       const std::string &version)
{
  LOG_WARN(Loggers::getAptitudeChangelog(),
	   "Sending failure with the message \""
	   << errmsg << "\" to all listeners on the changelog of "
	   << package << " " << version);

  const std::pair<std::string, std::string> key(package, version);

  sigc::slot<void, std::string> failure_slot =
    pending_downloads[key].get_failure();

  pending_downloads.erase(key);

  failure_slot(errmsg);
}

void changelog_cache::get_changelogs(const std::vector<std::pair<pkgCache::VerIterator, changelog_cache::download_callbacks> > &versions,
				     const safe_slot1<void, boost::shared_ptr<download_manager> > &k)
{
  log4cxx::LoggerPtr logger(Loggers::getAptitudeChangelog());

  std::vector<download_changelog_manager::entry> entries;

  for(std::vector<std::pair<pkgCache::VerIterator, download_callbacks> >::const_iterator
	it = versions.begin(); it != versions.end(); ++it)
    {
      pkgCache::VerIterator ver(it->first);

      if(ver.end())
	continue;

      if(ver.FileList().end())
	{
	  LOG_TRACE(logger, "Skipping " << ver.ParentPkg().Name() << " " << ver.VerStr()
		    << ": it isn't available from any archive.");
	  continue;
	}

      // Look up the source package.
      pkgRecords::Parser &rec =
	apt_package_records->Lookup(ver.FileList());
      string srcpkg =
	rec.SourcePkg().empty() ? ver.ParentPkg().Name() : rec.SourcePkg();

      const string sourcever =
	rec.SourceVer().empty() ? ver.VerStr() : rec.SourceVer();

      LOG_TRACE(logger, "For " << ver.ParentPkg().Name()
		<< " " << ver.VerStr() << ", downloading the changelog of the source package "
		<< srcpkg << " " << sourcever);

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
	  LOG_ERROR(logger, "Can't create a temporary file: " << e.errmsg());
	  continue;
	}

      const download_callbacks &callbacks(it->second);

      // NOTE: this is safe because finish() is invoked in the main
      // thread and so is get_changelogs() (by assumption); the
      // pending download signals can't be invoked between when we
      // look them up and when we insert our callbacks into them.
      // Similarly, it's OK to copy the slots into the signal lists
      // directly: they're main-thread slots and the callbacks are
      // invoked in the main thread.
      const std::map<std::pair<std::string, std::string>, download_signals>::iterator
	pending_found = pending_downloads.find(std::make_pair(srcpkg, sourcever));

      if(pending_found != pending_downloads.end())
	{
	  LOG_TRACE(logger, "Found an existing download of " << srcpkg << " " << sourcever);
	  pending_found->second.get_success().connect(callbacks.get_success().get_slot());
	  pending_found->second.get_failure().connect(callbacks.get_failure().get_slot());
	}
      else
	{
	  LOG_TRACE(logger, "Creating a new download of " << srcpkg << " " << sourcever);

	  download_signals &signals(pending_downloads[std::make_pair(srcpkg, sourcever)]);
	  signals.get_success().connect(callbacks.get_success().get_slot());
	  signals.get_failure().connect(callbacks.get_failure().get_slot());

	  const sigc::slot1<void, temp::name> register_slot =
	    sigc::bind(sigc::mem_fun(*this, &changelog_cache::register_changelog),
		       srcpkg,
		       sourcever);
	  const sigc::slot1<void, std::string> failure_slot =
	    sigc::bind(sigc::mem_fun(*this, &changelog_cache::changelog_failed),
		       srcpkg, sourcever);

	  changelog_cache::download_callbacks
	    callbacks(make_safe_slot(register_slot),
		      make_safe_slot(failure_slot));

	  entries.push_back(download_changelog_manager::entry(srcpkg, sourcever, ver.Section(), ver.ParentPkg().Name(),
							      tempname, callbacks));
	}
    }

  download_changelog_manager::create(entries.begin(), entries.end(), k);
}

boost::shared_ptr<download_manager>
changelog_cache::get_changelog(const pkgCache::VerIterator &ver,
			       const safe_slot1<void, temp::name> &success,
			       const safe_slot1<void, std::string> &failure)
{
  std::vector<std::pair<pkgCache::VerIterator, download_callbacks> > versions;
  versions.push_back(std::make_pair(ver, download_callbacks(success, failure)));

  cw::threads::box<boost::shared_ptr<download_manager> > rval;
  sigc::slot1<void, boost::shared_ptr<download_manager> >
    put_rval_slot(sigc::mem_fun(rval, &cw::threads::box<boost::shared_ptr<download_manager> >::put));

  get_changelogs(versions,
		 make_safe_slot(put_rval_slot));
  return rval.take();
}

boost::shared_ptr<download_manager>
changelog_cache::get_changelog_from_source(const string &srcpkg,
					   const string &ver,
					   const string &section,
					   const string &name,
					   const safe_slot1<void, temp::name> &success,
					   const safe_slot1<void, std::string> &failure)
{
  log4cxx::LoggerPtr logger(Loggers::getAptitudeChangelog());

  LOG_TRACE(logger, "Setting up a download of the changelog for the source package "
	    << srcpkg << " " << ver << " in section "
	    << section << " (display name " << name << ")");

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
      LOG_ERROR(logger, "Can't create a temporary file: " << e.errmsg());
      return boost::shared_ptr<download_changelog_manager>();
    }

  const std::map<std::pair<std::string, std::string>, download_signals>::iterator
    pending_found = pending_downloads.find(std::make_pair(srcpkg, ver));

  std::vector<download_changelog_manager::entry> entries;

  if(pending_found != pending_downloads.end())
    {
      LOG_TRACE(logger, "Found an existing download of " << srcpkg << " " << ver);
      pending_found->second.get_success().connect(success.get_slot());
      pending_found->second.get_failure().connect(failure.get_slot());
    }
  else
    {
      LOG_TRACE(logger, "Creating a new download of " << srcpkg << " " << ver);

      download_signals &signals(pending_downloads[std::make_pair(srcpkg, ver)]);
      signals.get_success().connect(success.get_slot());
      signals.get_failure().connect(failure.get_slot());

      sigc::slot1<void, temp::name>
	register_and_return(sigc::bind(sigc::mem_fun(*this, &changelog_cache::register_changelog),
				       srcpkg,
				       ver));
      const sigc::slot1<void, std::string>
	fail_and_return(sigc::bind(sigc::mem_fun(*this, &changelog_cache::changelog_failed),
				   srcpkg, ver));

      changelog_cache::download_callbacks
	callbacks(make_safe_slot(register_and_return),
		  make_safe_slot(fail_and_return));

      entries.push_back(download_changelog_manager::entry(srcpkg, ver, section, name, tempname, callbacks));
    }

  return download_changelog_manager::create_blocking(entries.begin(), entries.end());
}

    changelog_cache global_changelog_cache;
  }
}
