// pkg_changelog.h    -*-c++-*-
//
//  Copyright 2000, 2005, 2008-2009 Daniel Burrows
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
//


#ifndef PKG_CHANGELOG_H
#define PKG_CHANGELOG_H

#include <map>
#include <string>

#include <generic/util/safe_slot.h>
#include <generic/util/temp.h>

#include <apt-pkg/pkgcache.h>

#include <sigc++/signal.h>
#include <sigc++/slot.h>

#include <boost/shared_ptr.hpp>

/** \brief Routines to download a Debian changelog for a given package.
 *
 *  \file pkg_changelog.h
 */

class download_manager;

namespace aptitude
{
  namespace apt
  {
    /** \brief Manager for ongoing changelog downloads, responsible
     *  for merging simultaneous downloads of the same changelog.
     *
     *  This class was originally responsible for caching downloaded
     *  changelogs.  However, that responsibility has been handed off
     *  to the file_cache class, which maintains a persistent global
     *  cache of downloaded files.  This class's name is now a
     *  misnomer (see the note below for how it should change in the
     *  future).
     *
     *  \note This should be refactored to produce a generic merging
     *  download queue.  Queue items would be identified by a list of
     *  URIs that they want to fetch.
     *
     *  Instances of changelog_cache should only be accessed from the
     *  foreground thread.
     */
    class changelog_cache
    {
    public:
      /** \brief The callbacks associated with a download.
       *
       *  Used to pass the callbacks to get_changelogs().
       */
      class download_callbacks
      {
	safe_slot1<void, temp::name> success;
	safe_slot1<void, std::string> failure;

      public:
	/** \brief Create a new pair of download callbacks.
	 *
	 *  \param _success A slot that will be invoked with the name
	 *                  of the downloaded file when the download
	 *                  succeeds.
	 *
	 *  \param _failure A slot that will be invoked with an error
	 *                  message when the download fails.
	 */
	download_callbacks(const safe_slot1<void, temp::name> &_success,
			   const safe_slot1<void, std::string> &_failure)
	  : success(_success),
	    failure(_failure)
	{
	}

	const safe_slot1<void, temp::name> &get_success() const { return success; }
	const safe_slot1<void, std::string> &get_failure() const { return failure; }
      };

      /** \brief The signals associated with a download. */
      class download_signals
      {
	sigc::signal<void, temp::name> success;
	sigc::signal<void, std::string> failure;

      public:
	/** \brief Create a new pair of download signals. */
	download_signals()
	{
	}

	const sigc::signal<void, temp::name> &get_success() const { return success; }
	sigc::signal<void, temp::name> &get_success() { return success; }

	const sigc::signal<void, std::string> &get_failure() const { return failure; }
	sigc::signal<void, std::string> &get_failure() { return failure; }
      };

    private:
      // Maps each in-progress download to its completion signal.
      //
      // Used to avoid spawning extra downloads when we can piggyback
      // off of an existing one.
      std::map<std::pair<std::string, std::string>,
	       download_signals> pending_downloads;

      // Registers a new cache entry, then invokes a callback.
      void register_changelog(const temp::name &n,
			      const std::string &package,
			      const std::string &version);

      void changelog_failed(const std::string &errmsg,
			    const std::string &package,
			    const std::string &version);

    public:
      changelog_cache();

      /** Generate a download process object that retrieves changelogs
       *  for the given package versions from the cache or the
       *  network.  When the download is complete for a version, the
       *  corresponding slot will be invoked with the file to which
       *  the changelog was downloaded as an argument.  The callback
       *  might be invoked in the main thread or in a background
       *  thread.  (but the current implementation will always invoke
       *  it in a background thread)
       *
       *  The changelog download is prepared in a background thread so
       *  that some potentially time-consuming I/O (checking the
       *  filesystem to see if the changelog is already installed or
       *  in the cache) doesn't block the main thread.
       *
       *  If one of the entries in the vector is an end iterator or has no
       *  file lists, it will be silently dropped from the list.
       *
       *  \warning get_changelogs() must always be invoked from the
       *  main thread.
       *
       *  \param versions The package versions to fetch changelogs
       *                  for, with each version paired with the
       *                  callbacks that should be invoked when it is
       *                  done downloading.
       *
       *  \param k A slot that is invoked in a background thread when
       *           the download manager has been constructed and is
       *           ready for use.  Typically this will pass the
       *           pointer to the main thread and invoke prepare() on
       *           it.
       *
       *  \note Using a shared pointer to pass the slot around makes
       *        it a lot easier to ensure that the manager is deleted
       *        when it should be.
       */
      void get_changelogs(const std::vector<std::pair<pkgCache::VerIterator, download_callbacks> > &versions,
			  const safe_slot1<void, boost::shared_ptr<download_manager> > &k);

      /** \brief Blocking call to get a download process object for a
       *  single changelog.
       */
      boost::shared_ptr<download_manager> get_changelog(const pkgCache::VerIterator &ver,
							const safe_slot1<void, temp::name> &success,
							const safe_slot1<void, std::string> &failure);

      boost::shared_ptr<download_manager> get_changelog(const pkgCache::VerIterator &ver,
							const sigc::slot<void, temp::name> &success,
							const sigc::slot<void, std::string> &failure)
      {
	return get_changelog(ver, make_safe_slot(success), make_safe_slot(failure));
      }

      /** \brief Blocking call to get a download process object for a
       *  single source package.
       *
       *  \param srcpkg the source package name
       *  \param ver the version of the source package
       *  \param section the section of the source package
       *  \param name the name of the package that the user provided
       *              (e.g., the binary package that the changelog command
       *               was executed on)
       *  \param success the callback to invoke if the download completes successfully.
       *  \param failure the callback to invoke if the download fails.
       */
      boost::shared_ptr<download_manager> get_changelog_from_source(const std::string &srcpkg,
								    const std::string &ver,
								    const std::string &section,
								    const std::string &name,
								    const safe_slot1<void, temp::name> &success,
								    const safe_slot1<void, std::string> &failure);

      boost::shared_ptr<download_manager> get_changelog_from_source(const std::string &srcpkg,
								    const std::string &ver,
								    const std::string &section,
								    const std::string &name,
								    const sigc::slot<void, temp::name> &success,
								    const sigc::slot<void, std::string> &failure)
      {
	return get_changelog_from_source(srcpkg, ver, section, name,
					 make_safe_slot(success),
					 make_safe_slot(failure));
      }
    };

    extern changelog_cache global_changelog_cache;
  }
}

#endif
