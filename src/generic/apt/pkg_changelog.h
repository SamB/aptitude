// pkg_changelog.h    -*-c++-*-
//
//  Copyright 2000, 2005, 2008 Daniel Burrows
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

#include <generic/util/temp.h>

#include <apt-pkg/pkgcache.h>

#include <sigc++/signal.h>
#include <sigc++/slot.h>

/** \brief Routines to download a Debian changelog for a given package.
 *
 *  \file pkg_changelog.h
 */

class download_manager;

namespace aptitude
{
  namespace apt
  {
    /** \brief A cache storing changelogs that have been downloaded
     *  from the Debian servers.
     *
     *  This should only be accessed from the foreground
     *  thread.
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
	sigc::slot<void, temp::name> success;
	sigc::slot<void, std::string> failure;

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
	download_callbacks(const sigc::slot<void, temp::name> &_success,
			   const sigc::slot<void, std::string> &_failure)
	  : success(_success),
	    failure(_failure)
	{
	}

	const sigc::slot<void, temp::name> &get_success() const { return success; }
	const sigc::slot<void, std::string> &get_failure() const { return failure; }
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
      // Maps (source-package, version) to the corresponding
      // changelog.
      std::map<std::pair<std::string, std::string>, temp::name> cache;

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

      /** \brief Retrieve the name corresponding to the given
       *  package/version pair from the cache, or an invalid name if
       *  the changelog isn't cached.
       */
      temp::name get_from_cache(const std::string &package, const std::string &name);

      /** Generate a download process object that retrieves changelogs
       *  for the given package versions from the cache or the
       *  network.  When the download is complete for a version, the
       *  corresponding slot will be invoked with the file to which
       *  the changelog was downloaded as an argument.  If the
       *  changelog is already present in the cache, the callback will
       *  be invoked immediately.  If the callback is invoked during
       *  the download, it will be invoked in whichever thread called
       *  do_download().  If do_download might be invoked from a
       *  background thread, the slot must be safe to copy and to call
       *  from another thread.  (ideally, it should point to a
       *  function, possibly with some bound arguments that are passed
       *  by value)
       *
       *  If one of the entries in the vector is an end iterator or has no
       *  file lists, it will be silently dropped from the list.
       */
      download_manager *get_changelogs(const std::vector<std::pair<pkgCache::VerIterator, download_callbacks> > &versions);

      download_manager *get_changelog(const pkgCache::VerIterator &ver,
				      const sigc::slot<void, temp::name> &success,
				      const sigc::slot<void, std::string> &failure);

      /** Generate a download process object that retrieves a changelog for
       *  the given source package.
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
      download_manager *get_changelog_from_source(const std::string &srcpkg,
						  const std::string &ver,
						  const std::string &section,
						  const std::string &name,
						  const sigc::slot<void, temp::name> &success,
						  const sigc::slot<void, std::string> &failure);
    };

    extern changelog_cache global_changelog_cache;
  }
}

#endif
