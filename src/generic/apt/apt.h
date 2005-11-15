// apt.h  -*-c++-*-
//
//  Copyright 1999-2002, 2004-2005 Daniel Burrows
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
//  Argh.  It seems that it's pretty much necessary to have a central
// repository for apt structures -- the cache in particular.  This is it.
// No class wrapper because, well, the cache is already a class and you can't
// open more than one at once, so there :)

#ifndef APT_H
#define APT_H

#include "aptcache.h"

#include <utility>

class OpProgress;
class pkgRecords;
class pkgSourceList;
class pkg_hier;
class signalling_config;
class undo_list;
class resolver_manager;

// Global state variables for the apt stuff:
extern signalling_config *aptcfg;
extern aptitudeCacheFile *apt_cache_file;
extern resolver_manager *resman;
extern pkgSourceList *apt_source_list;
extern pkgRecords *apt_package_records;

pkg_hier *get_user_pkg_hier();
// Currently, package hierarchies are (by default) loaded using
// DATADIR/aptitude/hier_groups and ~/.aptitude/pkgclass.  This is to
// facilitate the editing of hierarchies.  user_pkg_hier contains the
// information which is loaded in this way.  It is accessible only
// through an accessor method in order to implement lazy loading (so
// people who don't use the browser don't take the hit of having to
// load in the (BIG) hierarchy info file)

void apt_preinit();
// Performs initialization of stuff that has to happen before apt_init is
// called (eg, pkgInitialize and setting up the undo structure)

void apt_init(OpProgress *progess_bar,
	      bool do_initselections, const char * status_fname=NULL);
//  It actually doesn't do what you expect!  This routine is a NOP if
// it has already been called; the rationale is that it allows any apt-using
// class to call this in its constructor (thus guaranteeing that the cache
// file is in a sane state)  To force a reload, call reload_cache().
//
//  One more note: pkgInitialize is not called in this routine, as some info
// from that (eg, config data) may be needed before apt_init itself is called.
// This routine loads in the cache, source lists, etc.


/** Close the cache file and destroy the associated data structures. */
void apt_close_cache();

/** If the cache is closed, open it; otherwise do nothing.
 *
 *  \param progress_bar a progress bar with which to display the
 *                      status of loading the cache.
 *  \param do_initselections if \b true, the selection status
 *                           of packages will be set from aptitude's
 *                           sticky database.
 *  \param status_fname if not \b NULL, a filename to use in lieu
 *                      of /var/lib/aptitude/pkgstates.
 */
void apt_load_cache(OpProgress *progress_bar,
		    bool do_initselections, const char *status_fname = NULL);

void apt_reload_cache(OpProgress *progress_bar,
		      bool do_initselections, const char * status_fname=NULL);
//  Forces the cache to be reloaded.
//
//  NOTE: at the moment, the interface won't work too well while this
// routine runs :)  I have Plans [ insert evil laughter here ] to do this in
// a background thread, but I need to know more about various bits of apt
// first.

extern sigc::signal0<void> cache_closed;
// Announces when the cache is (or is about to be) closed.
// This means that anyone using it should immediately drop references to it.
// Generally followed by cache_reloaded; always followed by cache_reloaded
// or cache_reload_failed.
extern sigc::signal0<void> hier_reloaded;
// Announces that user_pkg_hier has been reloaded.  (it's necessary to
// do this BEFORE cache_reloaded, as some things try to use the old
// user_pkg_hier in cache_reloaded if we don't)
extern sigc::signal0<void> cache_reloaded;
// Announces that the cache has been reloaded
extern sigc::signal0<void> cache_reload_failed;
// Announces that reloading the cache failed.

/** Called when the UI should display and remove all pending errors
 *  from the queue.  It is incumbent on the UI to remove errors on its
 *  own after calling APT routines -- this just indicates that some
 *  errors might need to be removed while a routine is executing.
 */
extern sigc::signal0<void> consume_errors;

void apt_dumpcfg(const char *root);
// Dumps a subtree of the configuration to ~/.aptitude/config

void apt_revertoptions();
// Reloads the standard options from apt.conf (ignoring the user's
// configuration file)

void forget_new();
// Discard all in-memory information about what packages are newly seen.  This
// actually involves generating new information, but you can think of it as
// if it discarded info. :)

extern undo_list *apt_undos;
// There's a global undo stack for apt actions, to keep things sane..

enum pkg_action_state {pkg_unchanged=-1,
		       pkg_broken,
		       pkg_unused_remove,
		       pkg_auto_hold,
		       pkg_auto_install,
		       pkg_auto_remove,
		       pkg_downgrade,
		       pkg_hold,
		       pkg_reinstall,
		       pkg_install,
		       pkg_remove,
		       pkg_upgrade};
const int num_pkg_action_states=11;
pkg_action_state find_pkg_state(pkgCache::PkgIterator pkg);
// A utility routine to return a useful notion of a package's "action-state"
// and an enum associated with it

bool pkg_obsolete(pkgCache::PkgIterator pkg);
// Returns true if the package is "obsolete".

/** Finds the OR group enclosing the given (forward or reverse) dependency.
 *
 *  The range [start,end) is the OR group when this terminates.
 *  \param cache the cache file in which to operate, or \b NULL to use
 *               apt_cache_file.
 */
void surrounding_or(pkgCache::DepIterator dep,
		    pkgCache::DepIterator &start,
		    pkgCache::DepIterator &end,
		    pkgCache *cache = NULL);


/** \return a short description string corresponding to the given
 *  version.
 */
std::wstring get_short_description(const pkgCache::VerIterator &ver);

/** \return a long description string corresponding to the given
 *  version.
 */
std::wstring get_long_description(const pkgCache::VerIterator &ver);

/** \return true if pkg is suggested by another package which will be
 *  installed.
 */
bool package_suggested(const pkgCache::PkgIterator &pkg);

/** \return true if pkg is recommended by another package which will
 *  be installed or upgraded.
 */
bool package_recommended(const pkgCache::PkgIterator &pkg);

/** \return true if the given package version is available solely from
 * a "trusted" source.
 */
bool package_trusted(const pkgCache::VerIterator &ver);

/** A pair (veriterator,verfile) -- used for building a list of
 *  versions sorted by file location.
 */
typedef std::pair<pkgCache::VerIterator, pkgCache::VerFileIterator> loc_pair;

/** Used to compare two version files based on their location. */
struct location_compare
{
  bool operator()(loc_pair a, loc_pair b) const
  {
    if(a.second->File == b.second->File)
      return a.second->Offset<b.second->Offset;
    else
      return a.second->File<b.second->File;
  }
};

/** \return \b true if the given dependency is "interesting":
 *          specifically, if it's either critical or a Recommends
 *          that's "new" or currently satisfied.
 *
 *  \param d the dependency to test
 *  \param cache the cache in which to check d (used to find out whether
 *         d is currently satisfied).
 */
bool is_interesting_dep(const pkgCache::DepIterator &d,
			pkgDepCache *cache);

/** Sort packages by name. */
struct pkg_name_lt
{
public:
  bool operator()(const pkgCache::PkgIterator &p1,
		  const pkgCache::PkgIterator &p2) const
  {
    return strcmp(p1.Name(), p2.Name()) < 0;
  }
};

/** Sort versions by package name. */
struct ver_name_lt
{
public:
  bool operator()(const pkgCache::VerIterator &v1,
		  const pkgCache::VerIterator &v2) const
  {
    return strcmp(v1.ParentPkg().Name(), v2.ParentPkg().Name()) < 0;
  }
};

#endif
