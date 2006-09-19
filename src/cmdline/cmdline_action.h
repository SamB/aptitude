// cmdline_action.h                                   -*-c++-*-
//
//   Copyright 2004 Daniel Burrows

#ifndef CMDLINE_ACTION_H
#define CMDLINE_ACTION_H

#include "cmdline_common.h"

/// \todo The command-line state should probably be encapsulated
/// as an object.

/// \todo allow_auto in cmdline_applyaction() is a hack.  We should
/// use action-groups to suppress auto-installation (not currently
/// done but makes sense) and tidy up afterwards?

/** \brief Apply the given command-line action to the given package,
 *  updating the command-line state appropriately.
 *
 *  \param action The action to apply to the package.
 *
 *  \param pkg The package whose state is to be modified.
 *
 *  \param to_install The set of packages that are currently marked
 *  to-be-installed; will be updated in accordance with the selected
 *  action.
 *
 *  \param to_hold The set of packages that are currently marked
 *  to-be-held; will be updated in accordance with the selected
 *  action.
 *
 *  \param to_remove The set of packages that are currently marked
 *  to-be-removed; will be updated in accordance with the selected
 *  action.
 *
 *  \param to_purge The set of packages that are currently marked
 *  to-be-purged; will be updated in accordance with the selected
 *  action.
 *
 *  \param verbose The verbosity level at which this command should
 *  operate.
 *
 *  \param source The version-source to be used for this package.
 *
 *  \param sourcestr The string associated with the version source, or
 *  "" if there is no associated string.
 *
 *  \param allow_auto If \b false, auto-installation of dependencies
 *  will be disabled regardless of the value of Auto-Install.
 */
bool cmdline_applyaction(cmdline_pkgaction_type action,
			 pkgCache::PkgIterator pkg,
			 pkgset &to_install, pkgset &to_hold,
			 pkgset &to_remove, pkgset &to_purge,
			 int verbose,
			 cmdline_version_source source,
			 const string &sourcestr,
			 bool allow_auto);

/** \brief Apply the given command-line action to the given package,
 *  updating the command-line state appropriately.
 *
 *  \param action The action to apply to the package.
 *
 *  \param pkg The package whose state is to be modified.
 *
 *  \param to_install The set of packages that are currently marked
 *  to-be-installed; will be updated in accordance with the selected
 *  action.
 *
 *  \param to_hold The set of packages that are currently marked
 *  to-be-held; will be updated in accordance with the selected
 *  action.
 *
 *  \param to_remove The set of packages that are currently marked
 *  to-be-removed; will be updated in accordance with the selected
 *  action.
 *
 *  \param to_purge The set of packages that are currently marked
 *  to-be-purged; will be updated in accordance with the selected
 *  action.
 *
 *  \param verbose The verbosity level at which this command should
 *  operate.
 *
 *  \param allow_auto If \b false, auto-installation of dependencies
 *  will be disabled regardless of the value of Auto-Install.
 */
bool cmdline_applyaction(string s,
			 cmdline_pkgaction_type action,
			 pkgset &to_install, pkgset &to_hold,
			 pkgset &to_remove, pkgset &to_purge,
			 int verbose, bool allow_auto);

/** \brief Parses a list of actions and executes them.
 *
 *  If there is a syntax or other error, all the commands up to the
 *  point of the error will be executed regardless.
 *
 *  \param s The string to be parsed.
 *
 *  \param to_install The set of packages that are currently marked
 *  to-be-installed; will be updated in accordance with the selected
 *  action.
 *
 *  \param to_hold The set of packages that are currently marked
 *  to-be-held; will be updated in accordance with the selected
 *  action.
 *
 *  \param to_remove The set of packages that are currently marked
 *  to-be-removed; will be updated in accordance with the selected
 *  action.
 *
 *  \param to_purge The set of packages that are currently marked
 *  to-be-purged; will be updated in accordance with the selected
 *  action.
 *
 *  \param verbose The verbosity level at which this command should
 *  operate.
 *
 *  \param allow_auto If \b false, auto-installation of dependencies
 *  will be disabled regardless of the value of Auto-Install.
 */
void cmdline_parse_action(string s,
			  pkgset &to_install, pkgset &to_hold,
			  pkgset &to_remove, pkgset &to_purge,
			  int verbose, bool allow_auto);

#endif // CMDLINE_ACTION_H
