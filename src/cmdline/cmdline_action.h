// cmdline_action.h                                   -*-c++-*-
//
//   Copyright 2004 Daniel Burrows

#ifndef CMDLINE_ACTION_H
#define CMDLINE_ACTION_H

#include "cmdline_common.h"

bool cmdline_applyaction(cmdline_pkgaction_type action,
			 pkgCache::PkgIterator pkg,
			 pkgset &to_install, pkgset &to_hold,
			 pkgset &to_remove, pkgset &to_purge,
			 int verbose,
			 cmdline_version_source source,
			 const string &sourcestr);

bool cmdline_applyaction(string s,
			 cmdline_pkgaction_type action,
			 pkgset &to_install, pkgset &to_hold,
			 pkgset &to_remove, pkgset &to_purge,
			 int verbose);

/** Parses a list of actions and executes them.  If there is a
 *  syntax or other error, all the commands up to the point of
 *  the error will be executed.
 */
void cmdline_parse_action(string s,
			  pkgset &to_install, pkgset &to_hold,
			  pkgset &to_remove, pkgset &to_purge,
			  int verbose);

#endif // CMDLINE_ACTION_H
