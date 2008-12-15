// history_entry.h         -*-c++-*-
//
//   Copyright (C) 2008 Daniel Burrows
//
//   This program is free software; you can redistribute it and/or
//   modify it under the terms of the GNU General Public License as
//   published by the Free Software Foundation; either version 2 of
//   the License, or (at your option) any later version.
//
//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//   General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with this program; see the file COPYING.  If not, write to
//   the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//   Boston, MA 02111-1307, USA.

#ifndef HISTORY_ENTRY_H
#define HISTORY_ENTRY_H

/** \file history_entry.h */

namespace aptitude
{
  /** \brief Interfaces dedicated to storing the history of the user's actions. */
  namespace history
  {
    /** \brief One action that was performed.
     *
     *  For actions that should be reversible, we err on the side of
     *  providing too much information about the previous state, to
     *  make it simpler to ensure that we've.
     *
     *  History entries use some common concepts:
     *
     *   * Selection state: the action that will be performed on a
     *     package.  Can be "install", "delete", "purge", "keep" or
     *     "hold".  For "install", a version number is also included.
     *     For "remove", the reason is included (one of "manual",
     *     "unused", and "automatic").
     *
     *   * Flags: extra information about the selection state.
     *     Includes "Auto", indicating that the package was
     *     automatically installed.
     *
     *   * Dpkg selected state: the "future state" recorded by
     *     dselect.  One of Unknown, Install, Hold, DeInstall, and
     *     Purge.
     *
     *   * Dpkg current state: the current status of this package in
     *     the dpkg database.  One of NotInstalled, UnPacked,
     *     HalfConfigured, HalfInstalled, ConfigFiles, Installed,
     *     TriggersAwaited, and TriggersPending.  All states except
     *     NotInstalled have an attached version.
     */
    class entry
    {
    public:
      /** \name Entry types */

      // @{

      /** \brief Represents the type of a history entry. */
      enum type
	{
	  /** \brief A package's selection state was changed by an
	   * unknown process.
	   *
	   *  This is generated whenever we can't determine the source
	   *  of a change.  Hopefully the number of these entries will
	   *  decrease over time.
	   *
	   *  change <package> <oldstate> <oldflags> -> <newstate> <newflags>
	   */
	  change,

	  /** \brief A package's selection state was changed by the autoresolver.
	   *
	   *  dep-change <dep> <package> <oldversion> <oldstate> <oldflags> -> <newversion> <newstate> <newflags>
	   */
	  dep_change,

	  /** \brief A package's actual status was changed by dpkg.
	   *
	   *  dpkg-change <package> <oldselectedstate> <oldcurrentstate> -> <selectedstate> <currentstate>
	   *
	   *  This are generated on startup, when the cache is
	   *  initially loaded.
	   */
	  dpkg_change,

	  /** \brief One or more history entries, grouped together.
	   *
	   *  group of <number> actions:
	   */
	  group,

	  /** \brief aptitude is about to run dpkg.
	   *
	   *  plan-dpkg <package> <oldselectedstate> <oldcurrentstate> -> <planselectedstate> <plancurrentstate>
	   */
	  plan_dpkg,

	  /** \brief A resolver solution was applied.
	   *
	   *  resolver-apply:
	   *     install <pkg> <version> [-> <version>] for <dep>
	   *     keep <pkg> <version> for <dep>
	   *     remove <pkg> <version> for <dep>
	   *     unresolved <dep>
	   *
	   *  The solution is serialized with all information
	   *  preserved, including the dependency attached to each
	   *  action.  Dependencies are apt-style: e.g., "foo 5.4
	   *  Depends bar, baz (>= 1.0), xyzzy | blah".  Actions are
	   *  written in the order they were inserted into the
	   *  solution.
	   */
	  resolver_apply
	};
    };
  }
}

#endif
