// \file cmdline_versions.h          -*-c++-*-
//
// Copyright (C) 2010 Daniel Burrows
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; see the file COPYING.  If not, write to
// the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

#ifndef CMDLINE_VERSIONS_H
#define CMDLINE_VERSIONS_H

#include <string>

/** \brief Represents the possible values of the "group-by"
 *  command-line option.
 */
enum group_by_option
  {
    /** \brief Group by package unless there is exactly one pattern
     *  AND that pattern matches a single package exactly by name, in
     *  which case don't group at all.
     *
     *  This is the default value.  It means that
     *  "aptitude versions foo" shows the versions of "foo" without
     *  grouping, but a more complex search performs the grouping
     *  automatically.
     */
    group_by_auto,

    /** \brief No grouping. */
    group_by_none,

    /** \brief Group by package. */
    group_by_package,

    /** \brief Group by source package. */
    group_by_source_package,
  };

/** \brief Invoke the "versions" command-line action.
 *
 *  \param argc             The number of entries in argv.
 *
 *  \param argv             The command-line arguments not parsed by main().
 *
 *  \param status_fname     If non-NULL, a file-name from which to read status
 *                          information.
 *
 *  \param display_format   A string describing the columns to use in displaying
 *                          results.
 *
 *  \param width            The width to use in formatting results.
 *
 *  \param sort             A string describing how to sort results, using the
 *                          same syntax as the curses frontend.
 *
 *  \param disable_columns  \b true to disable columnar formatting and simply
 *                          separate fields with whitespace.
 *
 *  \param debug            \b true to print debugging information to stdout.
 *                          \todo  Should be handled by the logging subsystem.
 *
 *  \param group_by         Controls how to group versions; see group_by_option.
 */
int cmdline_versions(int argc, char *argv[], const char *status_fname,
                     std::string display_format, std::string width,
                     std::string sort, bool disable_columns, bool debug,
                     group_by_option group_by);

#endif // CMDLINE_VERSIONS_H
