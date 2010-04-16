// cmdline_search.h                         -*-c++-*-
//
//   Copyright 2004 Daniel Burrows

#ifndef CMDLINE_SEARCH_H
#define CMDLINE_SEARCH_H

#include <string>

/** \file cmdline_search.h
 */

/** \brief Represents the possible values of the "group-by-package"
 *  command-line option.
 */
enum group_by_package_option
  {
    /** \brief Never group by package. */
    group_by_package_never,

    /** \brief Group by package automatically if there's more than one
     *  package to display OR if the single package returned was not
     *  chosen by exact name.
     *
     *  This is the default value.
     */
    group_by_package_auto,

    /** \brief Always group by package. */
    group_by_package_always
  };

/** \brief Invoke the "search" and "versions" command-line actions.
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
 *  \param search_versions  \b true to search for versions instead of packages.
 *
 *  \param group_by_package If search_versions is true, set this to \b true to
 *                          group the output by package.
 *
 *  \todo Can this perhaps be refactored a bit so that search_versions and
 *        search_packages can be passed in a more extensible way?  Probably
 *        requires refactoring at the level of aptitude::matching too.
 */
int cmdline_search(int argc, char *argv[], const char *status_fname,
		   std::string display_format, std::string width, std::string sort,
		   bool disable_columns, bool debug,
                   bool search_versions, group_by_package_option group_by_package);

#endif // CMDLINE_SEARCH_H
