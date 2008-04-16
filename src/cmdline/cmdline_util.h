// cmdline_util.h                                   -*-c++-*-
//
//   Copyright 2004 Daniel Burrows

#ifndef CMDLINE_UTIL_H
#define CMDLINE_UTIL_H

#include "cmdline_common.h"

// For download_manager::result
#include <generic/apt/download_manager.h>

#include <string>

#include <apt-pkg/srcrecords.h>

/** \file cmdline_util.h
 */

void cmdline_show_pkglist(pkgvector &items);
void cmdline_show_stringlist(strvector &items);

/** Finds a candidate version for the package using the given source.
 */
pkgCache::VerIterator cmdline_find_ver(pkgCache::PkgIterator pkg,
				       cmdline_version_source source,
				       string sourcestr);

/** Starts up the visual UI in preview mode, and exits with status 0
 *  when the UI shuts down.
 */
void ui_preview();

/** Starts up the visual UI with the solution screen visible, exiting
 *  when the UI shuts down.
 */
void ui_solution_screen();

/** Splits the given input string into a package name/pattern and a
 *  version source.  If the input string is an output string, the
 *  function will still behave sanely.
 *
 *  \param input the string to be split
 *  \param source will be set to the type of source specified
 *  \param package will be set to the package name/pattern
 *  \param sourcestr will be set to the string associated with the source,
 *                   if any
 *
 *  \return \b true if the source was successfully parsed.
 */
bool cmdline_parse_source(const string &input,
			  cmdline_version_source &source,
			  string &package,
			  string &sourcestr);

/** Run the given download and post-download commands using the
 *  standard command-line UI.  Runs the preparation routine, the
 *  actual download, and the post-download commands.
 *
 *  \param m        the download process to run.
 *  \param verbose  the verbosity level; controls how many
 *                  stats are printed when the run completes.
 *
 *  \return the success status of the post-download commands, or
 *  failure if the process failed before they could be run.
 */
download_manager::result cmdline_do_download(download_manager *m,
					     int verbose);

/** \brief Test whether a string looks like a search pattern.
 *
 *  \param s  the string to test.
 *
 *  Looks for tildes and question marks.
 *
 *  \return \b true if the string qualifies as a search pattern.
 */
bool cmdline_is_search_pattern(const std::string &s);

namespace aptitude
{
  namespace matching
  {
    class pkg_matcher;
  }

  namespace cmdline
  {
    /** \brief Hack to handle memory management of apt source parsers.
     *
     *  apt parsers belong to the source-list class they are
     *  instantiated by.  This makes it tricky to safely return them
     *  from any routine.  Rather than play games with carefully
     *  managing their use, I just use this class: a safe,
     *  copy-constructable object holding all the information stored
     *  by a source package.
     */
    class source_package
    {
      std::string package;
      std::string version;
      std::string maintainer;
      std::string section;
      std::vector<std::string> binaries;
      std::vector<pkgSrcRecords::Parser::BuildDepRec> build_deps;

    public:
      source_package();
      source_package(pkgSrcRecords::Parser *parser);

      /** \return \b true if this represents a real source package.
       *
       *  Invalid packages are analogous to NULL returns.
       */
      bool valid() const { return !package.empty(); }

      const std::string &get_package() const { return package; }
      const std::string &get_version() const { return version; }
      const std::string &get_maintainer() const { return maintainer; }
      const std::string &get_section() const { return section; }
      const std::vector<std::string> &get_binaries() const { return binaries; }
      const std::vector<pkgSrcRecords::Parser::BuildDepRec> &get_build_deps() const { return build_deps; }
    };

    /** Find a source record in the given set of source records
     *  corresponding to the given package and archive.
     *
     *  \param records the source records object
     *  \param pkg the package name to match on
     *  \param ver the version string to match on
     *
     *  \return the source record, or an invalid package if none is
     *  found.
     */
    source_package find_source_by_archive(const std::string &pkg,
					  const std::string &archive);

    /** \brief Interpret a source-package specification from
     *  the command line.
     *
     *  \brief package   the name of the package to search for.
     *                   If this is a binary package, then that
     *                   package's source is used; otherwise
     *                   the list of source packages is searched.
     *  \brief version_source   how to find the source version
     *                          (implicit, candidate, or archive)
     *  \brief version_source_string    which source version
     *                                  to use.
     *
     *  \return the located source record, or an invalid package if
     *  unsuccessful.
     */
    source_package find_source_package(const std::string &package,
				       cmdline_version_source version_source,
				       const std::string &version_source_string);

    /** \brief Represents a user request to add or remove a user-tag.
     *
     *  This corresponds to the command-line arguments --add-user-tag,
     *  --remove-user-tag, --add-user-tag-to, and
     *  --remove-user-tag-from.
     */
    class tag_application
    {
      bool is_add;
      std::string tag;
      matching::pkg_matcher *matcher; // or NULL for implicit matchers.

    public:
      tag_application(bool _is_add,
		      const std::string &_tag,
		      matching::pkg_matcher *_matcher)
      {
	is_add = _is_add;
	tag = _tag;
	matcher = _matcher;
      }

      bool get_is_add() const { return is_add; }
      const std::string &get_tag() const { return tag; }
      matching::pkg_matcher *get_matcher() const { return matcher; }
    };

    /** \brief Apply explicit and implicit user-tags to packages.
     *
     *  Explicit tags are applied where their associated matcher holds;
     *  implicit tags are applied to packages that the user requested (as
     *  indicated in to_installed et al) and for which the requested
     *  action is being performed.
     *
     *  \param tags The actions requested by the user; they will be
     *              applied in turn to each package, so later actions
     *              will override earlier ones.
     *
     *  \param to_upgrade The packages that the user asked to have upgraded.
     *  \param to_install The packages that the user asked to have installed.
     *  \param to_hold The package that the user asked to have held back.
     *  \param to_remove The packages that the user asked to have removed.
     *  \param to_purge The packages that the user asked to have purged.
     */
    void apply_user_tags(const std::vector<tag_application> &user_tags);
  }
}

#endif // CMDLINE_UTIL_H
