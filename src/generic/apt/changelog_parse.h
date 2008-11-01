// changelog_parse.h                        -*-c++-*-
//
//   Copyright (C) 2005, 2008 Daniel Burrows
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

#ifndef CHANGELOG_PARSE_H
#define CHANGELOG_PARSE_H

#include <time.h>

#include <apt-pkg/pkgcache.h>

#include <vector>

#include <cwidget/generic/util/ref_ptr.h>

#include <generic/util/refcounted_base.h>

/** \file changelog_parse.h
 */

class FileFd;

namespace cwidget
{
  class fragment;
}
namespace temp {class name;}

namespace aptitude
{
  namespace apt
  {
    /** \brief Represents a single entry in a Debian changelog. */
    class changelog_entry
    {
      std::string source;
      std::string version;
      std::string distribution;
      std::string urgency;
      std::string changes;
      std::string maintainer;
      std::string date_str;
      bool could_parse_date;
      time_t date;

    public:
      /** \brief Create a new changelog entry.
       *
       *  \param _source       The source package of the entry.
       *  \param _version      The version number of the entry.
       *  \param _distribution The distribution of the entry.
       *  \param _urgency      The urgency of the entry.
       *  \param _changes      The text of the entry.
       *  \param _maintainer   The maintainer field of the entry.
       *  \param _date         The date of the entry.
       */
      changelog_entry(const std::string &_source,
		      const std::string &_version,
		      const std::string &_distribution,
		      const std::string &_urgency,
		      const std::string &_changes,
		      const std::string &_maintainer,
		      const std::string &_date);

      /** \return the source package name of the changelog entry. */
      const std::string &get_source() const { return source; }
      /** \return the version number of the changelog entry. */
      const std::string &get_version() const { return version; }
      /** \return the distribution of the changelog entry. */
      const std::string &get_distribution() const { return distribution; }
      /** \return the urgency of the changelog entry. */
      const std::string &get_urgency() const { return urgency; }
      /** \return the text of the changelog entry. */
      const std::string &get_changes() const { return changes; }
      /** \return the maintainer field of the changelog entry. */
      const std::string &get_maintainer() const { return maintainer; }
      /** \return the date string of the changelog entry. */
      const std::string &get_date_str() const { return date_str; }
      /** \return \b true if the date string in this changelog entry
       *  was correctly parsed.
       */
      bool get_could_parse_date() const { return could_parse_date; }
      /** \return the date that was parsed from the date string in this
       *  changelog entry, if get_could_parse_date() returned \b true.
       */
      time_t get_date() const { return date; }
    };

    /** \brief Represents an entire changelog.
     *
     *  Changelogs are containers of changelog_entry objects.  The
     *  entries are stored in the same order that they appear in the
     *  original changelog.
     */
    class changelog : public util::refcounted_base
    {
      std::vector<changelog_entry> entries;

      changelog(FileFd &file);

    public:
      static cwidget::util::ref_ptr<changelog> create(FileFd &file)
      {
	return new changelog(file);
      }

      /** \brief The type of an iterator over this changelog. */
      typedef std::vector<changelog_entry>::const_iterator const_iterator;
      typedef std::vector<changelog_entry>::size_type size_type;

      size_type size() const { return entries.size(); }
      const_iterator begin() const { return entries.begin(); }
      const_iterator end() const { return entries.end(); }
    };

    /** Parse the contents of the given file as a Debian changelog.
     *  for some reason the file cannot be parsed, returns \b NULL.
     *
     *  \param file a temporary file object containing the changelog.  A
     *         second temporary file will be created in the parent
     *         directory of this file, in which an intermediate form of
     *         the changelog will be stored.

     */
    cwidget::util::ref_ptr<changelog> parse_changelog(const temp::name &file);
  }
}

#endif
