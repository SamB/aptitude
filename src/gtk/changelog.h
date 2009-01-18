// changelog.h             -*-c++-*-
//
//  Copyright 1999-2009 Daniel Burrows
//  Copyright 2008 Obey Arthur Liu
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

#ifndef CHANGELOG_H_
#define CHANGELOG_H_

#undef OK
#include <gtkmm.h>
#include <libglademm/xml.h>

#include <apt-pkg/pkgcache.h>
#include <apt-pkg/acquire.h>

#include <cwidget/generic/util/ref_ptr.h>

#include <generic/util/refcounted_base.h>
#include <generic/util/temp.h>
#include <generic/util/util.h>

namespace aptitude
{
  namespace apt
  {
    class changelog;
  }
}

namespace gui
{
  class dummyPkgAcquireStatus : public pkgAcquireStatus
  { // must also derive to read protected members..
    public:
      dummyPkgAcquireStatus();
      bool MediaChange(std::string, std::string);
  };

  /** \brief Render a changelog object into a buffer.
   *
   *  \param cl                The changelog to render.
   *  \param textBuffer        The text buffer in which to store
   *                           the rendered text.
   *  \param current_version   The currently installed source
   *                           version of the package whose
   *                           changelog this is (used to
   *                           determine which versions are
   *                           newer).
   *  \param where             The buffer location at which to render
   *                           the changelog.
   *  \param only_new          Set to \b true to only show entries that
   *                           are newer than current_version.  If this
   *                           is set, the changelog will be truncated
   *                           if version numbers that are out of order
   *                           are encountered.  (this avoids displaying
   *                           the whole changelog for packages where a
   *                           past version "reset" the version number)
   *
   *  \return A new iterator to the end of the rendered text.
   */
  Gtk::TextBuffer::iterator
  render_changelog(const cwidget::util::ref_ptr<aptitude::apt::changelog> &cl,
		   const Glib::RefPtr<Gtk::TextBuffer> &textBuffer,
		   const std::string &current_version,
		   Gtk::TextBuffer::iterator where,
		   bool only_new);

  /** \brief Parse a changelog and render it into a buffer.
   *
   *  \param file   The raw changelog text.
   *  \param textBuffer        The text buffer in which to store
   *                           the rendered text.
   *  \param current_version   The currently installed source
   *                           version of the package whose
   *                           changelog this is (used to
   *                           determine which versions are
   *                           newer).
   *  \param where             The buffer location at which to render
   *                           the changelog.
   *  \param only_new          Set to \b true to only show entries that
   *                           are newer than current_version.
   *
   *  \return A new iterator to the end of the rendered text.
   *
   *  If the changelog can't be parsed, a hopefully appropriate
   *  error message is displayed.
   */
  Gtk::TextBuffer::iterator
  parse_and_render_changelog(const temp::name &file,
			     const Glib::RefPtr<Gtk::TextBuffer> &textBuffer,
			     const std::string &current_version,
			     Gtk::TextBuffer::iterator where,
			     bool only_new);

  /** \brief A changelog job says to download the changelog of a
   *  binary package version at the given buffer location.
   */
  class changelog_download_job
  {
    Glib::RefPtr<Gtk::TextBuffer::Mark> begin;
    Glib::RefPtr<Gtk::TextBuffer> text_buffer;
    pkgCache::VerIterator ver;
    bool only_new;

  public:
    /** \brief Create a new changelog download job.
     *
     *  \param _begin   The buffer location where the changelog should be inserted.
     *  \param _text_buffer  The text buffer in which to insert the changelog.
     *  \param _ver     The version whose changelog should be downloaded.
     *  \param _only_new  If \b true, only new entries will be displayed.
     */
    changelog_download_job(const Glib::RefPtr<Gtk::TextBuffer::Mark> &_begin,
			   const Glib::RefPtr<Gtk::TextBuffer> &_text_buffer,
			   const pkgCache::VerIterator &_ver,
			   bool _only_new)
      : begin(_begin),
	text_buffer(_text_buffer),
	ver(_ver),
	only_new(_only_new)
    {
    }

    const Glib::RefPtr<Gtk::TextBuffer::Mark> &get_begin() const { return begin; }
    const Glib::RefPtr<Gtk::TextBuffer> &get_text_buffer() const { return text_buffer; }
    const pkgCache::VerIterator &get_ver() const { return ver; }
    bool get_only_new() const { return only_new; }
  };

  /** \brief Start downloading changelogs for the given package
   *  versions and insert them into the corresponding buffers.
   *
   *  \param changelogs   The versions whose changelogs should
   *                      be downloaded and where to put them.
   */
  void fetch_and_show_changelogs(const std::vector<changelog_download_job> &changelogs);

  /** \brief Start downloading a changelog for a single version,
   *  inserting it at the given location in the given buffer.
   *
   *  \param ver   The version whose changelog should be downloaded.
   *  \param text_buffer  The text buffer in which to display the changelog.
   *  \param where  The location in text_buffer where the changelog
   *                should appear.
   *
   *  This is a convenience wrapper for fetch_and_show_changelogs.
   */
  void fetch_and_show_changelog(const pkgCache::VerIterator &ver,
				const Glib::RefPtr<Gtk::TextBuffer> &text_buffer,
				const Gtk::TextBuffer::iterator &where);
}

#endif /* CHANGELOG_H_ */
