// changelog.h             -*-c++-*-
//
//  Copyright 1999-2008 Daniel Burrows
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

namespace gui
{
  class dummyPkgAcquireStatus : public pkgAcquireStatus
  { // must also derive to read protected members..
    public:
      dummyPkgAcquireStatus();
      bool MediaChange(std::string, std::string);
  };

  class ChangeLogView : public aptitude::util::refcounted_base
  {
    private:
      Gtk::TextView * textview;
      Glib::RefPtr<Gtk::TextBuffer> textBuffer;
      void parse_predigested_changelog(const temp::name &digest, const std::string &curver);
      temp::name digest_changelog(const temp::name &changelog);
      void do_view_changelog(temp::name n, string pkgname, string curverstr);
      void add_changelog_buffer(const temp::name &file, const std::string &curver);

      /** \brief Construct a new changelog view.
       *
       *  \param textview   The text-view to attach to.
       */
      ChangeLogView(Gtk::TextView *textview);

    public:
      static cwidget::util::ref_ptr<ChangeLogView> create(Gtk::TextView *textview)
      {
	return new ChangeLogView(textview);
      }

      virtual ~ChangeLogView();

      void load_version(pkgCache::VerIterator ver);
  };

}

#endif /* CHANGELOG_H_ */
