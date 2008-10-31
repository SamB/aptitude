// -*-c++-*-

// changelog.cpp
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

#include "changelog.h"
#include "aptitude.h"

#undef OK
#include <gtkmm.h>

#include <apt-pkg/fileutl.h>
#include <apt-pkg/pkgsystem.h>
#include <apt-pkg/tagfile.h>
#include <apt-pkg/version.h>

#include <generic/apt/changelog_parse.h>
#include <generic/apt/download_manager.h>
#include <generic/apt/pkg_changelog.h>

#include <gtk/gui.h>
#include <gtk/progress.h>

using aptitude::apt::global_changelog_cache;

namespace gui
{
  void ChangeLogView::do_view_changelog(temp::name n,
                                string pkgname,
                                string curverstr)
  {
    string menulabel =
      ssprintf(_("ChangeLog of %s"), pkgname.c_str());
    string tablabel = ssprintf(_("%s changes"), pkgname.c_str());
    string desclabel = _("View the list of changes made to this Debian package.");

    // Create a new text buffer to ensure that we have a blank state.
    textBuffer = Gtk::TextBuffer::create();
    add_changelog_buffer(n, curverstr);
    textview->set_buffer(textBuffer);
  }


  void append_change_text(const std::string &s,
			  const Glib::RefPtr<Gtk::TextBuffer> &buffer)
  {
    std::string::size_type start = 0;
    std::string::size_type next_nl;

    Glib::RefPtr<Gtk::TextBuffer::Tag> bold_tag = buffer->create_tag();
    bold_tag->property_weight() = Pango::WEIGHT_BOLD;

    do
      {
        next_nl = s.find('\n', start);

        if(s[start] == ' ')
          ++start;

        if(next_nl == start + 1 && s[start] == '.')
          {
	    buffer->insert(buffer->end(), "\n");
            start = next_nl + 1;
            continue;
          }

	std::pair<const char *, const char *> this_line;
        if(next_nl != std::string::npos)
          this_line = std::make_pair(s.c_str() + start,
				     s.c_str() + next_nl);
        else
          this_line = std::make_pair(s.c_str() + start,
				     s.c_str() + s.size());

        size_t first_nonspace = 0;
        while((signed)first_nonspace < this_line.second - this_line.first &&
	      isspace(this_line.first[first_nonspace]))
          ++first_nonspace;

        bool has_bullet = false;
        if((signed)first_nonspace < this_line.second - this_line.first)
          switch(this_line.first[first_nonspace])
            {
            case '*':
            case '+':
            case '-':
              has_bullet = true;
              break;
            }

        if(has_bullet)
          {
	    buffer->insert(buffer->end(),
			   this_line.first,
			   this_line.first + first_nonspace);

	    buffer->insert_with_tag(buffer->end(),
				    this_line.first + first_nonspace,
				    this_line.first + first_nonspace + 1,
				    bold_tag);

	    buffer->insert(buffer->end(),
			   this_line.first + first_nonspace + 1,
			   this_line.second);

	    buffer->insert(buffer->end(), "\n");
          }
        else
	  {
	    buffer->insert(buffer->end(), this_line.first, this_line.second);
	    buffer->insert(buffer->end(), "\n");
	  }

        start = next_nl + 1;
      } while(next_nl != std::string::npos);
  }

  void ChangeLogView::add_changelog_buffer(const temp::name &file,
                                        const std::string &curver)
  {
    using aptitude::apt::changelog;
    cwidget::util::ref_ptr<changelog> cl =
      aptitude::apt::parse_changelog(file);

    if(cl.valid())
      {
	for(changelog::const_iterator it = cl->begin(); it != cl->end(); ++it)
	  {
	    if(it != cl->begin())
	      textBuffer->insert(textBuffer->end(), "\n\n");

	    append_change_text(it->get_changes(), textBuffer);

	    textBuffer->insert(textBuffer->end(), "\n");
	    textBuffer->insert(textBuffer->end(), " -- ");
	    textBuffer->insert(textBuffer->end(), it->get_maintainer());
	    textBuffer->insert(textBuffer->end(), " ");
	    textBuffer->insert(textBuffer->end(), it->get_date_str());
	  }
      }
    // \todo Offer to install libparse-debianchangelog-perl if we
    // can't parse the changelog because it's missing.
  }

  ChangeLogView::ChangeLogView(Gtk::TextView *_textview)
    : textview(_textview)
  {
  }

  ChangeLogView::~ChangeLogView()
  {
    // TODO Auto-generated destructor stub
  }

  void ChangeLogView::load_version(pkgCache::VerIterator ver)
  {
    textBuffer = Gtk::TextBuffer::create();

    bool in_debian=false;

    string pkgname = ver.ParentPkg().Name();

    pkgCache::VerIterator curver = ver.ParentPkg().CurrentVer();
    string curverstr;
    if(!curver.end() && curver.VerStr() != NULL)
      curverstr = curver.VerStr();

    // TODO: add a configurable association between origins and changelog URLs.
    for(pkgCache::VerFileIterator vf=ver.FileList();
        !vf.end() && !in_debian; ++vf)
      if(!vf.File().end() && vf.File().Origin()!=NULL &&
         strcmp(vf.File().Origin(), "Debian")==0)
        in_debian=true;

    if(!in_debian)
      {
        textBuffer->set_text(_("You can only view changelogs of official Debian packages."));
        return;
      }

    // \todo It would be uber-cool to have a progress bar for the
    // particular changelog being downloaded, but we need more
    // information from the download backend to do that.
    textBuffer->set_text(_("Downloading changelog; please wait..."));

    textview->set_buffer(textBuffer);

    std::auto_ptr<download_manager> manager(global_changelog_cache.get_changelog(ver, sigc::bind(sigc::mem_fun(*this, &ChangeLogView::do_view_changelog), pkgname, curverstr)));

    start_download(manager.release(),
		   _("Downloading changelogs"),
		   false,
		   pMainWindow->get_notifyview());
  }
}
