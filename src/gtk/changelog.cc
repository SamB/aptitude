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
#include <apt-pkg/pkgrecords.h>
#include <apt-pkg/pkgsystem.h>
#include <apt-pkg/tagfile.h>
#include <apt-pkg/version.h>

#include <cwidget/generic/util/ssprintf.h>

#include <generic/apt/apt.h>
#include <generic/apt/changelog_parse.h>
#include <generic/apt/download_manager.h>
#include <generic/apt/pkg_changelog.h>

#include <gtk/gui.h>
#include <gtk/progress.h>

using aptitude::apt::global_changelog_cache;

namespace cw = cwidget;

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

  namespace
  {
    void view_bug(const std::string &bug_number)
    {
      std::string URL = cw::util::ssprintf("http://bugs.debian.org/%s", bug_number.c_str());

      std::vector<std::string> arguments;
      arguments.push_back("/usr/bin/sensible-browser");
      arguments.push_back(URL);

      Glib::spawn_async(".", arguments);
    }
  }

  using aptitude::apt::changelog_element_list;
  using aptitude::apt::changelog_element;
  Gtk::TextBuffer::iterator
  render_change_elements(const std::string &text,
			 const std::vector<changelog_element> &elements,
			 const Glib::RefPtr<Gtk::TextBuffer> &buffer,
			 Gtk::TextBuffer::iterator where)
  {
    std::vector<changelog_element>::const_iterator elements_end = elements.end();

    Glib::RefPtr<Gtk::TextBuffer::Tag> bullet_tag = buffer->create_tag();
    bullet_tag->property_weight() = Pango::WEIGHT_BOLD;
    bullet_tag->property_weight_set() = true;

    for(std::vector<changelog_element>::const_iterator it =
	  elements.begin(); it != elements_end; ++it)
      {
	switch(it->get_type())
	  {
	  case changelog_element::text_type:
	    where = buffer->insert(where,
				   text.c_str() + it->get_begin(),
				   text.c_str() + it->get_end());
	    break;

	  case changelog_element::bullet_type:
	    where = buffer->insert_with_tag(where,
					    text.c_str() + it->get_begin(),
					    text.c_str() + it->get_end(),
					    bullet_tag);
	    break;

	  case changelog_element::closes_type:
	    {
	      const std::string bug_number(text, it->get_begin(), it->get_end() - it->get_begin());
	      where = add_hyperlink(buffer, where,
				    bug_number,
				    sigc::bind(sigc::ptr_fun(&view_bug),
					       bug_number));
	    }
	  }
      }

    return where;
  }

  // \todo Maybe support hiding older versions by default, with a
  // clickable link at the end saying "show older versions...".
  //
  // NB: some changelogs aren't monotonically increasing, so that
  // support should put a link wherever the "newness state" goes from
  // newer to not-newer.
  Gtk::TextBuffer::iterator
  render_changelog(const cwidget::util::ref_ptr<aptitude::apt::changelog> &cl,
		   const Glib::RefPtr<Gtk::TextBuffer> &textBuffer,
		   const std::string &current_version,
		   Gtk::TextBuffer::iterator where)
  {
    using aptitude::apt::changelog;

    Glib::RefPtr<Gtk::TextBuffer::Tag> newer_tag(textBuffer->create_tag());
    newer_tag->property_weight() = Pango::WEIGHT_SEMIBOLD;
    newer_tag->property_weight_set() = true;

    Glib::RefPtr<Gtk::TextBuffer::Tag> number_tag = textBuffer->create_tag();
    number_tag->property_scale() = Pango::SCALE_LARGE;

    Glib::RefPtr<Gtk::TextBuffer::Tag> urgency_low_tag = textBuffer->create_tag();

    Glib::RefPtr<Gtk::TextBuffer::Tag> urgency_medium_tag = textBuffer->create_tag();
    urgency_medium_tag->property_weight() = Pango::WEIGHT_BOLD;
    urgency_medium_tag->property_weight_set() = true;

    Glib::RefPtr<Gtk::TextBuffer::Tag> urgency_high_tag = textBuffer->create_tag();
    urgency_high_tag->property_weight() = Pango::WEIGHT_BOLD;
    urgency_high_tag->property_weight_set() = true;
    urgency_high_tag->property_foreground() = "#FF0000";
    urgency_high_tag->property_foreground_set() = true;

    // NB: "emergency" and "critical" are the same; thus saith
    // Policy.
    Glib::RefPtr<Gtk::TextBuffer::Tag> urgency_critical_tag = textBuffer->create_tag();
    urgency_critical_tag->property_weight() = Pango::WEIGHT_BOLD;
    urgency_critical_tag->property_weight_set() = true;
    urgency_critical_tag->property_scale() = Pango::SCALE_LARGE;
    urgency_critical_tag->property_foreground() = "#FF0000";
    urgency_critical_tag->property_foreground_set() = true;

    Glib::RefPtr<Gtk::TextBuffer::Tag> date_tag = textBuffer->create_tag();

    for(changelog::const_iterator it = cl->begin(); it != cl->end(); ++it)
      {
	cw::util::ref_ptr<aptitude::apt::changelog_entry> ent(*it);

	bool newer =
	  !current_version.empty() &&
	  _system->VS->CmpVersion(ent->get_version(), current_version) > 0;

	if(it != cl->begin())
	  where = textBuffer->insert(where, "\n\n");

	Glib::RefPtr<Gtk::TextBuffer::Mark> changelog_entry_mark;
	if(newer)
	  changelog_entry_mark = textBuffer->create_mark(where);

	// Can't hyperlink to the package name because it's a
	// source package name.  Plus, it might not exist.
	where = textBuffer->insert(where, ent->get_source());
	where = textBuffer->insert(where, " (");
	where = textBuffer->insert_with_tag(where,
					    ent->get_version(),
					    number_tag);
	where = textBuffer->insert(where, ") ");
	where = textBuffer->insert(where, ent->get_distribution());
	where = textBuffer->insert(where, "; urgency=");
	Glib::RefPtr<Gtk::TextBuffer::Tag> urgency_tag;
	const std::string &urgency = ent->get_urgency();
	if(urgency == "low")
	  urgency_tag = urgency_low_tag;
	else if(urgency == "medium")
	  urgency_tag = urgency_medium_tag;
	else if(urgency == "high")
	  urgency_tag = urgency_high_tag;
	else if(urgency == "critical" || urgency == "emergency")
	  urgency_tag = urgency_critical_tag;

	if(urgency_tag)
	  where = textBuffer->insert_with_tag(where, urgency, urgency_tag);
	else
	  where = textBuffer->insert(where, urgency);

	where = textBuffer->insert(where, "\n");

	where = render_change_elements(ent->get_changes(), ent->get_elements()->get_elements(), textBuffer, where);

	where = textBuffer->insert(where, "\n\n");
	where = textBuffer->insert(where, " -- ");
	where = textBuffer->insert(where, ent->get_maintainer());
	where = textBuffer->insert(where, " ");
	where = textBuffer->insert_with_tag(where, ent->get_date_str(), date_tag);

	if(newer)
	  {
	    Gtk::TextBuffer::iterator start = textBuffer->get_iter_at_mark(changelog_entry_mark);
	    textBuffer->apply_tag(newer_tag, start, where);
	  }
      }

    return where;
  }

  void ChangeLogView::add_changelog_buffer(const temp::name &file,
					   const std::string &curver)
  {
    cw::util::ref_ptr<aptitude::apt::changelog> cl =
      aptitude::apt::parse_changelog(file);

    if(cl.valid())
      render_changelog(cl, textBuffer, curver, textBuffer->end());
    else
      {
        Glib::RefPtr<Gtk::TextBuffer::Tag> warning_tag = textBuffer->create_tag();
        warning_tag->property_weight() = Pango::WEIGHT_BOLD;
        warning_tag->property_weight_set() = true;
        warning_tag->property_foreground() = "#FF0000";
        warning_tag->property_foreground_set() = true;
        textBuffer->insert_with_tag(textBuffer->end(),
            "Can't parse changelog, did you install the libparse-debianchangelog-perl package ?", warning_tag);
        // \todo Offer to install libparse-debianchangelog-perl if we
        // can't parse the changelog because it's missing.
        // Maybe we could add an action button that does that ?
      }
  }

  ChangeLogView::ChangeLogView(Gtk::TextView *_textview)
    : textview(_textview)
  {
  }

  ChangeLogView::~ChangeLogView()
  {
    // TODO Auto-generated destructor stub
  }

  namespace
  {
    // Helper function to post the "changelog download complete" event
    // to the main thread.
    void do_view_changelog_trampoline(temp::name name,
				      safe_slot1<void, temp::name> slot)
    {
      post_event(safe_bind(slot, name));
    }
  }

  void ChangeLogView::load_version(pkgCache::VerIterator ver)
  {
    textBuffer = Gtk::TextBuffer::create();
    textview->set_buffer(textBuffer);

    bool in_debian=false;

    string pkgname = ver.ParentPkg().Name();

    pkgCache::VerIterator curver = ver.ParentPkg().CurrentVer();
    std::string current_source_ver;
    if(!curver.end())
      {
	pkgRecords::Parser &current_source_rec =
	  apt_package_records->Lookup(curver.FileList());

	current_source_ver =
	  current_source_rec.SourceVer().empty()
	  ? (curver.VerStr() == NULL ? "" : curver.VerStr())
	  : current_source_rec.SourceVer();
      }

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

    // The changelog continuation is triggered in the background
    // thread, so we need to safely wrap it and post it to the main
    // thread.
    sigc::slot1<void, temp::name> k(sigc::bind(sigc::mem_fun(*this, &ChangeLogView::do_view_changelog),
					       pkgname, current_source_ver));
    safe_slot1<void, temp::name>  k_safe(make_safe_slot(k));
    sigc::slot1<void, temp::name> k_trampoline(sigc::bind(sigc::ptr_fun(&do_view_changelog_trampoline),
							  k_safe));
    std::auto_ptr<download_manager> manager(global_changelog_cache.get_changelog(ver, k_trampoline));

    start_download(manager.release(),
		   _("Downloading changelogs"),
		   download_progress_item_count,
		   pMainWindow->get_notifyview());
  }
}
