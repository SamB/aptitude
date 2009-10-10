// -*-c++-*-

// changelog.cpp
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

#include "changelog.h"
#include "aptitude.h"

#undef OK
#include <gtkmm.h>

#include <apt-pkg/fileutl.h>
#include <apt-pkg/pkgrecords.h>
#include <apt-pkg/pkgsystem.h>
#include <apt-pkg/tagfile.h>
#include <apt-pkg/version.h>

#include <cwidget/generic/threads/threads.h>
#include <cwidget/generic/util/ssprintf.h>

#include <loggers.h>

#include <generic/apt/apt.h>
#include <generic/apt/changelog_parse.h>
#include <generic/apt/download_manager.h>
#include <generic/apt/pkg_changelog.h>

#include <gtk/hyperlink.h>
#include <gtk/gui.h>
#include <gtk/progress.h>

using aptitude::apt::changelog_cache;
using aptitude::apt::global_changelog_cache;

namespace cw = cwidget;

namespace gui
{
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

  namespace
  {
    class TextBufferUserAction
    {
      Glib::RefPtr<Gtk::TextBuffer> buffer;

    public:
      TextBufferUserAction(const Glib::RefPtr<Gtk::TextBuffer> &_buffer)
	: buffer(_buffer)
      {
	buffer->begin_user_action();
      }

      ~TextBufferUserAction()
      {
	buffer->end_user_action();
      }
    };
  }

  // \todo Maybe support hiding older versions by default, with a
  // clickable link at the end saying "show older versions...".
  //
  // NB: some changelogs aren't monotonically increasing, so that
  // support should put a link wherever the "newness state" goes from
  // newer to not-newer.
  static Gtk::TextBuffer::iterator
  do_render_changelog(const cwidget::util::ref_ptr<aptitude::apt::changelog> &cl,
		      const Glib::RefPtr<Gtk::TextBuffer> &textBuffer,
		      const std::string &current_version,
		      Gtk::TextBuffer::iterator where,
		      bool only_new)
  {
    using aptitude::apt::changelog;

    // Don't update the display until we finish everything.
    TextBufferUserAction text_buffer_user_action_scope(textBuffer);

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

    // Remember whether we added any changelog entries, so we can show
    // a message if there aren't any.
    bool added_at_least_one = false;
    std::string last_version;

    for(changelog::const_iterator it = cl->begin(); it != cl->end(); ++it)
      {
	cw::util::ref_ptr<aptitude::apt::changelog_entry> ent(*it);

	if(only_new)
	  {
	    // Check whether the version numbers in the changelog are
	    // out-of-order.  We start with the most recent changelog
	    // entry, so they should be decreasing.  If they aren't in
	    // order, stop generating output.
	    //
	    // This is necessary because in practice, some package
	    // changelogs contain many entries that are "newer" than
	    // the most recent entry.  Including those entries
	    // produces output that is both excessive and wrong.
	    const bool retrograde =
	      !last_version.empty() &&
	      _system->VS->CmpVersion(ent->get_version(), last_version) > 0;
	    if(retrograde)
	      break;
	    last_version = ent->get_version();
	  }

	bool newer =
	  !current_version.empty() &&
	  _system->VS->CmpVersion(ent->get_version(), current_version) > 0;

	// If the current entry isn't newer, we would have to have
	// out-of-order entries in order to see a newer one, so stop
	// scanning the changelog at that point.
	if(only_new && !newer)
	  break;

	added_at_least_one = true;

	const bool use_newer_tag = !only_new && newer;

	if(it != cl->begin())
	  where = textBuffer->insert(where, "\n\n");

	Glib::RefPtr<Gtk::TextBuffer::Mark> changelog_entry_mark;
	if(use_newer_tag)
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

	if(use_newer_tag)
	  {
	    Gtk::TextBuffer::iterator start = textBuffer->get_iter_at_mark(changelog_entry_mark);
	    textBuffer->apply_tag(newer_tag, start, where);
	  }
      }

    if(!added_at_least_one)
      {
	if(cl->size() == 0)
	  where = textBuffer->insert(where, _("The changelog is empty."));
	else if((*cl->begin())->get_version() == current_version)
	  where = textBuffer->insert(where, _("No new changelog entries; it looks like you installed a locally compiled version of this package."));
	else
	  where = textBuffer->insert(where, _("No new changelog entries; this is likely due to a binary-only upload of this package."));
      }

    return where;
  }

  Gtk::TextBuffer::iterator
  render_changelog(const cw::util::ref_ptr<aptitude::apt::changelog> &cl,
		   const Glib::RefPtr<Gtk::TextBuffer> &textBuffer,
		   const std::string &current_version,
		   Gtk::TextBuffer::iterator where,
		   bool only_new)
  {
    if(cl.valid())
      return do_render_changelog(cl, textBuffer, current_version, where, only_new);
    else
      {
        Glib::RefPtr<Gtk::TextBuffer::Tag> warning_tag = textBuffer->create_tag();
        warning_tag->property_weight() = Pango::WEIGHT_BOLD;
        warning_tag->property_weight_set() = true;
        warning_tag->property_foreground() = "#FF0000";
        warning_tag->property_foreground_set() = true;
        return textBuffer->insert_with_tag(where,
					   "Can't parse changelog, did you install the libparse-debianchangelog-perl package ?", warning_tag);
        // \todo Offer to install libparse-debianchangelog-perl if we
        // can't parse the changelog because it's missing.
        // Maybe we could add an action button that does that ?
      }
  }

  namespace
  {
    // Value type carrying information for the helper below.  It says
    // which region of the buffer should be replaced with the
    // changelog text.
    struct finish_changelog_download_info
    {
      Glib::RefPtr<Gtk::TextBuffer::Mark> begin, end;
      Glib::RefPtr<Gtk::TextBuffer> text_buffer;
      std::string current_version;
      bool only_new;

      finish_changelog_download_info(const Glib::RefPtr<Gtk::TextBuffer::Mark> &_begin,
				     const Glib::RefPtr<Gtk::TextBuffer::Mark> &_end,
				     const Glib::RefPtr<Gtk::TextBuffer> &_text_buffer,
				     const std::string &_current_version,
				     bool _only_new)
	: begin(_begin),
	  end(_end),
	  text_buffer(_text_buffer),
	  current_version(_current_version),
	  only_new(_only_new)
      {
      }
    };

    // Deletes a range between two marks, inserting a changelog parsed
    // from the given file in the gap.
    //
    // This is invoked in the main thread (via the trampoline
    // do_view_changelog_trampoline) when a changelog is finished
    // downloading.  WARNING: the binding is only thread-safe because
    // it's bound up as a "safe" slot from the main thread, so the
    // RefPtrs are't copied in a background thread!
    void finish_changelog_download(const cw::util::ref_ptr<aptitude::apt::changelog> &cl,
				   finish_changelog_download_info download_info)
    {
      const Gtk::TextBuffer::iterator begin = download_info.text_buffer->get_iter_at_mark(download_info.begin);
      const Gtk::TextBuffer::iterator end = download_info.text_buffer->get_iter_at_mark(download_info.end);

      // Zap the old text.
      const Gtk::TextBuffer::iterator where = download_info.text_buffer->erase(begin, end);

      const bool only_new = download_info.only_new;

      // Now insert the changelog.
      render_changelog(cl,
		       download_info.text_buffer,
		       download_info.current_version,
		       where,
		       only_new);
    }

    void changelog_download_error(const std::string &error,
				  finish_changelog_download_info download_info)
    {
      const Gtk::TextBuffer::iterator begin = download_info.text_buffer->get_iter_at_mark(download_info.begin);
      const Gtk::TextBuffer::iterator end = download_info.text_buffer->get_iter_at_mark(download_info.end);

      // Zap the old text.
      const Gtk::TextBuffer::iterator where = download_info.text_buffer->erase(begin, end);

      download_info.text_buffer->insert(where,
				       ssprintf(_("Failed to download the changelog: %s"), error.c_str()));
    }

    class parse_changelog_job
    {
      // Remember that this is passed by copy-construction, so
      // everything here has to be safe to copy (it is).

      temp::name name;
      safe_slot1<void, cw::util::ref_ptr<aptitude::apt::changelog> > slot;
      const std::string from;

    public:
      parse_changelog_job(const temp::name &_name,
			  const safe_slot1<void, cw::util::ref_ptr<aptitude::apt::changelog> > &_slot,
			  const std::string &_from)
	: name(_name), slot(_slot), from(_from)
      {
      }

      /** \brief Return the temporary file name of the changelog. */
      const temp::name &get_name() const { return name; }
      /** \brief Return the slot that should be invoked when the changelog is parsed. */
      const safe_slot1<void, cw::util::ref_ptr<aptitude::apt::changelog> > &get_slot() const { return slot; }
      /** \brief Return the earliest version that should be included in the parse. */
      const std::string &get_from() const { return from; }
    };

    /** \brief Parses a queue of changelogs in the background.
     *
     *  The purpose of the queue is to ensure that aptitude only
     *  parses one changelog at a time and doesn't waste a ton of time
     *  starting new changelog parse threads and spawning copies of
     *  parsechangelog.
     *
     *  This is a self-terminating singleton thread.
     */
    class parse_changelog_thread
    {
      static std::deque<parse_changelog_job> jobs;
      // The active thread, or NULL if there isn't one.  As it exits,
      // the active thread will destroy this and NULL it out while
      // holding the state mutex.
      static cw::threads::thread *active_thread;

      // This mutex protects all accesses to "jobs" and
      // "active_thread".
      static cw::threads::mutex state_mutex;

    public:
      parse_changelog_thread()
      {
      }

      static void add_job(const parse_changelog_job &job)
      {
	using aptitude::Loggers;

	cw::threads::mutex::lock l(state_mutex);

	LOG_TRACE(Loggers::getAptitudeGtkChangelog(),
		  "Adding a changelog parse job to the queue (name="
		  << job.get_name().get_name() << ", from=" << job.get_from() << ")");

	jobs.push_back(job);
	// Start the parse thread if it's not running.
	if(active_thread == NULL)
	  {
	    LOG_TRACE(Loggers::getAptitudeGtkChangelog(), "Starting a new background changelog parse thread.");
	    active_thread = new cw::threads::thread(parse_changelog_thread());
	  }
      }

      void operator()() const
      {
	using aptitude::Loggers;

	try
	  {
	    cw::threads::mutex::lock l(state_mutex);

	    while(!jobs.empty())
	      {
		parse_changelog_job next(jobs.front());
		jobs.pop_front();

		// Unlock the state mutex for the actual parsing:
		l.release();

		try
		  {
		    cw::util::ref_ptr<aptitude::apt::changelog> parsed =
		      aptitude::apt::parse_changelog(next.get_name(), next.get_from());
		    post_event(safe_bind(next.get_slot(), parsed));
		  }
		catch(const std::exception &ex)
		  {
		    LOG_WARN(Loggers::getAptitudeGtkChangelog(), "Changelog thread: got std::exception: " << ex.what());
		  }
		catch(const cw::util::Exception &ex)
		  {
		    LOG_WARN(Loggers::getAptitudeGtkChangelog(), "Changelog thread: got cwidget::util::Exception: " << ex.errmsg());
		  }
		catch(...)
		  {
		    LOG_WARN(Loggers::getAptitudeGtkChangelog(), "Changelog thread: got an unknown exception.");
		  }

		l.acquire();
	      }

	    delete active_thread;
	    active_thread = NULL;
	    return; // Unless there's an unlikely error, we exit here;
		    // otherwise we try some last-chance error
		    // handling below.
	  }
	catch(const std::exception &ex)
	  {
	    LOG_WARN(Loggers::getAptitudeGtkChangelog(), "Changelog thread aborting with std::exception: " << ex.what());
	  }
	catch(const cw::util::Exception &ex)
	  {
	    LOG_WARN(Loggers::getAptitudeGtkChangelog(), "Changelog thread aborting with cwidget::util::Exception: " << ex.errmsg());
	  }
	catch(...)
	  {
	    LOG_WARN(Loggers::getAptitudeGtkChangelog(), "Changelog thread aborting with an unknown exception.");
	  }

	// Do what we can to recover, although there might be jobs
	// that can't be processed!
	{
	  cw::threads::mutex::lock l(state_mutex);
	  delete active_thread;
	  active_thread = NULL;
	}
      }
    };
    std::deque<parse_changelog_job> parse_changelog_thread::jobs;
    cw::threads::thread *parse_changelog_thread::active_thread = NULL;
    cw::threads::mutex parse_changelog_thread::state_mutex;

    // Helper function to post the "changelog download complete" event
    // to the main thread.
    void do_view_changelog_trampoline(temp::name name,
				      safe_slot1<void, cw::util::ref_ptr<aptitude::apt::changelog> > slot,
				      std::string from)
    {
      parse_changelog_thread::add_job(parse_changelog_job(name, slot, from));
    }

    void do_changelog_download_error_trampoline(const std::string &error,
						safe_slot1<void, std::string> slot)
    {
      post_event(safe_bind(slot, error));
    }

    void process_changelog_job(const changelog_download_job &entry,
			       std::vector<std::pair<pkgCache::VerIterator, changelog_cache::download_callbacks> > &output_jobs)
    {
      Glib::RefPtr<Gtk::TextBuffer> textBuffer = entry.get_text_buffer();
      pkgCache::VerIterator ver = entry.get_ver();

      bool in_debian = false;

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
      std::set<std::string> origins;
      for(pkgCache::VerFileIterator vf=ver.FileList();
	  !vf.end() && !in_debian; ++vf)
	{
	  if(!vf.File().end() && vf.File().Origin() != NULL)
	    origins.insert(vf.File().Origin());
	}

      if(origins.find("Debian") == origins.end())
	{
	  std::string origins_str;
	  for(std::set<std::string>::const_iterator it = origins.begin();
	      it != origins.end(); ++it)
	    {
	      if(it->empty())
		continue;

	      if(!origins_str.empty())
		origins_str += ", ";
	      origins_str += "\"";
	      origins_str += *it;
	      origins_str += "\"";
	    }

	  const Glib::RefPtr<Gtk::TextBuffer::Mark> begin = entry.get_begin();
	  const Gtk::TextBuffer::iterator begin_iter =
	    textBuffer->get_iter_at_mark(begin);
	  if(origins_str.empty())
	    textBuffer->insert(begin_iter,
			       ssprintf(_("You can only view changelogs of official Debian packages; the origin of %s is unknown."),
					pkgname.c_str()));
	  else
	    textBuffer->insert(begin_iter,
			       ssprintf(_("You can only view changelogs of official Debian packages; %s is from %s."),
					pkgname.c_str(), origins_str.c_str()));
	}
      else
	{
	  // \todo It would be uber-cool to have a progress bar for
	  // the particular changelog being downloaded, but we need
	  // more information from the download backend to do that.

	  // Insert a temporary message telling the user that we're
	  // downloading a changelog, and store sticky anchors
	  // pointing to its beginning and end.
	  Glib::RefPtr<Gtk::TextBuffer::Mark> begin = entry.get_begin();
	  Glib::RefPtr<Gtk::TextBuffer::Mark> end;

	  {
	    const Gtk::TextBuffer::iterator begin_iter =
	      textBuffer->get_iter_at_mark(begin);
	    const Gtk::TextBuffer::iterator end_iter =
	      textBuffer->insert(begin_iter,
				 _("Downloading changelog; please wait..."));

	    end = textBuffer->create_mark(end_iter);
	  }

	  // Bind up the buffer location, the buffer pointer, and the
	  // source version with the finalization code (so we can
	  // invoke it when the changelog downloads).
	  //
	  // The changelog continuation is triggered in the background
	  // thread, so we need to safely wrap it and post it to the
	  // main thread.
	  const bool only_new = entry.get_only_new();
	  finish_changelog_download_info
	    download_info(begin, end, textBuffer, current_source_ver, only_new);

	  const sigc::slot1<void, cw::util::ref_ptr<aptitude::apt::changelog> > finish_changelog_download_slot =
	    sigc::bind(sigc::ptr_fun(&finish_changelog_download),
		       download_info);

	  const safe_slot1<void, cw::util::ref_ptr<aptitude::apt::changelog> > finish_changelog_download_safe_slot =
	    make_safe_slot(finish_changelog_download_slot);

	  const sigc::slot1<void, temp::name> finish_changelog_download_trampoline =
	    sigc::bind(sigc::ptr_fun(&do_view_changelog_trampoline),
		       finish_changelog_download_safe_slot,
		       only_new ? current_source_ver : "");

	  const sigc::slot1<void, std::string> changelog_download_error_slot =
	    sigc::bind(sigc::ptr_fun(&changelog_download_error),
		       download_info);

	  const safe_slot1<void, std::string> changelog_download_error_safe_slot =
	    make_safe_slot(changelog_download_error_slot);

	  const sigc::slot1<void, std::string> changelog_download_error_trampoline =
	    sigc::bind(sigc::ptr_fun(&do_changelog_download_error_trampoline),
		       changelog_download_error_safe_slot);

	  const changelog_cache::download_callbacks
	    callbacks(make_safe_slot(finish_changelog_download_trampoline),
		      make_safe_slot(changelog_download_error_trampoline));

	  output_jobs.push_back(std::make_pair(ver, callbacks));
	}
    }
  }

  void fetch_and_show_changelogs_start_download(boost::shared_ptr<download_manager> manager)
  {
    start_download(manager,
		   _("Downloading changelogs"),
		   NULL,
		   download_progress_item_count,
		   pMainWindow->get_notifyview());
  }

  void fetch_and_show_changelogs(const std::vector<changelog_download_job> &changelogs)
  {
    std::vector<std::pair<pkgCache::VerIterator, changelog_cache::download_callbacks> > jobs;

    for(std::vector<changelog_download_job>::const_iterator it = changelogs.begin();
	it != changelogs.end(); ++it)
      // If the job can be downloaded, insert it into the queue.
      process_changelog_job(*it, jobs);

    // After the changelog download is prepared, invoke the above
    // routine to start it.
    sigc::slot<void, boost::shared_ptr<download_manager> > k =
      sigc::ptr_fun(&fetch_and_show_changelogs_start_download);

    global_changelog_cache.get_changelogs(jobs, make_safe_slot(k));
  }

  void fetch_and_show_changelog(const pkgCache::VerIterator &ver,
				const Glib::RefPtr<Gtk::TextBuffer> &text_buffer,
				const Gtk::TextBuffer::iterator &where)
  {
    Glib::RefPtr<Gtk::TextBuffer::Mark> where_mark =
      text_buffer->create_mark(where);

    changelog_download_job job(where_mark, text_buffer, ver, false);

    std::vector<changelog_download_job> changelogs;
    changelogs.push_back(job);

    fetch_and_show_changelogs(changelogs);
  }
}
