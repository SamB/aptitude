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

#include <generic/util/file_cache.h>

#include <gtk/hyperlink.h>
#include <gtk/gui.h>
#include <gtk/progress.h>

#include <boost/make_shared.hpp>

using aptitude::apt::changelog_cache;
using aptitude::apt::changelog_info;
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
      const std::string to;
      const std::string source_package;
      bool digested;

    public:
      parse_changelog_job(const temp::name &_name,
			  const safe_slot1<void, cw::util::ref_ptr<aptitude::apt::changelog> > &_slot,
			  const std::string &_from,
			  const std::string &_to,
			  const std::string &_source_package,
			  bool _digested)
	: name(_name), slot(_slot), from(_from),
	  to(_to), source_package(_source_package),
	  digested(_digested)
      {
      }

      /** \brief Return the temporary file name of the changelog. */
      const temp::name &get_name() const { return name; }
      /** \brief Return the slot that should be invoked when the changelog is parsed. */
      const safe_slot1<void, cw::util::ref_ptr<aptitude::apt::changelog> > &get_slot() const { return slot; }
      /** \brief Return the earliest version that should be included in the parse. */
      const std::string &get_from() const { return from; }
      /** \brief Return the version whose changelog is being parsed. */
      const std::string &get_to() const { return to; }
      /** \brief Return the source package whose changelog is being parsed. */
      const std::string &get_source_package() const { return source_package; }
      /** \brief Return \b true if the input file has already been
       *  passed through digest_changelog().
       *
       *  This is true, for instance, when retrieving a digested
       *  changelog from the cache.
       */
      bool get_digested() const { return digested; }
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
      static boost::shared_ptr<cw::threads::thread> active_thread;

      // Set to true when the thread should suspend itself.
      static bool suspended;

      // This mutex protects all accesses to "jobs", "suspended and
      // "active_thread".
      static cw::threads::mutex state_mutex;

      // Set to true when the global signal handlers are connected up.
      static bool signals_connected;

    public:
      parse_changelog_thread()
      {
	if(!signals_connected)
	  {
	    cache_closed.connect(sigc::ptr_fun(&parse_changelog_thread::stop));
	    cache_reloaded.connect(sigc::ptr_fun(&parse_changelog_thread::maybe_start));
	    signals_connected = true;
	  }
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
	if(!suspended && active_thread == NULL)
	  {
	    LOG_TRACE(Loggers::getAptitudeGtkChangelog(), "Starting a new background changelog parse thread.");
	    active_thread = boost::make_shared<cw::threads::thread>(parse_changelog_thread());
	  }
      }

      static void stop()
      {
	using aptitude::Loggers;

	cw::threads::mutex::lock l(state_mutex);

	LOG_TRACE(Loggers::getAptitudeGtkChangelog(),
		  "Pausing the changelog parse thread.");

	suspended = true;
	// Copy this since it'll be zeroed out when the thread exits,
	// which can happen as soon as the lock below is released.
	boost::shared_ptr<cw::threads::thread> active_thread_copy(active_thread);

	l.release();

	if(active_thread_copy.get() != NULL)
	  active_thread_copy->join();
      }

      static void maybe_start()
      {
	using aptitude::Loggers;

	cw::threads::mutex::lock l(state_mutex);

	suspended = false;

	if(active_thread != NULL)
	  LOG_TRACE(Loggers::getAptitudeGtkChangelog(),
		    "Not starting the background changelog parse thread: it's already running.");
	else if(jobs.empty())
	  LOG_TRACE(Loggers::getAptitudeGtkChangelog(),
		    "Not starting the background changelog parse thread: it has no jobs.");
	else
	  {
	    LOG_TRACE(Loggers::getAptitudeGtkChangelog(),
		      "Starting the background changelog parse thread.");
	    active_thread = boost::make_shared<cw::threads::thread>(parse_changelog_thread());
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
		    std::string changelog_uri;
		    if(next.get_from().empty())
		      changelog_uri = ssprintf("changelog://%s/%s",
					       next.get_source_package().c_str(),
					       next.get_to().c_str());
		    else
		      changelog_uri = ssprintf("delta-changelog://%s/%s/%s",
					       next.get_source_package().c_str(),
					       next.get_from().c_str(),
					       next.get_to().c_str());

		    temp::name digested;
		    if(next.get_digested())
		      digested = next.get_name();
		    else
		      digested = aptitude::apt::digest_changelog(next.get_name(), next.get_from());

		    // Note that we don't re-cache the digested
		    // changelog if it was retrieved from the cache
		    // earlier (i.e., if next.get_digested() is true).
		    if(digested.valid() && !next.get_digested())
		      {
			LOG_TRACE(Loggers::getAptitudeGtkChangelog(),
				  "Caching digested changelog as " << changelog_uri);
			download_cache->putItem(changelog_uri, digested.get_name());
		      }

		    cw::util::ref_ptr<aptitude::apt::changelog> parsed =
		      aptitude::apt::parse_digested_changelog(digested);
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

	    active_thread.reset();
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
	  active_thread.reset();
	}
      }
    };
    std::deque<parse_changelog_job> parse_changelog_thread::jobs;
    boost::shared_ptr<cw::threads::thread> parse_changelog_thread::active_thread;
    bool parse_changelog_thread::suspended = false;
    cw::threads::mutex parse_changelog_thread::state_mutex;
    bool parse_changelog_thread::signals_connected = false;

    // Helper function to post the "changelog download complete" event
    // to the main thread.
    void parse_and_view_changelog_trampoline(temp::name name,
					     safe_slot1<void, cw::util::ref_ptr<aptitude::apt::changelog> > slot,
					     std::string from,
					     std::string to,
					     std::string source_package)
    {
      parse_changelog_thread::add_job(parse_changelog_job(name, slot,
							  from, to,
							  source_package,
							  false));
    }

    void do_changelog_download_error_trampoline(const std::string &error,
						safe_slot1<void, std::string> slot)
    {
      post_event(safe_bind(slot, error));
    }

    /** \brief The job structure after the frontend routine has
     *  processed it and before it's passed through
     *  process_changelog_job.
     *
     *  All references to structures in the apt cache are stripped
     *  out, so that it's safe to use this even after a cache reload.
     *  That avoids a world of hurt around trying to manage the job
     *  queue and ensure that it's entirely cleaned out when the cache
     *  is closed.
     */
    class preprocessed_changelog_job
    {
      Glib::RefPtr<Gtk::TextBuffer::Mark> begin;
      Glib::RefPtr<Gtk::TextBuffer> text_buffer;

      bool only_new;

      std::string binary_package_name;

      boost::shared_ptr<changelog_info> target_info;
      boost::shared_ptr<changelog_info> current_info;

      std::set<std::string> origins;

    public:
      explicit preprocessed_changelog_job(const changelog_download_job &job)
	: begin(job.get_begin()),
	  text_buffer(job.get_text_buffer()),
	  only_new(job.get_only_new()),
	  binary_package_name(job.get_ver().ParentPkg().Name()),
	  target_info(changelog_info::create(job.get_ver())),
	  current_info(changelog_info::create(job.get_ver().ParentPkg().CurrentVer()))
      {
	for(pkgCache::VerFileIterator vf = job.get_ver().FileList();
	    !vf.end(); ++vf)
	  {
	    if(!vf.File().end() && vf.File().Origin() != NULL)
	      origins.insert(vf.File().Origin());
	  }
      }
      const Glib::RefPtr<Gtk::TextBuffer::Mark> &get_begin() const { return begin; }
      const Glib::RefPtr<Gtk::TextBuffer> get_text_buffer() const { return text_buffer; }
      bool get_only_new() const { return only_new; }
      const std::string &get_binary_package_name() const { return binary_package_name; }
      const boost::shared_ptr<changelog_info> &get_target_info() const { return target_info; }
      const boost::shared_ptr<changelog_info> &get_current_info() const { return current_info; }
      const std::set<std::string> &get_origins() const { return origins; }
    };

    /** \brief Try to generate a changelog-download object for the
     *  given job and add it to a queue of output jobs to process.
     *
     *  \param entry         The download job to process.
     *
     *  \param digested_file The pre-digested changelog, if one is
     *                       available.
     *
     *  \param output_jobs   The queue of output download jobs; if it's
     *                       possible and necessary to do so, a new
     *                       job will be pushed onto this queue.
     *
     *  This function must be invoked in the main thread.
     */
    void process_changelog_job(const preprocessed_changelog_job &entry,
			       const temp::name &digested_file,
			       std::vector<std::pair<boost::shared_ptr<changelog_info>, changelog_cache::download_callbacks> > &output_jobs)
    {
      Glib::RefPtr<Gtk::TextBuffer> textBuffer = entry.get_text_buffer();

      const std::string &binary_package_name(entry.get_binary_package_name());
      const boost::shared_ptr<changelog_info> &target_info(entry.get_target_info());
      const boost::shared_ptr<changelog_info> &current_info(entry.get_current_info());
      const std::set<std::string> &origins(entry.get_origins());

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
					binary_package_name.c_str()));
	  else
	    textBuffer->insert(begin_iter,
			       ssprintf(_("You can only view changelogs of official Debian packages; %s is from %s."),
					binary_package_name.c_str(), origins_str.c_str()));
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
	    download_info(begin, end, textBuffer,
			  only_new ? current_info->get_source_version() : "",
			  only_new);


	  const sigc::slot1<void, cw::util::ref_ptr<aptitude::apt::changelog> > finish_changelog_download_slot =
	    sigc::bind(sigc::ptr_fun(&finish_changelog_download),
		       download_info);

	  const safe_slot1<void, cw::util::ref_ptr<aptitude::apt::changelog> > finish_changelog_download_safe_slot =
	    make_safe_slot(finish_changelog_download_slot);

	  const sigc::slot1<void, temp::name> parse_and_view_changelog_download_trampoline =
	    sigc::bind(sigc::ptr_fun(&parse_and_view_changelog_trampoline),
		       finish_changelog_download_safe_slot,
		       only_new ? current_info->get_source_version() : "",
		       target_info->get_source_version(),
		       target_info->get_source_package());

	  const sigc::slot1<void, std::string> changelog_download_error_slot =
	    sigc::bind(sigc::ptr_fun(&changelog_download_error),
		       download_info);

	  const safe_slot1<void, std::string> changelog_download_error_safe_slot =
	    make_safe_slot(changelog_download_error_slot);

	  const sigc::slot1<void, std::string> changelog_download_error_trampoline =
	    sigc::bind(sigc::ptr_fun(&do_changelog_download_error_trampoline),
		       changelog_download_error_safe_slot);

	  const changelog_cache::download_callbacks
	    callbacks(make_safe_slot(parse_and_view_changelog_download_trampoline),
		      make_safe_slot(changelog_download_error_trampoline));


	  // The normal flow here is that the download invoked
	  // parse_and_view_changelog_download_trampoline, which tells
	  // the parse thread to parse the changelog and invoke
	  // finish_changelog_download_slot in the main thread when
	  // it's done.

	  // However, if we have a pre-digested changelog, we can call
	  // out to the parse thread directly, bypassing the download
	  // process.
	  if(digested_file.valid())
	    {
	      LOG_TRACE(aptitude::Loggers::getAptitudeGtkChangelog(),
			"Using a predigested file to display the changelog of "
			<< target_info->get_source_package() << " "
			<< target_info->get_source_version());

	      parse_changelog_job parse_job(digested_file,
					    finish_changelog_download_safe_slot,
					    only_new ? current_info->get_source_version() : "",
					    target_info->get_source_version(),
					    target_info->get_source_package(),
					    true);

	      parse_changelog_thread::add_job(parse_job);
	    }
	  else
	    output_jobs.push_back(std::make_pair(target_info, callbacks));
	}
    }
  }

  void get_and_show_changelogs_start_download(boost::shared_ptr<download_manager> manager)
  {
    start_download(manager,
		   _("Downloading changelogs"),
		   NULL,
		   download_progress_item_count,
		   pMainWindow->get_notifyview());
  }

  // Intermediate callback to take the manager produced below and pass
  // it to the above routine, in the main thread, with the usual
  // ten-foot pole.
  void get_and_show_changelogs_start_download_trampoline(boost::shared_ptr<download_manager> manager)
  {
    sigc::slot<void, boost::shared_ptr<download_manager> > k =
      sigc::ptr_fun(&get_and_show_changelogs_start_download);

    post_event(safe_bind(make_safe_slot(k), manager));
  }

  /** \param Start the process of actually downloading, parsing, and
   *  displaying changelogs.
   *
   *  This function must be invoked in the main thread.
   *
   *  \param changelogs A list of what needs to be done.  Each entry
   *                    in this list combines the pre-digested
   *                    changelog file with the job to run; if the
   *                    pre-digested file is invalid, the changelog
   *                    will be downloaded and digested.
   */
  void get_and_show_changelogs(boost::shared_ptr<std::vector<std::pair<temp::name, boost::shared_ptr<preprocessed_changelog_job> > > > changelogs)
  {
    std::vector<std::pair<boost::shared_ptr<changelog_info>, changelog_cache::download_callbacks> > jobs;

    for(std::vector<std::pair<temp::name, boost::shared_ptr<preprocessed_changelog_job> > >::const_iterator
	  it = changelogs->begin(); it != changelogs->end(); ++it)
      // For each input job, process_changelog_job will either
      // immediately pass a predigested file to the parse thread, or
      // queue up a new download entry (if possible, anyway).
      process_changelog_job(*it->second, it->first, jobs);

    // Now prepare and start the actual download:
    sigc::slot<void, boost::shared_ptr<download_manager> > k =
      sigc::ptr_fun(&get_and_show_changelogs_start_download_trampoline);

    global_changelog_cache.get_changelogs(jobs, make_safe_slot(k));
  }



  // The following code powers the thread that checks for parsed
  // (pre-digested) changelogs in the cache, handing each one that it
  // finds off to the "display parsed changelog" code.  Each request
  // contains a list of download jobs, and the ones whose parses are
  // not found in the cache are passed off to be downloaded and/or
  // parsed.

  class check_cache_for_parsed_changelogs_job
  {
    boost::shared_ptr<std::vector<boost::shared_ptr<preprocessed_changelog_job> > > requests;

  public:
    check_cache_for_parsed_changelogs_job(const boost::shared_ptr<std::vector<boost::shared_ptr<preprocessed_changelog_job> > > &_requests)
      : requests(_requests)
    {
    }

    const std::vector<boost::shared_ptr<preprocessed_changelog_job> > &get_requests() const { return *requests; }
  };

  /** \brief A singleton thread that preprocesses changelog jobs by
   *  checking for parsed changelogs in the cache.
   *
   *  \todo I believe it should be possible to write a skeleton "queue
   *  processing thread" class that would subsume this and
   *  parse_changelog_thread.  That would also make it reasonable to
   *  add special features (like spawning up to N of a given thread).
   */
  class check_cache_for_parsed_changelogs_thread
  {
    // Each job is a vector of changelog downloads.
    static std::deque<check_cache_for_parsed_changelogs_job> jobs;

    // The active thread, or NULL if there isn't one.  As it exits,
    // the active thread will destroy this and NULL it out while
    // holding the state mutex.
    static boost::shared_ptr<cw::threads::thread> active_thread;

    // Set to true when the thread should suspend itself.
    static bool suspended;

    // This mutex protects all accesses to "jobs" and "active_thread".
    static cw::threads::mutex state_mutex;

    // Set to true when the global signal handlers are connected up.
    static bool signals_connected;

  public:
    check_cache_for_parsed_changelogs_thread()
    {
      if(!signals_connected)
	{
	  cache_closed.connect(sigc::ptr_fun(&check_cache_for_parsed_changelogs_thread::stop));
	  cache_reloaded.connect(sigc::ptr_fun(&check_cache_for_parsed_changelogs_thread::maybe_start));
	  signals_connected = true;
	}
    }

    static void add_job(check_cache_for_parsed_changelogs_job &job)
    {
      using aptitude::Loggers;

      cw::threads::mutex::lock l(state_mutex);

      LOG_TRACE(Loggers::getAptitudeGtkChangelog(),
		"Adding a find-parsed-changelog job to the queue.");

      jobs.push_back(job);
      // Start the parse thread if it's not running.
      if(active_thread == NULL)
	{
	  LOG_TRACE(Loggers::getAptitudeGtkChangelog(), "Starting a new background find-parsed-changelogs thread.");
	  active_thread = boost::make_shared<cw::threads::thread>(check_cache_for_parsed_changelogs_thread());
	}
    }

    static void stop()
    {
      using aptitude::Loggers;

      cw::threads::mutex::lock l(state_mutex);

      LOG_TRACE(Loggers::getAptitudeGtkChangelog(),
		"Pausing the find-parsed-changelogs thread.");

      suspended = true;
      // Copy this since it'll be zeroed out when the thread exits,
      // which can happen as soon as the lock below is released.
      boost::shared_ptr<cw::threads::thread> active_thread_copy(active_thread);

      l.release();

      if(active_thread_copy.get() != NULL)
	active_thread_copy->join();
    }

    static void maybe_start()
    {
      using aptitude::Loggers;

      cw::threads::mutex::lock l(state_mutex);

      suspended = false;

      if(active_thread != NULL)
	LOG_TRACE(Loggers::getAptitudeGtkChangelog(),
		  "Not starting the background find-parsed-changelogs thread: it's already running.");
      else if(jobs.empty())
	LOG_TRACE(Loggers::getAptitudeGtkChangelog(),
		  "Not starting the background find-parsed-changelogs thread: it has no jobs.");
      else
	{
	  LOG_TRACE(Loggers::getAptitudeGtkChangelog(),
		    "Starting the background find-parsed-changelogs thread.");
	  active_thread = boost::make_shared<cw::threads::thread>(check_cache_for_parsed_changelogs_thread());
	}
    }

    void operator()() const
    {
      log4cxx::LoggerPtr logger(aptitude::Loggers::getAptitudeGtkChangelog());

      try
	{
	  cw::threads::mutex::lock l(state_mutex);

	  while(!jobs.empty())
	    {
	      check_cache_for_parsed_changelogs_job next(jobs.front());
	      jobs.pop_front();

	      // Unlock the state mutex for the actual work:
	      l.release();

	      // Used to queue up the work for the main thread (the
	      // jobs to process and any pre-digested files that
	      // were found).
	      boost::shared_ptr<std::vector<std::pair<temp::name, boost::shared_ptr<preprocessed_changelog_job> > > >
		jobs_with_predigested_files = boost::make_shared<std::vector<std::pair<temp::name, boost::shared_ptr<preprocessed_changelog_job> > > >();

	      for(std::vector<boost::shared_ptr<preprocessed_changelog_job> >::const_iterator it = next.get_requests().begin();
		  it != next.get_requests().end(); ++it)
		{
		  const preprocessed_changelog_job &job(**it);

		  temp::name predigested_file;

		  try
		    {
		      std::string uri;
		      if(!job.get_only_new())
			uri = ssprintf("changelog://%s/%s",
				       job.get_target_info()->get_source_package().c_str(),
				       job.get_target_info()->get_source_version().c_str());
		      else
			uri = ssprintf("delta-changelog://%s/%s/%s",
				       job.get_target_info()->get_source_package().c_str(),
				       job.get_current_info()->get_source_version().c_str(),
				       job.get_target_info()->get_source_version().c_str());


		      LOG_TRACE(logger,
				"Changelog preparation thread: checking for the changelog of "
				<< job.get_target_info()->get_source_package()
				<< " " << job.get_target_info()->get_source_version()
				<< " under the cache URI "
				<< uri);

		      predigested_file = download_cache->getItem(uri);
		      if(predigested_file.valid())
			LOG_TRACE(logger, "Changelog preparation thread: found a predigested changelog for "
				  << job.get_target_info()->get_source_package()
				  << " " << job.get_target_info()->get_source_version()
				  << " in the file " << predigested_file.get_name());
		    }
		  catch(const std::exception &ex)
		    {
		      LOG_WARN(logger, "Changelog preparation thread: got std::exception: " << ex.what());
		    }
		  catch(const cw::util::Exception &ex)
		    {
		      LOG_WARN(logger, "Changelog preparation thread: got cwidget::util::Exception: " << ex.errmsg());
		    }
		  catch(...)
		    {
		      LOG_WARN(logger, "Changelog preparation thread: got an unknown exception.");
		    }


		  jobs_with_predigested_files->push_back(std::make_pair(predigested_file, *it));
		}

	      sigc::slot<void, boost::shared_ptr<std::vector<std::pair<temp::name, boost::shared_ptr<preprocessed_changelog_job> > > > >
		get_and_show_changelogs_slot = sigc::ptr_fun(&get_and_show_changelogs);

	      post_event(safe_bind(make_safe_slot(get_and_show_changelogs_slot),
				   jobs_with_predigested_files));

	      l.acquire();
	    }

	  active_thread.reset();
	  return; // Unless there's an unlikely error, we exit here;
	  // otherwise we try some last-chance error
	  // handling below.
	}
	catch(const std::exception &ex)
	  {
	    LOG_WARN(logger, "Changelog preparation thread aborting with std::exception: " << ex.what());
	  }
	catch(const cw::util::Exception &ex)
	  {
	    LOG_WARN(logger, "Changelog preparation thread aborting with cwidget::util::Exception: " << ex.errmsg());
	  }
	catch(...)
	  {
	    LOG_WARN(logger, "Changelog preparation thread aborting with an unknown exception.");
	  }

	// Do what we can to recover, although there might be jobs
	// that can't be processed!
	{
	  cw::threads::mutex::lock l(state_mutex);
	  active_thread.reset();
	}
    }
  };
  std::deque<check_cache_for_parsed_changelogs_job> check_cache_for_parsed_changelogs_thread::jobs;
  boost::shared_ptr<cw::threads::thread> check_cache_for_parsed_changelogs_thread::active_thread;
  bool check_cache_for_parsed_changelogs_thread::suspended = false;
  cw::threads::mutex check_cache_for_parsed_changelogs_thread::state_mutex;
  bool check_cache_for_parsed_changelogs_thread::signals_connected = false;




  // The top-level entry point for changelog fetching.
  //
  // The overall changelog fetch process has three steps:
  //
  // 1. First, we check whether each changelog exists as a parsed
  //    entry in the cache.  The URI used is one of:
  //
  //        changelog://PACKAGE/NEW_VERSION
  //        delta-changelog://PACKAGE/CURRENT_VERSION/NEW_VERSION
  //
  //    In the second case, only "new" entries are present.  The
  //    source package name is always used.
  //
  // 2. Second, we check whether the remaining changelogs exist on
  //    disk or in the file cache.
  //
  // 3. Finally, any changelogs not resolved by steps 1 and 2 are
  //    retrieved over the network.
  //
  // Step 1 is implemented in this file; 2 and 3 are implemented in
  // src/generic/pkg_changelog.{cc,h}.  (TODO: the code in this file
  // should probably be generalized for use with, e.g., the curses
  // frontend)
  //
  // Changelogs retrieved in steps 2 and 3 are parsed, then cached for
  // later use in step 1.  To conserve time, aptitude looks in the
  // file cache for the parsed changelog once more before proceeding
  // to parse it (the parse process is quite slow, hence all this
  // caching).

  void fetch_and_show_changelogs(const std::vector<changelog_download_job> &changelogs)
  {
    // Preprocess and strip out references to the apt cache:
    boost::shared_ptr<std::vector<boost::shared_ptr<preprocessed_changelog_job> > > preprocessed =
      boost::make_shared<std::vector<boost::shared_ptr<preprocessed_changelog_job> > >();

    preprocessed->reserve(changelogs.size());

    for(std::vector<changelog_download_job>::const_iterator it = changelogs.begin();
	it != changelogs.end(); ++it)
      preprocessed->push_back(boost::make_shared<preprocessed_changelog_job>(*it));

    check_cache_for_parsed_changelogs_job job(preprocessed);
    check_cache_for_parsed_changelogs_thread::add_job(job);
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
