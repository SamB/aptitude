/** \file screenshot_cache.cc */


// Copyright (C) 2009 Daniel Burrows
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

#include "screenshot_cache.h"

#include <boost/enable_shared_from_this.hpp>
#include <boost/multi_index_container.hpp>
#include <boost/multi_index/hashed_index.hpp>
#include <boost/multi_index/sequenced_index.hpp>
#include <boost/weak_ptr.hpp>

#include <cwidget/generic/util/ssprintf.h>

#include <gdkmm/pixbufloader.h>

#include <generic/apt/apt.h>
#include <generic/apt/config_signal.h>
#include <generic/apt/download_queue.h>

#include <generic/util/job_queue_thread.h>

#include <sigc++/trackable.h>

#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>


#include <gtk/gui.h>
#include <loggers.h>

using namespace aptitude;

using aptitude::util::job_queue_thread;

namespace cw = cwidget;

namespace gui
{
  namespace
  {
    class screenshot_cache_entry;

    class load_screenshot_job
    {
      temp::name filename;
      boost::shared_ptr<screenshot_cache_entry> cache_entry;

    public:
      load_screenshot_job(const temp::name &_filename,
			  const boost::shared_ptr<screenshot_cache_entry> &_cache_entry)
	: filename(_filename),
	  cache_entry(_cache_entry)
      {
      }

      const temp::name &get_filename() const { return filename; }
      const boost::shared_ptr<screenshot_cache_entry> &get_cache_entry() const { return cache_entry; }
    };

    // Needed for job_queue_thread.
    std::ostream &operator<<(std::ostream &out, const load_screenshot_job &job);

    /** \brief A background thread used to load whole screenshots.
     *
     *  Small screenshots or ones that are fetched instantly can be
     *  loaded in a background thread, avoiding slowing down the main
     *  thread with loading individual chunks.
     */
    class load_screenshot_thread : public job_queue_thread<load_screenshot_thread, load_screenshot_job>
    {
    public:
      static log4cxx::LoggerPtr get_log_category()
      {
	return Loggers::getAptitudeGtkScreenshot();
      }

      void process_job(const load_screenshot_job &job);
    };


    /** \brief A single cached screenshot.
     *
     *  Screenshots in the cache might be in the process of being
     *  loaded.  If they are, they have a PixbufLoader object that's
     *  being used to read in the screenshot.
     */
    class screenshot_cache_entry : public cached_screenshot, public download_callbacks,
				   public boost::enable_shared_from_this<screenshot_cache_entry>,
				   public sigc::trackable
    {
      // This is NULL until we get the first partial-load message.  If
      // we never get one, we just load the image directly.
      //
      // When the download is complete, this is set to NULL.
      Glib::RefPtr<Gdk::PixbufLoader> loader;
      // This stores the number of bytes tat have been read from the
      // file into the loader.  It's initially 0.
      off64_t num_bytes_read;

      // If the download isn't started yet, this is NULL.  Otherwise,
      // this is set to the loaded pixbuf.
      Glib::RefPtr<Gdk::Pixbuf> image;


      // The associated download object, if any.
      //
      // This is discarded when the pixbuf is finished loading.  No
      // strong reference loop here because download_request only uses
      // weak references.
      boost::shared_ptr<download_request> request;

      // Tracks how much space the cache thinks this screenshot takes
      // up; used to ensure that the total cache size is correctly
      // computed.
      int size;


      // Information used to generate this entry's key:
      //
      // The name of the package that this is a screenshot for.
      std::string package_name;

      // The screenshot type.
      screenshot_type type;

      /** \brief Incrementally load a portion of the file.
       *
       *  \param name   The file to load from.
       *  \param endpos  The position to stop reading, or -1 to
       *                 load the whole file.
       *
       *  \return \b true if the load succeeded, \b false otherwise.
       */
      bool incremental_load(const temp::name &name,
			    int endpos)
      {
	int fdnum = open(name.get_name().c_str(), O_RDONLY);
	if(fdnum < 0)
	  {
	    int errnum = errno;
	    LOG_WARN(Loggers::getAptitudeGtkScreenshot(),
		     "Loading " << show()
		     << " from the file " << name.get_name()
		     << " failed: open() failed:"
		     << cw::util::sstrerror(errnum));

	    return false;
	  }

	// Use RAII to avoid leaking the fd.  Didn't use the
	// FileFd constructor because I wanted control over error
	// handling.
	FileFd fd(fdnum);

	off64_t where = lseek64(fdnum, SEEK_SET, num_bytes_read);
	if(where == (off64_t)-1)
	  {
	    int errnum = errno;
	    LOG_WARN(Loggers::getAptitudeGtkScreenshot(),
		     "Loading " << show()
		     << " from the file " << name.get_name()
		     << " failed: lseek64() failed: "
		     << cw::util::sstrerror(errnum));

	    return false;
	  }

	// Now read in the rest of the image, generating events as
	// we do.
	ssize_t amt_read = 0;
	const int blockSize = 1024 * 8;
	guint8 buf[blockSize];

	while( (amt_read = read(fdnum, buf, blockSize)) > 0)
	  {
	    loader->write(buf, amt_read);

	    num_bytes_read += amt_read;
	  }

	if(amt_read < 0)
	  {
	    int errnum = errno;
	    LOG_WARN(Loggers::getAptitudeGtkScreenshot(),
		     "Loading " << show()
		     << " from the file " << name.get_name()
		     << " failed: read() failed: "
		     << cw::util::sstrerror(errnum));

	    return false;
	  }
	else if(endpos >= 0 && num_bytes_read < endpos)
	  {
	    LOG_WARN(Loggers::getAptitudeGtkScreenshot(),
		     "Loading " << show()
		     << " from the file " << name.get_name()
		     << " failed: unexpected EOF after "
		     << num_bytes_read << " bytes.");

	    return false;
	  }
	else
	  return true;
      }

      void area_prepared()
      {
	image = loader->get_pixbuf();
      }

    public:
      screenshot_cache_entry(const std::string &_package_name,
			     screenshot_type _type)
	: loader(Gdk::PixbufLoader::create()),
	  num_bytes_read(0),
	  image(),
	  size(0),
	  package_name(_package_name),
	  type(_type)
      {
      }

      /** \brief Actually start the download.
       *
       *  This must be invoked immediately after the constructor.  It
       *  can't be in the constructor because shared_from_this() isn't
       *  allowed to run until the shared_ptr has been created.
       */
      void go()
      {
	// This doesn't create a reference loop because the request
	// object holds only weak references.
	request = aptitude::get_screenshot(package_name,
					   shared_from_this(),
					   post_thunk,
					   type);
      }

      ~screenshot_cache_entry()
      {
	// Clear the signals in case the PixbufLoader emits something.
	get_signal_failed().clear();
	get_signal_prepared().clear();
	get_signal_updated().clear();
	get_signal_ready().clear();

	// The PixbufLoader complain loudly if we don't do this:
	if(loader)
	  {
	    try
	      {
		loader->close();
	      }
	    catch(Glib::Exception &ex)
	      {
		// The loader likes to throw exceptions when it gets
		// closed on a partial file.
		LOG_TRACE(Loggers::getAptitudeGtkScreenshot(),
			  "~screenshot_cache_entry(): ignoring an error from Gdk::PixbufLoader::close(): "
			  << ex.what());
	      }
	  }
      }

      const std::string &get_package_name() const { return package_name; }
      screenshot_type get_type() const { return type; }
      int get_size() const { return size; }
      Glib::RefPtr<Gdk::Pixbuf> get_image() const { return image; }
      void set_size(int new_size)
      {
	size = new_size;
      }


      /** \brief Retrieve a string that can be used in log messages to
       *  describe this entry.
       */
      std::string show() const
      {
	std::string typestr;
	switch(type)
	  {
	  case screenshot_thumbnail:
	    typestr = "thumbnail";
	    break;

	  case screenshot_full:
	    typestr = "full-size";
	    break;

	  default:
	    typestr = "BADTYPE";
	    break;
	  }

	return "the " + typestr + " screenshot of " + package_name;
      }

      // cached_screenshot implementation:
      Glib::RefPtr<Gdk::Pixbuf> get_screenshot() { return image; }
      void cancel();

      /** \brief Invoked when the image was loaded directly from a file.
       */
      void image_loaded(const Glib::RefPtr<Gdk::Pixbuf> &new_image)
      {
	LOG_TRACE(Loggers::getAptitudeGtkScreenshot(),
		  "Replacing " << show()
		  << " with a newly loaded image.");

	if(loader)
	  {
	    try
	      {
		loader->close();
	      }
	    catch(Glib::Exception &ex)
	      {
		// Ignore errors, since we aren't using the loader's
		// image anyway.
		LOG_TRACE(Loggers::getAptitudeGtkScreenshot(),
			  "Ignoring an error that occurred while closing the old pixbuf loader: "
			  << ex.what());
	      }
	  }
	loader.reset();
	num_bytes_read = 0;
	image = new_image;
	request.reset();

	get_signal_ready()();
      }

      // download_callbacks implementation:
      void success(const temp::name &filename)
      {
	request.reset();

	if(num_bytes_read == 0)
	  {
	    LOG_TRACE(Loggers::getAptitudeGtkScreenshot(),
		      "Loading " << show()
		      << " from the file " << filename.get_name()
		      << " in the background thread.");

	    load_screenshot_thread::add_job(load_screenshot_job(filename, shared_from_this()));
	  }
	else
	  {
	    LOG_TRACE(Loggers::getAptitudeGtkScreenshot(),
		      "Loading " << show()
		      << " from the file " << filename.get_name()
		      << " from position " << num_bytes_read);

	    if(incremental_load(filename, -1))
	      {
		try
		  {
		    loader->close();
		  }
		catch(Glib::Exception &ex)
		  {
		    LOG_WARN(Loggers::getAptitudeGtkScreenshot(),
			     "Incremental load of " << show()
			     << " from the file " << filename.get_name()
			     << " failed, falling back to loading the whole file: "
			     << ex.what());
		    load_screenshot_thread::add_job(load_screenshot_job(filename, shared_from_this()));
		  }
	      }
	    else
	      {
		LOG_WARN(Loggers::getAptitudeGtkScreenshot(),
			 "Incremental load of " << show()
			 << " from the file " << filename.get_name()
			 << " failed, falling back to loading the whole file.");

		load_screenshot_thread::add_job(load_screenshot_job(filename, shared_from_this()));
	      }
	  }
      }

      void failure(const std::string &msg)
      {
	LOG_WARN(Loggers::getAptitudeGtkScreenshot(),
		 "Failed to acquire " << show()
		 << ": " << msg);
	get_signal_failed()(msg);
	request.reset();
      }

      void partial_download(const temp::name &filename,
			    long currentSize,
			    long totalSize)
      {
	int incrementalLoadLimit =
	  aptcfg->FindI(PACKAGE "::Screenshot::Incremental-Load-Limit",
			16384);

	if(currentSize >= incrementalLoadLimit &&
	   currentSize > num_bytes_read)
	  {
	    if(loader == NULL)
	      {
		loader = Gdk::PixbufLoader::create();

		loader->signal_area_prepared().connect(sigc::mem_fun(*this, &screenshot_cache_entry::area_prepared));

		loader->signal_area_prepared().connect(get_signal_prepared().make_slot());
		loader->signal_area_updated().connect(get_signal_updated().make_slot());
		loader->signal_closed().connect(get_signal_ready().make_slot());
	      }

	    // TODO: what do we do if a chunk fails?  Cancel?  Just
	    // try to load the whole thing when it's done?
	    incremental_load(filename, currentSize);
	  }
      }
    };

    using namespace boost::multi_index;

    /** \brief The global cache of loaded screenshots.
     *
     *  All members here are static; the class is just a way of
     *  organizing them.
     */
    class screenshot_cache
    {
      // No locking since all these routines run in the foreground
      // thread (the screenshot layer posts all its events there, even
      // if they originate in a background thread).
      class ordered_tag;
      class by_screenshot_tag;

      class hash_cache_entry
      {
      public:
	size_t operator()(const boost::shared_ptr<screenshot_cache_entry> &entry) const
	{
	  size_t seed = 0;

	  boost::hash_combine(seed, entry->get_package_name());
	  boost::hash_combine(seed, entry->get_type());

	  return seed;
	}
      };

      class cache_entry_equal
      {
      public:
	bool operator()(const boost::shared_ptr<screenshot_cache_entry> &entry1,
			const boost::shared_ptr<screenshot_cache_entry> &entry2) const
	{
	  return
	    entry1->get_type() == entry2->get_type() &&
	    entry1->get_package_name() == entry2->get_package_name();
	}
      };

      typedef multi_index_container<
	boost::shared_ptr<screenshot_cache_entry>,
	indexed_by<
	  hashed_unique<tag<by_screenshot_tag>,
			identity<boost::shared_ptr<screenshot_cache_entry> >,
			hash_cache_entry,
			cache_entry_equal>,
	  sequenced<tag<ordered_tag> > >
	> cache_map;

      typedef cache_map::index<ordered_tag>::type ordered_index;
      typedef cache_map::index<by_screenshot_tag>::type by_screenshot_index;

      static cache_map cache;
      static int cache_size; // Last computed size of the cache.

      static int get_max_cache_size()
      {
	// How much memory to tie up in loaded screenshots; defaults
	// to 4MB.
	return aptcfg->FindI(PACKAGE "::Screenshot::Max-Memory-Usage",
			     1024 * 1024 * 4);
      }

      static int get_entry_size(const screenshot_cache_entry &entry)
      {
	Glib::RefPtr<Gdk::Pixbuf> screenshot(entry.get_image());

	return screenshot->get_rowstride() * screenshot->get_height();
      }

      static void update_cache_size(int new_cache_size)
      {
	cache_size = new_cache_size;

	// If the cache is too large, repeatedly remove the front of
	// the ordered index (subtracting its size) until it's small
	// enough again.
	const int max_cache_size = get_max_cache_size();
	while(cache_size > max_cache_size)
	  {
	    ordered_index &ordered(cache.get<ordered_tag>());

	    if(ordered.size() == 0)
	      {
		LOG_WARN(Loggers::getAptitudeGtkScreenshot(),
			 "Sanity-check failed: there are no cached screenshots, but the cache size is too large ("
			 << cache_size << ")!");
		break;
	      }
	    else
	      {
		boost::shared_ptr<screenshot_cache_entry> victim =
		  ordered.front();

		LOG_INFO(Loggers::getAptitudeGtkScreenshot(),
			 "Dropping " << victim->show()
			 << " from the cache to free up "
			 << victim->get_size() << " bytes.");

		ordered.pop_front();
		// Note that update_entry_size could recur, so instead
		// we just call set_size() directly.
		cache_size -= victim->get_size();
		victim->set_size(0);
	      }
	  }
      }

      // Update the cache's stored knowledge of the entry's size.
      static void update_entry_size(screenshot_cache_entry &entry,
				    int new_size)
      {
	const int new_cache_size = cache_size - entry.get_size() + new_size;
	LOG_TRACE(Loggers::getAptitudeGtkScreenshot(),
		  "Updating the size of " << entry.show()
		  << " from " << entry.get_size() << " to "
		  << new_size << ", cache size changes from "
		  << cache_size << " to " << new_cache_size);

	entry.set_size(new_size);
	update_cache_size(new_cache_size);
      }

      static void screenshot_size_computed(const boost::weak_ptr<screenshot_cache_entry> &entryWeak)
      {
	boost::shared_ptr<screenshot_cache_entry> entry(entryWeak);

	if(entry.get() != NULL)
	  update_entry_size(*entry, get_entry_size(*entry));
      }

      static void download_failed(const std::string &msg,
				  const boost::weak_ptr<screenshot_cache_entry> &entryWeak)
      {
	boost::shared_ptr<screenshot_cache_entry> entry(entryWeak);

	if(entry.get() != NULL)
	  {
	    // Drop it from the cache.  Be careful here: if the
	    // download already expired, we don't want to drop some
	    // other entry from the cache.
	    by_screenshot_index &by_screenshot(cache.get<by_screenshot_tag>());

	    by_screenshot_index::iterator found = by_screenshot.find(entry);
	    if(found != by_screenshot.end())
	      {
		if(*found != entry)
		  LOG_TRACE(Loggers::getAptitudeGtkScreenshot(),
			    "Failed to download " << entry->show()
			    << ", but not dropping it from the cache: it was already replaced.");
		else
		  {
		    LOG_INFO(Loggers::getAptitudeGtkScreenshot(),
			     "Failed to download " << entry->show()
			     << ", dropping it from the cache.");

		    // Forget about its size if it had a size.
		    update_entry_size(*entry, 0);
		    by_screenshot.erase(found);
		  }
	      }
	  }
      }

      static void add_entry(const boost::shared_ptr<screenshot_cache_entry> &entry)
      {
	LOG_INFO(Loggers::getAptitudeGtkScreenshot(),
		 "Adding " << entry->show() << " to the cache ("
		 << entry->get_size() <<" bytes).");

	// If it exists already, we need to evict the existing
	// element.
	by_screenshot_index &by_screenshot(cache.get<by_screenshot_tag>());

	by_screenshot_index::iterator found = by_screenshot.find(entry);
	if(found != by_screenshot.end())
	  {
	    // Should never happen!
	    LOG_WARN(Loggers::getAptitudeGtkScreenshot(),
		     "Dropping " << (*found)->show()
		     << " from the cache to make room for the new entry.");
	    update_entry_size(**found, 0);
	    by_screenshot.replace(found, entry);
	  }
	else
	  cache.get<ordered_tag>().push_back(entry);

	update_cache_size(cache_size + entry->get_size());
      }

    public:
      static boost::shared_ptr<screenshot_cache_entry>
      find(const std::string &package_name,
	   screenshot_type type)
      {
	boost::shared_ptr<screenshot_cache_entry> rval =
	  boost::make_shared<screenshot_cache_entry>(package_name, type);

	boost::weak_ptr<screenshot_cache_entry> rvalWeak(rval);

	rval->get_signal_failed().connect(sigc::bind(sigc::ptr_fun(&screenshot_cache::download_failed),
						     rvalWeak));
	rval->get_signal_prepared().connect(sigc::bind(sigc::ptr_fun(&screenshot_cache::screenshot_size_computed),
						       rvalWeak));

	rval->go();

	return rval;
      }

      static void canceled(const boost::shared_ptr<screenshot_cache_entry> &entry)
      {
	by_screenshot_index &by_screenshot(cache.get<by_screenshot_tag>());

	by_screenshot_index::iterator found = by_screenshot.find(entry);
	if(found != by_screenshot.end())
	  {
	    LOG_INFO(Loggers::getAptitudeGtkScreenshot(),
		     entry->show() << " was canceled, dropping it from the cache.");
	    update_entry_size(*entry, 0);
	    by_screenshot.erase(found);
	  }
      }
    };

    screenshot_cache::cache_map screenshot_cache::cache;
    int screenshot_cache::cache_size = 0;

    void screenshot_cache_entry::cancel()
    {
      if(request.get() != NULL)
	{
	  request->cancel();
	  screenshot_cache::canceled(shared_from_this());
	}
    }

    // Used to avoid accessing the signal, even to make_slot(), from a
    // background thread.
    void emit_failed(const boost::shared_ptr<screenshot_cache_entry> &job,
		     const std::string &msg)
    {
      job->get_signal_failed()(msg);
    }

    std::ostream &operator<<(std::ostream &out, const load_screenshot_job &job)
    {
      return out << "loadScreenshot(" << job.get_filename().get_name()
		 << ", " << job.get_cache_entry()->show() << ")";
    }

    void load_screenshot_thread::process_job(const load_screenshot_job &job)
    {
      LOG_INFO(Loggers::getAptitudeGtkScreenshot(),
	       "Loading " << job.get_cache_entry()->show()
	       << " from " << job.get_filename().get_name());

      try
	{
	  Glib::RefPtr<Gdk::Pixbuf> pixbuf =
	    Gdk::Pixbuf::create_from_file(job.get_filename().get_name());

	  sigc::slot<void, Glib::RefPtr<Gdk::Pixbuf> > set_image_slot =
	    sigc::mem_fun(*job.get_cache_entry(), &screenshot_cache_entry::image_loaded);
          post_event(safe_bind(make_safe_slot(set_image_slot), pixbuf));
	}
      catch(Glib::Exception &ex)
	{
	  sigc::slot<void> failed_slot = sigc::bind(sigc::ptr_fun(&emit_failed),
						    job.get_cache_entry(),
						    ex.what());
	  post_event(make_safe_slot(failed_slot));
	}
    }
  } // End anonymous namespace

  cached_screenshot::~cached_screenshot()
  {
  }

  boost::shared_ptr<cached_screenshot> get_screenshot(const std::string &package_name,
						      aptitude::screenshot_type type)
  {
    return screenshot_cache::find(package_name, type);
  }
}
