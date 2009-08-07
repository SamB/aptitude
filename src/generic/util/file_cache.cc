/** \file file_cache.cc */     // -*-c++-*-

//   Copyright (C) 2009 Daniel Burrows
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

#include "file_cache.h"

#include "sqlite.h"
#include "util.h"

#include <boost/make_shared.hpp>

#include <loggers.h>

using namespace aptitude::sqlite;

namespace aptitude
{
  namespace util
  {
    namespace
    {
      /** \brief An SQLite-backed cache.
       *
       *  The cache schema is defined below as a gigantic string constant.
       *  The reason for splitting blobs out from the main table is
       *  primarily that the incremental blob-reading functions cause
       *  trouble if someone else updates the row containing the blob;
       *  the blob table above will be immutable until it's removed.
       *
       *  Note that referential integrity is maintained on delete by
       *  removing the corresponding entry in the other table; this
       *  makes it easier to remove entries from the cache.
       *
       *  The "format" table is for future use (to support upgrades to
       *  the cache format); it contains a single row, which currently
       *  contains the integer "1" in the "version" field.
       */
      class file_cache_sqlite : public file_cache
      {
	boost::shared_ptr<db> store;
	std::string filename; // Used to report errors.
	/** \brief The maximum size of the cache, in bytes.
	 *
	 *  \todo What's the best way to compute the current size?  I
	 *  could store it in the database and update it as part of
	 *  the transaction that inserts / removes blobs.  Or I could
	 *  use triggers to keep it up-to-date.  Or perhaps there's an
	 *  sqlite query that will return the number of data pages
	 *  used by a table/column?
	 *
	 *  Also, I need to decide when to vacuum the database: after
	 *  each update, after deletes, or what?
	 */
	int max_size;

	static const int current_version_number = 1;

	void create_new_database()
	{
	  std::string schema = "                                        \
begin transaction							\
create table format ( version integer )					\
									\
create table cache ( CacheId integer primary key,			\
                     LastUsedDate text not null,			\
                     BlobId integer not null )				\
									\
create table blobs ( BlobId integer primary key,			\
                     CacheId integer not null,				\
                     Data blob not null )				\
									\
create trigger fki_cache_blob_id					\
before insert on cache							\
for each row begin							\
    select raise(rollback, 'Insert on table \"cache\" violates foreign key constraint \"fki_cache_blob_id\"') \
    where (select id from blobs where blobs.BlobId = NEW.BlobId) is NULL; \
end									\
									\
create trigger fki_blob_cache_id					\
before insert on blobs							\
for each row begin							\
    select raise(rollback, 'Insert on table \"blobs\" violates foreign key constraint \"fki_blob_cache_id\"') \
    where (select id from cache where cache.BlobId = NEW.BlobId) is NULL; \
									\
create trigger fku_cache_blob_id					\
before update on cache							\
for each row begin							\
    select raise(rollback, 'Insert on table \"cache\" violates foreign key constraint \"fki_cache_blob_id\"') \
    where (select id from blobs where blobs.BlobId = NEW.BlobId) is NULL; \
end									\
									\
create trigger fku_blob_cache_id					\
before update on blobs							\
for each row begin							\
    select raise(rollback, 'Insert on table \"blobs\" violates foreign key constraint \"fki_blob_cache_id\"') \
    where (select id from cache where cache.BlobId = NEW.BlobId) is NULL; \
end									\
									\
create trigger fkd_cache_blob_id					\
before delete on cache							\
for each row begin							\
    delete from blobs where blobs.BlobId = OLD.BlobId			\
end									\
									\
create trigger fkd_blob_cache_id					\
before delete on blobs							\
for each row begin							\
    delete from cache where cache.BlobId = OLD.BlobId			\
end									\
";

	  store->exec(schema, NULL, NULL);

	  boost::shared_ptr<statement> set_version_statement =
	    statement::prepare(*store, "insert into format(version) values(?)");

	  set_version_statement->bind_int(1, current_version_number);
	  set_version_statement->exec();

	  statement::prepare(*store, "commit")->exec();
	}

	void sanity_check_database()
	{
	  // Currently just checks the version -- but we could also
	  // check the format and so on here.
	  boost::shared_ptr<statement> get_version_statement =
	    statement::prepare(*store, "select version from format");
	  if(!get_version_statement->step())
	    throw FileCacheException("Can't read the cache version number.");
	  else if(get_version_statement->get_int(0) != current_version_number)
	    throw FileCacheException("Unsupported version number.");
	}

      public:
	file_cache_sqlite(const std::string &_filename, int _max_size)
	  : store(db::create(_filename)),
	    filename(_filename),
	    max_size(_max_size)
	{
	  // Set up the database.  First, check the format:
	  boost::shared_ptr<statement> check_for_format_statement =
	    statement::prepare(*store, "select 1 from sqlite_master where name = 'format'");
	  if(!check_for_format_statement->step())
	    create_new_database();
	  else
	    sanity_check_database();
	}

	void putItem(const std::string &key,
		     const std::string &path)
	{
	  throw FileCacheException("Not implemented");
	}

	temp::name getItem(const std::string &key)
	{
	  throw FileCacheException("Not implemented");
	}
      };

      /** \brief A multilevel cache.
       *
       *  "get" requests are serviced from each sub-cache in turn,
       *  failing if the object isn't found in any cache.
       *
       *  "put" requests are forwarded to all sub-caches.
       */
      class file_cache_multilevel : public file_cache
      {
	std::vector<boost::shared_ptr<file_cache> > caches;

      public:
	file_cache_multilevel()
	{
	}

	void push_back(const boost::shared_ptr<file_cache> &cache)
	{
	  caches.push_back(cache);
	}


	void putItem(const std::string &key, const std::string &path)
	{
	  for(std::vector<boost::shared_ptr<file_cache> >::const_iterator
		it = caches.begin(); it != caches.end(); ++it)
	    (*it)->putItem(key, path);
	}

	temp::name getItem(const std::string &key)
	{
	  for(std::vector<boost::shared_ptr<file_cache> >::const_iterator
		it = caches.begin(); it != caches.end(); ++it)
	    {
	      temp::name found = (*it)->getItem(key);
	      if(found.valid())
		return found;
	    }

	  return temp::name();
	}
      };
    }

    boost::shared_ptr<file_cache> file_cache::create(const std::string &filename,
						     int memory_size,
						     int disk_size)
    {
      boost::shared_ptr<file_cache_multilevel> rval = boost::make_shared<file_cache_multilevel>();

      if(memory_size > 0)
	{
	  try
	    {
	      // \note A boost::multi_index_container might be more
	      // efficient for the in-memory cache.  OTOH, it would
	      // require more code.
	      rval->push_back(boost::make_shared<file_cache_sqlite>(":memory:", memory_size));
	    }
	  catch(const cwidget::util::Exception &ex)
	    {
	      LOG_WARN(Loggers::getAptitudeDownloadCache(),
		       "Unable to create the in-memory cache: " << ex.errmsg());
	    }
	}
      else
	LOG_INFO(Loggers::getAptitudeDownloadCache(),
		 "In-memory cache disabled.");


      if(disk_size > 0)
	{
	  try
	    {
	      rval->push_back(boost::make_shared<file_cache_sqlite>(filename, disk_size));
	    }
	  catch(const cwidget::util::Exception &ex)
	    {
	      LOG_WARN(Loggers::getAptitudeDownloadCache(),
		       "Unable to open the on-disk cache \"" << filename << "\": " << ex.errmsg());
	    }
	}
      else
	LOG_INFO(Loggers::getAptitudeDownloadCache(),
		 "On-disk cache disabled.");

      return rval;
    }

    file_cache::~file_cache()
    {
    }
  }
}
