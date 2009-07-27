/** \file sqlite.cc */


//   Copyright (C) 2009 Daniel Burrows

//   This program is free software; you can redistribute it and/or
//   modify it under the terms of the GNU General Public License as
//   published by the Free Software Foundation; either version 2 of
//   the License, or (at your option) any later version.

//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//   General Public License for more details.

//   You should have received a copy of the GNU General Public License
//   along with this program; see the file COPYING.  If not, write to
//   the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//   Boston, MA 02111-1307, USA.

#include "sqlite.h"

namespace aptitude
{
  namespace sqlite
  {
    /** \brief The default maximum size of a database's statement
     *  cache.
     */
    const unsigned int default_statement_cache_limit = 100;

    db::lock::lock(db &parent)
      : handle(parent.handle)
    {
      sqlite3_mutex_enter(sqlite3_db_mutex(handle));
    }

    db::lock::~lock()
    {
      sqlite3_mutex_leave(sqlite3_db_mutex(handle));
    }

    db::db(const std::string &filename,
	   int flags,
	   const char *vfs)
      : statement_cache_limit(default_statement_cache_limit)
    {
      const int result =
	sqlite3_open_v2(filename.c_str(), &handle,
			flags, vfs);
      if(result != SQLITE_OK)
	{
	  std::string msg(get_error());
	  // sqlite3_open will allocate a database object even on
	  // error, probably to ensure we can retrieve the message (as
	  // above).  To avoid a resource leak, it's necessary to
	  // close the object after retrieving the message.
	  if(handle != NULL)
	    sqlite3_close(handle);
	  throw exception(msg, result);
	}
    }

    db::~db()
    {
      // NULL out the handles of all outstanding statements, to avoid
      // trying to access freed objects.
      for(boost::unordered_set<statement *>::const_iterator
	    it = active_statements.begin();
	  it != active_statements.end(); ++it)
	{
	  (*it)->handle = NULL;
	}

      // Close all active statements on the database.
      for(sqlite3_stmt *stmt = sqlite3_next_stmt(handle, NULL);
	  stmt != NULL; stmt = sqlite3_next_stmt(handle, stmt))
	{
	  sqlite3_finalize(stmt);
	}

      // Close all active blobs on the database.
      for(boost::unordered_set<blob *>::const_iterator
	    it = active_blobs.begin();
	  it != active_blobs.end(); ++it)
	{
	  sqlite3_blob_close((*it)->handle);
	  (*it)->handle = NULL;
	}

      sqlite3_close(handle);
    }

    std::string db::get_error()
    {
      lock l(*this);

      // Note that we're careful to copy the error message into a
      // local string while holding the lock.
      std::string rval(sqlite3_errmsg(handle));

      return rval;
    }

    void db::cache_statement(const statement_cache_entry &entry)
    {
      statement_cache_mru &mru(get_cache_mru());
      mru.push_back(entry);

      // Drop old entries from the cache if it's too large.
      while(mru.size() > statement_cache_limit)
	mru.pop_front();
    }

    db::statement_proxy_impl::~statement_proxy_impl()
    {
      // Careful here: the database might have been deleted while the
      // proxy is active.  WE RELY ON THE FACT THAT DELETING THE
      // DATABASE NULLS OUT THE STATEMENT HANDLE.
      if(entry.stmt->handle == NULL)
	return; // The database is dead; nothing to do.
      else
	entry.stmt->parent.cache_statement(entry);
    }

    db::statement_proxy db::get_cached_statement(const std::string &sql)
    {
      // Check whether the statement exists in the cache.
      statement_cache_hash_index &index(get_cache_hash_index());

      statement_cache_hash_index::const_iterator found =
	index.find(sql);

      if(found != index.end())
	{
	  // Extract the element from the set and return it.
	  statement_cache_entry entry(*found);
	  entry.stmt->reset();

	  index.erase(sql);

	  boost::shared_ptr<statement_proxy_impl> rval(new statement_proxy_impl(entry));
	  return statement_proxy(rval);
	}
      else
	{
	  // Prepare a new SQL statement and return a proxy to it.  It
	  // won't be added to the cache until the caller is done with
	  // it.
	  boost::shared_ptr<statement> stmt(statement::prepare(*this, sql));

	  statement_cache_entry entry(sql, stmt);
	  boost::shared_ptr<statement_proxy_impl> rval(new statement_proxy_impl(entry));
	  return statement_proxy(rval);
	}
    }



    statement::statement(db &_parent, sqlite3_stmt *_handle)
      : parent(_parent),
	handle(_handle),
	has_data(false)
    {
      parent.active_statements.insert(this);
    }

    statement::~statement()
    {
      if(handle != NULL)
	sqlite3_finalize(handle);
      parent.active_statements.erase(this);
    }

    boost::shared_ptr<statement>
    statement::prepare(db &parent,
		       const std::string &sql)
    {
      sqlite3_stmt *handle = NULL;

      // Serialize access to the database for this call.
      db::lock l(parent);

      const int result =
	sqlite3_prepare_v2(parent.handle,
			   sql.c_str(),
			   static_cast<int>(sql.size()),
			   &handle,
			   NULL);

      if(result != SQLITE_OK)
	{
	  // Paranoia: the docs say that "handle" is now NULL, but
	  // just in case...
	  if(handle != NULL)
	    sqlite3_finalize(handle);

	  throw exception(parent.get_error(), result);
	}
      else
	return boost::shared_ptr<statement>(new statement(parent, handle));
    }

    boost::shared_ptr<statement>
    statement::prepare(db &parent,
		       const char *sql)
    {
      sqlite3_stmt *handle = NULL;

      // Serialize access to the database for this call.
      db::lock l(parent);

      const int result =
	sqlite3_prepare_v2(parent.handle,
			   sql,
			   -1,
			   &handle,
			   NULL);

      if(result != SQLITE_OK)
	{
	  // Paranoia: the docs say that "handle" is now NULL, but
	  // just in case...
	  if(handle != NULL)
	    sqlite3_finalize(handle);

	  throw exception(parent.get_error(), result);
	}
      else
	return boost::shared_ptr<statement>(new statement(parent, handle));
    }

    void statement::reset()
    {
      sqlite3_reset(handle);
      has_data = false;
    }

    bool statement::step()
    {
      int result = sqlite3_step(handle);
      if(result == SQLITE_ROW)
	{
	  has_data = true;
	  return true;
	}
      else if(result == SQLITE_DONE)
	{
	  has_data = false;
	  return false;
	}
      else
	throw exception(parent.get_error(), result);
    }

    const void *statement::get_blob(int column, int &bytes)
    {
      require_data();
      const void *rval = sqlite3_column_blob(handle, column);
      bytes = sqlite3_column_bytes(handle, column);

      return rval;
    }

    double statement::get_double(int column)
    {
      require_data();
      return sqlite3_column_double(handle, column);
    }

    int statement::get_int(int column)
    {
      require_data();
      return sqlite3_column_int(handle, column);
    }

    sqlite3_int64 statement::get_int64(int column)
    {
      require_data();
      return sqlite3_column_int64(handle, column);
    }

    std::string statement::get_string(int column)
    {
      require_data();

      const unsigned char * const rval = sqlite3_column_text(handle, column);
      const int bytes = sqlite3_column_bytes(handle, column);

      return std::string(rval, rval + bytes);
    }



    blob::blob(db &_parent, sqlite3_blob *_handle)
      : parent(_parent),
	handle(_handle)
    {
      parent.active_blobs.insert(this);
    }

    blob::~blob()
    {
      if(handle != NULL)
	sqlite3_blob_close(handle);
      parent.active_blobs.erase(this);
    }
  }
}
