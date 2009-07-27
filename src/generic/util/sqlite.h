/** \file sqlite.h */   // -*-c++-*-

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

#ifndef SQLITE_H
#define SQLITE_H

#include <cwidget/generic/util/exception.h>

#include <sqlite3.h>

#include <boost/shared_ptr.hpp>
#include <boost/unordered_set.hpp>

// C++ wrapper for sqlite to handle tracking and releasing resources.

namespace aptitude
{
  namespace sqlite
  {
    class exception : public cwidget::util::Exception
    {
      std::string msg;
      int error_code;

    public:
      exception(const std::string &_msg, int _error_code)
	: msg(_msg), error_code(_error_code)
      {
      }

      std::string errmsg() const { return msg; }
      int get_error_code() const { return error_code; }
    };

    class blob;
    class statement;

    /** \brief Wraps a single connection to an SQLite database.
     *
     *  If the database has been changed to multithreaded mode (as
     *  opposed to the default serialized mode), users should avoid
     *  simultaneous calls to database methods.  While it should be
     *  safe to access the database itself, the wrong error message
     *  might be returned from a database call in this case.
     */
    class db
    {
      sqlite3 *handle;

      friend class blob;
      friend class statement;

      // For safety's sake, we keep around a collection of weak
      // references to the active statements, in cooperation with the
      // constructor and destructor of the statement class.  When the
      // database itself is destroyed, this is used to invalidate all
      // the statement objects.
      boost::unordered_set<statement *> active_statements;

      // Similarly, a set of active blob objects.
      boost::unordered_set<blob *> active_blobs;

      db(const std::string &filename, int flags, const char *vfs);
    public:
      /** \brief Used to make the wrapper routines atomic.
       *
       *  When the database is opened in serialized mode (the
       *  default), this class provides an RAII way to acquire and
       *  release the global database lock.  This ensures, among other
       *  things, that error codes are properly matched to the call
       *  that triggered the error.
       */
      class lock
      {
	sqlite3 *handle;
      public:
	lock(db &parent);
	~lock();
      };

      /** \brief Open an SQLite database.
       *
       *  \param filename   The name of the database file to open.
       *  \param flags      The flags with which to open the database.
       *  \param vfs        The name of the VFS module that should be
       *                    used to access the database.
       *
       *  See the sqlite3_open documentation for details.
       */
      static boost::shared_ptr<db>
      create(const std::string &filename,
	     int flags = SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE,
	     const char *vfs = NULL)
      {
	return boost::shared_ptr<db>(new db(filename, flags, vfs));
      }

      /** \brief Close the encapsulated database. */
      ~db();

      /** \brief Retrieve the last error that was generated on this
       *  database.
       *
       *  If the database is not opened in the default "serialized"
       *  mode, the last error might be an error for an operation on
       *  another thread.
       */
      std::string get_error();
    };

    /** \brief Wraps a prepared sqlite3 statement.
     *
     *  This class is explicitly *not* thread-safe.  You should place
     *  locks around it if it's going to be accessed from multiple
     *  threads.
     */
    class statement
    {
      db &parent;
      sqlite3_stmt *handle;
      /** \brief Set to \b true when results are available.
       *
       *  Used to sanity-check that the database is being used
       *  correctly (according to the rules laid down in the docs).
       *  Maybe sqlite does this already, but since it's not
       *  documented I don't want to rely on it.
       */
      bool has_data;

      friend class db;

      statement(db &_parent, sqlite3_stmt *_handle);

      /** \brief Throw an exception if there isn't result data ready
       *  to be read.
       */
      void require_data()
      {
	if(!has_data)
	  throw exception("No data to retrieve.", SQLITE_MISUSE);
      }

    public:
      ~statement();

      /** \brief Prepare an SQL statement.
       */
      static boost::shared_ptr<statement>
      prepare(db &parent,
	      const std::string &sql);

      /** \brief Prepare an SQL statement.
       */
      static boost::shared_ptr<statement>
      prepare(db &parent,
	      const char *sql);

      /** \brief Return to the beginning of the statement's result set
       *  and discard parameter bindings.
       */
      void reset();

      /** \brief Step to the next result row of the statement.
       *
       *  Mirroring the underlying sqlite behavior, there is no
       *  "result" object -- meaning that if multiple threads might
       *  retrieve results from the same statement, they need to lock
       *  each other out.
       *
       *  \return \b true if a new row of results was retrieved, \b
       *  false otherwise.
       */
      bool step();

      /** \brief Execute a statement and discard its results.
       *
       *  This is equivalent to invoking step() until it returns \b
       *  false.  Useful for, e.g., side-effecting statements.
       */
      void exec()
      {
	while(step())
	  ; // Do nothing.
      }


      /** \brief Retrieve the value stored in a column as a BLOB.
       *
       *  The data block might be invalidated by any other method
       *  invoked on this statement.
       *
       *  \param column  The zero-based index of the column that is to be
       *                 retrieved.
       *  \param bytes   A location in which to store the size of the
       *                 result in bytes.
       *  \return A pointer to the block of data stored in the
       *  given column.
       */
      const void *get_blob(int column, int &bytes);

      /** \brief Retrieve the value stored in a column as a double.
       *
       *  \param column The zero-based index of the column that is to be
       *                retrieved.
       */
      double get_double(int column);

      /** \brief Retrieve the value stored in a column as an integer.
       *
       *  \param column The zero-based index of the column that is to be
       *                retrieved.
       */
      int get_int(int column);

      /** \brief Retrieve the value stored in a column as a 64-bit integer.
       *
       *  \param column The zero-based index of the column that is to be
       *                retrieved.
       */
      sqlite3_int64 get_int64(int column);

      /** \brief Retrieve the value stored in a column as a string. */
      std::string get_string(int column);
    };

    class blob
    {
      db &parent;
      sqlite3_blob *handle;

      friend class db;

      blob(db &_parent, sqlite3_blob *_handle);

    public:
      ~blob();
    };
  }
}

#endif
