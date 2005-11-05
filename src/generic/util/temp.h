// temp.h                                 -*-c++-*-
//
//   Copyright (C) 2005 Daniel Burrows
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
//
// Code to support safely creating files in a temporary directory and
// deleting the directory when finished.

#ifndef TEMP_H
#define TEMP_H

#include <string>

#include "eassert.h"
#include "exception.h"
#include "threads.h"

namespace temp
{
  /** An exception thrown when a temporary object cannot be created. */
  class TemporaryCreationFailure : public Exception
  {
    std::string msg;
  public:
    TemporaryCreationFailure(const std::string &_msg)
      : msg(_msg)
    {
    }

    std::string errmsg() const;
  };

  /** This object represents a directory in which temporary files can
   *  be created.  While you can extract the file name, it is
   *  recommended that you instead create temporary file(name)
   *  objects.
   */
  class dir
  {
    class impl;;

    impl *real_dir;
  public:
    /** Create a temporary directory object with no backing directory. */
    dir()
      : real_dir(NULL)
    {
    }

    /** Create a new temporary directory whose name begins with the
     *  text in prefix.
     *
     *  \throws TemporaryCreationFailure
     */
    dir(const std::string &prefix);

    /** Create a new temporary directory that is a subdirectory of an
     *  existing directory and whose name begins with prefix.
     *
     *  \throws TemporaryCreationFailure
     */
    dir(const std::string &prefix, const dir &parent);
    /** Create a new reference to an existing temporary directory. */
    dir(const dir &other);
    /** Copy a reference to an existing temporary directory. */
    dir &operator=(const dir &other);

    /** \return \b true if the directory is a valid reference. */
    bool valid() const;

    /** This method may be invoked only on valid directories.
     *
     *  \return the name of this directory.  It is recommended that
     *  you use this only for informational purposes; files should be
     *  created via (e.g.) temp::name.
     */
    std::string get_name() const;

    /** \return the parent of this directory, or an invalid reference
     *  if it has none.
     */
    dir get_parent() const;

    ~dir();
  };

  class dir::impl
  {
    /** The name of this directory. */
    std::string dirname;

    /** The parent of this directory, if any. */
    dir parent;

    threads::mutex m;

    int refcount;

    /** Set up a temporary directory with the given prefix.
     *
     *  Contains common code for the constructors.
     */
    void init_dir(const std::string &prefix);

  public:
    /** Create a new temporary directory whose name begins with the
     *  text in prefix.  For instance, passing "aptitude" might
     *  create "/tmp/aptitude45j2hs" or somesuch.
     *
     *  The initial refcount is 1.
     *
     *  \param prefix the prefix of this directory's name.  If
     *  prefix begins with a '/', then it is considered an absolute
     *  path; otherwise, it is considered a relative path within the
     *  system temporary directory.
     *
     *  \throws TemporaryCreationFailure
     */
    impl(const std::string &prefix);

    /** Create a new temporary directory within another temporary
     *  directory.
     *
     *  \param prefix the prefix of the name of this directory
     *                within the parent directory; must not begin
     *                with a '/'
     *  \param parent the parent directory to create this directory
     *                within
     *
     *  \throws TemporaryCreationFailure
     */
    impl(const std::string &prefix, const dir &parent);

    /** Attempt to remove the temporary directory. */
    ~impl();

    std::string get_name() const
    {
      return dirname;
    }

    /** \return the parent directory of this temporary directory, if
     *  any.  If none, an invalid reference is returned.
     */
    dir get_parent() const
    {
      return parent;
    }

    /** Increment the reference count of this impl. */
    void incref()
    {
      threads::mutex::lock l(m);
      ++refcount;
    }

    /** Decrement the reference count of this impl. */
    void decref()
    {
      threads::mutex::lock l(m);

      eassert(refcount > 0);
      --refcount;

      if(refcount == 0)
	delete this;
    }
  };

  inline dir::dir(const std::string &prefix)
    : real_dir(new impl(prefix))
  {
  }

  inline dir::dir(const std::string &prefix, const dir &parent)
    : real_dir(new impl(prefix, parent))
  {
  }

  inline dir::dir(const dir &other)
    : real_dir(other.real_dir)
  {
    if(real_dir != NULL)
      real_dir->incref();
  }

  inline dir &dir::operator=(const dir &other)
  {
    if(other.real_dir != NULL)
      other.real_dir->incref();

    if(real_dir != NULL)
      real_dir->decref();

    real_dir = other.real_dir;

    return *this;
  }

  inline bool dir::valid() const
  {
    return real_dir != NULL;
  }

  inline std::string dir::get_name() const
  {
    return real_dir->get_name();
  }

  inline dir dir::get_parent() const
  {
    return real_dir->get_parent();
  }

  inline dir::~dir()
  {
    if(real_dir != NULL)
      real_dir->decref();
  }

  /** A temporary name -- at the moment of its creation it is
   *  guaranteed to be unique, but it is up to you to ensure that it
   *  is created uniquely.
   */
  class name
  {
    class impl;

    impl *real_name;

  public:
    /** Create a new temporary filename in the given directory.
     *
     *  \param d the directory in which to create the filename
     *  \param prefix the prefix of the new filename
     *
     *  \throws TemporaryCreationFailure if no temporary name can be
     *  reserved.
     */
    name(const dir &d, const std::string &prefix);

    /** Create an empty temporary name. */
    name();

    /** Create a new reference to an existing name. */
    name(const name &other);

    ~name();

    /** Copy a reference to an existing temporary name. */
    name &operator=(const name &other);


    bool valid() const;
    std::string get_name() const;
    /** \return the enclosing directory of this temporary name; will
     *          never be an invalid reference unless this is invalid.
     */
    dir get_parent() const;
  };

  class name::impl
  {
    /** The name of this temporary object. */
    std::string filename;

    /** The directory in which this temporary should be created. */
    dir parent;

    /** The mutex of this name's reference count. */
    threads::mutex m;

    /** The reference count of this name. */
    int refcount;
  public:
    /** Create a new temporary filename in the given directory.
     *
     *  \param dir the temporary directory in which to create the file
     *  \param filename the prefix of the temporary filename
     *
     *  \throws TemporaryCreationFailure if no temporary name can be
     *  reserved.
     */
    impl(const dir &parent,
	 const std::string &filename);

    /** Remove the filename associated with this temporary. */
    ~impl();

    /** \return the temporary's name. */
    std::string get_name() const
    {
      return filename;
    }

    dir get_parent() const
    {
      return parent;
    }

    /** Increment the reference count of this impl. */
    void incref()
    {
      threads::mutex::lock l(m);
      ++refcount;
    }

    /** Decrement the reference count of this impl. */
    void decref()
    {
      threads::mutex::lock l(m);

      eassert(refcount > 0);
      --refcount;

      if(refcount == 0)
	delete this;
    }
  };

  inline name::name(const dir &d, const std::string &prefix)
    : real_name(new impl(d, prefix))
  {
  }

  inline name::name()
    : real_name(NULL)
  {
  }

  inline name::name(const name &other)
    : real_name(other.real_name)
  {
    if(real_name != NULL)
      real_name->incref();
  }

  inline name::~name()
  {
    if(real_name != NULL)
      real_name->decref();
  }

  inline name &name::operator=(const name &other)
  {
    if(other.real_name != NULL)
      other.real_name->incref();

    if(real_name != NULL)
      real_name->decref();

    real_name = other.real_name;

    return *this;
  }

  inline bool name::valid() const
  {
    return real_name != NULL;
  }

  inline std::string name::get_name() const
  {
    return real_name->get_name();
  }

  inline dir name::get_parent() const
  {
    if(real_name != NULL)
      return real_name->get_parent();
    else
      return dir();
  }
};

#endif // TEMP_H
