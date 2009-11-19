// temp.cc
//
//   Copyright (C) 2005, 2007, 2009 Daniel Burrows
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

#include "temp.h"

#include "util.h"

#include <aptitude.h>
#include <loggers.h>

#include <stdlib.h>
#include <string.h>

#include <apt-pkg/error.h>

#include <boost/format.hpp>
#include <boost/scoped_array.hpp>

using aptitude::Loggers;
namespace cw = cwidget;

namespace temp
{
  std::string TemporaryCreationFailure::errmsg() const
  {
    return msg;
  }

  // The name of the root temporary directory, or an empty string if
  // the system is shut down.
  namespace
  {
    std::string temp_base;
    bool created_atexit_handler = false;
    cw::threads::mutex temp_state_mutex;
  }

  void initialize(const std::string &initial_prefix)
  {
    cw::threads::mutex::lock l(temp_state_mutex);

    if(!temp_base.empty())
      {
	LOG_WARN(Loggers::getAptitudeTemp(),
		 "Ignoring the second attempt to initialize the temporary file module.");
	return;
      }

    LOG_TRACE(Loggers::getAptitudeTemp(),
	      "Initializing the temporary file module.");

    if(initial_prefix.size() > 0 && initial_prefix[0] == '/')
      {
	LOG_ERROR(Loggers::getAptitudeTemp(),
		  "Invalid attempt to create a rooted temporary directory.");
	return;
      }

    std::string prefix(initial_prefix);

    const char *tmpdir = getenv("TMPDIR");

    if(tmpdir == NULL)
      tmpdir = getenv("TMP");

    if(tmpdir == NULL)
      tmpdir = "/tmp";

    // User name and process information is encoded into the temporary
    // directory's name as a convenience for users, so they can
    // identify which aptitude created a given directory.  It's not
    // for security; that comes from mkdtemp().
    prefix = ( boost::format("%s/%s-%s.%d:")
	       % std::string(tmpdir)
	       % prefix.c_str()
	       % get_username()
	       % getpid() ).str();


    size_t bufsize = prefix.size() + 6 + 1;
    boost::scoped_array<char> tmpl(new char[bufsize]);
    strcpy(tmpl.get(), prefix.c_str());
    strcat(tmpl.get(), "XXXXXX");


    if(mkdtemp(tmpl.get()) == NULL)
      {
	int errnum = errno;
	std::string err = sstrerror(errnum);
	LOG_ERROR(Loggers::getAptitudeTemp(),
		  boost::format(_("Unable to create temporary directory from template \"%s\": %s"))
		  % tmpl % err);

	return;
      }

    if(!created_atexit_handler)
      {
	LOG_TRACE(Loggers::getAptitudeTemp(),
		  "Adding an atexit handler for the temporary directory shut-down routine.");
	atexit(&temp::shutdown);
	created_atexit_handler = true;
      }

    temp_base.assign(tmpl.get());
    LOG_INFO(Loggers::getAptitudeTemp(),
	     "Initialized the temporary file module using the base directory "
	     << temp_base);
  }

  void shutdown()
  {
    cw::threads::mutex::lock l(temp_state_mutex);

    if(temp_base.empty())
      {
	LOG_TRACE(Loggers::getAptitudeTemp(),
		  "Ignoring a second attempt to shut down the temporary directory module.");
	return;
      }

    LOG_TRACE(Loggers::getAptitudeTemp(),
	      "Recursively deleting the base temporary directory "
	      << temp_base);

    if(!aptitude::util::recursive_remdir(temp_base))
      LOG_WARN(Loggers::getAptitudeTemp(),
	       "Failed to recursively remove the temporary directory.");

    LOG_INFO(Loggers::getAptitudeTemp(),
	     "Shut down the temporary file module and deleted the directory "
	     << temp_base);
    temp_base.clear();
  }



  void dir::impl::init_dir(const std::string &initial_prefix)
  {
    LOG_TRACE(Loggers::getAptitudeTemp(),
	      "Creating a temporary directory with prefix " << initial_prefix);

    std::string prefix(initial_prefix);

    if(prefix.size() > 0 && prefix[0] == '/')
      throw TemporaryCreationFailure("Invalid attempt to create an absolutely named temporary directory.");


    size_t bufsize = prefix.size() + 6 + 1;
    boost::scoped_array<char> tmpl(new char[bufsize]);
    strcpy(tmpl.get(), prefix.c_str());
    strcat(tmpl.get(), "XXXXXX");


    if(mkdtemp(tmpl.get()) == NULL)
      {
	int errnum = errno;
	std::string err = sstrerror(errnum);

	LOG_FATAL(Loggers::getAptitudeTemp(),
		  "Unable to create temporary directory from template \""
		  << tmpl.get() << "\": " << err);

	std::string errmsg = ssprintf(_("Unable to create temporary directory from template \"%s\": %s"),
				      tmpl.get(), err.c_str());

	throw TemporaryCreationFailure(errmsg);
      }

    dirname.assign(tmpl.get());

    LOG_INFO(Loggers::getAptitudeTemp(),
	     "Temporary directory created in " << dirname);
  }

  dir::impl::impl(const std::string &prefix)
    : refcount(1)
  {
    init_dir(prefix);
  }

  dir::impl::~impl()
  {
    aptitude::util::recursive_remdir(dirname);
  }


  name::impl::impl(const std::string &_filename)
    : refcount(1)
  {
    std::string parentdir;

    LOG_TRACE(Loggers::getAptitudeTemp(),
	      "Creating temporary file name with base name " << _filename);

    {
      cw::threads::mutex::lock l(temp_state_mutex);
      if(temp_base.empty())
	{
	  const char * const msg = "Can't create a temporary filename: the temporary filename system hasn't been initialized yet.";
	  LOG_FATAL(Loggers::getAptitudeTemp(), msg);
	  throw TemporaryCreationFailure(msg);
	}
      else
	parentdir = temp_base;
    }

    // Warn early about bad filenames.
    if(_filename.find('/') != _filename.npos)
      {
	std::string msg = ssprintf("Invalid temporary filename (contains directory separator): \"%s\"",
				   _filename.c_str());
	LOG_FATAL(Loggers::getAptitudeTemp(), msg);
	throw TemporaryCreationFailure(msg);
      }

    size_t parentsize = parentdir.size();
    size_t filenamesize = _filename.size();
    size_t bufsize = parentsize + 1 + filenamesize + 6 + 1;
    boost::scoped_array<char> tmpl(new char[bufsize]);

    strncpy(tmpl.get(), parentdir.c_str(), bufsize);
    strncat(tmpl.get(), "/", bufsize - parentsize);
    strncat(tmpl.get(), _filename.c_str(), bufsize - parentsize - 1);
    strncat(tmpl.get(), "XXXXXX", bufsize - parentsize - 1 - filenamesize);

    errno = 0;

    // This use of mktemp is safe under the assumption that 'dir' is
    // safe (because it was created using mkdtemp, which unlike mktemp
    // is safe) and that the user running the program didn't screw with
    // the permissions of the temporary directory.
    if(mktemp(tmpl.get()) == NULL)
      {
	std::string err = sstrerror(errno);

	LOG_FATAL(Loggers::getAptitudeTemp(),
		  "Unable to create temporary filename from template \"" << tmpl.get()
		  << "\": " << err);

	throw TemporaryCreationFailure(ssprintf(_("Unable to create temporary filename from template \"%s\": %s"),
						tmpl.get(),
						err.c_str()));
      }

    if(tmpl[0] == '\0')
      {
	std::string err;
	if(errno == 0)
	  err = _("Unknown error");
	else
	  err = sstrerror(errno);

	LOG_FATAL(Loggers::getAptitudeTemp(),
		  "Unable to create temporary filename from template \"" << tmpl.get()
		  << "\": " << err);

	throw TemporaryCreationFailure(ssprintf(_("Unable to create temporary filename from template \"%s\": %s"),
						tmpl.get(),
						err.c_str()));
      }

    filename.assign(tmpl.get());
  }

  // We don't know if it's a filename or a directory, so try blowing
  // both away (ignoring errors).
  name::impl::~impl()
  {
    rmdir(filename.c_str());
    unlink(filename.c_str());
  }
}

