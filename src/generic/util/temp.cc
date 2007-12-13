// temp.cc
//
//   Copyright (C) 2005, 2007 Daniel Burrows
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

#include <stdlib.h>

#include <apt-pkg/error.h>

namespace temp
{
  std::string TemporaryCreationFailure::errmsg() const
  {
    return msg;
  }

  void dir::impl::init_dir(const std::string &_prefix)
  {
    // Need to modify it
    std::string prefix(_prefix);

    if(prefix.size() > 0 && prefix[0] != '/')
      {
	const char *tmpdir = getenv("TMPDIR");

	if(tmpdir == NULL)
	  tmpdir = getenv("TMP");

	if(tmpdir == NULL)
	  tmpdir = "/tmp";

	prefix = std::string(tmpdir) + "/" + prefix.c_str();
      }


    size_t bufsize = prefix.size() + 6 + 1;
    char *tmpl = new char[bufsize];
    strcpy(tmpl, prefix.c_str());
    strcat(tmpl, "XXXXXX");


    if(mkdtemp(tmpl) == NULL)
      {
	std::string err = sstrerror(errno);
	std::string errmsg = ssprintf(_("Unable to create temporary directory from template \"%s\": %s"),
				      tmpl, err.c_str());

	delete[] tmpl;

	throw TemporaryCreationFailure(errmsg);
      }

    dirname.assign(tmpl);
    delete[] tmpl;
  }

  dir::impl::impl(const std::string &prefix, bool _forceful_delete)
    : refcount(1), forceful_delete(_forceful_delete)
  {
    init_dir(prefix);
  }

  dir::impl::impl(const std::string &prefix, const dir &_parent, bool _forceful_delete)
    : parent(_parent), refcount(1), forceful_delete(_forceful_delete)
  {
    if(prefix.size() > 0 && prefix[0] == '/')
      throw TemporaryCreationFailure("Invalid attempt to create an absolute rooted temporary directory.");

    init_dir(parent.get_name() + '/' + prefix);
  }

  dir::impl::~impl()
  {
    if(forceful_delete)
      aptitude::util::recursive_remdir(dirname);
    else
      {
	if(rmdir(dirname.c_str()) != 0)
	  _error->Errno("rmdir", _("Unable to delete temporary directory \"%s\""), dirname.c_str());
      }
  }


  name::impl::impl(const dir &_parent, const std::string &_filename)
    : parent(_parent), refcount(1)
  {
    // Warn early about bad filenames.
    if(_filename.find('/') != _filename.npos)
      throw TemporaryCreationFailure(ssprintf("Invalid temporary filename (contains directory separator): \"%s\"",
					      _filename.c_str()));

    if(!_parent.valid())
      throw TemporaryCreationFailure("NULL parent directory passed to temp::name constructor");

    size_t parentsize = parent.get_name().size();
    size_t filenamesize = _filename.size();
    size_t bufsize = parentsize + 1 + filenamesize + 6 + 1;
    char *tmpl = new char[bufsize];

    strncpy(tmpl, parent.get_name().c_str(), bufsize);
    strncat(tmpl, "/", bufsize - parentsize);
    strncat(tmpl, _filename.c_str(), bufsize - parentsize - 1);
    strncat(tmpl, "XXXXXX", bufsize - parentsize - 1 - filenamesize);

    errno = 0;

    // This use of mktemp is safe under the assumption that 'dir' is
    // safe (because it was created using mkdtemp, which unlike mktemp
    // is safe) and that the user running the program didn't screw with
    // the permissions of the temporary directory.
    if(mktemp(tmpl) == NULL)
      {
	std::string err = sstrerror(errno);

	delete[] tmpl;
	throw TemporaryCreationFailure(ssprintf(_("Unable to create temporary directory from template \"%s\": %s"),
						(_parent.get_name() + "/" + _filename).c_str(),
						err.c_str()));
      }

    if(tmpl[0] == '\0')
      {
	std::string err;
	if(errno == 0)
	  err = _("Unknown error");
	else
	  err = sstrerror(errno);

	delete[] tmpl;
	throw TemporaryCreationFailure(ssprintf(_("Unable to create temporary directory from template \"%s\": %s"),
						(_parent.get_name() + "/" + _filename).c_str(),
						err.c_str()));
      }

    filename.assign(tmpl);
    delete[] tmpl;
  }

  // We don't know if it's a filename or a directory, so try blowing
  // both away (ignoring errors).
  name::impl::~impl()
  {
    rmdir(filename.c_str());
    unlink(filename.c_str());
  }
}

