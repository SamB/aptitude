// pattern.cc
//
//   Copyright (C) 2008 Daniel Burrows
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

/// \file pattern.cc

#include "pattern.h"

#include <cwidget/generic/util/ssprintf.h>

#include <aptitude.h>

using cwidget::util::ssprintf;

namespace aptitude
{
  namespace matching
  {
    MatchingException::MatchingException(const std::string &_msg)
      : msg(_msg)
    {
    }

    std::string MatchingException::errmsg() const
    {
      return msg;
    }

    regex::regex(const std::string &pattern, int cflags)
    {
      int err = regcomp(&r, pattern.c_str(), cflags);
      if(err != 0)
	{
	  size_t needed = regerror(err, &r, NULL, 0);

	  char *buf = new char[needed+1];

	  regerror(err, &r, buf, needed+1);

	  std::string msg(ssprintf("Regex compilation error: %s", buf));

	  delete[] buf;

	  throw MatchingException(msg);
	}
    }

    regex::~regex()
    {
      regfree(&r);
    }

    bool regex::exec(const char *s, regmatch_t *matches, size_t num_matches,
		     int eflags) const
    {
      return 0 == regexec(&r, s, num_matches, matches, eflags);
    }

    cwidget::util::ref_ptr<pattern>
    pattern::make_action(const action_type act)
    {
      return new pattern(action, act);
    }
  }
}
