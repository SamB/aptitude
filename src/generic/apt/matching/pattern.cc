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

    bool regex::exec(const char *s, regmatch_t *matches, size_t num_matches,
		     int eflags) const
    {
      return 0 == regexec(&r, s, num_matches, matches, eflags);
    }

    cwidget::util::ref_ptr<pattern>
    pattern::make_action(const std::string &s)
    {
      std::string s_lower(s);
      for(std::string::iterator it = s_lower.begin();
	  it != s_lower.end(); ++it)
	*it = tolower(*it);

      // Match packages to be installed
      if(!strcmp(s_lower.c_str(), "install"))
	return new pattern(action, action_install, s_lower);

      // Match packages to be upgraded
      else if(!strcmp(s_lower.c_str(), "upgrade"))
	return new pattern(action, action_upgrade, s_lower);

      else if(!strcmp(s_lower.c_str(), "downgrade"))
	return new pattern(action, action_downgrade, s_lower);

      // Match packages to be removed OR purged
      else if(!strcmp(s_lower.c_str(), "remove"))
	return new pattern(action, action_remove, s_lower);

      // Match packages to be purged
      else if(!strcmp(s_lower.c_str(), "purge"))
	return new pattern(action, action_purge, s_lower);

      // Match packages to be reinstalled
      else if(!strcmp(s_lower.c_str(), "reinstall"))
	return new pattern(action, action_reinstall, s_lower);

      // Match held packages
      else if(!strcmp(s_lower.c_str(), "hold"))
	return new pattern(action, action_hold, s_lower);
      else if(!strcmp(s_lower.c_str(), "keep"))
	return new pattern(action, action_keep, s_lower);

      else
	throw MatchingException(ssprintf(_("Unknown action type: %s"),
					 s.c_str()));
    }

    cwidget::util::ref_ptr<pattern>
    pattern::make_priority(const string &priority_str)
    {
      const char *s = priority_str.c_str();

      if(strcasecmp(s, "important") == 0 ||
	 (apt_cache_file &&
	  strcasecmp(s, (*apt_cache_file)->GetCache().Priority(pkgCache::State::Important)) == 0))
	return new pattern(priority, pkgCache::State::Important, s);
      else if(strcasecmp(s, "required") == 0 ||
	      (apt_cache_file &&
	       strcasecmp(s, (*apt_cache_file)->GetCache().Priority(pkgCache::State::Required)) == 0))
	return new pattern(priority, pkgCache::State::Required, s);
      else if(strcasecmp(s, "standard") == 0 ||
	      (apt_cache_file &&
	       strcasecmp(s, (*apt_cache_file)->GetCache().Priority(pkgCache::State::Standard)) == 0))
	return new pattern(priority, pkgCache::State::Standard, s);
      else if(strcasecmp(s, "optional") == 0 ||
	      (apt_cache_file &&
	       strcasecmp(s, (*apt_cache_file)->GetCache().Priority(pkgCache::State::Optional)) == 0))
	return new pattern(priority, pkgCache::State::Optional, s);
      else if(strcasecmp(s, "extra") == 0 ||
	      (apt_cache_file &&
	       strcasecmp(s, (*apt_cache_file)->GetCache().Priority(pkgCache::State::Extra)) == 0))
	return new pattern(priority, pkgCache::State::Extra, s);
      else
	throw MatchingException(ssprintf(_("Unknown priority %s"),
					 s));
    }
  }
}
