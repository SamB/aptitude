// changelog_parse.cc
//
//   Copyright (C) 2005, 2008 Daniel Burrows
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
// At the moment this code uses parsechangelog to convert changelogs
// into something easier to read.

#include "changelog_parse.h"

#include "desc_render.h"

#include <apt-pkg/fileutl.h>
#include <apt-pkg/tagfile.h>
#include <apt-pkg/strutl.h>

#include <stdlib.h>

#include <generic/util/temp.h>

#include <cwidget/fragment.h>
#include <cwidget/generic/util/ssprintf.h>
#include <cwidget/generic/util/transcode.h>

namespace cw = cwidget;

namespace aptitude
{
  namespace apt
  {
    changelog_entry::changelog_entry(const std::string &_source,
				     const std::string &_version,
				     const std::string &_distribution,
				     const std::string &_urgency,
				     const std::string &_changes,
				     const std::string &_maintainer,
				     const std::string &_date)
      : source(_source),
	version(_version),
	distribution(_distribution),
	urgency(_urgency),
	changes(_changes),
	maintainer(_maintainer),
	date_str(_date),
	could_parse_date(false),
	date(0)
    {
      // I use StrToTime instead of strptime because strptime is
      // locale-dependent.
      could_parse_date = StrToTime(date_str, date);
    }

    changelog::changelog(FileFd &digest)
    {
      if(digest.IsOpen())
	{
	  pkgTagFile tagfile(&digest);

	  pkgTagSection sec;

	  std::vector<cw::fragment *> fragments;

	  while(tagfile.Step(sec))
	    {
	      std::string source(sec.FindS("Source"));
	      std::string version(sec.FindS("Version"));
	      std::string distribution(sec.FindS("Distribution"));
	      std::string urgency(sec.FindS("Urgency"));
	      std::string changes(sec.FindS("Changes"));
	      std::string maintainer(sec.FindS("Maintainer"));
	      std::string date(sec.FindS("Date"));

	      entries.push_back(changelog_entry(source,
						version,
						distribution,
						urgency,
						changes,
						maintainer,
						date));
	    }
	}
    }

    namespace
    {
      temp::name digest_changelog(const temp::name &changelog)
      {
	temp::name rval(changelog.get_parent(), "parsedchangelog");
    
	if(system(cw::util::ssprintf("/usr/bin/parsechangelog --all --format rfc822 -l %s > %s 2> /dev/null",
				     changelog.get_name().c_str(),
				     rval.get_name().c_str()).c_str()) == 0)
	  return rval;
	else
	  return temp::name();
      }
    }

    cw::util::ref_ptr<changelog> parse_changelog(const temp::name &file)
    {
      temp::name digested = digest_changelog(file);

      if(!digested.valid())
	return NULL;
      else
	{
	  FileFd digestedfd(digested.get_name(),
			    FileFd::ReadOnly);
	  if(!digestedfd.IsOpen())
	    return NULL;
	  else
	    return changelog::create(digestedfd);
	}
    }
  }
}
