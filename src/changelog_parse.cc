// changelog_parse.cc
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
// At the moment this code uses parsechangelog to convert changelogs
// into something easier to read.

#include "changelog_parse.h"

#include "desc_parse.h"

#include <apt-pkg/fileutl.h>
#include <apt-pkg/pkgsystem.h>
#include <apt-pkg/tagfile.h>
#include <apt-pkg/version.h>

#include <generic/util/temp.h>
#include <generic/util/util.h>

#include <vscreen/fragment.h>
#include <vscreen/transcode.h>

static
fragment *change_text_fragment(const std::string &s)
{
  std::vector<fragment *> lines;

  std::string::size_type start = 0;
  std::string::size_type next_nl;

  do
    {
      next_nl = s.find('\n', start);

      if(s[start] == ' ')
	++start;

      if(next_nl == start + 1 && s[start] == '.')
	{
	  lines.push_back(newline_fragment());
	  start = next_nl + 1;
	  continue;
	}

      std::string this_line;
      if(next_nl != std::string::npos)
	this_line.assign(s, start, next_nl - start);
      else
	this_line.assign(s, start, std::string::npos);

      size_t first_nonspace = 0;
      while(first_nonspace < this_line.size() && isspace(this_line[first_nonspace]))
	++first_nonspace;

      bool has_bullet = false;
      if(first_nonspace < this_line.size())
	switch(this_line[first_nonspace])
	  {
	  case '*':
	  case '+':
	  case '-':
	    has_bullet = true;
	    break;
	  }

      if(has_bullet)
	lines.push_back(hardwrapbox(fragf("%s%F%s%n",
					  std::string(this_line, 0, first_nonspace).c_str(),
					  text_fragment(std::string(this_line, first_nonspace, 1).c_str(),
							get_style("Bullet")),
					  std::string(this_line, first_nonspace + 1).c_str())));
      else
	lines.push_back(hardwrapbox(fragf("%s%n", this_line.c_str())));

      start = next_nl + 1;
    } while(next_nl != std::string::npos);

  return sequence_fragment(lines);
}

static
fragment *parse_predigested_changelog(const temp::name &digest,
				      const std::string &curver)
{
  FileFd digestfd(digest.get_name(), FileFd::ReadOnly);

  if(!digestfd.IsOpen())
    return NULL;
  else
    {
      pkgTagFile tagfile(&digestfd);

      pkgTagSection sec;

      std::vector<fragment *> fragments;

      bool first = true;

      while(tagfile.Step(sec))
	{
	  std::string version(sec.FindS("Version"));
	  std::string changes(sec.FindS("Changes"));
	  std::string maintainer(sec.FindS("Maintainer"));
	  std::string date(sec.FindS("Date"));

	  fragment *f = fragf(first ? "%F%F" : "%n%F%F",
			      change_text_fragment(changes),
			      hardwrapbox(fragf("%n -- %s  %s",
						maintainer.c_str(),
						date.c_str())));

	  first = false;

	  if(!curver.empty() && _system->VS->CmpVersion(version, curver) > 0)
	    {
	      style s = get_style("ChangelogNewerVersion");
	      fragments.push_back(style_fragment(f, s));
	    }
	  else
	    fragments.push_back(f);
	}

      return sequence_fragment(fragments);
    }
}

static
temp::name digest_changelog(const temp::name &changelog)
{
  temp::name rval(changelog.get_parent(), "parsedchangelog");

  if(system(ssprintf("parsechangelog --all --format rfc822 -l %s > %s 2> /dev/null",
		     changelog.get_name().c_str(),
		     rval.get_name().c_str()).c_str()) == 0)
    return rval;
  else
    return temp::name();
}

fragment *make_changelog_fragment(const temp::name &file,
				  const std::string &curver)
{
  temp::name digested = digest_changelog(file);

  if(!digested.valid())
    return NULL;
  else
    return parse_predigested_changelog(digested, curver);
}
