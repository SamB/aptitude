// changelog_parse.cc
//
//   Copyright (C) 2005, 2008-2009 Daniel Burrows
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

#include <generic/util/util.h>

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
				     const cw::util::ref_ptr<changelog_element_list> &_elements,
				     const std::string &_maintainer,
				     const std::string &_date)
      : source(_source),
	version(_version),
	distribution(_distribution),
	urgency(_urgency),
	changes(_changes),
	elements(_elements),
	maintainer(_maintainer),
	date_str(_date),
	could_parse_date(false),
	date(0)
    {
      // I use StrToTime instead of strptime because strptime is
      // locale-dependent.
      could_parse_date = StrToTime(date_str, date);
    }

    // Skips over whitespace in a text element, spilling elements as
    // needed to handle newline conditions.
    void skip_text_whitespace(const std::string &s,
			      std::string::size_type &start,
			      std::string::size_type &curr,
			      std::vector<changelog_element> &elements)
    {
      while(curr < s.size() && isspace(s[curr]))
	{
	  if(s[curr] != '\n')
	    ++curr;
	  else
	    {
	      ++curr;

	      elements.push_back(changelog_element(changelog_element::text_type,
						   start, curr));

	      if(curr < s.size() && s[curr] == ' ')
		++curr;

	      if(curr < s.size() && s[curr] == '.' &&

		 curr + 1 < s.size() && s[curr + 1] == '\n')
		++curr;

	      start = curr;
	    }
	}
    }

    cw::util::ref_ptr<changelog_element_list>
    parse_changes(const std::string &s)
    {
      std::vector<changelog_element> elements;

      std::string::size_type curr = 0;

      // The first line is generated mechanically before we get here, so skip it.
      {
	std::string::size_type first_nl = s.find('\n', 0);
	if(first_nl != std::string::npos)
	  curr = first_nl + 1;
	else
	  curr = s.size();
      }

      // Manually handle the start-of-line special casing here.
      if(curr < s.size() && s[curr] == ' ')
	++curr;
      if(curr < s.size() && s[curr] == '.' &&
	 curr + 1 < s.size() && s[curr + 1] == '\n')
	++curr;

      std::string::size_type start = curr;

      while(curr < s.size())
	{
	  const char c = s[curr];
	  switch(c)
	    {
	    case '\n':
	      {
		// Skipping whitespace might skip past several blank
		// lines, but that's OK (they don't contain bullets,
		// obviously).
		skip_text_whitespace(s, start, curr, elements);

		if(curr < s.size())
		  {
		    switch(s[curr])
		      {
		      case '*':
		      case '+':
		      case '-':
			if(curr != start)
			  elements.push_back(changelog_element(changelog_element::text_type,
							       start, curr));

			elements.push_back(changelog_element(changelog_element::bullet_type,
							     curr, curr + 1));

			++curr;
			start = curr;
			break;
		      }
		  }
	      }

	    case 'c':
	    case 'C':
	      {
		// Look for "closes:".
		++curr;
		if(!(curr < s.size() && (s[curr] == 'l' || s[curr] == 'L')))
		  break;

		++curr;
		if(!(curr < s.size() && (s[curr] == 'o' || s[curr] == 'O')))
		  break;

		++curr;
		if(!(curr < s.size() && (s[curr] == 's' || s[curr] == 'S')))
		  break;

		++curr;
		if(!(curr < s.size() && (s[curr] == 'e' || s[curr] == 'E')))
		  break;

		++curr;
		if(!(curr < s.size() && (s[curr] == 's' || s[curr] == 'S')))
		  break;

		++curr;
		if(!(curr < s.size() && s[curr] == ':'))
		  break;

		++curr;

		// Glom onto all the bug numbers we can find.
		bool done = false;
		while(!done)
		  {
		    skip_text_whitespace(s, start, curr, elements);

		    if(curr < s.size() && (s[curr] == 'b' || s[curr] == 'B'))
		      {
			++curr;
			if(!(curr < s.size() && (s[curr] == 'u' || s[curr] == 'U')))
			  break;

			++curr;
			if(!(curr < s.size() && (s[curr] == 'g' || s[curr] == 'G')))
			  break;

			++curr;
			skip_text_whitespace(s, start, curr, elements);
		      }

		    if(curr < s.size() && s[curr] == '#')
		      {
			++curr;

			skip_text_whitespace(s, start, curr, elements);
		      }

		    if(curr < s.size() && isdigit(s[curr]))
		      {
			// We have a digit for sure.  Spit out the
			// preceding text...
			elements.push_back(changelog_element(changelog_element::text_type,
							     start, curr));
			// Update the start pointer...
			start = curr;

			// Find the whole number...
			while(curr < s.size() && isdigit(s[curr]))
			  ++curr;

			// Put it onto the element list...
			elements.push_back(changelog_element(changelog_element::closes_type,
							     start, curr));

			// Move the start pointer past the number....
			start = curr;

			// Now see if there's a continuation (i.e., a
			// comma).  If there is we look for another
			// bug number; otherwise we stop.
			skip_text_whitespace(s, start, curr, elements);

			if(curr < s.size() && s[curr] == ',')
			  ++curr;
			else
			  done = true;
		      }
		    else // If there wasn't a digit, this isn't part of a Closes.
		      done = true;
		  }
	      }
	      break;

	    default:
	      ++curr;
	      break;
	    }
	}

      if(curr != start)
	elements.push_back(changelog_element(changelog_element::text_type,
					     start, curr));

      return changelog_element_list::create(elements);
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

	      cw::util::ref_ptr<changelog_element_list> changelog_elements =
		parse_changes(changes);

	      entries.push_back(changelog_entry::create(source,
							version,
							distribution,
							urgency,
							changes,
							changelog_elements,
							maintainer,
							date));
	    }
	}
    }

    namespace
    {
      temp::name digest_changelog(const temp::name &changelog,
				  const std::string &from)
      {
	temp::name rval(changelog.get_parent(), "parsedchangelog");

	std::string version_fragment;
	if(from.empty())
	  version_fragment = "--all";
	else
	  {
	    version_fragment = "-f";
	    // Note that escaping the version is *critical*, because
	    // it is untrusted data.
	    version_fragment += backslash_escape_nonalnum(from);
	  }

	std::string cmd =
	  cw::util::ssprintf("/usr/bin/dpkg-parsechangelog --format rfc822 %s -l%s > %s 2> /dev/null",
			     version_fragment.c_str(),
			     changelog.get_name().c_str(),
			     rval.get_name().c_str());

	if(system(cmd.c_str()) == 0)
	  return rval;
	else
	  return temp::name();
      }
    }

    cw::util::ref_ptr<changelog> parse_changelog(const temp::name &file,
						 const std::string &from)
    {
      temp::name digested = digest_changelog(file, from);

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
