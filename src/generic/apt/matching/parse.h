// matchers.h  -*-c++-*-
//
//  Copyright 2000-2001, 2005, 2007-2008 Daniel Burrows
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; see the file COPYING.  If not, write to
//  the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//  Boston, MA 02111-1307, USA.

#ifndef MATCHERS_H
#define MATCHERS_H

#include <cwidget/generic/util/ref_ptr.h>

/** \brief Code to parse search patterns. */

namespace aptitude
{
  namespace matching
  {
    class pattern;


    cwidget::util::ref_ptr<pattern>
      parse(std::string::const_iterator &start,
	    const std::string::const_iterator &end,
	    const std::vector<const char *> &terminators,
	    bool search_descriptions,
	    bool flag_errors,
	    bool require_full_parse);

    inline cwidget::util::ref_ptr<pattern>
      parse(const std::string &s,
	    const std::vector<const char *> &terminators,
	    bool search_descriptions,
	    bool flag_errors,
	    bool require_full_parse)
    {
      std::string::const_iterator start = s.begin();
      return parse(start, s.end(),
		   terminators,
		   search_descriptions,
		   flag_errors,
		   require_full_parse);
    }

    inline cwidget::util::ref_ptr<pattern>
      parse(const std::string &s,
	    bool search_descriptions,
	    bool flag_errors,
	    bool require_full_parse)
    {
      return parse(s, std::vector<const char *>(),
		   search_descriptions,
		   flag_errors,
		   require_full_parse);
    }

    inline cwidget::util::ref_ptr<pattern>
      parse(const std::string &s)
    {
      return parse(s, false, true, true);
    }
  }
}

#endif
