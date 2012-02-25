// load_sortpolicy.cc
//
//   Copyright (C) 2001, 2005 Daniel Burrows
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
//  Loads sorting policies -- similar to grouping policies.  (in fact, I did
// this with cut&paste, and think maybe I should consider trying to share
// code between the two parsers..)

#include "load_sortpolicy.h"

#include "aptitude.h"
#include "pkg_sortpolicy.h"

#include <apt-pkg/error.h>

#include <boost/algorithm/string.hpp>
#include <boost/tokenizer.hpp>

#include <string>
#include <vector>
#include <map>


using namespace std;

typedef pkg_sortpolicy* (*pkg_sortpolicy_parser)(pkg_sortpolicy* chain,
						 bool reversed);
typedef map<string, pkg_sortpolicy_parser> parser_map;

pkg_sortpolicy* parse_name_policy(pkg_sortpolicy* chain,
				  bool reversed)
{
  return pkg_sortpolicy_name(chain, reversed);
}

pkg_sortpolicy* parse_ver_policy(pkg_sortpolicy* chain,
				 bool reversed)
{
  return pkg_sortpolicy_ver(chain, reversed);
}

pkg_sortpolicy* parse_installsize_policy(pkg_sortpolicy* chain,
					 bool reversed)
{
  return pkg_sortpolicy_installed_size(chain, reversed);
}

pkg_sortpolicy* parse_priority_policy(pkg_sortpolicy* chain,
				      bool reversed)
{
  return pkg_sortpolicy_priority(chain, reversed);
}

static parser_map parse_types;

static void init_parse_types()
{
  if(parse_types.empty())
    {
      parse_types["name"] = parse_name_policy;
      parse_types["version"] = parse_ver_policy;
      parse_types["installsize"] = parse_installsize_policy;
      parse_types["priority"] = parse_priority_policy;
    }
}

struct parse_atom
{
  string name;
  bool reversed;

  parse_atom() : reversed(false) {}
  parse_atom(const string& name_, bool reversed_) : name(name_), reversed(reversed_) {}
};

pkg_sortpolicy* parse_sortpolicy(const string& s)
{
  init_parse_types();

  vector<parse_atom> parsed;

  // get the list of sorting policies
  typedef boost::tokenizer<boost::char_separator<char> > tokenizer;
  boost::char_separator<char> separator(",");
  tokenizer tok(s, separator);

  for (tokenizer::iterator it = tok.begin(); it != tok.end(); ++it) {
    bool reversed = (it->at(0) == '~');
    string name = (reversed ? it->substr(1) : *it);
    boost::algorithm::to_lower(name);
    parsed.push_back(parse_atom(name, reversed));
  }

  // Now run through the parsed stuff from back-to-front and instantiate it.
  pkg_sortpolicy* policy = NULL;
  for (vector<parse_atom>::reverse_iterator it = parsed.rbegin(); it != parsed.rend(); ++it)
    {
      // Look up the parse function, Die gracefully if it's bad.
      parser_map::iterator policyIt = parse_types.find(it->name);
      if(policyIt == parse_types.end())
	{
	  _error->Error(_("Invalid sorting policy type '%s'"),
			  it->name.c_str());
	  delete policy;
	  return NULL;
	}

      // create new policy (chaining previously parsed policies, if present)
      pkg_sortpolicy* newPolicy = policyIt->second(policy, it->reversed);
      if(!newPolicy)
	{
	  _error->Error(_("Could not create sorting policy type '%s'"),
			  it->name.c_str());
	  delete policy;
	  return NULL;
	}

      // use new policy as base policy for the next iteration
      policy = newPolicy;
    }

  return policy;
}
