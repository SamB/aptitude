/** \file aptitude_resolver_cost_syntax.cc */


// Copyright (C) 2010 Daniel Burrows

// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of the
// License, or (at your option) any later version.

// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program; see the file COPYING.  If not, write to
// the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

#include "aptitude_resolver_cost_syntax.h"

#include <ostream>

namespace
{
  class ResolverCostParseException : public cwidget::util::Exception
  {
    std::string msg;

  public:
    ResolverCostParseException(const std::string &_msg)
      : msg(_msg)
    {
    }

    std::string errmsg() const { return msg; }
  };
}

boost::shared_ptr<std::vector<cost_component_structure> >
parse_cost_settings(const std::string &settings)
{
  throw ResolverCostParseException("Cost settings parsing: not implemented.");
}


void dump_settings(std::ostream &out, const boost::shared_ptr<std::vector<cost_component_structure> > &settings)
{
  bool first_component = true;
  for(std::vector<cost_component_structure>::const_iterator
        settings_it = settings->begin(); settings_it != settings->end(); ++settings_it)
    {
      const std::vector<cost_component_structure::entry> &entries =
        *settings_it->get_entries();

      if(entries.empty())
        continue;

      if(first_component)
        first_component = false;
      else
        out << ", ";

      if(settings_it->get_combining_op() == cost_component_structure::combine_max)
        out << "max(";

      bool first_entry = true;
      for(std::vector<cost_component_structure::entry>::const_iterator
            entries_it = entries.begin(); entries_it != entries.end();
          ++entries_it)
        {
          if(entries_it->get_scaling_factor() == 0)
            continue;

          if(first_entry)
            first_entry = false;
          else switch(settings_it->get_combining_op())
                 {
                 case cost_component_structure::combine_add:
                   out << " + ";
                   break;

                 case cost_component_structure::combine_max:
                   out << ", ";
                   break;

                 default:
                   // Should never happen.
                   out << " ";
                   break;
                 }

          if(entries_it->get_scaling_factor() != 1)
            out << entries_it->get_scaling_factor() << "*";
          out << entries_it->get_name();
        }

      if(settings_it->get_combining_op() == cost_component_structure::combine_max)
        out << ")";
    }
}

