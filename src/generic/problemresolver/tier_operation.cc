/** \file tier_operation.cc */


// Copyright (C) 2010 Daniel Burrows
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; see the file COPYING.  If not, write to
// the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

#include "tier_operation.h"

#include <ostream>

tier_operation::op_impl::op_impl(const op_impl &op1, const op_impl &op2, combine_tag)
  : structural_level(std::max<int>(op1.structural_level,
				   op2.structural_level))
{
  // Straightforward merge (why doesn't STL have a merge that lets
  // you combine equivalent elements instead of just copying one
  // of them to the output?
  std::vector<std::pair<level_index, level> >::const_iterator
    it1 = op1.actions.begin(), it2 = op2.actions.begin();

  while(it1 != op1.actions.end() && it2 != op2.actions.end())
    {
      if(it1->first < it2->first)
	{
	  actions.push_back(*it1);
	  ++it1;
	}
      else if(it2->first < it1->first)
	{
	  actions.push_back(*it2);
	  ++it2;
	}
      else
	{
	  actions.push_back(std::make_pair(it1->first,
					   level::combine(it1->second, it2->second)));
	  ++it1;
	  ++it2;
	}
    }

  if(it1 != op1.actions.end())
    actions.insert(actions.end(), it1, op1.actions.end());
  else if(it2 != op2.actions.end())
    actions.insert(actions.end(), it2, op2.actions.end());
}

tier tier_operation::op_impl::apply(const tier &t) const
{
  int out_structural_level =
    std::max<int>(t.get_structural_level(), structural_level);
  std::vector<level> out_user_levels(t.user_levels_begin(), t.user_levels_end());

  if(!actions.empty())
    {
      // If the actions array will modify slots off the end of the
      // tier's list of levels, we first pre-extend that list.
      if(out_user_levels.size() <= actions.back().first)
	{
	  level_index
	    gap_size = actions.back().first + 1 - out_user_levels.size();

	  out_user_levels.insert(out_user_levels.end(),
				 gap_size,
				 level());
	}

      for(std::vector<std::pair<level_index, level> >::const_iterator it =
	    actions.begin(); it != actions.end(); ++it)
	{
	  level &target(out_user_levels[it->first]);
	  target = level::combine(target, it->second);
	}
    }

  return tier(out_structural_level,
	      out_user_levels.begin(),
	      out_user_levels.end());
}

void tier_operation::op_impl::dump(std::ostream &out) const
{
  out << "(";
  if(structural_level != INT_MIN)
    out << "advance: " << structural_level;
  else
    out << "nop";

  std::size_t column = 0;
  for(std::vector<std::pair<level_index, level> >::const_iterator
	it = actions.begin(); it != actions.end(); ++it)
    {
      out << ", ";

      while(column < it->first)
	{
	  out << "nop, ";
	  ++column;
	}

      switch(it->second.get_state())
	{
	case level::added:
	  out << "add: ";
	  break;
	case level::lower_bounded:
	  out << "advance: ";
	  break;
	case level::unmodified:
	  // Shouldn't happen:
	  out << "(unmodified): ";
	  break;
	}
      out << it->second.get_value();
      ++column;
    }

  out << ")";
}

tier_operation::op_impl::op_impl(const op_impl &op1, const op_impl &op2, upper_bound_tag)
  : structural_level(std::max<int>(op1.structural_level,
				   op2.structural_level))
{
  actions.reserve(std::max<std::size_t>(op1.actions.size(), op2.actions.size()));

  std::vector<std::pair<level_index, level> >::const_iterator
    it1 = op1.actions.begin(),
    it2 = op2.actions.begin();

  const std::vector<std::pair<level_index, level> >::const_iterator
    end1 = op1.actions.end(),
    end2 = op2.actions.end();

  while(it1 != end1 && it2 != end2)
    {
      const std::pair<level_index, level> &p1 = *it1, &p2 = *it2;

      if(p1.first < p2.first)
	{
	  actions.push_back(p1);
	  ++it1;
	}
      else if(p2.first < p1.first)
	{
	  actions.push_back(p2);
	  ++it2;
	}
      else
	{
	  actions.push_back(std::make_pair(p1.first,
					   level::upper_bound(p1.second, p2.second)));
	  ++it1;
	  ++it2;
	}
    }

  if(it1 != end1)
    actions.insert(actions.end(),
		   it1, end1);
  else if(it2 != end2)
    actions.insert(actions.end(),
		   it2, end2);
}

tier_operation::op_impl::op_impl(const op_impl &op1, const op_impl &op2, lower_bound_tag)
  : structural_level(std::min<int>(op1.structural_level,
				   op2.structural_level))
{
  std::vector<std::pair<level_index, level> >::const_iterator
    it1 = op1.actions.begin(),
    it2 = op2.actions.begin();

  const std::vector<std::pair<level_index, level> >::const_iterator
    end1 = op1.actions.end(),
    end2 = op2.actions.end();

  while(it1 != end1 && it2 != end2)
    {
      const std::pair<level_index, level> &p1 = *it1, &p2 = *it2;

      if(p1.first < p2.first)
	++it1;
      else if(p2.first < p1.first)
	++it2;
      else
	{
	  actions.push_back(std::make_pair(p1.first,
					   level::lower_bound(p1.second, p2.second)));
	  ++it1;
	  ++it2;
	}
    }
}

tier_operation tier_operation::least_upper_bound(const tier_operation &op1,
                                                 const tier_operation &op2)
{
  return tier_operation(op1, op2, upper_bound_tag());
}

tier_operation tier_operation::greatest_lower_bound(const tier_operation &op1,
                                                    const tier_operation &op2)
{
  return tier_operation(op1, op2, lower_bound_tag());
}

std::size_t hash_value(const tier_operation &op)
{
  return op.get_hash_value();
}

std::ostream &operator<<(std::ostream &out, const tier_operation &t)
{
  t.dump(out);

  return out;
}
