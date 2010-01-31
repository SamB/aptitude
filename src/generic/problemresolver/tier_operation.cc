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

void tier_operation::normalize()
{
  if(add_levels.get_num_user_levels() > 0 &&
     add_levels.get_user_level(add_levels.get_num_user_levels() - 1) == 0)
    {
      tier::user_level_iterator
        begin = add_levels.user_levels_begin(),
        end = add_levels.user_levels_end();

      while(end > begin && end[-1] == 0)
        --end;

      add_levels = tier(add_levels.get_structural_level(), begin, end);
    }
}

tier tier_operation::levelwise_maximum(const tier &t1, const tier &t2)
{
  const int out_structural_level =
    std::max<int>(t1.get_structural_level(),
                  t2.get_structural_level());

  std::vector<int> out_user_levels;
  out_user_levels.reserve(std::max<std::size_t>(t1.get_num_user_levels(),
                                                t2.get_num_user_levels()));

  tier::user_level_iterator
    it1 = t1.user_levels_begin(),
    it2 = t2.user_levels_begin();

  const tier::user_level_iterator
    end1 = t1.user_levels_end(),
    end2 = t2.user_levels_end();

  while(it1 != end1 && it2 != end2)
    {
      out_user_levels.push_back(std::max<int>(*it1, *it2));
      ++it1;
      ++it2;
    }

  if(it1 != end1)
    out_user_levels.insert(out_user_levels.end(),
                           it1, end1);
  else if(it2 != end2)
    out_user_levels.insert(out_user_levels.end(),
                           it2, end2);

  return tier(out_structural_level,
              out_user_levels.begin(),
              out_user_levels.end());
}

tier tier_operation::levelwise_minimum(const tier &t1, const tier &t2)
{
  const int out_structural_level =
    std::min<int>(t1.get_structural_level(),
                  t2.get_structural_level());

  std::vector<int> out_user_levels;
  out_user_levels.reserve(std::min<std::size_t>(t1.get_num_user_levels(),
                                                t2.get_num_user_levels()));

  tier::user_level_iterator
    it1 = t1.user_levels_begin(),
    it2 = t2.user_levels_begin();

  const tier::user_level_iterator
    end1 = t1.user_levels_end(),
    end2 = t2.user_levels_end();

  while(it1 != end1 && it2 != end2)
    {
      out_user_levels.push_back(std::min<int>(*it1, *it2));
      ++it1;
      ++it2;
    }

  return tier(out_structural_level,
              out_user_levels.begin(),
              out_user_levels.end());
}

inline int tier_operation::safe_add_levels(int l1, int l2)
{
  if(l1 < 0 && l2 < 0)
    throw NegativeTierAdditionException();

  // Check for overflow.
  //
  // If one level is nonnegative, we can only overflow if both are
  // strictly positive.
  if(l1 > 0 && l2 > 0 &&
     l1 > (INT_MAX - l2))
    throw TierTooBigException();

  return l1 + l2;
}

tier tier_operation::levelwise_add(const tier &t1, const tier &t2)
{
  int out_structural_level =
    safe_add_levels(t1.get_structural_level(), t2.get_structural_level());

  std::vector<int> out_user_levels;

  tier::user_level_iterator
    it1 = t1.user_levels_begin(),
    it2 = t2.user_levels_begin();

  const tier::user_level_iterator
    end1 = t1.user_levels_end(),
    end2 = t2.user_levels_end();

  while(it1 != end1 && it2 != end2)
   {
     out_user_levels.push_back(safe_add_levels(*it1, *it2));
     ++it1;
     ++it2;
   }

  if(it1 != end1)
    out_user_levels.insert(out_user_levels.end(),
                           it1, end1);
  else if(it2 != end2)
    out_user_levels.insert(out_user_levels.end(),
                           it2, end2);

  return tier(out_structural_level,
              out_user_levels.begin(),
              out_user_levels.end());
}

tier_operation tier_operation::least_upper_bound(const tier_operation &op1,
                                                 const tier_operation &op2)
{
  return tier_operation(levelwise_maximum(op1.add_levels,
                                          op2.add_levels));
}

tier_operation tier_operation::greatest_lower_bound(const tier_operation &op1,
                                                    const tier_operation &op2)
{
  return tier_operation(levelwise_minimum(op1.add_levels,
                                          op2.add_levels));
}

tier_operation tier_operation::operator+(const tier_operation &other) const
{
  return tier_operation(levelwise_add(add_levels,
                                      other.add_levels));
}

tier tier_operation::apply(const tier &t) const
{
  return levelwise_add(t, add_levels);
}

void tier_operation::dump(std::ostream &out) const
{
  out << "(";

  if(add_levels != tier(0))
    {
      out << "add: " << add_levels;
    }

  out << ")";
}

int tier_operation::compare(const tier_operation &other) const
{
  // This comparison is correct only because we pre-normalize the
  // object to discard trailing zeroes.
  return aptitude::util::compare3(add_levels, other.add_levels);
}

std::size_t tier_operation::get_hash_value() const
{
  return hash_value(add_levels);
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
