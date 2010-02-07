/** \file tier.cc */

// Copyright (C) 2010 Daniel Burrows
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; see the file COPYING.  If not, write to
// the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

#include "tier.h"
#include "tier_limits.h"

#include <iostream>

std::ostream &operator<<(std::ostream &out, const level &l)
{
  if(l.get_value() == tier_limits::minimum_level)
    out << "minimum";
  else if(l.get_value() == tier_limits::maximum_level)
    out << "maximum";
  else
    out << l.get_value();

  return out;
}

std::ostream &operator<<(std::ostream &out, const tier &t)
{
  out << "(";

  const int t_structural_level = t.get_structural_level();

  if(t_structural_level == tier_limits::conflict_structural_level)
    out << "conflict";
  else if(t_structural_level == tier_limits::already_generated_structural_level)
    out << "already-generated";
  else if(t_structural_level == tier_limits::defer_structural_level)
    out << "defer";
  else if(t_structural_level == tier_limits::minimum_level)
    out << "minimum";
  else
    out << t_structural_level;

  for(tier::user_level_iterator it = t.user_levels_begin();
      it != t.user_levels_end(); ++it)
    out << ", " << *it;

  out << ")";

  return out;
}

