/** \file tier_limits.h */    // -*-c++-*-


//   Copyright (C) 2009 Daniel Burrows
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

#ifndef TIER_LIMITS_H
#define TIER_LIMITS_H

template<typename PackageUniverse>
class generic_tier_limits
{
  typedef typename PackageUniverse::tier tier;

public:
  static const int maximum_tier_num = INT_MAX;

  /** \brief The maximum tier; reserved for solutions that contain a
   *  logical conflict and thus are dead-ends.
   *
   *  Search nodes at this tier are discarded without being visited.
   */
  static const int conflict_tier_num = maximum_tier_num;

  /** \brief The second highest tier; reserved for solutions that were
   *  already generated (to prevent them from being generated again).
   *
   *  Search nodes at this tier are discarded without being visited.
   */
  static const int already_generated_tier_num = conflict_tier_num - 1;

  /** \brief The third highest tier; reserved for solutions that
   *  violate a user constraint and will be deferred until the
   *  constraints are changed.
   */
  static const int defer_tier_num = conflict_tier_num - 2;

  /** \brief The minimum tier; this is the initial tier of the empty
   *  solution.
   */
  static const int minimum_tier_num = INT_MIN;

  static const tier maximum_tier;
  static const tier conflict_tier;
  static const tier already_generated_tier;
  static const tier defer_tier;
  static const tier minimum_tier;
};

// Appease the C++ gods by writing dummy definitions of these things.
// g++ happily accepts definitions of static constants inside a class,
// but they aren't actually defined unless you define them outside the
// class and don't give a definition in the definition, so without
// these dummy lines, the linker will complain that there are
// undefined references to the constants defined above because their
// definitions aren't really definitions.  Hopefully that's all
// obvious.
template<typename PackageUniverse>
const int generic_tier_limits<PackageUniverse>::maximum_tier_num;
template<typename PackageUniverse>
const int generic_tier_limits<PackageUniverse>::conflict_tier_num;
template<typename PackageUniverse>
const int generic_tier_limits<PackageUniverse>::already_generated_tier_num;
template<typename PackageUniverse>
const int generic_tier_limits<PackageUniverse>::defer_tier_num;
template<typename PackageUniverse>
const int generic_tier_limits<PackageUniverse>::minimum_tier_num;

template<typename PackageUniverse>
const typename PackageUniverse::tier generic_tier_limits<PackageUniverse>::maximum_tier(maximum_tier_num);

template<typename PackageUniverse>
const typename PackageUniverse::tier generic_tier_limits<PackageUniverse>::conflict_tier(conflict_tier_num);

template<typename PackageUniverse>
const typename PackageUniverse::tier generic_tier_limits<PackageUniverse>::already_generated_tier(already_generated_tier_num);

template<typename PackageUniverse>
const typename PackageUniverse::tier generic_tier_limits<PackageUniverse>::defer_tier(defer_tier_num);

template<typename PackageUniverse>
const typename PackageUniverse::tier generic_tier_limits<PackageUniverse>::minimum_tier(minimum_tier_num);

#endif
