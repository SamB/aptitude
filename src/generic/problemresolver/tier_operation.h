d/** \file tier_operation.h */  // -*-c++-*-

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

#ifndef TIER_OPERATION_H
#define TIER_OPERATION_H

#include "exceptions.h"
#include "tier.h"

/** \brief A tier operation describes how any solution's tier will
 * change as a result of adding a choice.
 *
 *  Tier operations are associative and closed under composition.
 *  They are ordered in the natural way (if o1 < o2, then for any tier
 *  t, o1(t) < o2(t) -- this ordering exists due to the above
 *  properties).
 *
 *  There are two base types of tier operations: increase-level and
 *  add-to-level.  increase-level raises one or more levels within a
 *  tier to the given values, possibly extending the tier in the
 *  process.  add-to-level increments the components of a tier
 *  levelwise.  When these operations are composed, increase-level
 *  always takes place first (so the increased levels are added to,
 *  rather than the added-to levels being increased).
 */
class tier_operation
{
  // We use tiers internally to store the list of levels to modify,
  // because the information that each component of the operation
  // stores is exactly isomorphic to a tier.

  // The tier levels that are to be increased by this operation.
  tier increase_levels;

  // Values that this operation should add to a tier's level.
  //
  // Each level in this tier must be a nonnegative integer; if not,
  // constructing the operation will throw an exception.
  tier add_levels;

  tier_operation(const tier &_increase_levels,
		 const tier &_add_levels)
    : increase_levels(_increase_levels),
      add_levels(_add_levels)
  {
    if(add_levels.get_structural_level() < 0)
      throw NegativeTierAdditionException();

    for(std::size_t i = 0; i < add_levels.size(); ++i)
      {
	if(add_levels[i] < 0)
	  throw NegativeTierAdditionException();
      }
  }

public:
  /** \brief Create the identity tier operation: an operation with no
   *  effect.
   */
  tier_operation()
    : increase_levels(),
      add_levels(0)
  {
  }

  /** \brief Create a tier operation that increases each level in the
   *  target to the corresponding lower bound (levels that are already
   *  above their lower bound are unchanged).
   *
   *  \param bounds The lower bounds to apply to affected tier
   *  objects.  If the target tier is longer than bounds, the
   *  unmatched levels are unaffected; if it is shorter, it is
   *  extended to be as long as bounds.
   */
  static tier_operation make_increase_levels(const tier &bounds)
  {
    return tier_operation(bounds, tier(0));
  }

  /** \brief Create a tier operation that adds a value to each level
   *  in the target.
   *
   *  \param increments The values to add to affected tiers.  The
   *  value at each level is added to the corresponding level in the
   *  target; the values must be nonnegative.  If the target is longer
   *  than increments, the extra levels are unaffected; if the target
   *  is shorter, the extra levels of increments are added to
   *  tier_limits::minimum_level and then appended to the target tier.
   */
  static tier_operation make_add_to_levels(const tier &increments)
  {
    return tier_operation(tier(), increments);
  }

  /** \brief Compose two tier operations.
   *
   *  The composition of tier operations is both associative and
   *  commutative.
   */
  static tier_operation operator+(const tier_operation &other)
  {
    std::vector<int>
      output_increase_user_levels(increase_levels.user_levels_begin(),
				  increase_levels.user_levels_end());

    output_increase_user_levels.reserve(std::max<std::vector<int>::size_type>(output_increase_user_levels.size(), other.increase_levels.get_num_user_levels()));

    // TODO: take corresponding maximums.
  }
};

#endif // TIER_OPERATION_H
