/** \file tier_operation.h */  // -*-c++-*-

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

#include <iosfwd>

/** \brief A tier operation describes how any solution's tier will
 * change as a result of adding a choice.
 *
 *  Tier operations are associative and closed under composition.
 *  They are partially ordered in the natural way (if o1 < o2, then
 *  for any tier t, o1(t) < o2(t) -- this ordering exists due to the
 *  above properties) and exist in a lattice.  (least upper and
 *  greatest lower bounds are levelwise minimum and maximum,
 *  respectively)
 *
 *  Currently the only available tier operation is "add X to a level".
 *  The old operation (increase a level to a value if it was below
 *  that value) couldn't be combined with this one because they don't
 *  commute.
 */
class tier_operation
{
  // We use tiers internally to store the list of levels to modify,
  // because the information that each component of the operation
  // stores is exactly isomorphic to a tier.

  // Values that this operation should add to a tier's level.
  //
  // Each level in this tier must be a nonnegative integer; if not,
  // constructing the operation will throw an exception.
  tier add_levels;

  tier_operation(const tier &_add_levels)
    : add_levels(_add_levels)
  {
    if(add_levels.get_structural_level() < 0)
      throw NegativeTierAdditionException();

    const std::size_t add_levels_size(add_levels.get_num_user_levels());
    for(std::size_t i = 0; i < add_levels_size; ++i)
      {
	if(add_levels.get_user_level(i) < 0)
	  throw NegativeTierAdditionException();
      }
  }

  /** \brief Compute the levelwise maximum of two tiers.
   *
   *  The output is a tier in which each level is equal to the maximum
   *  of the corresponding entries in the input tiers.  Unpaired
   *  levels (in the event that one of the tiers is longer than the
   *  other) are assumed to equal tier_limits::minimum_level, with the
   *  effect that the longer tier's elements are copied unchanged.
   *
   *  This function is implemented here instead of in tier.h because
   *  tier operations require exactly this behavior and nothing else
   *  does.
   */
  static tier levelwise_maximum(const tier &t1, const tier &t2);

  /** \brief Compute the levelwise minimum of two tiers.
   *
   *  The output is a tier in which each level is equal to the minimum
   *  of the corresponding entries in the input tiers.  Unpaired
   *  levels (in the event that one of the tiers is longer than the
   *  other) are assumed to equal tier_limits::maximum_level, with the
   *  effect that the longer tier's elements are copied unchanged.
   *
   *  This function is implemented here instead of in tier.h because
   *  tier operations require exactly this behavior and nothing else
   *  does.
   */
  static tier levelwise_minimum(const tier &t1, const tier &t2);

  /** \brief Safely add two tier levels.
   *
   *  Checks that at least one operand is nonnegative and that the
   *  result won't overflow.
   */
  static int safe_add_levels(int l1, int l2);

  /** \brief Compute the levelwise sum of two tiers.
   *
   *  The output is a tier in which each level is equal to the sum of
   *  the corresponding levels in the input tiers.  If one tier is
   *  longer than the other, the missing levels are assumed to be 0.
   */
  static tier levelwise_add(const tier &t1, const tier &t2);

public:
  /** \brief Create the identity tier operation: an operation with no
   *  effect.
   */
  tier_operation()
    : add_levels(0)
  {
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
    return tier_operation(increments);
  }

  /** \brief Compute the least upper bound of two tier operations.
   *
   *  This is the smallest tier operation that produces tiers which
   *  are strictly greater than those produced by both input
   *  operations.
   */
  static tier_operation least_upper_bound(const tier_operation &op1,
                                          const tier_operation &op2);

  /** \brief Compute the greatest lower bound of two tier operations.
   *
   *  This is the largest tier operation that produces tiers which are
   *  strictly less than those produced by both input operations.
   */
  static tier_operation greatest_lower_bound(const tier_operation &op1,
                                             const tier_operation &op2);

  /** \brief Compose two tier operations.
   *
   *  The composition of tier operations is both associative and
   *  commutative.
   */
  tier_operation operator+(const tier_operation &other) const;

  /** \brief Apply this operation to a tier.
   *
   *  \param t  The tier that this operation should modify.
   */
  tier apply(const tier &t) const;

  /** \brief Write a description of a tier operation to an ostream.
   */
  void dump(std::ostream &out) const;
};

std::ostream &operator<<(std::ostream &out, const tier_operation &t);

#endif // TIER_OPERATION_H
