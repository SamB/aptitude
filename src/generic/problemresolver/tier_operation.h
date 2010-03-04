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

#include <boost/flyweight.hpp>

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
  // These classes are used to disambiguate constructors.
  class combine_tag { };
  class lower_bound_tag { };
  class upper_bound_tag { };

  // \todo Should have an abstracted "key"" that covers the 3 ways of
  // instantiating this object.  Also, should cache the hash value to
  // avoid recomputing it over and over.
  class op_impl
  {
    typedef std::vector<level>::size_type level_index;

    // The value that a target tier's structural level should be
    // increased to.
    int structural_level;

    // The operation is a collection of pairs giving level operations
    // and the level's location in the tier vector.  Storing tier
    // operations this way lets us save a little memory, considering
    // that most operations will probably modify only a few tiers.
    //
    // This vector is always sorted, and level numbers are unique.
    std::vector<std::pair<level_index, level> > actions;

  public:
    /** \brief Create a blank tier operation. */
    op_impl()
      : structural_level(INT_MIN)
    {
    }

    /** \brief Create a tier operation that modifies the structural
     *	level of the tier.
     */
    explicit op_impl(int _structural_level)
      : structural_level(_structural_level)
    {
    }

    /** \brief Create a tier operation that modifies a single level of
     *	the tier.
     */
    op_impl(int index, const level &l)
      : structural_level(INT_MIN)
    {
      if(l.get_state() == level::added &&
	 l.get_value() <= 0)
	throw NonPositiveTierAdditionException();

      actions.push_back(std::make_pair(index, l));
    }

    /** \brief Create a tier operation that combines two other operations. */
    op_impl(const op_impl &op1, const op_impl &op2, combine_tag);

    /** \brief Create a new operation that computes the lower bound of
     *  two input operations.
     */
    op_impl(const op_impl &op1, const op_impl &op2, lower_bound_tag);

    /** \brief Create a new operation that computes the upper bound of
     *  two input operations.
     */
    op_impl(const op_impl &op1, const op_impl &op2, upper_bound_tag);

    /** \brief Compute a hash on this tier operation.
     *
     *  \note Relies on the fact that the level's hash includes
     *  whether it's an addition or a lower-bound.
     */
    std::size_t get_hash_value() const
    {
      std::size_t rval = 0;

      boost::hash_combine(rval, structural_level);
      boost::hash_combine(rval, actions);

      return rval;
    }

    /** \brief Test two tier operations for equality.
     *
     *  \note Relies on the fact that the level's equality comparison
     *  returns "true" only when the two levels have the same state.
     */
    bool operator==(const op_impl &other) const
    {
      return
	structural_level == other.structural_level &&
	actions == other.actions;
    }

    /** \brief Apply the operation to the tier.
     *
     *  Each level is combined with the corresponding level in the
     *  target tier.  If there is no corresponding level, it is copied
     *  directly (which is exactly the desired behavior).
     */
    tier apply(const tier &t) const;

    /** \brief Compare two operations by their identity.
     */
    int compare(const op_impl &other) const;

    /** \brief Dump this operation to a stream. */
    void dump(std::ostream &out) const;
  };

  class op_impl_hasher
  {
  public:
    std::size_t operator()(const op_impl &op) const
    {
      return op.get_hash_value();
    }
  };

  typedef boost::flyweight<op_impl,
			   boost::flyweights::hashed_factory<op_impl_hasher> >
  op_impl_flyweight;

  op_impl_flyweight impl_flyweight;

  const op_impl &get_impl() const { return impl_flyweight.get(); }

  /** \brief Create a tier operation that modifies the structural
   *	level of the tier.
   */
  explicit tier_operation(int structural_level)
    : impl_flyweight(op_impl(structural_level))
  {
  }

  /** \brief Create a tier operation that combines two other
   *  operations.
   */
  tier_operation(const tier_operation &op1,
		 const tier_operation &op2)
    : impl_flyweight(op_impl(op1.get_impl(), op2.get_impl(),
			     combine_tag()))
  {
  }

  /** \brief Create a tier operation that modifies a single level of
   *  the tier.
   */
  tier_operation(int index, const level &l)
    : impl_flyweight(op_impl(index, l))
  {
  }

  /** \brief Create a new operation that computes the lower bound of
   *  two input operations.
   */
  tier_operation(const tier_operation &op1, const tier_operation &op2, lower_bound_tag)
    : impl_flyweight(op_impl(op1.get_impl(), op2.get_impl(),
			     lower_bound_tag()))
  {
  }

  /** \brief Create a new operation that computes the upper bound of
   *  two input operations.
   */
  tier_operation(const tier_operation &op1, const tier_operation &op2, upper_bound_tag)
    : impl_flyweight(op_impl(op1.get_impl(), op2.get_impl(),
			     upper_bound_tag()))
  {
  }

public:
  /** \brief Create the identity tier operation: an operation with no
   *  effect.
   */
  tier_operation()
    : impl_flyweight(op_impl())
  {
  }

  /** \brief Create a tier operation that increases the structural
   *  level of its target to a particular value.
   *
   *  \param structural_level The structural level that the resulting
   *  operation will increase affected tiers to; tiers at a higher
   *  structural level are unaffected.
   */
  static tier_operation make_advance_structural_level(int structural_level)
  {
    return tier_operation(structural_level);
  }

  /** \brief Create a tier operation that increases a single user
   *  level of its target to a particular value.
   *
   *  \param index The index within the user tier list of the level
   *               that is to be modified.
   *
   *  \param value The value that the resulting operation will
   *               increase affected user levels to; higher user
   *               levels are unaffected.
   */
  static tier_operation make_advance_user_level(int index,
						int value)
  {
    return tier_operation(index, level::make_lower_bounded(value));
  }

  /** \brief Create a tier operation that adds a fixed value to a
   *  single user level.
   *
   *  \param index The index within the user tier list of the level
   *               that is to be modified.
   *
   *  \param value The value that the resulting operation will add to
   *               affected user levels.
   */
  static tier_operation make_add_to_user_level(int index,
					       int value)
  {
    return tier_operation(index, level::make_added(value));
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
  tier_operation operator+(const tier_operation &other) const
  {
    return tier_operation(*this, other);
  }

  /** \brief Test whether two operations have the same effect. */
  bool operator==(const tier_operation &other) const
  {
    return impl_flyweight == other.impl_flyweight;
  }

  /** \brief Test whether two operations don't have the same effect. */
  bool operator!=(const tier_operation &other) const
  {
    return impl_flyweight != other.impl_flyweight;
  }

  /** \brief Apply this operation to a tier.
   *
   *  \param t  The tier that this operation should modify.
   */
  tier apply(const tier &t) const
  {
    return get_impl().apply(t);
  }

  /** \brief Write a description of a tier operation to an ostream.
   */
  void dump(std::ostream &out) const
  {
    get_impl().dump(out);
  }

  std::size_t get_hash_value() const
  {
    return get_impl().get_hash_value();
  }

  /** \brief Compare tier operations according to their
   *  identity, NOT their natural order.
   *
   *  This is a total ordering that can be used to place operations
   *  into ordered data structures.  It has no relation to the natural
   *  partial ordering on tier operations that least_upper_bound and
   *  greatest_upper_bound rely upon.
   */
  int compare(const tier_operation &other) const
  {
    const op_impl &this_op = get_impl(), &other_op = other.get_impl();

    // Rely on equality of flyweights being fast.
    if(this_op == other_op)
      return 0;
    else
      return this_op.compare(other_op);
  }
};

namespace aptitude
{
  namespace util
  {
    template<>
    class compare3_f<tier_operation>
    {
    public:
      int operator()(const tier_operation &op1, const tier_operation &op2) const
      {
	return op1.compare(op2);
      }
    };
  }
}

std::size_t hash_value(const tier_operation &op);

std::ostream &operator<<(std::ostream &out, const tier_operation &t);

#endif // TIER_OPERATION_H
