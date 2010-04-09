/** \file cost.h */  // -*-c++-*-

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

#ifndef COST_H
#define COST_H

#include "exceptions.h"

#include <generic/util/compare3.h>

#include <iosfwd>
#include <stdexcept>

#include <boost/flyweight.hpp>
/** \brief Represents one level in a search tier.
 *
 *  A search tier is an ordered sequence of levels.  Levels are
 *  integers that can be modified either by being incremented or by
 *  being increased to be at least the given value.  However, the two
 *  operations may not be applied to the same level; attempting to do
 *  so will raise an exception.  This does not apply to the identity
 *  of each operation (adding 0 or raising to INT_MIN).
 *
 *  The reason for representing levels this way is that it allows the
 *  resolver to support both "increase the tier to X" and "add X to
 *  the tier" operations in a sound manner; in particular, this way
 *  those two operations are both associative and commutative.  (in
 *  this case, they are associative and commutative in the sense that
 *  applying both types of operations always errors out)
 *
 *  Note: the "increase tier to X" operation is important for
 *  respecting priorities (there doesn't seem to be an obvious
 *  alternative there), for backwards-compatibility, and so that the
 *  resolver can easily become non-optimizing if necessary (e.g., if
 *  the optimizing version experiences too many performance problems,
 *  reverting to the old behavior is a simple change to the default
 *  settings).
 *
 *  Note that levels can represent both a tier entry, or a *change* to
 *  a tier entry.  The "combine" method will either merge two changes
 *  into a single change, or apply a change to a level.
 *
 *  Instead of failing out when two different operations are applied,
 *  I could instead resolve the conflict by accumulating lower-bounds
 *  separately from increments and always applying lower-bounds first.
 *  That would resolve the conflict, but it might produce a somewhat
 *  unintuitive result for the user.  I've chosen this route because
 *  at least the behavior is obvious (and I can't think of any use
 *  case for actually merging lower-bounds and increments).
 */
class level
{
public:
  // The level's state; if it's ever been modified, this tracks how it
  // was modified.
  enum state_enum { unmodified, added, lower_bounded };

private:
  // Note: I would use a single iinteger if I thought I could get away
  // with it.

  // Note 2: the reason for using signed integers is that it makes the
  // case of policy-priorities-as-tiers a bit clearer.

  int value;

  state_enum state;

  level(int _value, state_enum _state)
    : value(_value), state(_state)
  {
  }

public:
  /** \brief Create a new level at the minimum tier. */
  level() : value(INT_MIN), state(unmodified)
  {
  }

  /** \brief Create a new level with the given value and state "added". */
  static level make_added(int value)
  {
    return level(value, added);
  }

  /** \brief Create a new level with the given value and state "lower_bounded." */
  static level make_lower_bounded(int value)
  {
    return level(value, lower_bounded);
  }

  int get_value() const { return value; }
  state_enum get_state() const { return state; }

  /** \brief Test whether this level is greater than or equal to the
   *  given other level under the natural partial ordering of levels.
   */
  bool is_above_or_equal(const level &other) const
  {
    if(other.state == unmodified)
      return true;
    else if(state != other.state)
      return false;
    else
      switch(state)
	{
	case unmodified:
	  return true;

	case added:
	case lower_bounded:
	  return value >= other.value;
	}

    // Fall-through in case the case statement missed something;
    // outside the case so that the compiler checks that all enum
    // values are handled.
    return false;
  }

  void increase_tier_to(int new_value)
  {
    if(state == added)
      throw TierOperationMismatchException();
    else if(new_value != INT_MIN)
      {
	if(value < new_value)
	  value = new_value;
	state = lower_bounded;
      }
  }

  /** \brief Combine two levels and return a new level.
   *
   *  If either input level is unmodified, the result is the other
   *  level.  Otherwise, the two levels must have the same state, and
   *  the levels are combined according to that state.
   */
  static level combine(const level &l1, const level &l2)
  {
    if(l1.state == unmodified)
      return l2;
    else if(l2.state == unmodified)
      return l1;
    else if(l1.state != l2.state)
      throw TierOperationMismatchException();
    else if(l1.state == lower_bounded)
      return level(std::max<int>(l1.value, l2.value), lower_bounded);
    else // if(l1.state == added)
      {
	if(!(l1.value > 0 || l2.value > 0))
	  throw NonPositiveTierAdditionException();
	else if(l1.value > INT_MAX - l2.value)
          throw TierTooBigException();
        else
	  return level(l1.value + l2.value, added);
      }
  }

  /** \brief Compute the upper bound of two levels.
   *
   *  If the levels have incompatible states, throws an exception;
   *  otherwise, returns a level whose numerical value is the maximum
   *  of the numerical values of the two input levels and whose type
   *  is the upper-bound of their types.
   */
  static level upper_bound(const level &l1, const level &l2)
  {
    if(l1.state == unmodified)
      return l2;
    else if(l2.state == unmodified)
      return l1;
    else if(l1.state != l2.state)
      throw TierOperationMismatchException();
    else
      return level(std::max<int>(l1.value, l2.value),
		   l1.state);
  }

  /** \brief Compute the lower bound of two levels.
   *
   *  If the levels have incompatible states, throws an exception; if
   *  either is "unmodified", returns an unmodified level; otherwise,
   *  returns a level whose numerical value is the minimum of the
   *  numerical values of the two input levels and whose type is the
   *  lower-bound of their types.
   */
  static level lower_bound(const level &l1, const level &l2)
  {
    if(l1.state == unmodified || l2.state == unmodified)
      return level();
    else if(l1.state != l2.state)
      throw TierOperationMismatchException();
    else
      return level(std::min<int>(l1.value, l2.value),
		   l1.state);
  }

  /** \brief Compare two levels.
   *
   *  Only the value of each level is compared.  The additional
   *  information is discarded, so this should not be used to build an
   *  associative data structure where that information might matter.
   */
  int compare(const level &other) const
  {
    return aptitude::util::compare3(value, other.value);
  }

  /** \brief Hashes a level object. */
  std::size_t get_hash_value() const
  {
    std::size_t rval = 0;

    boost::hash_combine(rval, value);
    boost::hash_combine(rval, state);

    return rval;
  }

  /** \brief Returns \b true if two levels are identical (both value
   *  and state).
   */
  bool operator==(const level &other) const
  {
    return value == other.value && state == other.state;
  }
};

std::ostream &operator<<(std::ostream &out, const level &l);

inline std::size_t hash_value(const level &l)
{
  return l.get_hash_value();
}

namespace aptitude
{
  namespace util
  {
    template<>
    class compare3_f<level>
    {
    public:
      int operator()(const level &t1, const level &t2) const
      {
	return t1.compare(t2);
      }
    };
  }
}

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
      if(index < 0)
        throw std::out_of_range("Negative index used to construct a tier operation");

      if(l.get_state() == level::added &&
	 l.get_value() <= 0)
	throw NonPositiveTierAdditionException();

      if(index < 0)
	throw std::out_of_range("User level indices must be non-negative.");

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

    /** \brief Return \b true if this operation is greater than or
     *  equal to the other operation under the natural partial order.
     *
     *  Equivalent to testing whether least_upper_bound is this
     *  object, but doesn't create an intermediary.
     */
    bool is_above_or_equal(const op_impl &other) const;

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

    int get_structural_level() const
    {
      return structural_level;
    }

    level get_user_level(std::size_t idx) const;

    bool get_has_user_level_changes() const
    {
      return !actions.empty();
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

  /** \brief Return \b true if this operation is greater than or equal
   *  to the other operation under the natural partial order.
   *
   *  Equivalent to testing whether least_upper_bound is this object,
   *  but doesn't create an intermediary.
   */
  bool is_above_or_equal(const tier_operation &other) const
  {
    return get_impl().is_above_or_equal(other.get_impl());
  }

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

  /** \brief Write a description of a tier operation to an ostream.
   */
  void dump(std::ostream &out) const
  {
    get_impl().dump(out);
  }

  /** \brief Get the structural level that this operation raises its
   *  target to.
   */
  int get_structural_level() const
  {
    return get_impl().get_structural_level();
  }

  /** \brief Get the operation performed on a user level. */
  level get_user_level(int idx) const
  {
    return get_impl().get_user_level(idx);
  }

  /** \brief Check whether any actions performed by the operation
   *  affect user levels.
   */
  bool get_has_user_level_changes() const
  {
    return get_impl().get_has_user_level_changes();
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
