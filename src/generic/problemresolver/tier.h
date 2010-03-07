/** \file tier.h */ // -*-c++-*-

// Copyright (C) 2010 Daniel Burrows

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

#ifndef TIER_H
#define TIER_H

#include <generic/util/compare3.h>

#include <boost/functional/hash.hpp>
#include <boost/make_shared.hpp>
#include <boost/shared_ptr.hpp>

#include <iosfwd>
#include <vector>

#include <limits.h>

#include "exceptions.h"

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

/** \brief Represents the tier of a search node.
 *
 *  The resolver is \e guaranteed to produce solutions in increasing
 *  order by tier.  This is as opposed to score, which is merely used
 *  in a best-effort manner.
 *
 *  A tier is a sequence of levels (as defined above).  By convention,
 *  each tier in a search should have the same length; tiers are
 *  compared lexicographically.
 *
 *  The first level in the tier is reserved by the resolver to store
 *  "structural" priorities (for instance, to mark nodes as conflicted
 *  or deferred); to make things simpler for client code that doesn't
 *  care about this information, that level is stored and accessed
 *  separately from the main list.
 */
class tier
{
  /** \brief The object that actually stores the tier data. */
  struct tier_impl
  {
    // Level set by the resolver itself to control the search
    // algorithm.  This is always increased in an upper-bound / cutoff
    // fashion.
    int structural_level;

    // Levels set by client code to customize the search order.
    std::vector<level> user_levels;

    // Cache the hash value.  Initialized during construction.
    std::size_t hash_value;

    /** \brief Initialize a tier object from a collection of user levels.
     *
     *  The structural level is set to INT_MIN.
     */
    template<typename Iterator>
    tier_impl(Iterator user_levels_begin, Iterator user_levels_end)
      : structural_level(INT_MIN),
	user_levels(user_levels_begin, user_levels_end),
	hash_value(0)
    {
      hash_value = get_hash_value();
    }

    /** \brief Initialize a tier object with no user levels.
     *
     *  This will be the smallest tier in its structural level.
     */
    tier_impl(int _structural_level)
      : structural_level(_structural_level),
	user_levels(),
	hash_value(0)
    {
      hash_value = get_hash_value();
    }

    /** \brief Initialize a tier object given its contents. */
    template<typename Iterator>
    tier_impl(int _structural_level,
	      Iterator user_levels_begin, Iterator user_levels_end)
      : structural_level(_structural_level),
	user_levels(user_levels_begin, user_levels_end),
	hash_value(0)
    {
      hash_value = get_hash_value();
    }

    std::size_t get_hash_value() const
    {
      std::size_t rval = 0;

      boost::hash_combine(rval, structural_level);
      for(std::vector<level>::const_iterator it = user_levels.begin();
	  it != user_levels.end(); ++it)
	boost::hash_combine(rval, *it);

      return rval;
    }

    bool operator==(const tier_impl &other) const
    {
      return
	structural_level == other.structural_level &&
	user_levels == other.user_levels;
    }

    bool operator!=(const tier_impl &other) const
    {
      return
	structural_level != other.structural_level ||
	user_levels != other.user_levels;
    }
  };

  class tier_impl_hasher
  {
  public:
    std::size_t operator()(const tier_impl &impl) const
    {
      return impl.hash_value;
    }
  };

  typedef boost::shared_ptr<tier_impl> tier_impl_ref;

  tier_impl_ref impl_ref;

  tier(const tier_impl_ref &_impl_ref)
    : impl_ref(_impl_ref)
  {
  }

  const tier_impl &get_impl() const
  {
    return *impl_ref;
  }

public:
  /** \brief A default-constructed tier is the smallest possible tier. */
  tier()
    : impl_ref(boost::make_shared<tier_impl>(INT_MIN))
  {
  }

  /** \brief Create a new tier object with structural level INT_MIN.
   *
   *  \param user_level_begin The beginning of a range of levels to
   *                          insert into the user level list.
   *
   *  \param user_level_end The end of a range of levels to insert
   *                        into the user level list.
   */
  template<typename Iterator>
  tier(Iterator user_levels_begin, Iterator user_levels_end)
    : impl_ref(boost::make_shared<tier_impl>(user_levels_begin, user_levels_end))
  {
  }

  /** \brief Create a new tier object with the given structural level
   *  and no user levels.
   *
   *  The new tier will be the smallest tier in its structural level.
   */
  tier(int structural_level)
    : impl_ref(boost::make_shared<tier_impl>(structural_level))
  {
  }

  /** \brief Create a new tier object.
   *
   *  \param structural_level The structural level to store in the new
   *                          level object.
   *
   *  \param user_level_begin The beginning of a range of levels to
   *                          insert into the user level list.
   *
   *  \param user_level_end The end of a range of levels to insert
   *                        into the user level list.
   */
  template<typename Iterator>
  tier(int structural_level,
       Iterator user_levels_begin, Iterator user_levels_end)
    : impl_ref(boost::make_shared<tier_impl>(structural_level, user_levels_begin, user_levels_end))
  {
  }

  /** \brief Create a new tier object in which this object's
   *  structural level has been modified.
   *
   *  \param new_structural_level The structural level of the new
   *  level object.
   *
   *  \return a tier object with the same user levels as this object
   *  and the given structural level.
   */
  tier set_structural_level(int new_structural_level) const
  {
    const tier_impl &impl(get_impl());

    return tier(boost::make_shared<tier_impl>(new_structural_level,
					      impl.user_levels.begin(),
					      impl.user_levels.end()));
  }

  /** \brief Retrieve the value of the structural level slot. */
  int get_structural_level() const { return get_impl().structural_level; }

  typedef std::vector<level>::const_iterator user_level_iterator;

  /** \brief Get a reference to the first user level as a
   *  random-access iterator.
   */
  user_level_iterator user_levels_begin() const { return get_impl().user_levels.begin(); }
  /** \brief Get a reference to the end of the user level list as a
   *  random-access iterator.
   */
  user_level_iterator user_levels_end() const { return get_impl().user_levels.end(); }

  /** \brief Retrieve one user level from this tier. */
  const level &get_user_level(int index) const { return get_impl().user_levels[index]; }

  /** \brief Retrieve the number of user levels in this tier. */
  std::size_t get_num_user_levels() const { return get_impl().user_levels.size(); }

  std::size_t get_hash_value() const
  {
    return get_impl().hash_value;
  }

  tier &operator=(const tier &other)
  {
    impl_ref = other.impl_ref;

    return *this;
  }

  /** \brief Compare two tiers.
   *
   *  Tiers are compared lexicographically, ignoring level states.
   */
  int compare(const tier &other) const
  {
    const tier_impl &impl(get_impl());
    const tier_impl &other_impl(other.get_impl());

    using aptitude::util::compare3;

    const int cmp = compare3(impl.structural_level, other_impl.structural_level);
    if(cmp != 0)
      return cmp;
    else
      return compare3(impl.user_levels, other_impl.user_levels);
  }

  bool operator==(const tier &other) const
  {
    return compare(other) == 0;
  }

  bool operator!=(const tier &other) const
  {
    return compare(other) != 0;
  }

  bool operator<(const tier &other) const
  {
    return compare(other) < 0;
  }

  bool operator<=(const tier &other) const
  {
    return compare(other) <= 0;
  }

  bool operator>(const tier &other) const
  {
    return compare(other) > 0;
  }

  bool operator>=(const tier &other) const
  {
    return compare(other) >= 0;
  }
};

inline std::size_t hash_value(const tier &t)
{
  return t.get_hash_value();
}

namespace aptitude
{
  namespace util
  {
    template<>
    class compare3_f<tier>
    {
    public:
      int operator()(const tier &t1, const tier &t2) const
      {
	return t1.compare(t2);
      }
    };
  }
}

std::ostream &operator<<(std::ostream &, const tier &t);

#endif // TIER_H
