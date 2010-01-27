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

/** \brief Represents the tier of a search node.
 *
 *  The resolver is \e guaranteed to produce solutions in increasing
 *  order by tier.  This is as opposed to score, which is merely used
 *  in a best-effort manner.
 *
 *  A tier is simply a sequence of integers, each of which is known as
 *  a "level".  By convention, each tier in a search should have the
 *  same length; tiers are compared lexicographically.
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
    // algorithm.
    int structural_level;

    // Levels set by client code to customize the search order.
    std::vector<int> user_levels;

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

    /** \brief Initialize a tier object given its contents and a
     *         change to the list of user levels.
     */
    template<typename Iterator>
    tier_impl(int _structural_level,
	      Iterator user_levels_begin, Iterator user_levels_end,
	      int change_location,
	      int new_value)
      : structural_level(_structural_level),
	user_levels(user_levels_begin, user_levels_end),
	hash_value(0)
    {
      user_levels[change_location] = new_value;
      hash_value = get_hash_value();
    }

    std::size_t get_hash_value() const
    {
      std::size_t rval = 0;

      boost::hash_combine(rval, structural_level);
      for(std::vector<int>::const_iterator it = user_levels.begin();
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

  /** \brief Create a new tier object in which one of this object's
   *  user levels has been modified.
   *
   *  \param location A zero-based index into the list of user levels.
   *
   *  \param new_value The new level to store in the list in that
   *                   location.
   *
   *  \return a tier object with the same structural level as this
   *          object whose user levels are identical to this object's,
   *          except that the level in slot "location" has been set to
   *          new_value.
   */
  tier set_user_level(int location, int new_value) const
  {
    const tier_impl &impl(get_impl());

    return tier(boost::make_shared<tier_impl>(impl.structural_level,
					      impl.user_levels.begin(),
					      impl.user_levels.end(),
					      location, new_value));
  }

  /** \brief Retrieve the value of the structural level slot. */
  int get_structural_level() const { return get_impl().structural_level; }

  typedef std::vector<int>::const_iterator user_level_iterator;

  /** \brief Get a reference to the first user level as a
   *  random-access iterator.
   */
  user_level_iterator user_levels_begin() const { return get_impl().user_levels.begin(); }
  /** \brief Get a reference to the end of the user level list as a
   *  random-access iterator.
   */
  user_level_iterator user_levels_end() const { return get_impl().user_levels.end(); }

  /** \brief Retrieve one user level from this tier. */
  int get_user_level(int index) const { return get_impl().user_levels[index]; }

  std::size_t get_hash_value() const
  {
    return get_impl().hash_value;
  }

  tier &operator=(const tier &other)
  {
    impl_ref = other.impl_ref;

    return *this;
  }

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
