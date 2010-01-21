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

#include <boost/flyweight.hpp>
#include <boost/flyweight/hashed_factory.hpp>
#include <boost/functional/hash.hpp>

#include <vector>

/** \brief Represents the tier of a search node.
 *
 *  The resolver is \e guaranteed to produce solutions in increasing
 *  order by tier.  This is as opposed to score, which is merely used
 *  in a best-effort manner.
 *
 *  A tier is simply a sequence of integers.  By convention, each tier
 *  in a search should have the same length; tiers are compared
 *  lexicographically.
 *
 *  The first entry in the tier list is reserved by the resolver to
 *  store "structural" priorities (for instance, to mark nodes as
 *  conflicted or deferred); to make things simpler for client code
 *  that doesn't care about this information, that entry is stored and
 *  accessed separately from the main list.
 */
class tier
{
  // Tiers are transparently flyweighted; the advantage is that it'll
  // be easy for me to experiment with not flyweighting them or with
  // using other allocation schemes down the road.

  /** \brief The object that actually stores the tier data. */
  struct tier_impl
  {
    // Tier set by the resolver itself to control the search
    // algorithm.
    int structural_tier;

    // Tiers set by client code to customize the search order.
    std::vector<int> user_tiers;

    /** \brief Initialize a tier object from a collection of user tiers.
     *
     *  The structural tier is set to 0.
     */
    template<typename Iterator>
    tier_impl(Iterator user_tiers_begin, Iterator user_tiers_end)
      : structural_tier(0),
	user_tiers(user_tiers_begin, user_tiers_end)
    {
    }

    /** \brief Initialize a tier object given its contents. */
    template<typename Iterator>
    tier_impl(int _structural_tier,
	      Iterator user_tiers_begin, Iterator user_tiers_end)
      : structural_tier(_structural_tier),
	user_tiers(user_tiers_begin, user_tiers_end)
    {
    }

    /** \brief Initialize a tier object given its contents and a change to make. */
    template<typename Iterator>
    tier_impl(int _structural_tier,
	      Iterator user_tiers_begin, Iterator user_tiers_end,
	      int change_location, // 0 for the structural tier, 1 for
	                           // the first user tier.
	      int new_value)
      : structural_tier(_structural_tier),
	user_tiers(user_tiers_begin, user_tiers_end)
    {
      if(change_location == 0)
	structural_tier = new_value;
      else
	user_tiers[change_location] = new_value;
    }
  };

  class tier_impl_hasher
  {
  public:
    std::size_t operator()(const tier_impl &impl) const
    {
      std::size_t rval = 0;

      boost::hash_combine(rval, impl.structural_tier);
      for(std::vector<int>::const_iterator it = impl.user_tiers.begin();
	  it != impl.user_tiers.end(); ++it)
	boost::hash_combine(rval, *it);

      return rval;
    }
  };

  typedef boost::flyweight<tier_impl,
			   boost::flyweights::hashed_factory<tier_impl_hasher> >
  tier_impl_flyweight;

  tier_impl_flyweight impl_flyweight;

  tier(const tier_impl &impl)
    : impl_flyweight(impl)
  {
  }

  const impl &get_impl() const
  {
    return impl_flyweight.get();
  }

public:
  /** \brief Create a new tier object with structural tier 0.
   *
   *  \param user_tier_begin The beginning of a range of tiers to
   *                         insert into the user tier list.
   *
   *  \param user_tier_end The end of a range of tiers to insert into
   *                       the user tier list.
   */
  template<typename Iterator>
  tier create(Iterator user_tiers_begin, Iterator user_tiers_end)
  {
    tier(tier_impl(user_tiers_begin, user_tiers_end));
  }

  /** \brief Create a new tier object.
   *
   *  \param structural_tier The structural tier to store in the new
   *                         tier object.
   *
   *  \param user_tier_begin The beginning of a range of tiers to
   *                         insert into the user tier list.
   *
   *  \param user_tier_end The end of a range of tiers to insert into
   *                       the user tier list.
   */
  template<typename Iterator>
  boost::flyweight<tier> create(int structural_tier,
				Iterator user_tiers_begin, Iterator user_tiers_end)
  {
    return tier(tier_impl(structural_tier, user_tiers_begin, user_tiers_end));
  }

  /** \brief Create a new tier object in which this object's structural tier
   *  has been modified.
   *
   *  \param new_structural_tier The structural tier of the new tier
   *  object.
   *
   *  \return a tier object with the same user tiers as this object
   *  and the given structural tier.
   */
  boost::flyweight<tier> set_structural_tier(int new_structural_tier)
  {
    return tier(tier_impl(structural_tier,
			  user_tiers.begin(), user_tiers.end(),
			  0, new_structural_tier));
  }

  /** \brief Create a new tier object in which one of this object's
   *  user tiers has been modified.
   *
   *  \param location A zero-based index into the list of user tiers.
   *
   *  \param new_value The new tier to store in the list in that
   *                   location.
   *
   *  \return a tier object with the same structural tier as this
   *          object whose user tiers are identical to this object's,
   *          except that the tier in slot "location" has been set to
   *          new_value.
   */
  boost::flyweight<tier> set_user_tier(int location, int new_value)
  {
    return tier(tier_impl(structural_tier,
			  user_tiers.begin(), user_tiers.end(),
			  location + 1, new_value));
  }

  /** \brief Retrieve the value of the structural tier slot. */
  int get_structural_tier() const { return get_impl().structural_tier; }

  typedef std::vector<int>::const_iterator user_tier_iterator;

  /** \brief Get a reference to the first user tier as a random-access
   *  iterator.
   */
  user_tier_iterator user_tiers_begin() const { return get_impl().user_tiers.begin(); }
  /** \brief Get a reference to the end of the user tier list as a
   *  random-access iterator.
   */
  user_tier_iterator user_tiers_end() const { return get_impl().user_tiers.end(); }

  std::size_t get_hash_value() const
  {
    return boost::hash_value(impl_flyweight);
  }

  bool operator==(const tier &other) const
  {
    const tier_impl &impl(get_impl());
    const tier_impl &other_impl(other.get_impl());

    return
      impl.structural_tier == other_impl.structural_tier &&
      impl.user_tiers == other_impl.user_tiers;
  }

  bool operator!=(const tier &other) const
  {
    const tier_impl &impl(get_impl());
    const tier_impl &other_impl(other.get_impl());

    return
      impl.structural_tier != other_impl.structural_tier ||
      impl.user_tiers != other_impl.user_tiers;
  }

  int compare(const tier &other) const
  {
    const tier_impl &impl(get_impl());
    const tier_impl &other_impl(other.get_impl());

    using aptitude::util::compare3;

    const int cmp = compare3(impl.structural_tier, other_impl.structural_tier);
    if(cmp != 0)
      return cmp;
    else
      return compare3(impl.user_tiers, other_impl.user_tiers);
  }

  bool operator<(const tier &other) const
  {
    return compare(other) < 0;
  }
};

namespace boost
{
  std::size_t hash_value(const tier &t)
  {
    return t.get_hash_value();
  }
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

#endif // TIER_H
