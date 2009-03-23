/** \file promotion_set.h */             // -*-c++-*-

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

#ifndef PROMOTION_SET_H
#define PROMOTION_SET_H

#include <algorithm>
#include <list>
#include <map>
#include <set>
#include <vector>

#include <iostream>

#include <generic/util/immset.h>

#include <loggers.h>

#include "choice.h"
#include "choice_set.h"

/** \brief Represents a tier promotion: the knowledge that
 *  a set of choices forces a solution to a higher tier.
 */
template<typename PackageUniverse>
class generic_promotion
{
public:
  typedef generic_choice<PackageUniverse> choice;
  typedef generic_choice_set<PackageUniverse> choice_set;

private:
  choice_set choices;
  int tier;

public:
  generic_promotion()
    : choices(), tier(0)
  {
  }

  /** \brief Create a new promotion. */
  generic_promotion(const choice_set &_choices, int _tier)
    : choices(_choices), tier(_tier)
  {
  }

  const choice_set &get_choices() const { return choices; }
  int get_tier() const { return tier; }

  bool operator<(const generic_promotion &other) const
  {
    if(tier < other.tier)
      return false;
    else if(tier > other.tier)
      return true;
    else
      return choices < other.choices;
  }

  bool operator==(const generic_promotion &other) const
  {
    if(tier != other.tier)
      return false;

    if(!(choices == other.choices))
      return false;

    return true;
  }

  bool operator!=(const generic_promotion &other) const
  {
    if(tier != other.tier)
      return true;

    if(!(choices == other.choices))
      return true;

    return false;
  }
};

template<typename PackageUniverse>
std::ostream &operator<<(std::ostream &out, const generic_promotion<PackageUniverse> &p)
{
  return out << "(T" << p.get_tier() << ": " << p.get_choices() << ")";
}

/** \brief Represents a set of "promotions": mappings from sets of
 *  choices to tiers of the search space.
 *
 *  Wraps up the various customizations of dense_setset for this case.
 *
 *  Requirements for this structure:
 *
 *
 *   1. We want to be able to find a set of choices quickly from a
 *      superset of its elements, and then resolve that set to the
 *      tier it matches (this requires indexing on version OR
 *      dependency, depending on what type of choice we have).
 *
 *   2. We also want to be able to prune the structure of all entries
 *      below a particular tier, or to yank out the contents of a tier
 *      entirely (used when deferrals are removed, or when we advance
 *      to a new tier and want to get rid of cruft from the previous
 *      tier).
 *
 *   3. When a new tier promotion is inserted, it should override any
 *      lower promotions that it is a subset of or equal to, but not
 *      higher ones.  Conversely, if a new tier promotion contains an
 *      existing promotion that has a higher tier, it should not be
 *      inserted.
 *
 *  This object gets some of its efficiencies from actually knowing
 *  the structure of choices (e.g., it indexes choices to break soft
 *  dependencies differently from choices to install versions).
 *
 *  \sa generic_choice, generic_choice_set
 */
template<typename PackageUniverse>
class generic_promotion_set
{
public:
  typedef typename PackageUniverse::package package;
  typedef typename PackageUniverse::version version;
  typedef typename PackageUniverse::dep dep;

  typedef generic_choice<PackageUniverse> choice;
  typedef generic_choice_set<PackageUniverse> choice_set;
  typedef generic_promotion<PackageUniverse> promotion;

private:
  log4cxx::LoggerPtr logger;

  /** \brief The structure used to store information about
   *  a promotion.
   */
  struct entry
  {
    promotion p;

    /** \brief Used when searching for a promotion.
     *
     *  Never copied when entries are copied.
     */
    mutable unsigned int hit_count;

    entry(const promotion &_p)
      : p(_p), hit_count(0)
    {
    }
  };

  /** \brief Stores the promotions that exist, organized by tier.
   *
   *  This map is maintained so that it has no empty entries.

   *  Lists are used so that we can store stable references to
   *  entries.
   */
  std::map<int, std::list<entry> > entries;

  unsigned int num_promotions;

  typedef typename std::list<entry>::const_iterator entry_const_ref;
  typedef typename std::list<entry>::iterator entry_ref;

  /** \brief An entry in the index related to install_version entries.
   *
   *  This stores the choice that generated this index entry (used for
   *  quick comparisons), as well as a reference to the entry object
   *  in one of the main entry lists.
   */
  struct install_version_index_entry
  {
    /** \brief The entries that contain this version, not installed
     *  from the dependency source.
     */
    std::vector<entry_ref> not_from_dep_source_entries;

    /** \brief The entries that contain this version installed from
     *  the dependency source, indexed by the dependency they solved.
     *
     *  Each entry in this map includes all the elements of
     *  not_from_dep_source_entries: when we want to find the indexed
     *  hits for a version that was installed from a dependency
     *  source, we must also include the indexed hits for the
     *  "generic" version.  This is the simplest way to achieve that
     *  without allocating space during the search (instead we
     *  pre-allocate the exact list we'll want for each index
     *  location).  This moves the cost to the point where the
     *  structure is built, and in my off-the-cuff estimation this
     *  should be a win, since the promotion set is a read-mostly
     *  structure.
     */
    std::map<dep, std::vector<entry_ref> > from_dep_source_entries;
  };

  /** \brief The version count used to set up the internal index. */
  int num_versions;

  /** \brief The index of install_version choices.
   *
   *  This is an array of num_versions elements, indexed by version
   *  ID, each of which is a pointer to an index entry.
   *
   *  We use an array of pointers instead of an array of values to
   *  reduce the memory usage.  Each index entry takes 36 bytes on a
   *  32-bit Debian installation as of this writing, and I expect it
   *  would take 72 bytes on a 64-bit installation.  The index entry
   *  array will normally be very sparse, and so this decreases the
   *  space consumption by nearly 90% (to 4 bytes / 8 bytes).  For the
   *  current apt cache, this means that an empty index on a 32-bit
   *  machine consumes just over 200k, rather than just under two
   *  megabytes.
   */
  install_version_index_entry **install_version_index;

  // The index for break_soft_dep choices is (conceptually) just a map
  // that takes a dependency to a list of the entries that contain it.
  typedef std::vector<entry_ref> break_soft_dep_index_entry;

  // And in fact, that's also what it actually is.  We don't use an
  // array here because there are lots of dependencies (e.g., around
  // 180,000 as of this writing), most dependencies are not soft, and
  // I *think* that soft dependencies won't usually participate in
  // promotions.  So saving space is more important than being as fast
  // as possible.
  std::map<dep, break_soft_dep_index_entry> break_soft_dep_index;

public:
  typedef unsigned int size_type;
  size_type size() const { return num_promotions; }
  size_type tier_size(int tier) const
  {
    typename std::map<int, std::list<entry> >::const_iterator found =
      entries.find(tier);
    if(found == entries.end())
      return 0;
    else
      return found->second.size();
  }

  class const_iterator
  {
    // This is the only place where it's awkward to have a
    // map-of-lists be the canonical location where all the entries in
    // this object are stored.
    typename std::map<int, std::list<entry> >::const_iterator entries_it;
    // The end iterator for the map -- necessary so that we know
    // whether it's safe to start walking down the current list.
    typename std::map<int, std::list<entry> >::const_iterator entries_end;

    // The current entry in the current list.
    typename std::list<entry>::const_iterator entry_list_it;

    friend class generic_promotion_set;

    // This overload is used for non-end iterators.
    const_iterator(typename std::map<int, std::list<entry> >::const_iterator _entries_it,
		   typename std::map<int, std::list<entry> >::const_iterator _entries_end,
		   typename std::list<entry>::const_iterator _entry_list_it)
      : entries_it(_entries_it),
	entries_end(_entries_end),
	entry_list_it(_entry_list_it)
    {
      // Should never happen; this is pure defensiveness.
      while(entries_it != entries_end &&
	    entry_list_it == entries_it->second.end())
	{
	  LOG_ERROR(aptitude::Loggers::getAptitudeResolverSearchTiers(), "Empty tier in promotion set!");

	  ++entries_it;
	  if(entries_it != entries_end)
	    entry_list_it = entries_it->second.begin();
	}
    }

    // This overload is used only to create an end iterator.
    const_iterator(typename std::map<int, std::list<entry> >::const_iterator _entries_it,
		   typename std::map<int, std::list<entry> >::const_iterator _entries_end)
      : entries_it(_entries_it),
	entries_end(_entries_end)
    {
    }

  public:
    const_iterator()
    {
    }

    const promotion &operator*() const
    {
      eassert(entries_it != entries_end);

      return entry_list_it->p;
    }

    const promotion *operator->() const
    {
      eassert(entries_it != entries_end);

      return &entry_list_it->p;
    }

    const_iterator &operator++()
    {
      eassert(entries_it != entries_end);

      ++entry_list_it;
      bool first = true;
      while(entries_it != entries_end &&
	    entry_list_it == entries_it->second.end())
	{
	  if(first)
	    first = false;
	  else
	    LOG_ERROR(aptitude::Loggers::getAptitudeResolverSearchTiers(), "Empty tier in promotion set!");

	  ++entries_it;
	  if(entries_it != entries_end)
	    entry_list_it = entries_it->second.begin();
	}
      return *this;
    }

    bool operator==(const const_iterator &other) const
    {
      eassert(entries_end == other.entries_end);

      if(entries_it != other.entries_it)
	return false;
      else if(entries_it == entries_end)
	return true; // Don't compare the list iterators if they're
		     // invalid.
      else
	return entry_list_it == other.entry_list_it;
    }

    bool operator!=(const const_iterator &other) const
    {
      eassert(entries_end == other.entries_end);

      if(entries_it != other.entries_it)
	return true;
      else if(entries_it == entries_end)
	return false; // Don't compare the list iterators if they're
		      // invalid.
      else
	return entry_list_it == other.entry_list_it;
    }
  };

  const_iterator begin() const
  {
    if(entries.empty())
      return end();
    else
      return const_iterator(entries.begin(), entries.end(), entries.begin()->second.begin());
  }

  const_iterator end() const
  {
    return const_iterator(entries.end(), entries.end());
  }

private:
  /** \brief Find the list of index entries associated with the given
   *  choice, or NULL if it is not indexed.
   */
  const std::vector<entry_ref> *find_index_list(const choice &c) const
  {
    switch(c.get_type())
      {
      case choice::install_version:
	{
	  const int id = c.get_ver().get_id();
	  const install_version_index_entry *index_entry = install_version_index[id];

	  if(index_entry != NULL)
	    {
	      if(!c.get_from_dep_source())
		{
		  LOG_TRACE(logger, "find_index_list(" << c
			    << ") returning a list of "
			    << index_entry->not_from_dep_source_entries.size()
			    << " index entries from the not-from-dep-source list.");
		  return &index_entry->not_from_dep_source_entries;
		}
	      else
		{
		  typename std::map<dep, std::vector<entry_ref> >::const_iterator found =
		    index_entry->from_dep_source_entries.find(c.get_dep());

		  if(found != index_entry->from_dep_source_entries.end())
		    {
		      LOG_TRACE(logger, "find_index_list(" << c << ") returning a list of "
				<< found->second.size()
				<< " index entries from the dep-source list for "
				<< found->first);
		      return &found->second;
		    }
		  else
		    {
		      LOG_TRACE(logger, "find_index_list(" << c <<
				") returning a list of "
				<< index_entry->not_from_dep_source_entries.size()
				<< " index entries from the not-from-dep-source-list: there are no from-dep index entries for " << c.get_dep());
		      return &index_entry->not_from_dep_source_entries;
		    }
		}
	    }
	  else
	    {
	      LOG_TRACE(logger, "find_index_list(" << c
			<< ") returning an empty list: there is no index cell for "
			<< c.get_ver());
	      return NULL;
	    }

	  break;
	}

      case choice::break_soft_dep:
	{
	  typename std::map<dep, break_soft_dep_index_entry>::const_iterator found =
	    break_soft_dep_index.find(c.get_dep());
	  if(found == break_soft_dep_index.end())
	    {
	      LOG_TRACE(logger, "find_index_list(" << c
			<< ") returning an empty list: there is no index cell for "
			<< c.get_dep());
	      return NULL;
	    }
	  else
	    {
	      LOG_TRACE(logger, "find_index_list(" << c
			<< ") returning a list of "
			<< found->second.size() << " entries.");
	      return &found->second;
	    }
	}
	break;

      default:
	LOG_ERROR(logger, "tally_intersections: bad choice type " << c.get_type());
	return NULL;
      }
  }

  /** \brief Apply an operation to each entry that matches the input
   * set.
   *
   *  The one catch here is that this isn't a simple subset relation.
   *  We look for sets where the value stored in the promotion
   *  "contains" the value in the input set (or vice versa, depending
   *  on the mode).  So we need to deal with the fact that certain
   *  choices can "contain" other choices.
   *
   *  Currently the only choices that can contain other choices are
   *  version installations.  An install_version choice that is not
   *  from a dependency source contains any install_version choice
   *  that is from a dependency source, but not vice versa.  This
   *  means that, e.g., a search node that installs a version will
   *  always match a promotion that contains an install_version action
   *  for that version that is not from the dependency source --
   *  regardless of whether the search node's version is from a
   *  dependency source.
   *
   *  This function is used when traversing a set of choices to find a
   *  sub- or super-set of a collection of choices.  It is not a
   *  generic traversal function.  In particular, it will abort early
   *  if it detects that there will never be any matching promotions.
   *  (if we're trying to find supersets of an input set and one
   *  element is not matched by anything, we can abort immediately)
   *
   *  The only reason this is generalized is because we use a two pass
   *  algorithm.  In the first pass, we count how many times we
   *  "touch" each entry; in the second pass, we find matching entries
   *  according to the output rule and zero out all the counts we set
   *  in the first pass.  Using a generic traversal function means
   *  that the traversal logic is automatically the same in both
   *  passes.  This also makes it easy to have different "readouts",
   *  so (eg) most of the time we can just return "true, it matched"
   *  or "false, it didn't match", but if we're trying to throw out
   *  conflicts that are redundant with something we inserted, we'll
   *  instead build up a list of the matching conflicts.
   */
  template<typename Op>
  struct traverse_intersections
  {
    const generic_promotion_set &parent;
    // If \b true, we are looking for a subset of the input.  If \b
    // false, we are looking for a superset.  This affects how we
    // compare choices when they are not identical, but one is a
    // "subset" of the other.
    bool subset_mode;
    Op op;

  public:
    traverse_intersections(const generic_promotion_set &_parent,
			   bool _subset_mode,
			   const Op &_op)
      : parent(_parent),
	subset_mode(_subset_mode),
	op(_op)
    {
    }

    /** \brief Extract the operation.
     *
     *  Useful for operations that accumulate information in the
     *  operation structure.
     */
    const Op &get_op() const { return op; }

    /** \brief For all the entries in this structure containing a
     *  choice that contains (if subset_mode is true) or that is
     *  contained by (if subset_mode is false) the choice c, increment
     *  the overlap count stored in the entry.
     */
    bool operator()(const choice &c) const
    {
      // The list of entries that we need to operate on.
      const std::vector<entry_ref> *entries = parent.find_index_list(c);

      if(entries == NULL || entries->empty())
	{
	  // If nothing in this set contains the current choice AND we
	  // are in superset mode, abort early: nothing will ever be a
	  // superset of the input set.
	  if(!subset_mode)
	    {
	      LOG_DEBUG(parent.logger, "traverse_intersections: breaking out of set traversal at "
			<< c << " because nothing matches it and we are looking for a superset.");
	      return false;
	    }
	  else
	    return true;
	}
      else
	{
	  for(typename std::vector<entry_ref>::const_iterator it = entries->begin();
	      it != entries->end(); ++it)
	    if(!op(*it))
	      return false;

	  return true;
	}
    }
  };

  struct increment_entry_count_op
  {
    log4cxx::LoggerPtr logger;

    increment_entry_count_op(const log4cxx::LoggerPtr &_logger)
      : logger(_logger)
    {
    }

    bool operator()(entry_ref r) const
    {
      LOG_TRACE(logger, "increment_entry_count: incrementing the hit count for " << r->p);
      ++r->hit_count;

      return true;
    }
  };

  // To find a subset of an input set S, we find an entry that is hit
  // by S exactly as many times as it has elements.  This works
  // because we assume that it's impossible for an element of S to hit
  // two distinct elements (no two choices in a search node can match
  // each other).
  //
  // This returns an arbitrary element that has the highest possible
  // search tier.  We don't return all elements because we don't need
  // to (this represents testing whether a set matches an existing
  // promotion).  NB: the only reason for returning a promotion
  // pointer rather than a boolean is so that we can provide
  // diagnostic logging.
  struct find_entry_subset_op
  {
    // The return value.  Needs to be mutable because the traversal
    // routine uses a const reference.  (I think maybe it shouldn't
    // use a const reference: that should be fixed)
    mutable entry_ref return_value_ref;
    // Set to true if the return value is meaningful.  Mutable for the
    // same reason as return_value_ref.
    mutable bool was_set;
    log4cxx::LoggerPtr logger;

    find_entry_subset_op(const log4cxx::LoggerPtr &_logger)
      : was_set(false), logger(_logger)
    {
    }

    bool operator()(entry_ref r) const
    {
      if(r->hit_count > 0)
	{
	  if(r->hit_count == r->p.get_choices().size())
	    {
	      if(!was_set ||
		 return_value_ref->p.get_tier() < r->p.get_tier())
		{
		  if(was_set)
		    LOG_DEBUG(logger, "find_entry_subset_op: resetting the hit count for "
			      << r->p << " to 0 and returning it (highest tier: "
			      << return_value_ref->p.get_tier() << " -> " << r->p.get_tier());
		  else
		    LOG_DEBUG(logger, "find_entry_subset_op: resetting the hit count for "
			      << r->p << " to 0 and returning it (new highest tier: "
			      << r->p.get_tier());

		  return_value_ref = r;
		  was_set = true;
		}
	      else
		LOG_TRACE(logger, "find_entry_subset_op: resetting the hit count for "
			  << r->p << " to 0, but not returning it, because its tier "
			  << r->p.get_tier() << " is lower than the current highest tier "
			  << return_value_ref->p.get_tier());
	    }
	  else
	    LOG_TRACE(logger, "find_entry_subset_op: " << r->p
		      << " is not matched (needed " << r->p.get_choices().size()
		      << " hits, but got only " << r->hit_count);

	  r->hit_count = 0;
	}
      else
	// Note that if the hit-count is 0, we must have already
	// processed this entry.
	LOG_TRACE(logger, "find_entry_subset_op: skipping already processed promotion " << r->p);

      return true;
    }
  };

  /** \brief Retrieve all the supersets of the input set: entries that
   *  contain elements which are all contained within entries of the
   *  input set, and whose tiers are not higher than the input set's
   *  tier.
   *
   *  As a side effect, resets all hit counts to 0.
   *
   *  This is used to purge redundant entries when a new entry is
   *  inserted.
   */
  struct find_entry_supersets_op
  {
    // Where to store the output entry references.
    std::vector<entry_ref> &output_entries;
    // How many hits to require from each entry we process.
    unsigned int required_hits;
    // The maximum tier to return; entries with a higher tier will be
    // ignored.
    int maximum_tier;
    log4cxx::LoggerPtr logger;

    /** \brief Create a find-supersets operation.
     *
     *  \param _output_entries   The location in which to place the
     *                           supersets that are found.
     *  \param _required_hits    The number of elements in the set
     *                           that we are searching for supersets
     *                           of; only entries with this many hits
     *                           are returned.
     *  \param _maximum_tier     The maximum tier to examine; only entries
     *                           at this tier or lower are returned.
     *  \param _logger           The logger to use to write messages
     *                           about this process.
     */
    find_entry_supersets_op(std::vector<entry_ref> &_output_entries,
			    unsigned int _required_hits,
			    int _maximum_tier,
			    const log4cxx::LoggerPtr &_logger)
      : output_entries(_output_entries),
	required_hits(_required_hits),
	maximum_tier(_maximum_tier),
	logger(_logger)
    {
    }

    bool operator()(entry_ref r) const
    {
      if(r->hit_count > 0)
	{
	  if(r->p.get_tier() > maximum_tier)
	    {
	      LOG_INFO(logger, "find_entry_supersets_op: resetting the hit count for "
		       << r->p << " to 0, but not returning it, because its tier "
		       << r->p.get_tier() << " is above the maximum tier "
		       << maximum_tier);
	    }
	  else if(r->hit_count == required_hits)
	    {
	      LOG_DEBUG(logger, "find_entry_supersets_op: resetting the hit count for " << r->p << " to 0 and adding it to the output list.");
	      output_entries.push_back(r);
	    }
	  else
	    {
	      if(r->hit_count > required_hits)
		// Something is quite wrong if this happens.
		LOG_ERROR(logger, "find_entry_supersets_op: resetting the hit count for " << r->p << " to 0, but not returning it: invalid hit count " << r->hit_count << " (expected maximum of " << required_hits << ")");
	      else
		LOG_TRACE(logger, "find_entry_supersets_op: resetting the hit count for " << r->p << " to 0, but not returning it: it only has " << r->hit_count << " hits (needed " << required_hits << ")");
	    }

	  r->hit_count = 0;
	}
      else
	LOG_TRACE(logger, "find_entry_supersets_op: skipping already processed promotion " << r->p);

      return true;
    }
  };

public:
  /** \brief Find a highest tier promotion that is a subset of the
   *  given set of choices.
   *
   *  Implements requirement (1).
   */
  const_iterator find_highest_promotion_for(const choice_set &choices) const
  {
    LOG_TRACE(logger, "Entering find_highest_promotion_for(" << choices << ")");

    traverse_intersections<increment_entry_count_op>
      increment_f(*this, true, increment_entry_count_op(logger));
    traverse_intersections<find_entry_subset_op>
      find_result_f(*this, true, find_entry_subset_op(logger));
    const find_entry_subset_op &find_result(find_result_f.get_op());

    choices.for_each(increment_f);
    // We have to run this even if the increment aborted, since we
    // need to reset all the counters to 0 for the next run.
    choices.for_each(find_result_f);

    if(!find_result.was_set)
      return end();
    else
      {
	const entry_ref result_entry(find_result.return_value_ref);
	const int tier = result_entry->p.get_tier();
	typename std::map<int, std::list<entry> >::const_iterator tier_found =
	  entries.find(tier);
	if(tier_found == entries.end())
	  {
	    LOG_ERROR(logger, "Unable to find tier " << tier << " even though we just found an entry in it!");
	    return end();
	  }
	else
	  {
	    LOG_TRACE(logger, "find_highest_promotion_for: For the set " << choices << ", returning an iterator to " << result_entry->p);
	    // Assume that result_entry is in this list, since if it
	    // isn't, something is very wrong.
	    return const_iterator(tier_found, entries.end(), result_entry);
	  }
      }
  }

private:
  // find_highest_promotion_containing helpers:

  /** \brief Used to build the local indices for
   *  find_highest_promotion_containing().
   */
  struct build_local_indices
  {
    // Maps versions to choices associated with installing those
    // versions.
    std::map<version, choice> &choices_by_install_version;

    // Stores the set of broken soft dependencies.
    std::set<dep> &broken_soft_deps;

    log4cxx::LoggerPtr logger;

    build_local_indices(std::map<version, choice> &_choices_by_install_version,
			std::set<dep> &_broken_soft_deps,
			const log4cxx::LoggerPtr &_logger)
      : choices_by_install_version(_choices_by_install_version),
	broken_soft_deps(_broken_soft_deps),
	logger(_logger)
    {
    }

    bool operator()(const choice &c) const
    {
      switch(c.get_type())
	{
	case choice::install_version:
	  LOG_TRACE(logger, "Adding entry to the local index: "
		    << c.get_ver() << " |-> " << c);
	  choices_by_install_version[c.get_ver()] = c;
	  break;

	case choice::break_soft_dep:
	  LOG_TRACE(logger, "Adding broken soft dep to the local index: "
		    << c.get_dep());
	  broken_soft_deps.insert(c.get_dep());
	  break;

	default:
	  LOG_ERROR(logger, "Bad choice type " << c.get_type());
	  break;
	}

      return true;
    }
  };

  /** \brief Used to check whether a set of choices matches the values
   *  stored in the local indices.
   */
  struct all_choices_in_local_indices
  {
    /** \brief Set to true if all the choices were found, and false
     *  otherwise.
     */
    mutable bool rval;

    // Maps versions to choices associated with installing those
    // versions.
    const std::map<version, choice> &choices_by_install_version;

    // Stores the set of broken soft dependencies.
    const std::set<dep> &broken_soft_deps;

    log4cxx::LoggerPtr logger;

    all_choices_in_local_indices(const std::map<version, choice> &_choices_by_install_version,
				 const std::set<dep> &_broken_soft_deps,
				 const log4cxx::LoggerPtr &_logger)
      : rval(true),
	choices_by_install_version(_choices_by_install_version),
	broken_soft_deps(_broken_soft_deps),
	logger(_logger)
    {
    }

    bool operator()(const choice &c) const
    {
      switch(c.get_type())
	{
	case choice::install_version:
	  {
	    typename std::map<version, choice>::const_iterator found =
	      choices_by_install_version.find(c.get_ver());

	    bool ok = false;
	    if(found == choices_by_install_version.end())
	      LOG_TRACE(logger, "The choice " << c << " is not in the local indices: " << c.get_ver() << " is not in the install version index.");
	    // OK, check that this choice contains the corresponding
	    // choice in the index.  (remember, the index is built
	    // from the set that we are finding a subset of)
	    else if(!c.get_from_dep_source())
	      // Installations not from the dep source match anything.
	      ok = true;
	    else
	      {
		const choice &found_c = found->second;
		if(!found_c.get_from_dep_source())
		  LOG_TRACE(logger, "The choice " << c << " is not in the local indices: the corresponding version entry " << found_c << " is not from a dep source.");
		else if(found_c.get_dep() != c.get_dep())
		  LOG_TRACE(logger, "The choice " << c << " is not in the local indices: the corresponding version entry " << found_c << " is from a different dep.");
		else
		  ok = true;
	      }

	    if(ok)
	      LOG_TRACE(logger, "The choice " << c << " contains the input choice " << found->second);
	    else
	      rval = false;
	  }

	  break;

	case choice::break_soft_dep:
	  {
	    typename std::set<dep>::const_iterator found =
	      broken_soft_deps.find(c.get_dep());

	    if(found == broken_soft_deps.end())
	      LOG_TRACE(logger, "The choice " << c << " is not in the local indices: " << c.get_dep() << " is not in the list of broken soft deps.");
	    else
	      LOG_TRACE(logger, "The choice " << c << " contains the input choice " << *found);
	  }

	  break;

	default:
	  LOG_ERROR(logger, "Bad choice type " << c.get_type());
	  rval = false;
	  break;
	}

      return rval;
    }
  };

public:
  /** \brief Find a highest tier promotion that is a subset of the
   *  given set of choices *and* that contains the given choice.
   *
   *  Implements requirement (1).
   */
  const_iterator find_highest_promotion_containing(const choice_set &choices,
						   const choice &c) const
  {
    LOG_TRACE(logger, "Entering find_highest_promotion_containing(" << choices << ", " << c << ")");

    const std::vector<entry_ref> *index_entries = find_index_list(c);

    if(index_entries == NULL || index_entries->empty())
      {
	LOG_TRACE(logger, "find_highest_promotion_containing: There are no index entries for " << c << "; returning an end iterator.");
	return end();
      }
    else
      {
	// Build local indices, used to make it reasonable to compare all
	// the promotions in index_entries to the input choice list.

	std::map<version, choice> choices_by_install_version;
	std::set<dep> broken_soft_deps;
	build_local_indices build_indices_f(choices_by_install_version,
					    broken_soft_deps, logger);


	LOG_TRACE(logger, "find_highest_promotion_containing: Building local index.");
	choices.for_each(build_indices_f);


	LOG_TRACE(logger, "find_highest_promotion_containing: Matching indexed entries for " << c << " to the local index.");

	bool found_anything = false;
	entry_ref highest_entry;
	for(typename std::vector<entry_ref>::const_iterator it = index_entries->begin();
	    it != index_entries->end(); ++it)
	  {
	    all_choices_in_local_indices
	      all_choices_found_f(choices_by_install_version,
				  broken_soft_deps, logger);

	    const promotion &p((*it)->p);
	    p.get_choices().for_each(all_choices_found_f);
	    if(all_choices_found_f.rval)
	      {
		if(!found_anything)
		  {
		    LOG_TRACE(logger, "find_highest_promotion_containing: found the first match: " << p);
		    found_anything = true;
		    highest_entry = *it;
		  }
		else if(highest_entry->p.get_tier() >= p.get_tier())
		  LOG_TRACE(logger, "find_highest_promotion_containing: found a match " << p
			    << ", but its tier is lower than the existing match ("
			    << p.get_tier() << " vs " << highest_entry->p.get_tier());
		else
		  {
		    LOG_TRACE(logger, "find_highest_promotion_containing: found a new highest match: " << p
			      << " (previous tier was " << highest_entry->p.get_tier());
		    highest_entry = *it;
		  }		  
	      }
	  }

	if(!found_anything)
	  {
	    LOG_TRACE(logger, "find_highest_promotion_containing: no matches found; returning an end iterator.");
	    return end();
	  }
	else
	  {
	    const int tier = highest_entry->p.get_tier();
	    typename std::map<int, std::list<entry> >::const_iterator tier_found =
	      entries.find(tier);

	    if(tier_found == entries.end())
	      {
		LOG_ERROR(logger, "Unable to find tier " << tier << " even though we just found an entry in it!");
		return end();
	      }
	    else
	      {
		LOG_TRACE(logger, "find_highest_promotion_containing: returning a reference to " << highest_entry->p);
		return const_iterator(tier_found, entries.end(), highest_entry);
	      }
	  }
      }
  }

private:
  /** \brief Retrieve all the promotions that are supersets of the
   * given promotion.
   *
   *  This is used internally to purge redundant entries when adding a
   *  new entry.  The goal is to find all the entries that are not at
   *  a higher tier, and for which every set of choices that contained
   *  them would also contain the new entry.  (i.e., removing those
   *  entries has no effect on the tiers assigned to search nodes,
   *  because the new entry assigns the same nodes an equal or higher
   *  tier)
   *
   *  \param p      The promotion whose supersets should be returned.
   *  \param output A vector in which to store the results.
   */
  void find_superseded_entries(const promotion &p,
			       std::vector<entry_ref> &output) const
  {
    traverse_intersections<increment_entry_count_op>
      increment_f(*this, false, increment_entry_count_op(logger));
    traverse_intersections<find_entry_supersets_op>
      find_results_f(*this, false,
		     find_entry_supersets_op(output,
					     p.get_choices().size(),
					     p.get_tier(),
					     logger));

    const choice_set &choices(p.get_choices());
    choices.for_each(increment_f);
    choices.for_each(find_results_f);
  }

  /** \brief Predicate testing whether an entry reference refers to
   *  something in a particular tier.
   */
  struct entry_ref_in_tier_pred
  {
    int tier;

    entry_ref_in_tier_pred(int _tier)
      : tier(_tier)
    {
    }

    bool operator()(entry_ref r) const
    {
      return r->p.get_tier() == tier;
    }
  };

  /** \brief Predicate testing whether an entry reference is strictly
   *  below a particular tier.
   */
  struct entry_ref_strictly_below_tier_pred
  {
    int tier;

    entry_ref_strictly_below_tier_pred(int _tier)
      : tier(_tier)
    {
    }

    bool operator()(entry_ref r) const
    {
      return r->p.get_tier() < tier;
    }
  };

  /** \brief Collect the versions and soft dependencies related
   *  to a single choice.
   */
  static void collect_indexers(const choice &c,
			       std::set<version> &installed_versions,
			       std::set<dep> &broken_soft_deps,
			       const log4cxx::LoggerPtr &logger)
  {
    switch(c.get_type())
      {
      case choice::install_version:
	LOG_TRACE(logger, "collect_indexers: adding " << c.get_ver() << " to the set of installed versions.");
	installed_versions.insert(c.get_ver());
	break;

      case choice::break_soft_dep:
	LOG_TRACE(logger, "collect_indexers: adding " << c.get_dep() << " to the set of broken soft deps.");
	broken_soft_deps.insert(c.get_dep());
	break;

      default:
	LOG_ERROR(logger, "collect_indexers: bad choice type " << c.get_type());
	break;
      }
  }

  /** \brief Function object that invokes collect_indexers() on a
   *  choice object.
   */
  struct do_collect_indexers
  {
    std::set<version> &installed_versions;
    std::set<dep> &broken_soft_deps;
    log4cxx::LoggerPtr logger;

    do_collect_indexers(std::set<version> &_installed_versions,
			std::set<dep> &_broken_soft_deps,
			const log4cxx::LoggerPtr &_logger)
      : installed_versions(_installed_versions),
	broken_soft_deps(_broken_soft_deps),
	logger(_logger)
    {
    }

    bool operator()(const choice &c) const
    {
      collect_indexers(c, installed_versions, broken_soft_deps, logger);
      return true;
    }
  }; 

  /** \brief Collect the versions and soft dependencies related
   *  to a single entry.
   */
  static void collect_indexers(const entry &e,
			       std::set<version> &installed_versions,
			       std::set<dep> &broken_soft_deps,
			       const log4cxx::LoggerPtr &logger)
  {
    e.p.get_choices().for_each(do_collect_indexers(installed_versions,
						   broken_soft_deps,
						   logger));
  }

  /** \brief Collect the versions and soft dependencies related to
   *  each choice in a tier.
   */
  static void collect_indexers(const std::list<entry> &tier_entries,
			       std::set<version> &installed_versions,
			       std::set<dep> &broken_soft_deps,
			       const log4cxx::LoggerPtr &logger)
  {
    for(typename std::list<entry>::const_iterator it = tier_entries.begin();
	it != tier_entries.end(); ++it)
      collect_indexers(*it, installed_versions, broken_soft_deps, logger);
  }

  /** \brief Drop entries from the given vector, using the given
   *  predicate to decide which ones to drop.
   */
  template<typename Pred>
  static void erase_vector_entries(std::vector<entry_ref> &entries,
				   const log4cxx::LoggerPtr &logger,
				   const Pred &pred)
  {
    if(LOG4CXX_UNLIKELY(logger->isTraceEnabled()))
      {
	for(typename std::vector<entry_ref>::const_iterator it =
	      entries.begin(); it != entries.end(); ++it)
	  if(pred(*it))
	    LOG_TRACE(logger, "  Removing " << (*it)->p);
      }

    typename std::vector<entry_ref>::iterator new_end = std::remove_if(entries.begin(), entries.end(), pred);
    entries.erase(new_end, entries.end());
  }

  /** \brief Remove all the promotion index entries for the given
   *  set of installed versions.
   *
   *  \tparam Pred The predicate type to use in deciding
   *               which entries to drop.
   *  \param pred  The predicate to use in deciding
   *               which entries to drop.
   */
  template<typename Pred>
  void drop_install_version_index_entries(const std::set<version> &installed_versions,
					  const Pred &pred)
  {
    for(typename std::set<version>::const_iterator it = installed_versions.begin();
	it != installed_versions.end(); ++it)
      {
	install_version_index_entry *index_entry(install_version_index[it->get_id()]);

	if(index_entry != NULL)
	  {
	    LOG_TRACE(logger, "Purging dead references from the index entries for " << *it << ":");
	    erase_vector_entries(index_entry->not_from_dep_source_entries,
				 logger, pred);
	    bool from_dep_source_map_empty = true;
	    for(typename std::map<dep, std::vector<entry_ref> >::iterator
		  from_dep_source_it
		  = index_entry->from_dep_source_entries.begin();
		from_dep_source_it !=
		  index_entry->from_dep_source_entries.end();
		++from_dep_source_it)
	      {
		erase_vector_entries(from_dep_source_it->second, logger, pred);
		if(!from_dep_source_it->second.empty())
		  from_dep_source_map_empty = false;
	      }

	    if(index_entry->not_from_dep_source_entries.empty() &&
	       from_dep_source_map_empty)
	      {
		LOG_DEBUG(logger, "All index entries for " << *it
			  << " have been removed; dropping the index cell.");
		delete index_entry;
		install_version_index[it->get_id()] = NULL;
	      }
	  }
	else
	  LOG_ERROR(logger, "The version " << *it << " didn't actually have index entries, but it should have.");
      }
  }

  template<typename Pred>
  void drop_broken_soft_dep_index_entries(const std::set<dep> &broken_soft_deps,
					  const Pred &pred)
  {
    for(typename std::set<dep>::iterator it = broken_soft_deps.begin();
	it != broken_soft_deps.end(); ++it)
      {
	typename std::map<dep, break_soft_dep_index_entry>::iterator
	  found = break_soft_dep_index.find(*it);

	if(found == break_soft_dep_index.end())
	  // Indicates an inconsistency in the book-keeping.
	  LOG_ERROR(logger, "Unable to find an index list for " << *it << ", but one should exist.");
	else
	  {
	    LOG_TRACE(logger, "Purging dead references from the index entries for " << *it << ":");
	    std::vector<entry_ref> &index_entries = found->second;
	    erase_vector_entries(index_entries, logger, pred);

	    if(index_entries.empty())
	      {
		LOG_DEBUG(logger, "All index entries for " << *it
			  << " have been removed; dropping the index cell.");
		break_soft_dep_index.erase(found);
	      }
	  }
      }
  }

public:
  /** \brief Remove all the promotions associated with the given tier.
   *
   *  Implements requirement (2).
   */
  void remove_tier(int tier)
  {
    LOG_DEBUG(logger, "Removing all promotions in tier " << tier);

    typename std::map<int, std::list<entry> >::iterator found =
      entries.find(tier);

    if(found == entries.end())
      LOG_TRACE(logger, "Not removing tier " << tier << ": it is empty.");
    else
      {
	// Collect the versions and soft dependencies related to each
	// choice in the tier.
	std::set<version> installed_versions;
	std::set<dep> broken_soft_deps;

	collect_indexers(found->second,
			 installed_versions,
			 broken_soft_deps,
			 logger);

	// Now zap all the index entries in this tier.
	drop_install_version_index_entries(installed_versions,
					   entry_ref_in_tier_pred(tier));
	drop_broken_soft_dep_index_entries(broken_soft_deps,
					   entry_ref_in_tier_pred(tier));

	int num_promotions_erased = found->second.size();

	// Delete the tier itself.
	entries.erase(found);
	num_promotions -= num_promotions_erased;

	LOG_TRACE(logger, "Removed tier " << tier << ", dropping " << num_promotions_erased << " promotions.");
      }
  }

  /** \brief Remove all the promotions below the given tier.
   *
   *  Implements requirement (2).
   */
  void remove_below_tier(int tier)
  {
    LOG_DEBUG(logger, "Removing all promotions below tier " << tier);

    std::set<version> installed_versions;
    std::set<dep> broken_soft_deps;

    int num_promotions_erased = 0;

    // This is set to one past the last iterator in this tier, so we
    // can use it to delete all the dropped tiers.  We don't use
    // upper_bound() because we have to examine each deleted tier to
    // (a) update the size correctly, and (b) collect the index
    // locations that need to be fixed up.
    typename std::map<int, std::list<entry> >::iterator delete_end;
    for(typename std::map<int, std::list<entry> >::iterator it =
	  entries.begin(), it_next = it;
	it != entries.end() && it->first < tier;
	it = it_next)
      {
	++it_next;
	delete_end = it_next;

	num_promotions_erased += it->second.size();

	collect_indexers(it->second,
			 installed_versions,
			 broken_soft_deps,
			 logger);
      }

    if(delete_end == entries.begin())
      LOG_TRACE(logger, "No entries below tier " << tier << " to remove.");
    else
      {
	drop_install_version_index_entries(installed_versions,
					   entry_ref_strictly_below_tier_pred(tier));
	drop_broken_soft_dep_index_entries(broken_soft_deps,
					   entry_ref_strictly_below_tier_pred(tier));

	// Delete the tiers in question.
	entries.erase(entries.begin(), delete_end);
	num_promotions -= num_promotions_erased;

	LOG_TRACE(logger, "Deleted all tiers below " << tier << ", dropping " << num_promotions_erased << " promotions.");
      }
  }

private:
  /** \brief Function object that inserts choices into the index
   *  structures one at a time.
   */
  struct make_index_entries
  {
    // A reference to the newly inserted promotion.
    entry_ref new_entry;
    install_version_index_entry **install_version_index;
    std::map<dep, break_soft_dep_index_entry> &break_soft_dep_index;
    log4cxx::LoggerPtr logger;

    make_index_entries(entry_ref _new_entry,
		       install_version_index_entry **_install_version_index,
		       std::map<dep, break_soft_dep_index_entry> &_break_soft_dep_index,
		       const log4cxx::LoggerPtr &_logger)
      : new_entry(_new_entry),
	install_version_index(_install_version_index),
	break_soft_dep_index(_break_soft_dep_index),
	logger(_logger)
    {
    }

    bool operator()(const choice &c) const
    {
      switch(c.get_type())
	{
	case choice::install_version:
	  if(!c.get_from_dep_source())
	    LOG_TRACE(logger, "Inserting an index entry: " << c.get_ver()
		      << " |-> " << new_entry->p);
	  else
	    LOG_TRACE(logger, "Inserting an index entry: " << c.get_ver()
		      << "[" << c.get_dep() << "] |-> " << new_entry->p);
	  {
	    const int id = c.get_ver().get_id();
	    install_version_index_entry *index_entry = install_version_index[id];
	    if(index_entry == NULL)
	      {
		LOG_DEBUG(logger, "Creating a new index cell for " << c.get_ver());
		index_entry = new install_version_index_entry;
		install_version_index[id] = index_entry;
	      }

	    if(!c.get_from_dep_source())
	      {
		LOG_TRACE(logger, "Inserting " << c << " into the not-from-dep-source-list.");
		index_entry->not_from_dep_source_entries.push_back(new_entry);
		for(typename std::map<dep, std::vector<entry_ref> >::iterator
		      from_dep_source_it = index_entry->from_dep_source_entries.begin();
		    from_dep_source_it != index_entry->from_dep_source_entries.end();
		    ++from_dep_source_it)
		  {
		    LOG_TRACE(logger, "Inserting " << c << " into the from-dep-source list for " << from_dep_source_it->first << ".");
		    from_dep_source_it->second.push_back(new_entry);
		  }
	      }
	    else
	      {
		typename std::map<dep, std::vector<entry_ref> >::iterator found =
		  index_entry->from_dep_source_entries.find(c.get_dep());

		if(found == index_entry->from_dep_source_entries.end())
		  {
		    LOG_DEBUG(logger, "Creating a new from-dep index cell for " << c.get_dep());
		    found = index_entry->from_dep_source_entries.insert(found, std::make_pair(c.get_dep(), std::vector<entry_ref>()));
		    // Make sure the new cell contains all the entries
		    // in the not-from-dep-source list.
		    found->second.insert(found->second.end(),
					 index_entry->not_from_dep_source_entries.begin(),
					 index_entry->not_from_dep_source_entries.end());
		  }

		LOG_TRACE(logger, "Inserting " << c << " into the from-dep-source-list.");
		found->second.push_back(new_entry);
	      }
	  }
	  break;

	case choice::break_soft_dep:
	  LOG_TRACE(logger, "Inserting an index entry: "
		    << c.get_dep() << " |-> " << new_entry->p);
	  {
	    const dep &d(c.get_dep());
	    // We could just do a straightforward insertion, but doing
	    // things this way lets us provide better debug traces
	    // (more info about when memory is being allocated).
	    typename std::map<dep, break_soft_dep_index_entry>::iterator found =
	      break_soft_dep_index.find(d);

	    if(found == break_soft_dep_index.end())
	      {
		LOG_DEBUG(logger, "Creating a new index cell for " << d);
		found = break_soft_dep_index.insert(found,
						    std::make_pair(d, break_soft_dep_index_entry()));
	      }

	    found->second.push_back(new_entry);
	  }
	  break;

	default:
	  LOG_ERROR(logger, "Bad choice type " << c.get_type());
	  break;
	}

      return true;
    }
  };

  /** \brief Predicate testing whether an entry reference is in 
   *  a set of dropped entries.
   */
  struct entry_ref_in_dropped_set_pred
  {
    const std::set<entry *> &dropped_set;

    entry_ref_in_dropped_set_pred(const std::set<entry *> &_dropped_set)
      : dropped_set(_dropped_set)
    {
    }

    bool operator()(entry_ref r) const
    {
      return dropped_set.find(&*r) != dropped_set.end();
    }
  };

public:
  /** \brief Insert a promotion into this set.
   *
   *  The promotion will not be inserted if an existing promotion of
   *  the same tier or higher is a subset of it; otherwise, it will be
   *  inserted and all existing promotions of the same tier or lower
   *  that are supersets of the new promotion will be removed.
   */
  void insert(const promotion &p)
  {
    const int tier = p.get_tier();
    const choice_set &choices = p.get_choices();

    LOG_DEBUG(logger, "Inserting " << p << " into the promotion set.");

    const const_iterator highest(find_highest_promotion_for(choices));
    if(highest != end() && highest->get_tier() >= tier)
      LOG_TRACE(logger, "Canceling the insertion of " << p << ": it is redundant with the existing promotion " << *highest);
    else
      {
	std::vector<entry_ref> superseded_entries;
	find_superseded_entries(p, superseded_entries);

	if(!superseded_entries.empty())
	{
	  // Purge the index entries associated with these superseded
	  // entries.
	  std::set<version> installed_versions;
	  std::set<dep> broken_soft_deps;

	  for(typename std::vector<entry_ref>::const_iterator it = superseded_entries.begin();
	      it != superseded_entries.end(); ++it)
	    collect_indexers(**it, installed_versions, broken_soft_deps, logger);

	  LOG_TRACE(logger, "Removing index entries associated with the superseded entries.");
	  // Build a set of pointers that we'll use to figure out
	  // which references-to-entries need to be dropped.
	  //
	  // Note: This relies on knowing that pointers to entries are
	  // stable as long as we don't modify the lists that contain
	  // them.
	  std::set<entry *> superseded_entries_set;
	  for(typename std::vector<entry_ref>::const_iterator it = superseded_entries.begin();
	      it != superseded_entries.end(); ++it)
	    superseded_entries_set.insert(&**it);
	  entry_ref_in_dropped_set_pred dropped_f(superseded_entries_set);
	  drop_install_version_index_entries(installed_versions,
					     dropped_f);
	  drop_broken_soft_dep_index_entries(broken_soft_deps,
					     dropped_f);
	}

	LOG_TRACE(logger, "Removing the superseded entries themselves.");
	for(typename std::vector<entry_ref>::const_iterator it = superseded_entries.begin();
	    it != superseded_entries.end(); ++it)
	  {
	    entry_ref ent(*it);
	    LOG_TRACE(logger, "Removing " << ent->p);
	    const int tier = ent->p.get_tier();
	    typename std::map<int, std::list<entry> >::iterator found =
	      entries.find(tier);

	    if(found == entries.end())
	      LOG_ERROR(logger, "Tier " << tier << " has gone missing!");
	    else
	      {
		typename std::list<entry>::size_type initial_size(found->second.size());
		found->second.erase(ent);
		if(found->second.size() + 1 != initial_size)
		  LOG_ERROR(logger, "Inconsistency after removing entry: the size should have decreased to " << initial_size - 1 << ", but instead it is now " << found->second.size());

		if(found->second.empty())
		  {
		    LOG_DEBUG(logger, "Tier " << tier << " is empty, deleting it.");
		    entries.erase(found);
		  }
	      }
          }
        num_promotions -= superseded_entries.size();

	LOG_TRACE(logger, "Inserting " << p << " into tier " << tier);

	// Find the tier list.  Use a roundabout means instead of
	// operator[] so we can log when we allocate a new tier.
	typename std::map<int, std::list<entry> >::iterator tier_found = entries.find(tier);

	if(tier_found == entries.end())
	  {
	    LOG_DEBUG(logger, "Allocating initial structures for tier " << tier);
	    tier_found = entries.insert(tier_found, std::make_pair(tier, std::list<entry>()));
	  }

	// Insert the new entry into the list of entries in this tier.
	std::list<entry> &tier_entries = tier_found->second;
	const entry_ref new_entry =
	  tier_entries.insert(tier_entries.end(), p);
	++num_promotions;

	LOG_TRACE(logger, "Building index entries for " << p);
	p.get_choices().for_each(make_index_entries(new_entry,
						    install_version_index,
						    break_soft_dep_index,
						    logger));
      }
  }

  generic_promotion_set(const PackageUniverse &u)
    : logger(aptitude::Loggers::getAptitudeResolverSearchTiers()),
      num_promotions(0),
      num_versions(u.get_version_count()),
      install_version_index(new install_version_index_entry*[num_versions])
  {
    for(int i = 0; i < num_versions; ++i)
      install_version_index[i] = NULL;
  }

  ~generic_promotion_set()
  {
    for(int i = 0; i < num_versions; ++i)
      delete install_version_index[i];
    delete[] install_version_index;
  }
};

template<typename PackageUniverse>
std::ostream &operator<<(std::ostream &out, const generic_promotion_set<PackageUniverse> &s)
{
  out << "{";
  for(typename generic_promotion_set<PackageUniverse>::const_iterator it =
	s.begin(); it != s.end(); ++it)
    {
      if(it != s.begin())
	out << ", ";
      out << *it;
    }
  out << "}";

  return out;
}

#endif
