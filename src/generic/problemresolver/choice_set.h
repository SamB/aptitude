/** \file choice_set.h */ // -*-c++-*-

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

#ifndef CHOICE_SET_H
#define CHOICE_SET_H

#include "choice.h"

#include <loggers.h>

#include <iostream>

#include <boost/unordered_map.hpp>
#include <boost/unordered_set.hpp>

template<typename PackageUniverse> class generic_choice_set;
template<typename PackageUniverse>
std::ostream &operator<<(std::ostream &out, const generic_choice_set<PackageUniverse> &choices);

/** \brief Stores a set of choices, with the ability to quickly answer
 *  questions about containment.
 *
 *  If several choices overlap, only the most specific choice is
 *  stored.  The reason for this is that this structure is used to
 *  store information of the form "if ever entry in this set of
 *  choices was chosen, then X".  The only way to choose both a more
 *  general option and a more specific one is to take the more
 *  specific one; hence, it makes no sense to include both of them.
 *  To emphasize this behavior, the "insert" routine is called
 *  "insert_or_narrow".  An "insert_widen" could be written without
 *  conflict; it just happens not to be needed right now.
 *
 *  This object requires that the choices form a coherent installation
 *  set.  In particular, two install_version choices that install
 *  different versions for the same package may not coexist.
 *  Furthermore, if two choices install the same version, one of the
 *  choices must contain the other.  The choice sets generated during
 *  dependency resolution all have these properties "automatically".
 *  (these restrictions could be lifted, at the cost of making this
 *  code a bit more complex and possibly slower; you'd need to
 *  introduce a map from dependencies to choices and store one of
 *  those for each version)
 *
 *  One set of choices contains another set if each choice in the
 *  first set is contained by a choice in the second set.  This class
 *  uses its knowledge of the structure of choices to accelerate that
 *  test.
 */
template<typename PackageUniverse>
class generic_choice_set
{
public:
  typedef generic_choice<PackageUniverse> choice;

private:
  typedef typename PackageUniverse::package package;
  typedef typename PackageUniverse::version version;
  typedef typename PackageUniverse::dep dep;

public:
  /** \brief A slow but convenient way to iterate over this set. */
  class const_iterator
  {
    friend class generic_choice_set;

    typename boost::unordered_map<package, choice>::const_iterator install_version_iter;
    typename boost::unordered_map<package, choice>::const_iterator install_version_end_iter;
    typename boost::unordered_set<choice>::const_iterator not_install_version_iter;

    const_iterator(const typename boost::unordered_map<package, choice>::const_iterator _install_version_iter,
		   const typename boost::unordered_map<package, choice>::const_iterator _install_version_end_iter,
		   const typename boost::unordered_set<choice>::const_iterator _not_install_version_iter)
      : install_version_iter(_install_version_iter),
	install_version_end_iter(_install_version_end_iter),
	not_install_version_iter(_not_install_version_iter)
    {
    }

  public:
    bool operator==(const const_iterator &other) const
    {
      return install_version_iter == other.install_version_iter &&
	not_install_version_iter == other.not_install_version_iter;
    }

    bool operator!=(const const_iterator &other) const
    {
      return !((*this) == other);
    }

    const choice &operator*() const
    {
      if(install_version_iter != install_version_end_iter)
	return install_version_iter->second;
      else
	return *not_install_version_iter;
    }

    const choice *operator->() const
    {
      return &**this;
    }

    const_iterator &operator++()
    {
      if(install_version_iter != install_version_end_iter)
	++install_version_iter;
      else
	++not_install_version_iter;

      return *this;
    }

    const_iterator operator++(int)
    {
      const_iterator rval(*this);
      ++(*this);
      return rval;
    }
  };

private:
  // Note: no iterators on this structure because they'd be a pain to
  // write and they should never be used (use for_each instead, it's a
  // lot more efficient).

  // Stores the install-this-version choices made in this set,
  // organized by package.  Needed to accelerate the containment test.
  boost::unordered_map<package, choice> install_version_choices;

  // Stores the dependencies that are broken by this choice set.  We
  // could just store dependencies instead; I don't know whether that
  // would be more efficient or not.
  boost::unordered_set<choice> not_install_version_choices;


  mutable bool hash_cache_dirty;
  mutable std::size_t hash_cache;

  friend std::ostream &operator<< <PackageUniverse>(std::ostream &out, const generic_choice_set<PackageUniverse> &choices);

  // Used to apply a for_each on choices to each element of the
  // install_version_choices set.
  template<typename F>
  struct for_each_choice_pair
  {
    const F &f;

    for_each_choice_pair(const F &_f) : f(_f)
    {
    }

    bool operator()(const std::pair<package, choice> &p) const
    {
      return f(p.second);
    }
  };

  // Used by operator<< -- could be declared outside this object, but
  // that would pollute the namespace.
  struct show_choice
  {
    mutable bool first;
    std::ostream &out;

    show_choice(std::ostream &_out, bool first)
      : first(true), out(_out)
    {
    }

    bool operator()(const choice &c) const
    {
      if(first)
	first = false;
      else
	out << ", ";
      out << c;

      return true;
    }
  };

  struct insert_choice_narrow
  {
    generic_choice_set &parent;

    insert_choice_narrow(generic_choice_set &_parent)
      : parent(_parent)
    {
    }

    bool operator()(const choice &c) const
    {
      switch(c.get_type())
	{
	case choice::install_version:
	  // Check whether this "hits" an existing choice.
	  {
	    const package p(c.get_ver().get_package());

	    std::pair<typename boost::unordered_map<package, choice>::iterator, typename boost::unordered_map<package, choice>::iterator>
	      found = parent.install_version_choices.equal_range(p);

	    if(found.first == found.second)
	      {
		parent.install_version_choices.insert(found.first, std::make_pair(p, c));
		parent.hash_cache_dirty = true;
	      }
	    else
	      {
		std::pair<const package, choice> &existing_choice_pair(*found.first);
		choice &existing_choice(existing_choice_pair.second);

		if(existing_choice.contains(c))
		  {
		    // Override the existing choice with the new one,
		    // which is more specific.
		    existing_choice = c;
		    parent.hash_cache_dirty = true;
		  }
		else if(c.contains(existing_choice))
		  ; // c is more general than the existing choice.
		else
		  LOG_ERROR(aptitude::Loggers::getAptitudeResolver(),
			    "Internal error: attempted to add conflicting choices "
			    << c << " and " << existing_choice << " to the same set.");
	      }
	  }
	  break;

	default:
	  parent.not_install_version_choices.insert(c);
	  parent.hash_cache_dirty = true;
	  break;
	}

      return true;
    }
  };

  generic_choice_set(const boost::unordered_map<package, choice> &_install_version_choices,
		     const boost::unordered_set<choice> &_not_install_version_choices)
    : install_version_choices(_install_version_choices),
      not_install_version_choices(_not_install_version_choices),
      hash_cache_dirty(true)
  {
  }

public:
  generic_choice_set()
    : hash_cache_dirty(true)
  {
  }

  const_iterator begin() const
  {
    return const_iterator(install_version_choices.begin(),
			  install_version_choices.end(),
			  not_install_version_choices.begin());
  }

  const_iterator end() const
  {
    return const_iterator(install_version_choices.end(),
			  install_version_choices.end(),
			  not_install_version_choices.end());
  }

  /** \brief Insert every choice in the given set into this set,
   *  overriding more general options with more specific ones.
   */
  void insert_or_narrow(const boost::unordered_set<choice> &choices)
  {
    for(typename boost::unordered_set<choice>::const_iterator it = choices.begin();
	it != choices.end(); ++it)
      insert_or_narrow(*it);
  }

  /** \brief Insert every choice in the given set into this set,
   *  overriding more general options with more specific ones.
   */
  void insert_or_narrow(const generic_choice_set &other)
  {
    other.for_each(insert_choice_narrow(*this));
  }

  /** \brief Insert the given choice into this set, overriding more
   * general options with more specific ones.
   *
   *  \param c  The choice to insert.
   *
   *  If the set contains a choice that is more general than c, that
   *  choice will be dropped in favor c.  On the other hand, if the
   *  set contains a choice that is more specific than c, c will be
   *  discarded in favor of that choice.
   */
  void insert_or_narrow(const choice &c)
  {
    insert_choice_narrow(*this)(c);
  }

  /** \brief Remove everything that overlaps the given choice from
   *  this set.
   *
   *  This is used to narrow promotion sets while backpropagating
   *  promotions.
   */
  void remove_overlaps(const choice &c)
  {
    switch(c.get_type())
      {
      case choice::install_version:
	install_version_choices.erase(c.get_ver().get_package());
	hash_cache_dirty = true;
	break;

      default:
	not_install_version_choices.erase(c);
	hash_cache_dirty = true;
	break;
      }
  }

  /** \brief If a choice in this set contains c, store it in
   *  out and return true.
   */
  bool get_containing_choice(const choice &c,
			     choice &out) const
  {
    switch(c.get_type())
      {
      case choice::install_version:
	{
	  typename boost::unordered_map<package, choice>::const_iterator found =
	    install_version_choices.find(c.get_ver().get_package());
	  if(found == install_version_choices.end())
	    return false;
	  else
	    {
	      const std::pair<package, choice> &existing_choice_pair(*found);
	      const choice &existing_choice(existing_choice_pair.second);
	      if(existing_choice.contains(c))
		{
		  out = existing_choice;
		  return true;
		}
	      else
		return false;
	    }
	}

      default:
	{
	  typename boost::unordered_set<choice>::const_iterator
	    found = not_install_version_choices.find(c);
	  if(found != not_install_version_choices.end())
	    {
	      out = *found;
	      return true;
	    }
	  else
	    return false;
	}
      }
  }

  bool contains(const choice &c) const
  {
    choice dummy;
    return get_containing_choice(c, dummy);
  }

  /** \brief If a choice in this set is contained in c, store it in
   *  out and return true.
   */
  bool get_choice_contained_by(const choice &c,
			       choice &out) const
  {
    switch(c.get_type())
      {
      case choice::install_version:
	{
	  typename boost::unordered_map<package, choice>::const_iterator found =
	    install_version_choices.find(c.get_ver().get_package());
	  if(found == install_version_choices.end())
	    return false;
	  else
	    {
	      const std::pair<package, choice> &existing_choice_pair(*found);
	      const choice &existing_choice(existing_choice_pair.second);
	      if(c.contains(existing_choice))
		{
		  out = existing_choice;
		  return true;
		}
	      else
		return false;
	    }
	}

      default:
	{
	  typename boost::unordered_set<choice>::const_iterator
	    found = not_install_version_choices.find(c);
	  if(found != not_install_version_choices.end())
	    {
	      out = *found;
	      return true;
	    }
	  else
	    return false;
	}
      }
  }

  /** \brief Test whether some element of this set is contained by c.
   *
   *  Used when testing promotions, for instance.
   */
  bool has_contained_choice(const choice &c) const
  {
    choice dummy;
    return get_choice_contained_by(c, dummy);
  }

  typedef unsigned int size_type;
  size_type size() const
  {
    return install_version_choices.size() + not_install_version_choices.size();
  }

  std::size_t get_hash_value() const
  {
    if(hash_cache_dirty)
      {
	// There's no guarantee that equivalent unordered_set objects
	// store objects in the same order (as you might guess from the
	// name) so we do a somewhat expensive operation here: throwing
	// all the hashes into a temporary vector, sorting it, and taking
	// the hash of the vector.
	std::vector<std::size_t> hashes;
	hashes.reserve(install_version_choices.size() + not_install_version_choices.size());

	for(typename boost::unordered_map<package, choice>::const_iterator it
	      = install_version_choices.begin();
	    it != install_version_choices.end(); ++it)
	  hashes.push_back(hash_value(it->second));

	for(typename boost::unordered_set<choice>::const_iterator it
	      = not_install_version_choices.begin();
	    it != not_install_version_choices.end(); ++it)
	  hashes.push_back(hash_value(*it));

	std::sort(hashes.begin(), hashes.end());

	hash_cache = boost::hash_value(hashes);
	hash_cache_dirty = false;
      }

    return hash_cache;
  }

  bool operator==(const generic_choice_set &other) const
  {
    return
      install_version_choices == other.install_version_choices &&
      not_install_version_choices == other.not_install_version_choices;
  }

  bool operator!=(const generic_choice_set &other) const
  {
    return !(*this == other);
  }

  /** \brief Check whether each entry in the other set is contained by
   *  an entry in this set.
   */
  bool contains(const generic_choice_set &other) const
  {
    for(typename boost::unordered_set<choice>::const_iterator it =
	  other.not_install_version_choices.begin();
	it != other.not_install_version_choices.end(); ++it)
      {
	if(not_install_version_choices.find(*it) == not_install_version_choices.end())
	  return false;
      }

    for(typename boost::unordered_map<package, choice>::const_iterator it =
	  other.install_version_choices.begin();
	it != other.install_version_choices.end(); ++it)
      {
	typename boost::unordered_map<package, choice>::const_iterator found =
	  install_version_choices.find(it->first);

	if(found == install_version_choices.end())
	  return false;
	else if(!found->second.contains(it->second))
	  return false;
      }

    return true;
  }

  /** \brief Check whether each entry in the other set contains an
   *  entry in this set.
   */
  bool subset_is_contained_in(const generic_choice_set &other) const
  {
    for(typename boost::unordered_set<choice>::const_iterator it =
	  other.not_install_version_choices.begin();
	it != other.not_install_version_choices.end(); ++it)
      {
	if(not_install_version_choices.find(*it) == not_install_version_choices.end())
	  return false;
      }

    for(typename boost::unordered_map<package, choice>::const_iterator it =
	  other.install_version_choices.begin();
	it != other.install_version_choices.end(); ++it)
      {
	typename boost::unordered_map<package, choice>::const_iterator found =
	  install_version_choices.find(it->first);

	if(found == install_version_choices.end())
	  return false;
	else if(!it->second.contains(found->second))
	  return false;
      }
  }

  /** \brief Apply a function object to each element in this set.
   *
   *  If the function returns \b false, it will short-circuit.
   */
  template<typename F>
  bool for_each(const F &f) const
  {
    for(typename boost::unordered_map<package, choice>::const_iterator
	  it = install_version_choices.begin();
	it != install_version_choices.end(); ++it)
      if(!f(it->second))
	return false;

    for(typename boost::unordered_set<choice>::const_iterator
	  it = not_install_version_choices.begin();
	it != not_install_version_choices.end(); ++it)
      if(!f(*it))
	return false;

    return true;
  }

  /** \brief Return a new choice set that does not share memory with
   *  this set.
   */
  generic_choice_set clone() const
  {
    return generic_choice_set(install_version_choices,
			      not_install_version_choices);
  }

  /** \brief Retrieve the version, if any, that was chosen for the
   *  given package.
   *
   *  \param p   The package to examine.
   *  \param out A location in which to store the retrieved version.
   *
   *  \return  \b true if a version of the package p is installed
   *  by this choice set; \b false if not (in which case out is
   *  unchanged).
   */
  bool get_version_of(const package &p, version &out) const
  {
    typename boost::unordered_map<package, choice>::const_iterator found = install_version_choices.find(p);

    if(found != install_version_choices.end())
      {
	out = found->second.get_ver();
	return true;
      }
    else
      return false;
  }
};

template<typename PackageUniverse>
std::size_t hash_value(const generic_choice_set<PackageUniverse> &p)
{
  return p.get_hash_value();
}

template<typename PackageUniverse>
std::ostream &operator<<(std::ostream &out, const generic_choice_set<PackageUniverse> &choices)
{
  out << "{";
  typename generic_choice_set<PackageUniverse>::show_choice f(out, true);
  choices.for_each(f);
  out << "}";
  return out;
}

#endif // CHOICE_SET_H
