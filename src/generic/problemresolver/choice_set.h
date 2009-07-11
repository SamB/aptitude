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

#include <boost/flyweight.hpp>
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

    typename boost::unordered_map<package, boost::flyweight<choice> >::const_iterator install_version_iter;
    typename boost::unordered_map<package, boost::flyweight<choice> >::const_iterator install_version_end_iter;
    typename boost::unordered_set<boost::flyweight<choice> >::const_iterator not_install_version_iter;

    const_iterator(const typename boost::unordered_map<package, boost::flyweight<choice> >::const_iterator _install_version_iter,
		   const typename boost::unordered_map<package, boost::flyweight<choice> >::const_iterator _install_version_end_iter,
		   const typename boost::unordered_set<boost::flyweight<choice> >::const_iterator _not_install_version_iter)
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

    // Note: these don't return flyweights because they're
    // (currently?)  mostly for outside consumption.
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
  boost::unordered_map<package, boost::flyweight<choice> > install_version_choices;

  // Stores the dependencies that are broken by this choice set.  We
  // could just store dependencies instead; I don't know whether that
  // would be more efficient or not.
  boost::unordered_set<boost::flyweight<choice> > not_install_version_choices;


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

    bool operator()(const std::pair<package, boost::flyweight<choice> > &p) const
    {
      return f(p.second);
    }
  };

  // Used by operator<< -- could be declared outside this object, but
  // that would pollute the namespace.
  struct accumulate_choices
  {
    std::vector<boost::flyweight<choice> > &out;

    accumulate_choices(std::vector<boost::flyweight<choice> > &_out)
      : out(_out)
    {
    }

    bool operator()(const boost::flyweight<choice> &c) const
    {
      out.push_back(c);
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

    bool operator()(const boost::flyweight<choice> &c_flyweight) const
    {
      // Extract the contained choice since we'll be looking at it a lot below.
      const choice &c(c_flyweight.get());
      switch(c.get_type())
	{
	case choice::install_version:
	  // Check whether this "hits" an existing choice.
	  {
	    const package p(c.get_ver().get_package());

	    std::pair<typename boost::unordered_map<package, boost::flyweight<choice> >::iterator, typename boost::unordered_map<package, boost::flyweight<choice> >::iterator>
	      found = parent.install_version_choices.equal_range(p);

	    if(found.first == found.second)
	      {
		parent.install_version_choices.insert(found.first, std::make_pair(p, c_flyweight));
		parent.hash_cache_dirty = true;
	      }
	    else
	      {
		std::pair<const package, boost::flyweight<choice> > &existing_choice_pair(*found.first);
		boost::flyweight<choice> &existing_choice(existing_choice_pair.second);

		if(existing_choice.get().contains(c))
		  {
		    // Override the existing choice with the new one,
		    // which is more specific.
		    existing_choice = c_flyweight;
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
	  parent.not_install_version_choices.insert(c_flyweight);
	  parent.hash_cache_dirty = true;
	  break;
	}

      return true;
    }
  };

  generic_choice_set(const boost::unordered_map<package, boost::flyweight<choice> > &_install_version_choices,
		     const boost::unordered_set<boost::flyweight<choice> > &_not_install_version_choices)
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
  void insert_or_narrow(const boost::unordered_set<boost::flyweight<choice> > &choices)
  {
    for(typename boost::unordered_set<boost::flyweight<choice> >::const_iterator it = choices.begin();
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
  void insert_or_narrow(const boost::flyweight<choice> &c)
  {
    insert_choice_narrow(*this)(c);
  }

  void insert_or_narrow(const choice &c)
  {
    insert_or_narrow(boost::flyweight<choice>(c));
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
	not_install_version_choices.erase(boost::flyweight<choice>(c));
	hash_cache_dirty = true;
	break;
      }
  }

  /** \brief If a choice in this set contains c, store it in
   *  out and return true.
   */
  bool get_containing_choice(const boost::flyweight<choice> &c_flyweight,
			     boost::flyweight<choice> &out) const
  {
    const choice &c(c_flyweight);
    switch(c.get_type())
      {
      case choice::install_version:
	{
	  typename boost::unordered_map<package, boost::flyweight<choice> >::const_iterator found =
	    install_version_choices.find(c.get_ver().get_package());
	  if(found == install_version_choices.end())
	    return false;
	  else
	    {
	      const std::pair<package, boost::flyweight<choice> > &existing_choice_pair(*found);
	      const boost::flyweight<choice> &existing_choice(existing_choice_pair.second);
	      if(existing_choice.get().contains(c))
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
	  typename boost::unordered_set<boost::flyweight<choice> >::const_iterator
	    found = not_install_version_choices.find(c_flyweight);
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

  bool get_containing_choice(const choice &c,
			     choice &out) const
  {
    boost::flyweight<choice> tmp;
    bool rval = get_containing_choice(boost::flyweight<choice>(c), tmp);

    if(rval)
      out = tmp.get();
    return rval;
  }

  bool contains(const boost::flyweight<choice> &c) const
  {
    choice dummy;
    return get_containing_choice(c, dummy);
  }

  bool contains(const choice &c) const
  {
    return contains(boost::flyweight<choice>(c));
  }

  /** \brief If a choice in this set is contained in c, store it in
   *  out and return true.
   */
  bool get_choice_contained_by(const boost::flyweight<choice> &c_flyweight,
			       boost::flyweight<choice> &out) const
  {
    const choice &c(c_flyweight);
    switch(c.get_type())
      {
      case choice::install_version:
	{
	  typename boost::unordered_map<package, boost::flyweight<choice> >::const_iterator found =
	    install_version_choices.find(c.get_ver().get_package());
	  if(found == install_version_choices.end())
	    return false;
	  else
	    {
	      const std::pair<package, boost::flyweight<choice> > &existing_choice_pair(*found);
	      const boost::flyweight<choice> &existing_choice(existing_choice_pair.second);
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
	  typename boost::unordered_set<boost::flyweight<choice> >::const_iterator
	    found = not_install_version_choices.find(c_flyweight);
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
  bool has_contained_choice(const boost::flyweight<choice> &c) const
  {
    boost::flyweight<choice> dummy;
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

	for(typename boost::unordered_map<package, boost::flyweight<choice> >::const_iterator it
	      = install_version_choices.begin();
	    it != install_version_choices.end(); ++it)
	  hashes.push_back(hash_value(it->second.get()));

	for(typename boost::unordered_set<boost::flyweight<choice> >::const_iterator it
	      = not_install_version_choices.begin();
	    it != not_install_version_choices.end(); ++it)
	  hashes.push_back(hash_value(it->get()));

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
    for(typename boost::unordered_set<boost::flyweight<choice> >::const_iterator it =
	  other.not_install_version_choices.begin();
	it != other.not_install_version_choices.end(); ++it)
      {
	if(not_install_version_choices.find(*it) == not_install_version_choices.end())
	  return false;
      }

    for(typename boost::unordered_map<package, boost::flyweight<choice> >::const_iterator it =
	  other.install_version_choices.begin();
	it != other.install_version_choices.end(); ++it)
      {
	typename boost::unordered_map<package, boost::flyweight<choice> >::const_iterator found =
	  install_version_choices.find(it->first);

	if(found == install_version_choices.end())
	  return false;
	else if(!found->second.get().contains(it->second))
	  return false;
      }

    return true;
  }

  /** \brief Check whether each entry in the other set contains an
   *  entry in this set.
   */
  bool subset_is_contained_in(const generic_choice_set &other) const
  {
    for(typename boost::unordered_set<boost::flyweight<choice> >::const_iterator it =
	  other.not_install_version_choices.begin();
	it != other.not_install_version_choices.end(); ++it)
      {
	if(not_install_version_choices.find(*it) == not_install_version_choices.end())
	  return false;
      }

    for(typename boost::unordered_map<package, boost::flyweight<choice> >::const_iterator it =
	  other.install_version_choices.begin();
	it != other.install_version_choices.end(); ++it)
      {
	typename boost::unordered_map<package, boost::flyweight<choice> >::const_iterator found =
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
    for(typename boost::unordered_map<package, boost::flyweight<choice> >::const_iterator
	  it = install_version_choices.begin();
	it != install_version_choices.end(); ++it)
      if(!f(it->second))
	return false;

    for(typename boost::unordered_set<boost::flyweight<choice> >::const_iterator
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
    typename boost::unordered_map<package, boost::flyweight<choice> >::const_iterator found = install_version_choices.find(p);

    if(found != install_version_choices.end())
      {
	out = found->second.get().get_ver();
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
struct choice_name_lt
{
  typedef typename PackageUniverse::version version;
  typedef typename PackageUniverse::package package;
  typedef typename PackageUniverse::dep dep;
  typedef generic_choice<PackageUniverse> choice;

  // The following operators are used to place the solution components
  // in order by name, to better permit comparison of debugging output
  // between versions.
  struct ver_name_lt
  {
  public:
    int cmp(const version &v1, const version &v2) const
    {
      // EW: I don't have a formal standard on what get_name()
      // returns, so force it to be a string here:
      int pcmp = std::string(v1.get_package().get_name()).compare(v2.get_package().get_name());

      if(pcmp != 0)
	return pcmp;
      else
	return std::string(v1.get_name()).compare(v2.get_name());
    }

    bool operator()(const version &v1, const version &v2) const
    {
      return cmp(v1, v2) < 0;
    }
  };

  struct dep_name_lt
  {
  public:
    bool operator()(const dep &d1, const dep &d2) const
    {
      ver_name_lt vlt;

      int scmp = vlt.cmp(d1.get_source(), d2.get_source());

      if(scmp != 0)
	return scmp < 0;
      else
	{
	  typename dep::solver_iterator si1 = d1.solvers_begin();
	  typename dep::solver_iterator si2 = d2.solvers_begin();

	  while(!si1.end() && !si2.end())
	    {
	      scmp = vlt.cmp(*si1, *si2);

	      if(scmp != 0)
		return scmp < 0;

	      ++si1;
	      ++si2;
	    }

	  if(si1.end())
	    {
	      if(si2.end())
		return false;
	      else
		return true;
	    }
	  else
	    return false;
	}
    }
  };

  bool operator()(const choice &c1,
		  const choice &c2) const
  {
    if(c1.get_type() < c2.get_type())
      return true;
    else if(c2.get_type() < c1.get_type())
      return false;
    else
      switch(c1.get_type())
	{
	case choice::install_version:
	  if(c1.get_from_dep_source() < c2.get_from_dep_source())
	    return true;
	  else if(c2.get_from_dep_source() < c1.get_from_dep_source())
	    return false;
	  else
	    return ver_name_lt()(c1.get_ver(), c2.get_ver());

	case choice::break_soft_dep:
	  return dep_name_lt()(c1.get_dep(), c2.get_dep());

	default:
	  eassert(!"Unhandled choice type in choice_name_lt.");
	}
  }
};

template<typename PackageUniverse>
std::ostream &operator<<(std::ostream &out, const generic_choice_set<PackageUniverse> &choices)
{
  typedef generic_choice<PackageUniverse> choice;
  typedef generic_choice_set<PackageUniverse> choice_set;

  std::vector<boost::flyweight<choice> > tmp;
  tmp.reserve(choices.size());
  typename choice_set::accumulate_choices f(tmp);
  choices.for_each(f);

  std::sort(tmp.begin(), tmp.end(), choice_name_lt<PackageUniverse>());

  out << "{";
  for(typename std::vector<boost::flyweight<choice> >::const_iterator it = tmp.begin();
      it != tmp.end(); ++it)
    {
      if(it != tmp.begin())
	out << ", ";
      out << *it;
    }
  out << "}";
  return out;
}

#endif // CHOICE_SET_H
