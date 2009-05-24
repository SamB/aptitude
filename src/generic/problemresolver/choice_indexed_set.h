/** \file choice_indexed_set.h */    // -*-c++-*-


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

#ifndef CHOICE_INDEXED_SET_H
#define CHOICE_INDEXED_SET_H

#include "choice.h"

#include <generic/util/immset.h>

#include <functional>
#include <utility>

/** \brief A set of objects, indexed for retrieval by an associated
 *  choice.
 *
 *  This differs from choice_set in that it stores tuples (choice, T)
 *  where "T" can be any LessThanComparable type.  It supports quickly
 *  iterating over all the tuples for which the choice is contained in
 *  an input choice.
 *
 *  \tparam T        The type of object stored in this set.
 *  \tparam Compare  How to compare T objects.
 */
template<typename PackageUniverse, typename T, typename Compare = std::less<T> >
class generic_choice_indexed_set
{
  typedef typename PackageUniverse::package package;
  typedef typename PackageUniverse::version version;
  typedef typename PackageUniverse::dep dep;

  typedef generic_choice<PackageUniverse> choice;

  /** \brief Stores all the objects whose choice is scoped to a
   *  particular dependency.
   */
  struct dep_info
  {
    imm::set<T, Compare> not_from_dep_source;
    imm::set<T, Compare> from_dep_source;
  };

  /** \brief Represents information about all the objects whose
   *  choices install a version.
   */
  struct version_info
  {
    imm::set<T, Compare> not_dep_scoped;
    imm::map<dep, dep_info> dep_scoped;
  };

  // Objects stored for the choice (Install(v), t).
  imm::map<version, version_info> install_version_objects;

  // Objects stored for the choice (Break(d), t).
  imm::map<dep, imm::set<T, Compare> > break_dep_objects;

  // The comparison object.
  Compare compare;

public:
  generic_choice_indexed_set(const Compare &_compare = Compare())
    : compare(_compare)
  {
  }

  /** \brief Insert a tuple (c, t) into this set. */
  void insert(const choice &c, T t)
  {
    switch(c.get_type())
      {
      case choice::install_version:
	{
	  typename imm::map<version, version_info>::node
	    found_ver = install_version_objects.lookup(c.get_ver());

	  version_info inf;
	  if(found_ver.isValid())
	    inf = found_ver.getVal();


	  if(!c.get_dep_scoped())
	    inf.not_dep_scoped.insert(t);
	  else
	    {
	      typename imm::map<dep, dep_info>::node
		found_dep = inf.dep_scoped.lookup(c.get_dep());

	      dep_info dep_inf;
	      if(found_dep.isValid())
		dep_inf = found_dep.getVal();

	      if(c.get_from_dep_source())
		dep_inf.from_dep_source.insert(t);
	      else
		dep_inf.not_from_dep_source.insert(t);

	      inf.dep_scoped.put(c.get_dep(), dep_inf);
	    }

	  install_version_objects.put(c.get_ver(), inf);
	}
	break;

      case choice::break_soft_dep:
	{
	  typename imm::map<dep, imm::set<T, Compare> >::node
	    found = break_dep_objects.lookup(c.get_dep());

	  imm::set<T, Compare> insert_target;
	  if(found.isValid())
	    insert_target = found.getVal();

	  insert_target.insert(t);
	  break_dep_objects.put(c.get_dep(), insert_target);
	}
	break;
      }
  }

  /** \brief Remove a tuple (c, t) from this set. */
  void remove(const choice &c, T t)
  {
    switch(c.get_type())
      {
      case choice::install_version:
	{
	  typename imm::map<version, version_info>::node
	    found_ver = install_version_objects.lookup(c.get_ver());

	  version_info inf;
	  if(found_ver.isValid())
	    inf = found_ver.getVal();


	  if(!c.get_dep_scoped())
	    inf.not_dep_scoped.erase(t);
	  else
	    {
	      typename imm::map<dep, dep_info>::node
		found_dep = inf.dep_scoped.lookup(c.get_dep());

	      dep_info dep_inf;
	      if(found_dep.isValid())
		dep_inf = found_dep.getVal();

	      if(c.get_from_dep_source())
		dep_inf.from_dep_source.erase(t);
	      else
		dep_inf.not_from_dep_source.erase(t);

	      inf.dep_scoped.put(c.get_dep(), dep_inf);
	    }

	  install_version_objects.put(c.get_ver(), inf);
	}
	break;

      case choice::break_soft_dep:
	{
	  typename imm::map<dep, imm::set<T, Compare> >::node
	    found = break_dep_objects.lookup(c.get_dep());

	  imm::set<T, Compare> insert_target;
	  if(found.isValid())
	    insert_target = found.getVal();

	  insert_target.erase(t);
	  break_dep_objects.put(c.get_dep(), insert_target);
	}
	break;
      }
  }

  // Applies a function to each element of a set for a given choice.
  template<typename F>
  struct for_each_with_choice
  {
    choice c;
    F f;

    for_each_by_choice(const choice &_c, const F &_f)
      : c(_c), f(_f)
    {
    }

    bool operator()(T t) const
    {
      return f(c, t);
    }
  };

  // Applies the given function to all the objects in the input
  // dependency scope.
  template<typename F>
  struct for_each_by_dep_scope
  {
    version v;
    F f;

    for_each_by_dep_scope(const version &_v, const F &_f)
      : f(_f), v(_v)
    {
    }

    bool operator()(const std::pair<dep, dep_info> &p) const
    {
      const imm::set<T, Compare> &not_from_dep_source = p.second.not_from_dep_source;
      const imm::set<T, Compare> &from_dep_source = p.second.from_dep_source;

      for_each_with_choice<F> not_from_dep_source_f(choice::make_install_version(v, true, p.first, -1), f);
      for_each_with_choice<F> from_dep_source_f(choice::make_install_version_from_dep_source(v, p.first, -1));

      return
	not_from_dep_source.for_each(not_from_dep_source_f) &&
	from_dep_source.for_each(from_dep_source_f);
    }
  };

  /** \brief Apply the given function object to (c', t) for each pair
   *  (c', t) in this set such that c' is contained in c.
   *
   *  If f returns false, the iteration will abort.  Modifications to
   *  this set after the iteration begins do not affect which values
   *  are visited.
   */
  template<typename F>
  bool for_each_contained_in(const choice &c, const F &f) const
  {
    switch(c.get_type())
      {
      case choice::install_version:
	{
	  const version &v(c.get_ver());
	  typename imm::map<version, ver_info>::node
	    found_ver(install_version_objects.find(v));
	  // Note that since we save a reference to the original,
	  // immutable node, we will always iterate over everything
	  // even if the structure of the tree is changed.

	  if(found_ver.isValid())
	    {
	      const version_inf &ver_inf(found_ver.getVal());

	      if(!c.get_dep_scoped())
		{
		  for_each_with_choice<F>
		    without_dep_f(choice::make_install_version(v, -1));
		  if(!ver_inf.not_dep_scoped.for_each(without_dep_f))
		    return false;

		  for_each_by_dep_scope<F> dep_scope_f(v, f);
		  if(!ver_inf.dep_scoped.for_each(dep_scope_f))
		    return false;
		}
	      else
		{
		  typename imm::map<dep, dep_info>::node found_dep =
		    ver_inf.dep_scoped.find(c.get_dep());

		  if(found_dep.isValid())
		    {
		      const dep &d(c.get_dep());
		      const dep_info &dep_inf(found_dep.getVal());

		      if(!c.get_from_dep_source())
			{
			  for_each_with_choice_f<F>
			    with_dep_scoped_f(choice::make_install_version(v, true, d, -1));
			  if(!dep_inf.not_from_dep_source.for_each(with_dep_scoped_f))
			    return false;
			}

		      for_each_with_choice_f<F>
			with_from_dep_source_f(choice::make_install_version_from_dep_source(v, d, -1));
		      if(!dep_inf.from_dep_souce.for_each(with_from_dep_source_f))
			return false;
		    }
		}
	    }
	}
	break;

      case choice::break_soft_dep:
	{
	  const dep &d(c.get_dep());
	  typename imm::map<dep, imm::set<T, Compare> >::node
	    found = break_dep_objects.lookup(d);

	  if(found.isValid())
	    {
	      const imm::set<T, Compare> &for_this_dep(found.getVal());

	      for_each_with_choice_f<F>
		with_break_dep_f(choice::make_break_soft_dep(d, -1));

	      if(!for_this_dep.for_each(with_break_dep_f))
		return false;
	    }
	}
	break;
      }

    return true;
  }
};

#endif // CHOICE_INDEXED_SET_H
