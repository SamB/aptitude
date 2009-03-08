/** \file choice.h */ // -*-c++-*-

//   Copyright (C) 2009 Daniel Burrows

//   This program is free software; you can redistribute it and/or
//   modify it under the terms of the GNU General Public License as
//   published by the Free Software Foundation; either version 2 of
//   the License, or (at your option) any later version.

//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//   General Public License for more details.

//   You should have received a copy of the GNU General Public License
//   along with this program; see the file COPYING.  If not, write to
//   the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//   Boston, MA 02111-1307, USA.

#ifndef CHOICE_H
#define CHOICE_H

#include <cwidget/generic/util/eassert.h>

#include <iostream>

/** \brief Represents a decision made by the resolver.
 *
 *  This is used to keep track of which choices imply that we end up
 *  at a higher tier than we otherwise would.  Note that only some of
 *  the fields are part of the "value" of the choice; id tags along
 *  for informational purposes only, and if the choice is of type
 *  install_version and is not from the dependency source, then the
 *  associated dependency is also present for informational purposes
 *  only.  Comparison operators, contains(), and other operations
 *  ignore fields that are present for informational purposes only.
 *
 *  \sa promotion_set
 */
template<typename PackageUniverse>
class generic_choice
{
public:
  typedef typename PackageUniverse::package package;
  typedef typename PackageUniverse::version version;
  typedef typename PackageUniverse::dep dep;

  /** \brief Indicates the type of choice represented by a
   *  choice object.
   */
  enum type
  {
    /** \brief A choice to install a single package version. */
    install_version,
    /** \brief A choice to break a single soft dependency. */
    break_soft_dep
  };

private:
  // The version, if any, associated with this choice.
  //
  // Meaningful for install_version choices.
  version ver;

  // The dependency, if any, associated with this choice.
  //
  // Meaningful for install_version choices and for break_soft_dep
  // choices.  If type is install_version and from_dep_source is set,
  // this is part of the "identity" of this object (so it will be used
  // in equality comparisons, contains() checks, etc); all other
  // install_version choices include it only for informational
  // purposes.
  dep d;

  // True if the choice was to remove the source of the dependency.
  bool from_dep_source:1;

  // The type of this choice object.
  type tp:1;

  /** \brief The order in which this choice was made.
   */
  int id:30;

  generic_choice(const version &_ver, bool _from_dep_source, const dep &_d, int _id)
    : ver(_ver), d(_d), from_dep_source(_from_dep_source), tp(install_version), id(_id)
  {
  }

  generic_choice(const dep &_d, int _id)
    : d(_d), from_dep_source(false), tp(break_soft_dep), id(_id)
  {
  }

public:
  generic_choice()
    : from_dep_source(false), tp(install_version), id(-1)
  {
  }

  /** \brief Create a new choice that installs the given version.
   *
   *  \param ver The version that is installed by this choice.
   *  \param d   The dependency that caused this choice.  This is
   *             included for informational purposes only and is
   *             ignored by all operations except get_dep().
   *  \param id  An arbitrary integer associated with this choice.
   *             Ignored by all operations except get_id().
   */
  static generic_choice make_install_version(const version &ver, const dep &d, int id)
  {
    return generic_choice(ver, false, d, id);
  }

  /** \brief Create a new choice that installs the given version to
   *  change the source of the given dependency.
   *
   *  \param ver The version that is installed by this choice.
   *  \param d   The dependency whose source was modified by installing
   *             ver.
   *  \param id  An arbitrary integer associated with this choice.
   *             Ignored by all operations except get_id().
   */
  static generic_choice make_install_version_from_dep_source(const version &ver, const dep &d, int id)
  {
    return generic_choice(ver, true, d, id);
  }

  /** \brief Create a new choice that leaves the given soft dependency
   *  unresolved.
   *
   *  \param id  An arbitrary integer associated with this choice.
   *             Ignored by all operations except get_id().
   */
  static generic_choice make_break_soft_dep(const dep &d, int id)
  {
    return generic_choice(d, id);
  }

  /** \brief Test whether this choice "contains" another choice.
   *
   *  This is true if the two choices are equal, or if this choice is
   *  "more general" than the other choice.  In particular: installing
   *  a version, not from a dependency source, is always more general
   *  than installing the same version from any dependency source.
   */
  bool contains(const generic_choice &other) const
  {
    if(tp != other.tp)
      return false;
    else // tp == other.tp
      switch(tp)
	{
	case install_version:
	  if(ver != other.ver)
	    return false;
	  else // ver == other.ver
	    {
	      if(!from_dep_source)
		return true;
	      else
		return other.from_dep_source && d == other.d;
	    }

	case break_soft_dep:
	  return d == other.d;
	}

    eassert(!"We should never get here.");
    return false;
  }

  /** \brief Compare two choices.
   *
   *  Choices are ordered arbitrarily.
   */
  bool operator<(const generic_choice &other) const
  {
    if(tp < other.tp)
      return true;
    else if(tp > other.tp)
      return false;
    else // tp == other.tp
      {
	switch(tp)
	  {
	  case install_version:
	    if(ver < other.ver)
	      return true;
	    else if(other.ver < ver)
	      return false;
	    else
	      {
		if(!from_dep_source && other.from_dep_source)
		  return true;
		else if(from_dep_source && !other.from_dep_source)
		  return false;
		else if(!from_dep_source)
		  return false; // They are equal.
		else
		  return d < other.d;
	      }

	  case break_soft_dep:
	    return d < other.d;
	  }
      }

    eassert(!"We should never get here.");
    return false;
  }

  bool operator==(const generic_choice &other) const
  {
    if(tp != other.tp)
      return false;
    else
      {
	switch(tp)
	  {
	  case install_version:
	    if(ver != other.ver)
	      return false;

	    if(from_dep_source != other.from_dep_source)
	      return false;

	    if(!from_dep_source)
	      return true;
	    else
	      return d == other.d;
	    break;

	  case break_soft_dep:
	    return d == other.d;
	  }
      }

    eassert(!"We should never get here.");
  }

  int get_id() const { return id; }
  type get_type() const { return tp; }

  const version &get_ver() const
  {
    eassert(tp == install_version);
    return ver;
  }

  bool get_from_dep_source() const
  {
    eassert(tp == install_version);
    return from_dep_source;
  }

  const dep &get_dep() const
  {
    return d;
  }
};

template<typename PackageUniverse>
inline std::ostream &operator<<(std::ostream &out, const generic_choice<PackageUniverse> &choice)
{
  switch(choice.get_type())
    {
    case generic_choice<PackageUniverse>::install_version:
      out << "Install(" << choice.get_ver();
      if(choice.get_from_dep_source())
	return out << " [" << choice.get_dep() << "])";
      else
	return out << ")";

    case generic_choice<PackageUniverse>::break_soft_dep:
      return out << "Break(" << choice.get_dep() << ")";

    default:
      // We should never get here.
      return out << "Error: corrupt choice object";
    }
}

#endif
