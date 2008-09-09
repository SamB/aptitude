// match.h    -*-c++-*-
//
//   Copyright (C) 2008 Daniel Burrows
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

#ifndef MATCH_H
#define MATCH_H

#include <generic/util/refcounted_base.h>

namespace aptitude
{
  namespace matching
  {
    /** \brief Represents information about how a package was matched.
     *
     *  This only represents the information that can't be derived
     *  from the type of matcher.  For instance, it might contain the
     *  string region that was matched or the dependency that was
     *  followed, but it won't contain package status information,
     *  because that is available from the pkgDepCache.
     *
     *  This means that if the user of a match needs information that
     *  will last across depCache changes, it must be copied.
     */
    class match : public util::refcounted_base
    {
      // The pattern that produced this particular match.
      cwidget::util::ref_ptr<pattern> p;

    public:
      /** \brief The type of a match. */
      enum type
	{
	  /** \brief A match that has no associated information.
	   *
	   *  This would represent, for instance, a match
	   *  corresponding to a ?action term.
	   */
	  atomic,

	  /** \brief A match using a regular expression.
	   *
	   *  The attached information consists of a list of ranges
	   *  within the input string that were matched.
	   */
	  regexp,

	  /** \brief A match that was made via a dependency.
	   *
	   *  The attached information consists of the package /
	   *  version that the sub-query matched against, and the
	   *  dependency the match was made through.
	   */
	  dependency,

	  /** \brief A match that was made against a list of sub-patterns.
	   *
	   *  The semantics of the sub-patterns are determined by the
	   *  pattern corresponding to this match: for instance, an
	   *  "?and" term requires all its sub-terms to match, whereas
	   *  a ?widen term matches if its sub-terms match any version
	   *  of the incoming package.
	   */
	  sub_pattern_list
	};

    private:
      type tp;

    public:
    };
  }
}

#endif
