// solution_fragment.h            -*-c++-*-
//
//   Copyright (C) 2005 Daniel Burrows
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
//

#ifndef SOLUTION_FRAGMENT_H
#define SOLUTION_FRAGMENT_H

#include <apt-pkg/pkgcache.h>

// For aptitude_solution::action
#include <generic/problemresolver/solution.h>

// So passing aptitude_solution::action to a function is legal
#include <generic/apt/aptitude_resolver_universe.h>

/** \brief Code to convert an aptitude resolver solution and some of its
 *  individual components to fragments.
 * 
 *  \file solution_fragment.h
 */

namespace cwidget
{
  class fragment;
}
class aptitude_universe;

cwidget::fragment *solution_fragment(const generic_solution<aptitude_universe> &solution);

/** \return a list of the archives to which a version
 *  belongs in the form "archive1,archive2,..."
 */
std::string archives_text(const pkgCache::VerIterator &v);

/** \return a cwidget::fragment describing the given action. */
cwidget::fragment *action_fragment(const generic_solution<aptitude_universe>::action &a);

/** \return descriptive text about a single dependency. */
std::wstring dep_text(const pkgCache::DepIterator &d);

/** \return descriptive text about a conflict through a provides.
 *
 *  \param conflict any dependency
 *  \param p a provides iterator corresponding to conflict.  If conflict
 *           is not a Conflict, then p is ignored and conflict_text is
 *           identical to dep_text.  Otherwise, p is taken to be
 *           the provides through which the conflict was discovered.
 */
std::wstring conflict_text(const pkgCache::DepIterator &conflict,
			   const pkgCache::PrvIterator &p);

/** \return descriptive text about the targets of a dependency. */
std::string dep_targets(const pkgCache::DepIterator &start);

#endif // SOLUTION_FRAGMENT_H
