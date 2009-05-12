/** \file immlist.h */   // -*-c++-*-


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

#include <cwidget/generic/util/ref_ptr.h>
#include "refcounted_base.h"

namespace imm
{
  // TODO: Maybe the list itself should be "mutable", following the
  // imm::set model?

  /** \brief Immutable list, using the standard head/tail breakdown.
   *
   *  The empty list is represented by a default-constructed ref_ptr.
   *
   *  Like imm::set, this is not threadsafe because it exists for use
   *  in some moderately intensive pieces of code that are
   *  single-threaded.
   */
  template<typename T>
  class list : public aptitude::util::refcounted_base_not_threadsafe
  {
    T head;

    cwidget::util::ref_ptr<list> tail;

    list(const T &_head, const cwidget::util::ref_ptr<list> &_tail)
      : head(_head), tail(_tail)
    {
    }

  public:
    static cwidget::util::ref_ptr<list> make_empty()
    {
      return cwidget::util::ref_ptr<list>();
    }

    static cwidget::util::ref_ptr<list> make_cons(const T &_head,
						  const cwidget::util::ref_ptr<list> &_tail)
    {
      return cwidget::util::ref_ptr<list>(new list(_head, _tail));
    }

    const T &get_head() const { return head; }
    const cwidget::util::ref_ptr<list> &get_tail() const { return tail; }

    /** \brief Iterates down a single imm::list.
     *
     *  Unlike the imm::set iterator, this one should be quite
     *  efficient, albeit still slightly less efficient than an
     *  intrinsic for_each().
     */
    class const_iterator
    {
      cwidget::util::ref_ptr<list> lst;

    public:
      /** \brief Construct a const_iterator that iterates down the
       *  given list.
       */
      const_iterator(const cwidget::util::ref_ptr<list> &_lst)
	: lst(_lst)
      {
      }

      /** \brief Construct a const_iterator pointing at the empty
       *  list.
       */
      const_iterator()
      {
      }

      /** \brief Return \b true if the iterator points at a valid
       *  member of the list.
       */
      bool valid() const { return lst.valid(); }

      const_iterator &operator++()
      {
	lst = lst->get_tail();
      }

      const T &operator*() const { return lst->get_head(); }
      const T *operator->() const { return lst->get_tail().operator->(); }
    };

    const_iterator begin() const
    {
      return const_iterator(cwidget::util::ref_ptr<list>(this));
    }

    const_iterator end() const
    {
      return const_iterator();
    }
  };
}
