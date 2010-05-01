/** \file dynamic_list_collection.h */ // -*-c++-*-

// Copyright (C) 2010 Daniel Burrows
//
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

#ifndef DYNAMIC_LIST_COLLECTION_H
#define DYNAMIC_LIST_COLLECTION_H

#include "dynamic_list.h"
#include "dynamic_list_impl.h"

#include <boost/make_shared.hpp>
#include <boost/shared_ptr.hpp>

namespace aptitude
{
  namespace util
  {
    /** \brief Assembles multiple dynamic lists into a single dynamic list.
     *
     *  The elements of any single list are placed in order, but the
     *  elements from different lists are interleaved according to the
     *  order in which they were appended.  If an element occurs in
     *  two separate lists, it will be added to this list twice.
     */
    template<typename T>
    class dynamic_list_collection : public dynamic_list<T>
    {
      boost::shared_ptr<writable_dynamic_list<boost::shared_ptr<dynamic_list<T> > > > sub_lists;

      class cell
      {
        int index_of_parent_list;
        int index_within_parent_list;
        T value;

      public:
        cell(int _index_of_parent_list,
             int _index_within_parent_list,
             const T &_value);

        int get_index_of_parent_list() const { return index_of_parent_list; }
        int get_index_within_parent_list() const { return index_within_parent_list; }
        const T &get_value() const { return value; }

        bool operator==(const cell &other) const
        {
          return
            index_of_parent_list == other.index_of_parent_list &&
            index_within_parent_list == other.index_within_parent_list &&
            value == other.value;
        }
      };

      // Used to ensure that the items appear in the order in which
      // the append/remove events say they should (iterating over the
      // sub-lists would append them, which isn't what happens).
      boost::shared_ptr<writable_dynamic_list<cell> > concrete_view;
      std::size_t concrete_view_size;

      void handle_append(const boost::shared_ptr<dynamic_list<T> > &list);
      void handle_remove(const boost::shared_ptr<dynamic_list<T> > &list, int idx);

      void handle_concrete_view_append(const cell &value);
      void handle_concrete_view_remove(const cell &value, int idx);

      class my_enumerator;

    public:
      // Only public for make_shared.
      dynamic_list_collection();

      void add_list(const boost::shared_ptr<dynamic_list<T> > &lst);
      void remove_list(const boost::shared_ptr<dynamic_list<T> > &lst);

      static boost::shared_ptr<dynamic_list_collection> create();

      boost::shared_ptr<enumerator<T> > enumerate();
    };

    template<typename T>
    dynamic_list_collection<T>::cell::cell(int _index_of_parent_list,
                                           int _index_within_parent_list,
                                           const T &_value)
      : index_of_parent_list(_index_of_parent_list),
        index_within_parent_list(_index_within_parent_list),
        value(_value)
    {
    }

    template<typename T>
    class dynamic_list_collection<T>::my_enumerator : public enumerator<T>
    {
      boost::shared_ptr<enumerator<cell> > concrete_view_enumerator;

    public:
      my_enumerator(const boost::shared_ptr<enumerator<cell> > &
                    _concrete_view_enumerator);

      bool advance();
      T get_current();
    };

    template<typename T>
    dynamic_list_collection<T>::my_enumerator::my_enumerator(const boost::shared_ptr<enumerator<cell> > &_concrete_view_enumerator)
      : concrete_view_enumerator(_concrete_view_enumerator)
    {
    }

    template<typename T>
    bool dynamic_list_collection<T>::my_enumerator::advance()
    {
      return concrete_view_enumerator->advance();
    }

    template<typename T>
    T dynamic_list_collection<T>::my_enumerator::get_current()
    {
      return concrete_view_enumerator->get_current().get_value();
    }

    template<typename T>
    dynamic_list_collection<T>::dynamic_list_collection()
      : sub_lists(dynamic_list_impl<boost::shared_ptr<dynamic_list<T> > >::create()),
        concrete_view(dynamic_list_impl<cell>::create()),
        concrete_view_size(0)
    {
      sub_lists->signal_appended.connect(sigc::mem_fun(*this, &dynamic_list_collection::handle_append));
      sub_lists->signal_removed.connect(sigc::mem_fun(*this, &dynamic_list_collection::handle_remove));

      concrete_view->signal_appended.connect(sigc::mem_fun(*this, &dynamic_list_collection::handle_concrete_view_append));
      concrete_view->signal_removed.connect(sigc::mem_fun(*this, &dynamic_list_collection::handle_concrete_view_remove));
    }

    template<typename T>
    boost::shared_ptr<dynamic_list_collection<T> >
    dynamic_list_collection<T>::create()
    {
      return boost::make_shared<dynamic_list_collection>();
    }

    template<typename T>
    void dynamic_list_collection<T>::handle_append(const boost::shared_ptr<dynamic_list<T> > &list)
    {
      std::size_t idx_within_list = 0;
      for(boost::shared_ptr<enumerator<T> > e = list->enumerate();
          e->advance(); )
        {
          const std::size_t concrete_view_idx = concrete_view_size;
          ++concrete_view_size;
          concrete_view->append(cell(concrete_view_idx, idx_within_list, e->get_current()));

          ++idx_within_list;
        }
    }

    template<typename T>
    void dynamic_list_collection<T>::handle_remove(const boost::shared_ptr<dynamic_list<T> > &list, int idx)
    {
      std::size_t idx_within_list = 0;
      for(boost::shared_ptr<enumerator<T> > e = list->enumerate();
          e->advance(); )
      {
        // Copying the value is just paranoia here; the concrete view
        // should handle that properly.
        T value = e->get_current();
        concrete_view->remove(cell(idx, idx_within_list, value));
      }
    }

    template<typename T>
    void dynamic_list_collection<T>::handle_concrete_view_append(const cell &value)
    {
      signal_appended(value.get_value());
    }

    template<typename T>
    void dynamic_list_collection<T>::handle_concrete_view_remove(const cell &value, int idx)
    {
      signal_removed(value.get_value(), idx);
    }

    template<typename T>
    boost::shared_ptr<enumerator<T> > dynamic_list_collection<T>::enumerate()
    {
      return boost::make_shared<my_enumerator>(concrete_view->enumerate());
    }

    template<typename T>
    void dynamic_list_collection<T>::add_list(const boost::shared_ptr<dynamic_list<T> > &lst)
    {
      sub_lists->append(lst);
    }

    template<typename T>
    void dynamic_list_collection<T>::remove_list(const boost::shared_ptr<dynamic_list<T> > &lst)
    {
      sub_lists->remove(lst);
    }
  }
}

#endif // DYNAMIC_LIST_COLLECTION_H
