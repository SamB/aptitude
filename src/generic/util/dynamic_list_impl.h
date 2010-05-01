/** \brief dynamic_list_impl.h */   // -*-c++-*-

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

#ifndef DYNAMIC_LIST_IMPL_H
#define DYNAMIC_LIST_IMPL_H

#include "dynamic_list.h"

#include <boost/enable_shared_from_this.hpp>
#include <boost/make_shared.hpp>
#include <boost/shared_ptr.hpp>

#include <vector>

namespace aptitude
{
  namespace util
  {
    /** \brief A basic implementation of the dynamic_list interface. */
    template<typename T>
    class dynamic_list_impl
      : public writable_dynamic_list<T>,
        public boost::enable_shared_from_this<dynamic_list_impl<T> >
    {
      typedef std::vector<T> collection;
      collection entries;

    public:
      dynamic_list_impl();

      /** \brief Create an empty list. */
      static boost::shared_ptr<dynamic_list_impl> create();

      boost::shared_ptr<enumerator<T> > enumerate();
      void append(const T &t);
      void remove(const T &t);
    };

    template<typename T>
    dynamic_list_impl<T>::dynamic_list_impl()
    {
    }

    template<typename T>
    boost::shared_ptr<dynamic_list_impl<T> >
    dynamic_list_impl<T>::create()
    {
      return boost::make_shared<dynamic_list_impl>();
    }

    template<typename T>
    boost::shared_ptr<enumerator<T> > dynamic_list_impl<T>::enumerate()
    {
      typedef iterator_enumerator_with_keepalive<typename collection::const_iterator, dynamic_list_impl> Tenum;

      return boost::make_shared<Tenum>(entries.begin(), entries.end(),
                                       this->shared_from_this());
    }

    template<typename T>
    void dynamic_list_impl<T>::append(const T &t)
    {
      entries.push_back(t);
      signal_appended(t);
    }

    template<typename T>
    void dynamic_list_impl<T>::remove(const T &t)
    {
      typename collection::iterator found =
        std::find(entries.begin(), entries.end(), t);

      if(found != entries.end())
        {
          const std::size_t idx = found - entries.begin();

          T val = *found;
          entries.erase(found);
          signal_removed(val, idx);
        }
    }
  }
}

#endif // DYNAMIC_LIST_IMPL_H
