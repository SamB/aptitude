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

#include "dynamic_list.h"

#include <boost/enable_shared_from_this.hpp>
#include <boost/make_shared.hpp>
#include <boost/multi_index_container.hpp>
#include <boost/multi_index/hashed_index.hpp>
#include <boost/multi_index/identity.hpp>
#include <boost/multi_index/mem_fun.hpp>
#include <boost/multi_index/sequenced_index.hpp>
#include <boost/shared_ptr.hpp>

namespace aptitude
{
  namespace util
  {
    /** \brief A basic implementation of the dynamic_list interface. */
    template<typename T>
    class dynamic_list_impl
      : public dynamic_list<T>,
        public boost::enable_shared_from_this<dynamic_list_impl<T> >
    {
      // Uses multi_index_container to ensure that removals scale
      // reasonably well.

      class hash_tag;
      class list_tag;
      typedef boost::multi_index::multi_index_container<
        T,
        boost::multi_index::indexed_by<
          boost::multi_index::hashed_unique<
            boost::multi_index::tag<hash_tag>,
            boost::multi_index::identity<T>
            >,
          boost::multi_index::sequenced<
            boost::multi_index::tag<list_tag>
            >
          >
        > collection;

      typedef typename collection::template index<hash_tag>::type hash_index;
      typedef typename collection::template index<list_tag>::type list_index;

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
      list_index &list = entries.template get<list_tag>();

      typedef iterator_enumerator_with_keepalive<typename list_index::const_iterator, dynamic_list_impl> Tenum;

      return boost::make_shared<Tenum>(list.begin(), list.end(),
                                       this->shared_from_this());
    }

    template<typename T>
    void dynamic_list_impl<T>::append(const T &t)
    {
      hash_index &hashed = entries.template get<hash_tag>();
      if(hashed.find(t) == hashed.end())
        {
          list_index &list = entries.template get<list_tag>();

          list.push_back(t);
          signal_appended(t);
        }
    }

    template<typename T>
    void dynamic_list_impl<T>::remove(const T &t)
    {
      hash_index &hashed = entries.template get<hash_tag>();
      typename hash_index::iterator found = hashed.find(t);

      if(found != hashed.end())
        {
          hashed.erase(found);
          signal_removed(*found);
        }
    }
  }
}

