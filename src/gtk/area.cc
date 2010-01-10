/** \file area.cc */

// Copyright (C) 2009-2010 Daniel Burrows

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

#include "area.h"

#include <boost/enable_shared_from_this.hpp>
#include <boost/make_shared.hpp>

#include <cwidget/generic/util/eassert.h>

namespace gui
{
  /** \brief An enumerator over a range of STL iterators. */
  template<typename ForwardIter>
  class iterator_enumerator : public enumerator<typename ForwardIter::value_type>
  {
    ForwardIter begin, end;
    bool first;

  public:
    iterator_enumerator(ForwardIter _begin, ForwardIter _end)
      : begin(_begin), end(_end), first(true)
    {
    }

    typename ForwardIter::value_type get_current()
    {
      eassert(!first);
      eassert(begin != end);

      return *begin;
    }

    bool advance()
    {
      if(begin == end)
	return false;

      if(first)
	first = false;
      else
	++begin;

      return true;
    }
  };

  /** \brief An enumerator over an iterator range that holds a strong
   *  shared reference to an object (presumably the one containing the
   *  iterators).
   */
  template<typename ForwardIter, typename T>
  class iterator_enumerator_with_keepalive : public iterator_enumerator<ForwardIter>
  {
    boost::shared_ptr<T> keptalive;

  public:
    iterator_enumerator_with_keepalive(ForwardIter begin, ForwardIter end,
				       boost::shared_ptr<T> _keptalive)
      : iterator_enumerator<ForwardIter>(begin, end),
	keptalive(_keptalive)
    {
    }
  };

  /** \brief Implements the static area list. */
  class area_list_impl : public area_list, public boost::enable_shared_from_this<area_list_impl>
  {
    std::vector<boost::shared_ptr<area_info> > areas;

  public:
    area_list_impl(const std::vector<boost::shared_ptr<area_info> > &_areas)
      : areas(_areas)
    {
    }

    int get_size()
    {
      return static_cast<int>(areas.size());
    }

    boost::shared_ptr<enumerator<boost::shared_ptr<area_info> > > get_areas()
    {
      return boost::make_shared<iterator_enumerator_with_keepalive<std::vector<boost::shared_ptr<area_info> >::const_iterator, area_list_impl> >(areas.begin(), areas.end(), shared_from_this());
    }
  };

  boost::shared_ptr<area_list> create_area_list(const std::vector<boost::shared_ptr<area_info> > &areas)
  {
    return boost::make_shared<area_list_impl>(areas);
  }

  class area_info_impl : public area_info, public boost::enable_shared_from_this<area_info_impl>
  {
    std::string name;
    std::string description;
    Glib::RefPtr<Gdk::Pixbuf> icon;
    std::deque<boost::shared_ptr<tab_info> > tabs;
    std::deque<boost::shared_ptr<notification_info> > notifications;

  public:
    area_info_impl(const std::string &_name,
		   const std::string &_description,
		   const Glib::RefPtr<Gdk::Pixbuf> &_icon)
      : name(_name),
	description(_description),
	icon(_icon)
    {
    }

    std::string get_name() { return name; }
    std::string get_description() { return description; }
    Glib::RefPtr<Gdk::Pixbuf> get_icon() { return icon; }

    boost::shared_ptr<tab_enumerator> get_tabs()
    {
      return boost::make_shared<iterator_enumerator_with_keepalive<std::deque<boost::shared_ptr<tab_info> >::const_iterator, area_info_impl> >(tabs.begin(), tabs.end(), shared_from_this());
    }

    void append_tab(const boost::shared_ptr<tab_info> &tab)
    {
      tabs.push_back(tab);
      signal_tab_appended(tab);
    }

    void remove_tab(const boost::shared_ptr<tab_info> &tab)
    {
      std::deque<boost::shared_ptr<tab_info> >::iterator new_end =
	std::remove(tabs.begin(), tabs.end(), tab);

      if(new_end != tabs.end())
	{
	  tabs.erase(new_end, tabs.end());
	  signal_tab_removed(tab);
	}
    }
  };
}
