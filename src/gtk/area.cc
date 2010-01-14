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

#include <boost/multi_index_container.hpp>
#include <boost/multi_index/hashed_index.hpp>
#include <boost/multi_index/mem_fun.hpp>
#include <boost/multi_index/sequenced_index.hpp>

#include <gtkmm/window.h>

using namespace boost::multi_index;

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
    // Note: I don't recall whether hash<> is defined for shared
    // pointers -- if it is, I could just use identity<> below instead
    // of mem_fun<>.
    class tab_hash_tag;
    class tab_list_tag;
    typedef multi_index_container<
      boost::shared_ptr<tab_info>,
      indexed_by<
	hashed_unique<tag<tab_hash_tag>,
		      const_mem_fun<boost::shared_ptr<tab_info>, tab_info *,
				    &boost::shared_ptr<tab_info>::get> >,
	sequenced<tag<tab_list_tag> > >
      > tab_collection;

    typedef tab_collection::index<tab_hash_tag>::type tab_hash;
    typedef tab_collection::index<tab_list_tag>::type tab_list;


    class notification_hash_tag;
    class notification_list_tag;
    typedef multi_index_container<
      boost::shared_ptr<notification_info>,
      indexed_by<
	hashed_unique<tag<notification_hash_tag>,
		      const_mem_fun<boost::shared_ptr<notification_info>, notification_info *,
				    &boost::shared_ptr<notification_info>::get> >,
	sequenced<tag<notification_list_tag> > >
      > notification_collection;

    typedef notification_collection::index<notification_hash_tag>::type notification_hash;
    typedef notification_collection::index<notification_list_tag>::type notification_list;

    tab_collection tabs;
    notification_collection notifications;

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
      const tab_list &tabs_ordered = tabs.get<tab_list_tag>();

      return boost::make_shared<iterator_enumerator_with_keepalive<tab_list::const_iterator, area_info_impl> >(tabs_ordered.begin(), tabs_ordered.end(), shared_from_this());
    }

    void append_tab(const boost::shared_ptr<tab_info> &tab)
    {
      tab_hash &tabs_hashed = tabs.get<tab_hash_tag>();

      if(tabs_hashed.find(tab.get()) == tabs_hashed.end())
	{
	  tab_list &tabs_ordered = tabs.get<tab_list_tag>();

	  tabs_ordered.push_back(tab);
	  signal_tab_appended(tab);
	}
    }

    void remove_tab(const boost::shared_ptr<tab_info> &tab)
    {
      tab_hash &tabs_hashed = tabs.get<tab_hash_tag>();

      tab_hash::iterator found = tabs_hashed.find(tab.get());

      if(found != tabs_hashed.end())
	{
	  tabs_hashed.erase(found);
	  signal_tab_removed(tab);
	}
    }




    boost::shared_ptr<notification_enumerator> get_notifications()
    {
      const notification_list &notifications_ordered = notifications.get<notification_list_tag>();

      return boost::make_shared<iterator_enumerator_with_keepalive<notification_list::const_iterator, area_info_impl> >(notifications_ordered.begin(), notifications_ordered.end(), shared_from_this());
    }

    void append_notification(const boost::shared_ptr<notification_info> &notification)
    {
      notification_hash &notifications_hashed = notifications.get<notification_hash_tag>();

      if(notifications_hashed.find(notification.get()) == notifications_hashed.end())
	{
	  notification_list &notifications_ordered = notifications.get<notification_list_tag>();

	  notifications_ordered.push_back(notification);
	  signal_notification_appended(notification);
	}
    }

    void remove_notification(const boost::shared_ptr<notification_info> &notification)
    {
      notification_hash &notifications_hashed = notifications.get<notification_hash_tag>();

      notification_hash::iterator found = notifications_hashed.find(notification.get());

      if(found != notifications_hashed.end())
	{
	  notifications_hashed.erase(found);
	  signal_notification_removed(notification);
	}
    }
  };

  boost::shared_ptr<area_info> create_area_info(const std::string &name,
						const std::string &description,
						const Glib::RefPtr<Gdk::Pixbuf> &icon)
  {
    return boost::make_shared<area_info_impl>(name, description, icon);
  }

  class tab_info_impl : public tab_info
  {
    std::string name;
    Glib::RefPtr<Gdk::Pixbuf> icon;

    std::string tooltip_text;
    Gtk::Window *tooltip_window;

    Gtk::Widget *tab;

    progress_info progress;

    bool active;

  public:
    tab_info_impl(const std::string &_name,
		  const Glib::RefPtr<Gdk::Pixbuf> &_icon,
		  Gtk::Widget *_tab)
      : name(_name),
	icon(_icon),
	tooltip_window(NULL),
	tab(_tab),
	progress(progress_info::none()),
	active(false)
    {
    }

    ~tab_info_impl()
    {
      delete tooltip_window;
    }

    std::string get_name() { return name; }

    void get_tooltip(std::string &out_tooltip_text,
		     Gtk::Window * &out_tooltip_window)
    {
      out_tooltip_text = tooltip_text;
      out_tooltip_window = tooltip_window;
    }

    void set_tooltip(const std::string &new_tooltip_text)
    {
      delete tooltip_window;
      tooltip_window = NULL;

      tooltip_text = new_tooltip_text;
      tooltip_changed(tooltip_text, tooltip_window);
    }

    void set_tooltip(Gtk::Window *new_tooltip_window)
    {
      tooltip_text.clear();

      delete tooltip_window;
      tooltip_window = new_tooltip_window;

      tooltip_changed(tooltip_text, tooltip_window);
    }

    Glib::RefPtr<Gdk::Pixbuf> get_icon() { return icon; }

    progress_info get_progress() { return progress; }

    void set_progress(const progress_info &info)
    {
      progress = info;
      progress_changed(progress);
    }

    Gtk::Widget *get_widget() { return tab; }

    bool get_active() { return active; }
    void set_active(bool new_active)
    {
      if(active != new_active)
	{
	  active = new_active;
	  active_changed(active);
	}
    }
  };

  boost::shared_ptr<tab_info> create_tab(const std::string &name,
					 const Glib::RefPtr<Gdk::Pixbuf> &icon,
					 Gtk::Widget *widget)
  {
    return boost::make_shared<tab_info_impl>(name, icon, widget);
  }
}
