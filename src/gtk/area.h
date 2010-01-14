/** \file area.h */    // -*-c++-*-

// Copyright (C) 2009-2010 Daniel Burrows
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

#ifndef AREA_H
#define AREA_H

#include <boost/shared_ptr.hpp>
#include <boost/weak_ptr.hpp>

#include <cwidget/generic/util/bool_accumulate.h>

#include <gtkmm/image.h>

#include <sigc++/signal.h>

// "areas" are the broad functional categories that tabs are grouped
// into.  Each area contains one or more "tabs" (views the user can
// select), along with one or more "notifications" (warnings for the
// user, or indications of the progress of an ongoing background
// operation).
//
// This file defines the abstractions used to describe the areas of
// all active tabs and ongoing notifications within each area.
//
// Areas aren't especially performance-critical, so I've hidden the
// implementations behind abstract interfaces in order to better
// insulate the rest of the code from the implementation details.
//
// Areas are not thread-safe: only the main GUI thread should read and
// write them.
//
// Below, the "view implementation" refers to the code that displays
// the areas, tabs and so on.

namespace gui
{
  class area_info;
  class notification_info;
  class tab_info;


  /** \brief Data types related to progress information. */
  // @{

  /** \brief The type of progress information being stored.
   */
  enum progress_type
  {
    /** \brief There is no progress information. */
    progress_type_none,
    /** \brief There is only a progress pulse. */
    progress_type_pulse,
    /** \brief Full progress information is available. */
    progress_type_bar
  };

  class progress_info
  {
    progress_type type;
    double progress_fraction;
    std::string progress_status;

    progress_info(double _progress_fraction,
		  std::string _progress_status)
      : type(progress_type_bar),
        progress_fraction(_progress_fraction),
        progress_status(_progress_status)
    {
    }

    progress_info(progress_type _type)
      : type(_type), progress_fraction(0)
    {
    }

  public:
    static progress_info none()
    {
      return progress_info(progress_type_none);
    }

    static progress_info pulse()
    {
      return progress_info(progress_type_pulse);
    }

    static progress_info bar(double fraction,
			     const std::string &status)
    {
      return progress_info(fraction, status);
    }
  };

  // @}

  /** \brief Utility class to allow generic iteration over a list.
   *
   *  Unlike an iterator, instantiations of this class can be used
   *  without the definition of the concrete object being visible to
   *  the user.  Like Java and .NET iterators, these iterators start
   *  "before" the list being iterated over and must be advanced to
   *  the first entry.
   */
  template<typename T>
  class enumerator
  {
  public:
    virtual ~enumerator() {}

    typedef T value_type;

    /** \brief Get the value at the current position. */
    virtual T get_current() = 0;

    /** \brief Advance to the next entry in the list.
     *
     *  \return \b true if there was another entry, or \b false if we
     *  reached the end of the list.
     */
    virtual bool advance() = 0;
  };

  /** \brief The abstract description of a static collection of areas.
   *
   *  It would be possible, and attractive from one point of view, to
   *  use a TreeModel here.  It would also make life way harder for
   *  consumers of the class, given that the list of areas is static.
   *
   *  (arguably it's overkill to have a generic interface this, but
   *   OTOH it avoids trouble if we, e.g., move to a mutable area list
   *   and change the representation)
   */
  class area_list : public sigc::trackable
  {
  public:
    virtual ~area_list() {}

    virtual int get_size() = 0;

    virtual boost::shared_ptr<enumerator<boost::shared_ptr<area_info> > > get_areas() = 0;
  };

  /** \brief Create an immutable area list from an STL vector of
   *  areas.
   */
  boost::shared_ptr<area_list> create_area_list(const std::vector<boost::shared_ptr<area_info> > &areas);

  /** \brief The abstract description of an area. */
  class area_info : public sigc::trackable
  {
  public:
    virtual ~area_info() {}

    /** \brief Get the name of this area (e.g., "Upgrade" or
     *	"Search").
     */
    virtual std::string get_name() = 0;

    /** \brief Get a description of this area. */
    virtual std::string get_description() = 0;

    /** \brief Get the icon of this area. */
    virtual Glib::RefPtr<Gdk::Pixbuf> get_icon() = 0;


    typedef enumerator<boost::shared_ptr<tab_info> > tab_enumerator;

    /** \brief Enumerate the tabs in this area.
     *
     *  To get a consistent picture of the tabs, the caller should
     *  enumerate them before any other process adds or removes a tab.
     *  Typically this means enumerating them in a tight loop.
     */
    virtual boost::shared_ptr<tab_enumerator> get_tabs() = 0;

    /** \brief Append a tab to this area's tab list. */
    virtual void append_tab(const boost::shared_ptr<tab_info> &tab) = 0;

    /** \brief Remove a tab from this area's tab list. */
    virtual void remove_tab(const boost::shared_ptr<tab_info> &tab) = 0;




    typedef enumerator<boost::shared_ptr<notification_info> > notification_enumerator;

    /** \brief Enumerate the notifications in this area.
     *
     *  To get a consistent picture of the notifications, the caller
     *  should enumerate them before any other process adds or removes
     *  a notification.  Typically this means enumerating them in a
     *  tight loop.
     */
    virtual boost::shared_ptr<notification_enumerator> get_notifications() = 0;

    /** \brief Append a notification to this area's notification list. */
    virtual void append_notification(const boost::shared_ptr<notification_info> &notification) = 0;

    /** \brief Remove a notification from this area's notification list. */
    virtual void remove_notification(const boost::shared_ptr<notification_info> &notification) = 0;


    /** \brief Signals */
    // @{

    /** \brief Emitted after a tab is appended to the area's tab list. */
    sigc::signal<void, boost::shared_ptr<tab_info> > signal_tab_appended;

    /** \brief Emitted after a tab is removed from the area's tab list. */
    sigc::signal<void, boost::shared_ptr<tab_info> > signal_tab_removed;



    /** \brief Emitted after a notification is appended to the area's notification list. */
    sigc::signal<void, boost::shared_ptr<notification_info> > signal_notification_appended;

    /** \brief Emitted after a notification is removed from the area's notification list. */
    sigc::signal<void, boost::shared_ptr<notification_info> > signal_notification_removed;

    // @}
  };

  boost::shared_ptr<area_info> create_area_info(const std::string &name,
						const std::string &description,
						const Glib::RefPtr<Gdk::Pixbuf> icon);

  /** \brief The abstract description of a tab. */
  class tab_info : public sigc::trackable
  {
  public:
    virtual ~tab_info() {}

    /** \brief Get the name of this tab. */
    virtual std::string get_name() = 0;

    /** \brief Get tooltip information for this tab.
     *
     *  \param tooltip_text A location in which to store the text of the
     *                      tooltip.
     *  \param tooltip_window A location in which to store a pointer to
     *                        a window that should be used to display
     *                        the tooltip.
     *
     *  tooltip_text is empty if there is no text; tooltip_window is
     *  NULL if there is no window.
     */
    virtual void get_tooltip(std::string &tooltip_text,
			     Gtk::Window * &tooltip_window) = 0;

    /** \brief Set the tooltip text for this tab.
     *
     *  Automatically deletes and clears the tooltip window.
     */
    virtual void set_tooltip(const std::string &tooltip_text) = 0;

    /** \brief Set the tooltip window for this tab.
     *
     *  As a side effect, deletes any old tooltip window and clears
     *  the old text.
     */
    virtual void set_tooltip(Gtk::Window *tooltip_window) = 0;

    /** \brief Get the icon of this tab. */
    virtual Glib::RefPtr<Gdk::Pixbuf> get_icon() = 0;

    /** \brief Get any progress information associated with this tab. */
    virtual progress_info get_progress() = 0;

    /** \brief Update the progress information associated with this tab. */
    virtual void set_progress(const progress_info &info) = 0;

    /** \brief Get the main widget of this tab. */
    virtual Gtk::Widget *get_widget() = 0;


    /** \return \b true if this tab is currently visible.
     *
     *  This property is maintained by the view and read by the tab's
     *  implementation.  Note that it is \e not automatically
     *  maintained by set_active_tab(), because that routine lacks the
     *  information necessary to determine which tab is visible;
     *  normally an implementation of the view will hook into
     *  signal_active_tab_changed and emit this as appropriate.
     */
    virtual bool get_active() = 0;

    /** \brief Set whether the tab is currently visible. */
    virtual void set_active(bool visible) = 0;


    /** \brief Signals */
    // @{

    /** \brief Emitted when the tooltip information changes. */
    sigc::signal<void, std::string, Gtk::Window *> tooltip_changed;

    /** \brief Emitted when the progress information changes. */
    sigc::signal<void, progress_info> progress_changed;

    /** \brief Emitted when the tab becomes active or inactive.
     *
     *  Emitted by the view implementation and read by the tab.
     */
    sigc::signal<void, bool> active_changed;

    /** \brief Emitted when the tab should be made visible.
     *
     *  This is emitted by any part of the GUI that wants to display
     *  a tab and read by the view implementation.
     */
    sigc::signal<void> activated;

    /** \brief Emitted when the tab is about to be closed.
     *
     *  Emitted by the view implementation and read by the tab.  The
     *  tab's widget is destroyed after this signal is emitted.
     */
    sigc::signal<void> closing;

    // @}
  };

  boost::shared_ptr<tab_info> create_tab(const std::string &name,
					 const Glib::RefPtr<Gdk::Pixbuf> &icon,
					 Gtk::Widget *widget);

  /** \brief The abstract description of a notification. */
  class notification : public sigc::trackable
  {
    virtual ~notification() {}

    /** \brief Get the name of this notification. */
    virtual std::string get_name() = 0;

    /** \brief Get a longer description of this notification. */
    virtual std::string get_description() = 0;

    /** \brief Get the icon of this notification. */
    virtual Glib::RefPtr<Gdk::Pixbuf> get_icon() = 0;


    /** \brief Retrieve the progress display associated with this
     *  notification.
     */
    virtual progress_info get_progress() = 0;

    /** \brief Update the progress display associated with this
     *	notification.
     */
    virtual void set_progress(const progress_info &progress) = 0;


    /** \brief Signals */

    // @{

    /** \brief Emitted when the progress information changes. */
    sigc::signal<void, progress_info> progress_changed;

    /** \brief Emitted when the user clicks the notification. */
    sigc::signal<void> clicked;

    // @}
  };
}

#endif // AREA_H

