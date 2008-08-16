// notify.h             -*-c++-*-
//
//  Copyright 1999-2008 Daniel Burrows
//  Copyright 2008 Obey Arthur Liu
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; see the file COPYING.  If not, write to
//  the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//  Boston, MA 02111-1307, USA.

#ifndef NOTIFY_H_
#define NOTIFY_H_

#undef OK
#include <gtkmm.h>

#include <gtk/tab.h>

namespace gui
{
  /** \brief Basic notification class.
   *
   *  \note This derives from EventBox so that it can support changing
   *  the color of the widget (you need a GdkWindow to do that).
   */
  class Notification : public Gtk::EventBox
  {
    private:
      bool onetimeuse;

      Gtk::HBox *hbox;
      Gtk::TextView * textview;
    public:
      /** \brief Create a notification.
       *
       * \param onetimeuse      If true, the notification is deleted when
       *                        the user clicks the close button; otherwise
       *                        it is only hidden.
       */
      Notification(bool onetimeuse);

      /** \brief Create a notification with the given text and no buttons. */
      Notification(const Glib::ustring &text, bool onetimeuse);
      bool is_onetimeuse() { return onetimeuse; };
      void add_button(Gtk::Button *);
      void set_buffer(const Glib::RefPtr<Gtk::TextBuffer> &buffer);
      /** \brief Change the background color of this notification. */
      void set_color(const Gdk::Color &color);
      /** \brief Add the elements of the notification that should appear to the
       *  right-hand side of the text it displays.
       *
       *  For instance, this adds the close button.
       */
      void finalize();
      sigc::signal0<void> close_clicked;
  };

  /** \brief Stores a stack of global and tab-local notifications.
   *
   *  Each notification is just a bunch of widgets packed into a box.
   */
  class NotifyView : public Gtk::VBox
  {
    private:
      Gtk::VBox * rows;
    public:
      NotifyView(BaseObjectType* cobject, const Glib::RefPtr<Gnome::Glade::Xml>& refGlade);
      ~NotifyView();
      /** \brief Add a new notification.
       *
       *  The notification is owned by this view after being added.
       *
       *  \param notification   The notification to add.
       *  The NotifyView will take ownership of this pointer.
       *  \param tab            The tab to add the notification for.
       *  The notification will be displayed only when this tab is the
       *  currently active one in the main notebook.
       */
      void add_notification(Notification * notification, Tab * tab);
      /** \brief Remove a notification.
       *
       *  \param notification   The notification to remove.
       */
      void remove_notification(Notification * notification);
      Gtk::VBox * get_rows() { return rows; };
  };

}

#endif /* NOTIFY_H_ */
