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

  // This class doesn't do much, yet
  class Notification : public Gtk::HBox
  {
    public:
      Notification(const Glib::RefPtr<Gtk::TextBuffer> buffer, std::vector<Gtk::Button *> buttons);
  };

  class NotifyView : public Gtk::VBox
  {
    private:
      Gtk::VBox * rows;
      std::vector<std::pair<Tab *, Notification *> > notifications_map;
      Tab * current_tab;
    public:
      NotifyView(BaseObjectType* cobject, const Glib::RefPtr<Gnome::Glade::Xml>& refGlade);
      ~NotifyView();
      void add_global_notification(Notification * notification);
      void add_local_notification(Notification * notification, Tab * tab);
      void remove_notification(Notification * notification);
      void tab_changed(Tab * tab);
      Gtk::VBox * get_rows() { return rows; };
  };

}

#endif /* NOTIFY_H_ */
