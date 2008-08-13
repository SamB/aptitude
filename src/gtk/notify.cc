// -*-c++-*-

// notify.h
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

#include "notify.h"
#include "aptitude.h"

#include <iostream>

namespace gui
{

  Notification::Notification(const Glib::RefPtr<Gtk::TextBuffer> buffer, std::vector<Gtk::Button *> buttons)
  {
    Gtk::TextView * textview = manage(new Gtk::TextView(buffer));
    textview->show();
    pack_start(*textview, true, true);
    for (std::vector<Gtk::Button *>::iterator button_iter = buttons.begin();
    button_iter != buttons.end(); button_iter++)
    {
      (*button_iter)->show();
      pack_start(*manage(*button_iter), true, true);
    }
    // TODO: Also pack a close button
  }

  NotifyView::NotifyView(BaseObjectType* cobject, const Glib::RefPtr<Gnome::Glade::Xml>& refGlade) : Gtk::VBox(cobject)
  {
    refGlade->get_widget("main_notify_rows", rows);
  }

  NotifyView::~NotifyView()
  {
    // TODO Auto-generated destructor stub
  }

  void NotifyView::add_global_notification(Notification * notification)
  {
    add_local_notification(notification, NULL);
  }

  void NotifyView::add_local_notification(Notification * notification, Tab * tab)
  {
    notifications_map.push_back(std::make_pair(tab, notification));
    notification->show();
    rows->pack_start(*notification);
  }

  void NotifyView::remove_notification(Notification * notification)
  {
    for (std::vector<std::pair<Tab *, Notification *> >::iterator tabnotification_iter = notifications_map.begin();
    tabnotification_iter != notifications_map.end(); tabnotification_iter++)
    {
      if (tabnotification_iter->second == notification)
      {
        notifications_map.erase(tabnotification_iter);
      }
    }
    remove(*notification);
  }

  void NotifyView::tab_changed(Tab * tab)
  {
    current_tab = tab;
    for (std::vector<std::pair<Tab *, Notification *> >::iterator tabnotification_iter = notifications_map.begin();
    tabnotification_iter != notifications_map.end(); tabnotification_iter++)
    {
      if (tabnotification_iter->first == current_tab)
      {
        tabnotification_iter->second->show();
      }
      else if (tabnotification_iter->first == NULL)
      {
        ;;
      }
      else
      {
        tabnotification_iter->second->hide();
      }
    }
  }

}
