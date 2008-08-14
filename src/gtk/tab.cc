// tab.cc
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

#include "tab.h"
#include "aptitude.h"

#undef OK
#include <gtkmm.h>
#include <libglademm/xml.h>

#include <gtk/gui.h>
#include <gtk/notify.h>

#include <iostream>

namespace gui
{
  namespace
  {
    /** \brief The named property we use to attach a tab pointer to
     *  its main widget for inspection or deletion.
     */
    Glib::Quark tab_property("aptitude-tab-manager-tab-object");
  }

  Tab::Tab(TabType _type, const Glib::ustring &_label,
           const Glib::RefPtr<Gnome::Glade::Xml> &_xml, const std::string &widgetName)
    : type(_type), label(_label),
      xml(_xml), widget(NULL)
  {
    // This is a hack to reparent the tab widget into a VBox along with a NotifyView
    Gtk::VBox * vbox = manage(new Gtk::VBox());
    xml->get_widget(widgetName, widget);
    vbox->pack_start(*widget, true, true);
    Glib::RefPtr<Gnome::Glade::Xml> glade_notify = Gnome::Glade::Xml::create(glade_main_file, "main_notify_rows");
    glade_notify->get_widget_derived("main_notify_rows", notifyview);
    vbox->pack_start(*notifyview, false, true);
    widget->show();
    widget = vbox;

    {
      // We're doing some testing here.
      Glib::RefPtr<Gtk::TextBuffer> buffer = Gtk::TextBuffer::create();
      buffer->set_text("This is a tab notification");
      std::vector<Gtk::Button *> buttons;
      Gtk::Button * button = new Gtk::Button("This is a button");
      Gtk::Button * button2 = new Gtk::Button("This is another button");
      button->show();
      buttons.push_back(button);
      buttons.push_back(button2);
      Notification * notification = new Notification(buffer, buttons);
      notifyview->add_notification(notification);
    }

    // TODO: Should do something about this. Create a dedicated toplevel for these widgets.
    Glib::RefPtr<Gnome::Glade::Xml> refGlade = Gnome::Glade::Xml::create(glade_main_file, "main_notebook_download_label_hbox");
    refGlade->get_widget("main_notebook_download_label_hbox", label_widget);
    refGlade->get_widget("main_notebook_download_label", label_label);
    refGlade->get_widget("main_notebook_download_close", label_button);
    // Maybe we should create a close() method on the Tab so it can clean itself up or make a destructor.
    label_button->signal_clicked().connect(close_clicked.make_slot());

    get_widget()->set_data(tab_property, this);

    if (_label != "")
    {
      label_label->set_text(_label);
    }
    else
    {
      label_label->set_text("generic tab: " + label);
    }
  }

  Tab::~Tab()
  {
  }

  void Tab::set_label(Glib::ustring label)
  {
    this->label_label->set_text(label);
  }

  int TabsManager::next_position(TabType type)
  {
    // TODO: implement something more elaborate and workflow-wise intuitive
    return get_n_pages();
  }

  int TabsManager::number_of(TabType type)
  {
    int count = 0;

    for(int i = 0; i < get_n_pages(); ++i)
      {
	Gtk::Widget *page = get_nth_page(i);
	Tab *tab = (Tab*)page->get_data(tab_property);

	if(tab != NULL && tab->get_type() == type)
	  ++count;
      }

    return count;
  }

  TabsManager::TabsManager(BaseObjectType* cobject, const Glib::RefPtr<Gnome::Glade::Xml>& refGlade) :
    Gtk::Notebook(cobject)
  {
    signal_page_removed().connect(sigc::mem_fun(this, &TabsManager::page_removed));
    signal_switch_page().connect(sigc::mem_fun(this, &TabsManager::do_switch_page));
  }

  int TabsManager::append_page(Tab &tab)
  {
    int rval = 0;
    switch (tab.get_type())
      {
    case Dashboard:
      // No more than one Dashboard at once
      if (number_of(Dashboard) == 0)
      {
        rval = insert_page(*(tab.get_widget()), *(tab.get_label_widget()), 0);
      }
      break;
      // TODO: handle other kinds of tabs
    default:
      rval = insert_page(*(tab.get_widget()), *(tab.get_label_widget()), next_position(tab.get_type()));
      }

    tab.close_clicked.connect(sigc::bind(sigc::mem_fun(*this, (void (Gtk::Notebook::*)(Gtk::Widget&))&Gtk::Notebook::remove_page),
					 sigc::ref(*tab.get_widget())));

    return rval;
  }

  void TabsManager::remove_page(Tab &tab)
  {
    Gtk::Notebook::remove_page(*(tab.get_widget()));
  }

  void TabsManager::page_removed(Gtk::Widget *widget, int page)
  {
    if(widget != NULL)
      {
	Tab *tab = (Tab*)widget->get_data(tab_property);

	if(tab != NULL)
	  tab->closed();

	delete tab;
      }
  }

  Tab *TabsManager::get_current_tab()
  {
    Gtk::Widget *current = get_nth_page(get_current_page());
    if(current != NULL)
      return (Tab *)current->get_data(tab_property);
    else
      return NULL;
  }

  void TabsManager::do_switch_page(GtkNotebookPage *page, guint page_idx)
  {
    Tab *tab = NULL;
    Widget *next = get_nth_page(page_idx);
    if(next != NULL)
      tab = (Tab *)next->get_data(tab_property);

    tab_status_button_changed(tab);
  }
}
