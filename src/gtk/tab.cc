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

#include <gui.h>

namespace gui
{

  Tab::Tab(TabType _type, const Glib::ustring &_label,
           const Glib::RefPtr<Gnome::Glade::Xml> &_xml, const std::string &widgetName)
    : type(_type), label(_label),
      xml(_xml), widget(NULL)
  {
    xml->get_widget(widgetName, widget);

    // TODO: Should do something about this. Create a dedicated toplevel for these widgets.
    Glib::RefPtr<Gnome::Glade::Xml> refGlade = Gnome::Glade::Xml::create(glade_main_file, "main_notebook_download_label_hbox");
    //Gtk::HBox * label_widget;
    refGlade->get_widget("main_notebook_download_label_hbox", label_widget);
    //Gtk::Label * label_label;
    refGlade->get_widget("main_notebook_download_label", label_label);
    Gtk::Button * label_button;
    refGlade->get_widget("main_notebook_download_close", label_button);
    // Maybe we should create a close() method on the Tab so it can clean itself up or make a destructor.
    label_button->signal_clicked().connect(sigc::bind(sigc::mem_fun(*(pMainWindow->get_notebook()), &TabsManager::remove_page), *this));
    if (_label != "")
    {
      label_label->set_text(_label);
    }
    else
    {
      label_label->set_text("generic tab: " + label);
    }
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
    return 0;
  }

  TabsManager::TabsManager(BaseObjectType* cobject, const Glib::RefPtr<Gnome::Glade::Xml>& refGlade) :
    Gtk::Notebook(cobject)
  {
    ;;
  }

  int TabsManager::append_page(Tab& tab)
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
    return rval;
  }

  void TabsManager::remove_page(Tab& tab)
  {
    Gtk::Notebook::remove_page(*(tab.get_widget()));
  }

}
