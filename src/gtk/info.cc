// -*-c++-*-

// info.h
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

#include "info.h"
#include "aptitude.h"

#undef OK
#include <gtkmm.h>

#include <gui.h>

namespace gui
{

  InfoTab::InfoTab(Glib::ustring label)
  : Tab(Info, label,
      Gnome::Glade::Xml::create(glade_main_file, "main_info_textview"), "main_info_textview")
      {
    get_xml()->get_widget("main_info_textview", textview);
    get_widget()->show();
      }

  void InfoTab::disp_package(pkgCache::PkgIterator pkg, pkgCache::VerIterator ver)
  {
    set_label("Info: " + Glib::ustring(pkg.Name()));

    Glib::RefPtr<Gtk::TextBuffer::Tag> refTagMatch = Gtk::TextBuffer::Tag::create();
    refTagMatch->property_background() = "orange";
    refTagMatch->property_scale() = 3;

    textview->get_buffer()->get_tag_table()->add(refTagMatch);

    textview->get_buffer()->insert_with_tag(textview->get_buffer()->end(), Glib::ustring(pkg.Name()), refTagMatch);
    textview->get_buffer()->insert(textview->get_buffer()->end(), "\n" + Glib::ustring(ver.VerStr()));
    textview->get_buffer()->insert_with_tag(textview->get_buffer()->end(), "\n\nSome information here?", refTagMatch);
  }

}
