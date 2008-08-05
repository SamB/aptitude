// info.h             -*-c++-*-
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

#ifndef INFO_H_
#define INFO_H_

#undef OK
#include <gtkmm.h>

#include <apt-pkg/pkgcache.h>

#include <gtk/tab.h>

#include <cwidget/generic/util/ref_ptr.h>

namespace gui
{
  class EntityView;

  class InfoTab : public Tab
  {
    private:
      Gtk::TextView * textview;
      cwidget::util::ref_ptr<EntityView> pVersionsView;
      cwidget::util::ref_ptr<EntityView> pDependsView;
    public:
      InfoTab(const Glib::ustring &label);
      void disp_package(pkgCache::PkgIterator pkg, pkgCache::VerIterator ver);

      /** \brief Convenience function to create and display a new tab
       *  showing the given package and version.
       */
      static void show_tab(const pkgCache::PkgIterator &pkg,
			   const pkgCache::VerIterator &ver);
  };

}

#endif /* INFO_H_ */
