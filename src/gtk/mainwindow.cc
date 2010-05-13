/** \file mainwindow.cc */

// Copyright (C) 2010 Daniel Burrows
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

#include "mainwindow.h"

#include <loggers.h>

#include <gtk/toplevel/model.h>
#include <gtk/toplevel/view.h>

#include <gtkmm.h>
#include <libglademm.h>

using aptitude::Loggers;

namespace gui
{
  namespace
  {
    class main_window : public Gtk::Window
    {
      Gtk::Bin *main_bin;
      logging::LoggerPtr logger;

    public:
      main_window(BaseObjectType *cobject, const Glib::RefPtr<Gnome::Glade::Xml> &glade)
        : Gtk::Window(cobject)
      {
        // ... maybe use an alignment to ensure we know where it is?
        glade->get_widget("main_bin", main_bin);
        logger = Loggers::getAptitudeGtkMainWindow();
      }

      /** \brief Attach a view to this main window.
       *
       *  Must be invoked exactly once.
       */
      void set_view(const boost::shared_ptr<toplevel::view> &view)
      {
        if(main_bin->get_child() != NULL)
          LOG_ERROR(logger, "Two views added to the main window, discarding the second one.");
        else
          {
            LOG_TRACE(logger, "Adding the view " << view << " to the main window.");
            main_bin->add(*view->get_widget());
          }
      }
    };
  }

  Gtk::Window *create_mainwindow(const Glib::RefPtr<Gnome::Glade::Xml> &glade,
                                 const boost::shared_ptr<toplevel::view> &view)
  {
    main_window *rval;
    glade->get_widget_derived("main_window", rval);
    rval->set_view(view);

    return rval;
  }
}
