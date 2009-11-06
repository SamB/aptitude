/** \file screenshot.cc */


// Copyright (C) 2009 Daniel Burrows
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

#include "screenshot.h"

#include <loggers.h>

#include <gdkmm/cursor.h>

#include <gtk/screenshot_cache.h>

using aptitude::Loggers;

namespace gui
{
  // TODO: this shows up in a couple places; it might be a sign that I
  // should have a "screenshot_id" class with its own operator<<,
  // hash, equality, etc.
  std::string screenshot_image::describe() const
  {
    std::string typestr;
    switch(type)
      {
      case aptitude::screenshot_thumbnail:
	typestr = "thumbnail";
	break;

      case aptitude::screenshot_full:
	typestr = "full-size";
	break;

      default:
	typestr = "BADTYPE";
	break;
      }

    return "the " + typestr + " screenshot of " + package_name;
  }

  void screenshot_image::disconnect()
  {
    if(screenshot.get() != NULL && !download_complete)
      {
	LOG_TRACE(Loggers::getAptitudeGtkScreenshotImage(),
		  "screenshot_image: disconnecting " << describe());

	screenshot_prepared_connection.disconnect();
	screenshot_failed_connection.disconnect();
	screenshot_updated_connection.disconnect();
	screenshot_ready_connection.disconnect();

	screenshot->cancel();
	screenshot.reset();

	// Discard any partial screenshot.
	clear();
      }
  }

  void screenshot_image::prepared()
  {
    set(screenshot->get_screenshot());

    LOG_TRACE(Loggers::getAptitudeGtkScreenshotImage(),
	      "screenshot_image: pixbuf prepared for " << describe());
  }

  void screenshot_image::success()
  {
    LOG_TRACE(Loggers::getAptitudeGtkScreenshotImage(),
	      "screenshot_image: registering success for " << describe());

    set(screenshot->get_screenshot());
    download_complete = true;
  }

  void screenshot_image::updated(int x, int y, int width, int height)
  {
    LOG_TRACE(Loggers::getAptitudeGtkScreenshotImage(),
	      "screenshot_image: update for (" << x << ", " << y << ", " << width << ", " << height << ")");

    queue_draw_area(x, y, width, height);
  }

  void screenshot_image::connect()
  {
    if(screenshot.get() == NULL)
      {
	LOG_TRACE(Loggers::getAptitudeGtkScreenshotImage(),
		  "screenshot_image: requesting " << describe());

	screenshot = get_screenshot(aptitude::screenshot_key(type, package_name));

	screenshot_failed_connection = screenshot->get_signal_failed().connect(sigc::hide(sigc::mem_fun(*this, &screenshot_image::disconnect)));
	screenshot_prepared_connection = screenshot->get_signal_prepared().connect(sigc::mem_fun(*this, &screenshot_image::prepared));
	screenshot_updated_connection = screenshot->get_signal_updated().connect(sigc::mem_fun(*this, &screenshot_image::updated));
	screenshot_ready_connection = screenshot->get_signal_ready().connect(sigc::mem_fun(*this, &screenshot_image::success));

	show();
      }
  }

  screenshot_image::screenshot_image(const std::string &_package_name,
				     aptitude::screenshot_type _type)
    : package_name(_package_name),
      type(_type),
      download_complete(false)
  {
    sigc::slot<void> connect_slot(sigc::mem_fun(*this, &screenshot_image::connect));
    signal_expose_event().connect(sigc::bind_return(sigc::hide(connect_slot), true));
    signal_no_expose_event().connect(sigc::bind_return(sigc::hide(connect_slot), true));
  }

  void screenshot_image::start_download()
  {
    connect();
  }

  void screenshot_image::cancel_download()
  {
    disconnect();
  }



  active_screenshot_image::active_screenshot_image(const std::string &package,
						   aptitude::screenshot_type type)
    : image(package, type)
  {
    image.show();
    add(image);
    show();
  }

  void active_screenshot_image::on_realize()
  {
    Gtk::EventBox::on_realize();

    // Check that the window really exists, just out of paranoia:
    if(clickable && is_realized() && get_window())
      get_window()->set_cursor(Gdk::Cursor(Gdk::HAND1));
  }

  bool active_screenshot_image::on_button_press_event(GdkEventButton *event)
  {
    switch(event->type)
      {
      case GDK_BUTTON_PRESS:
	if(clickable)
	  clicked();
	return true;
      default:
	return Gtk::EventBox::on_button_press_event(event);
      }
  }

  void active_screenshot_image::enable_clickable()
  {
    clickable = true;
    if(is_realized() && get_window())
      get_window()->set_cursor(Gdk::Cursor(Gdk::HAND1));
  }
}
