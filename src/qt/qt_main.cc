/** \file qt_main.cc */
//
// Copyright (C) 2010 Daniel Burrows
// Copyright 2008-2009 Obey Arthur Liu
// Copyright (C) 2010 Piotr Galiszewski
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

// Local includes
#include "qt_main.h"

#include "windows/main_window.h"

// System includes
#include <QtGui/QApplication>

#include <signal.h>

namespace aptitude
{
  namespace gui
  {
    namespace qt
    {
      bool main(int argc, char **argv)
      {
	// Don't crash if a subprocess breaks a pipe.
	signal(SIGPIPE, SIG_IGN);

	QApplication app(argc,argv);

	main_window *main = new main_window;
	main->show();

	app.exec();
	return true;
      }
    }
  }
}
