/** \file main_window.h */   // -*-c++-*-
//
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

#include <QtGui/QMainWindow>

class QMenu;

namespace aptitude
{
  namespace gui
  {
    namespace qt
    {
      class status_widget;
      class tab_widget;

      /** \brief Widget representing main window of the program.
       *
       *  main_window is a central point of program. It is splitted
       *  into tree main parts: menu bar, tab widget and status widget
       *
       *  menu bar is splitted into 3 parts and contains the most common
       *  program's actions
       *
       *  Tab widget contains tabs. Each tab is responsible for one particular
       *  functionality of the program e.g. searching of packages or resolving
       *  conflicts
       *
       *  Status widget shows state of the program (e.g. number of requested
       *  changes) and progress of active tasks
       */
      class main_window : public QMainWindow
      {
	Q_OBJECT

	tab_widget *tabs;
	status_widget *status;

	QMenu *file_menu;
	QMenu *packages_menu;
	QMenu *help_menu;

	/** \brief Create actions used in main menu and contexts menus. */
	void create_actions();

	/** \brief Create layouts and widgets. */
	void create_gui();

	/** \brief Create a main menu. */
	void create_menus();

	/** \brief Create a file menu in main menu. */
	void create_file_menu();

	/** \brief Create a help menu in main menu. */
	void create_help_menu();

	/** \brief Create a packages menu in main menu. */
	void create_packages_menu();

      public:
	/** \brief Create a new main_window object. */
	explicit main_window();

	virtual ~main_window();

	/** \brief Retrieve a pointer to a tab_widget added to this window. */
	tab_widget *get_tab_widget();
      };
    }
  }
}
