/** \file tab_widget.h */   // -*-c++-*-
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

#ifndef APTITUDE_QT_TAB_WIDGET_H
#define APTITUDE_QT_TAB_WIDGET_H

#include <QtGui/QTabWidget>

#include "tab.h"

class QKeyEvent;

namespace aptitude
{
  namespace gui
  {
    namespace qt
    {
      /** \brief Widget containing program's tabs.
       *
       *  This widget is a central part of main program's window
       *
       *  It is responsible for propagating signals about changing
       *  his state and responsing for user input (e.g. shortcuts or
       *  requests for closing tabs)
       */
      class tab_widget : public QTabWidget
      {
	Q_OBJECT

	/** \brief Switches active tab to next tab on the left. */
	void switch_tab_left();

	/** \brief Switches active tab to next tab on the right. */
	void switch_tab_right();

      private Q_SLOTS:
	/** \brief Slot invoked when user clicked on close button on
	 *  tab with given index.
	 *
	 *  Received signal is translated and propageted to the
	 *  tabs_manager
	 */
	void tab_deletion_requested(int index);

	/** \brief Slot invoked when active tabs has changed
	 *
	 *  Received signal is translated and propageted
	 */
	void on_tab_change(int index);

      public:
	/** \brief Create new tab widget with the given parent */
	explicit tab_widget(QWidget *parent = 0);

	virtual ~tab_widget();

	/** \brief Enable or disable close button on the given tab */
	void set_tab_closable(tab *closable_tab, bool enabled);

      public Q_SLOTS:
	/** \brief Slot invoked when user typed a shortcut
	 *
	 *  This slots translates key event to shortcut and checks
	 *  if given shortcut is registered for this widget.
	 *  If yes handled argument is set to true
	 */
	void key_pressed(QKeyEvent *e, bool &handled);

      Q_SIGNALS:
	/** \brief Signal emitted when the current tab has changed.
	 *  tab_type is a type of newly activated tab
	 */
	void current_tab_changed(tab::tab_type);

	/** \brief Signal emitted when user has requested closing
	 *   closing given tab
	 *
	 *  When the force parameter is true, the object has to be destroyed
	 *  (this is true when tab_widget is going to be destroyed)
	 */
	void tab_deletion_request(tab *, bool force);
      };
    }
  }
}

#endif // APTITUDE_QT_TAB_WIDGET_H
