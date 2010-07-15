/** \file status_widget.h */   // -*-c++-*-
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

#ifndef APTITUDE_QT_STATUS_WIDGET_H
#define APTITUDE_QT_STATUS_WIDGET_H

// Local includes
#include "tab.h"

// System includes
#include <QtGui/QWidget>

class QLabel;
class QPushButton;
class QStackedWidget;

namespace aptitude
{
  namespace gui
  {
    namespace qt
    {
      /** \brief Widget showing informations about current state of the program.
       *
       *  This widget is always displayed at the bottom of the main window and
       *  is divided into two parts.
       *  The first part shows infromations about requested changes of package
       *  state, and the second part shows progress of currently running program's
       *  activites. At the top of the widget, name of the last finished activity
       *  is displayed.
       *
       *  Widgets are placed in QStackedWidget, so only one of those widgets can be shown
       *  at the same time
       */
      class status_widget : public QWidget
      {
	Q_OBJECT

	QPushButton *show_changes_button;
	QPushButton *apply_changes_button;
	QPushButton *cancel_button;
	QPushButton *resolve_dependencies_button;

	QLabel *changes_summary;

	QWidget *changes_widget;
	QWidget *progress_widget;

	QStackedWidget *stacked_widget;

	/** \brief Create layouts and widgets. */
	void create_gui();

	/** \brief Create layout and widget of changes preview widget. */
	void create_changes_widget();

	/** \brief Create layout and widget of progress widget. */
	void create_progress_widget();

	/** \brief Update an information about requested changes in package cache. */
	void update_changes_summary();

      private Q_SLOTS:
	/** \brief Slot triggered when the Show Changes button has been clicked
	 *  by the user
	 *
	 *  This slot creates new (or make active if tab is already present)
	 *  Changes Preview tab
	 */
	void show_changes_button_clicked();

	/** \brief Slot triggered when the Apply Changes button has been clicked
	 *  by the user
	 *
	 *  This slots starts a process of appling changes
	 */
	void apply_changes_button_clicked();

	/** \brief Slot triggered when the Resolve Dependencies button has been clicked
	 *  by the user
	 *
	 *  This slot creates new (or make active if tab is already present)
	 *  Resolver tab
	 */
	void resolve_dependencies_button_clicked();

	/** \brief Slot triggered when the Cancel button has been clicked
	 *  by the user
	 *
	 *  This slot aborts curranly running task
	 */
	void cancel_button_clicked();

      public:
	/** \brief Create a new status_widget object. */
	explicit status_widget(QWidget *parent = 0);

	/** \brief Destroy a status_widget object. */
	virtual ~status_widget();

      public Q_SLOTS:
	/** \brief Slot triggered when the current tab has changed
	 *  on the main tab_widget
	 *
	 *  The slot is responsible for enabling and disabling widget's buttons.
	 *  For example when Changes Preview tab is active, Show Changes button should
	 *  be disabled to not confuse the user
	 */
	void current_tab_changed(tab::tab_type type);
      };
    }
  }
}

#endif // APTITUDE_QT_STATUS_WIDGET_H
