// vs_util.h      -*-c++-*-
//
//   Copyright (C) 2000, 2007 Daniel Burrows
//
//   This program is free software; you can redistribute it and/or
//   modify it under the terms of the GNU General Public License as
//   published by the Free Software Foundation; either version 2 of
//   the License, or (at your option) any later version.
//
//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//   General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with this program; see the file COPYING.  If not, write to
//   the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//   Boston, MA 02111-1307, USA.

//
//  Provides a bunch of utility functions to construct prefabricated
// widget trees (for instance, handy message boxes)

#ifndef VS_UTIL_H
#define VS_UTIL_H

#include "vs_editline.h"

#include <generic/util/slotarg.h>

#include <string>

class fragment;
class style;
class vscreen_widget;
class vs_pager;

template<class T>
class ref_ptr;

typedef ref_ptr<vscreen_widget> vs_widget_ref;

// Canned dialog-boxes:

/** Create a dialog box with a single button.
 *
 *  \param widget the widget to place above the button.
 *
 *  \param okslot the slot to be triggered when the button is pressed.
 *
 *  \param label the label of the button
 *
 *  \param attr the attributes to use for the background of the dialog
 *  box, defaults to reverse-video of DefaultWidgetBackground.
 */
vs_widget_ref vs_dialog_ok(const vs_widget_ref &widget,
			   slot0arg okslot, const std::wstring &label,
			   const style &st);

vs_widget_ref vs_dialog_ok(fragment *msg, slot0arg okslot=NULL, bool scrollbar=false);
vs_widget_ref vs_dialog_ok(fragment *msg, slot0arg okslot, const style &st, bool scrollbar=false);
vs_widget_ref vs_dialog_ok(fragment *msg, slot0arg okslot, const std::wstring &label,
			   const style &st, bool scrollbar=false);

vs_widget_ref vs_dialog_ok(const std::wstring &msg, slot0arg okslot=NULL);
vs_widget_ref vs_dialog_ok(const std::wstring &msg, slot0arg okslot,
			   const style &st);
vs_widget_ref vs_dialog_ok(const std::wstring &msg, slot0arg okslot, const std::wstring &label,
			   const style &st);

/** Create a dialog box with two buttons, labelled "yes" and "no".
 *
 *  \param widget the widget to place above the buttons
 *
 *  \param yesslot the callback to be triggered when "yes" is selected
 *
 *  \param yeslabel the label of the "yes" button
 *
 *  \param noslot the callback to be triggered when "no" is selected
 *
 *  \param yeslabel the label of the "no" button
 *
 *  \param attr the attribute to use as the background of widgets
 *  created by this routine
 *
 *  \param deflt if \b true, the "yes" button will be selected by default;
 *  otherwise, the "no" button will be selected by default.
 */
vs_widget_ref vs_dialog_yesno(const vs_widget_ref &widget,
			      slot0arg yesslot,
			      const std::wstring &yeslabel,
			      slot0arg noslot,
			      const std::wstring &nolabel,
			      const style &st,
			      bool deflt=true);

vs_widget_ref vs_dialog_yesno(fragment *msg,
			      slot0arg yesslot,
			      slot0arg noslot,
			      bool scrollbar=false,
			      bool deflt=true);
vs_widget_ref vs_dialog_yesno(fragment *msg,
			      slot0arg yesslot,
			      slot0arg noslot,
			      const style &st,
			      bool scrollbar=false,
			      bool deflt=true);
vs_widget_ref vs_dialog_yesno(fragment *msg,
			      slot0arg yesslot,
			      const std::wstring &yeslabel,
			      slot0arg noslot,
			      const std::wstring &nolabel,
			      const style &st,
			      bool scrollbar=false,
			      bool deflt=true);

vs_widget_ref vs_dialog_yesno(const std::wstring &msg,
			      slot0arg yesslot,
			      slot0arg noslot,
			      bool deflt=true);
vs_widget_ref vs_dialog_yesno(const std::wstring &msg,
			      slot0arg yesslot,
			      slot0arg noslot,
			      const style &st,
			      bool deflt=true);
vs_widget_ref vs_dialog_yesno(const std::wstring &msg,
			      slot0arg yesslot,
			      const std::wstring &yeslabel,
			      slot0arg noslot,
			      const std::wstring &nolabel,
			      const style &st,
			      bool deflt=true);

vs_widget_ref vs_dialog_fileview(const std::string &fn,
				 slot0arg okslot=NULL,
				 slotarg<sigc::slot1<void, vs_pager &> > search_slot=NULL,
				 slotarg<sigc::slot1<void, vs_pager &> > repeat_search_slot=NULL,
				 slotarg<sigc::slot1<void, vs_pager &> > repeat_search_back_slot=NULL,
				 const char *encoding=NULL);
vs_widget_ref vs_dialog_fileview(const std::string &fn,
				 slot0arg okslot,
				 slotarg<sigc::slot1<void, vs_pager &> > search_slot,
				 slotarg<sigc::slot1<void, vs_pager &> > repeat_search_slot,
				 slotarg<sigc::slot1<void, vs_pager &> > repeat_search_back_slot,
				 const style &st,
				 const char *encoding=NULL);

vs_widget_ref vs_dialog_string(fragment *msg,
			       const std::wstring &deflt,
			       slotarg<sigc::slot1<void, std::wstring> > okslot,
			       slotarg<sigc::slot0<void> > cancel_slot,
			       slotarg<sigc::slot1<void, std::wstring> > changed_slot,
			       vs_editline::history_list *history,
			       const style &st);

vs_widget_ref vs_dialog_string(const std::wstring &msg,
			       const std::wstring &deflt,
			       slotarg<sigc::slot1<void, std::wstring> > okslot,
			       slotarg<sigc::slot0<void> > cancel_slot,
			       slotarg<sigc::slot1<void, std::wstring> > changed_slot,
			       vs_editline::history_list *history,
			       const style &st);

vs_widget_ref vs_dialog_string(const std::wstring &msg,
			       const std::wstring &deflt,
			       slotarg<sigc::slot1<void, std::wstring> > slot,
			       slotarg<sigc::slot0<void> > cancel_slot,
			       slotarg<sigc::slot1<void, std::wstring> > changed_slot,
			       vs_editline::history_list *history);

#endif
