// -*-c++-*-

// dpkg_terminal.h
//
//  Copyright 2008 Daniel Burrows
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

#ifndef DPKG_TERMINAL_H
#define DPKG_TERMINAL_H

#include <gtkmm.h>

#include <apt-pkg/packagemanager.h>

#include <generic/util/safe_slot.h>

/** \brief Support for creating a GUI terminal in which dpkg can be
 *  invoked.
 *
 *  \file dpkg_terminal.h
 */

namespace gui
{
  /** \brief Run dpkg in a terminal, blocking this thread until it
   *  completes.
   *
   *  This may be invoked in either a foreground thread or a
   *  background thread.
   *
   *  \param f The function to invoke when we want to run dpkg.
   *
   *  \param register_terminal A function to invoke to register the
   *  terminal (e.g., to store it in a variable or to add it to a new
   *  tab).  This function assumes ownership of the terminal widget.
   */
  void run_dpkg_in_terminal(const safe_slot1<pkgPackageManager::OrderResult, int> &f,
			    const safe_slot1<void, Gtk::Widget *> &register_terminal,
			    const safe_slot1<void, pkgPackageManager::OrderResult> &k);
}

#endif
