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
   *  Note that this *must* be invoked in a background thread, or it
   *  will deadlock itself!
   *
   *  \param f The function to invoke when we want to run dpkg.
   *
   *  \param register_terminal A function to invoke to register the
   *  terminal (e.g., to store it in a variable or to add it to a new
   *  tab).  This function assumes ownership of the terminal widget.
   *
   *  register_terminal is invoked in the same thread that called
   *  run_dpkg_in_terminal().
   */
  pkgPackageManager::OrderResult run_dpkg_in_terminal(const sigc::slot1<pkgPackageManager::OrderResult, int> &f,
						      const sigc::slot1<void, Gtk::Widget *> &register_terminal);
}

#endif
