// -*-c++-*-

// dpkg_terminal.h
//
//  Copyright 2008-2009 Daniel Burrows
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

#include <generic/apt/parse_dpkg_status.h>
#include <generic/util/safe_slot.h>

struct sockaddr_un;

/** \brief Support for creating a GUI terminal in which dpkg can be
 *  invoked.
 *
 *  \file dpkg_terminal.h
 */

namespace gui
{
  /** \brief This object manages setting up and destroying the dpkg
   *  terminal.
   */
  class DpkgTerminal : public sigc::trackable
  {
    Gtk::Widget *terminal;
    bool sent_finished_signal;

    // Forbid copying.
    DpkgTerminal(const DpkgTerminal &);

    /** \brief The code that initializes the dpkg invocation in the
     *  terminal.
     *
     *  \param sa   A socket on which to report the child's status.
     *  \param f    A callback to invoke after dpkg finishes running.
     */
    void child_process(const struct sockaddr_un &sa,
		       const safe_slot1<pkgPackageManager::OrderResult, int> &f);

  public:
    /** \brief Initialize a new terminal. */
    DpkgTerminal();

    ~DpkgTerminal();

    /** \brief Return a wrapper around the terminal object.
     *
     *  The wrapper is owned by this DpkgTerminal.
     */
    Gtk::Widget *get_widget() const { return terminal; }

    /** \brief A signal that triggers in the main thread when dpkg
     *	finishes running.
     */
    sigc::signal1<void, pkgPackageManager::OrderResult> finished; 

    /** \brief A signal that triggers in the main thread when a
     *  message is received on the dpkg status pipe.
     */
    sigc::signal1<void, aptitude::apt::dpkg_status_message> status_message;

    /** \brief Start running dpkg in the encapsulated terminal.
     *
     *  This may be invoked in either a foreground thread or a
     *  background thread.
     *
     *  \param f The function that actually invokes dpkg.
     */
    void run(const safe_slot1<pkgPackageManager::OrderResult, int> &f);

    // It would be somewhat nicer to just pass around slots to say
    // "yes" or "no" (so only something that read a
    // conffile-replacement message would have a token that could
    // invoke these).

    /** \brief Send "yes" in reply to a "replace this conffile?"
     *  message.
     */
    void inject_yes();

    /** \brief Send "no" in reply to a "replace this conffile?" message.
     */
    void inject_no();

    /** \brief Emitted when the subprocess tries to read from standard
     *  input.
     */
    sigc::signal0<void> subprocess_read;
  };
}

#endif
