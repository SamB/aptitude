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
#include <generic/util/temp.h>

#include <log4cxx/logger.h>

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

    /** \brief The file descriptor that should be used to inform the
     *  subprocess about map / unmap events and for the subprocess to
     *  inform us about suspend / continue events on the dpkg
     *  descriptor.
     *
     *  The parent process writes single bytes down this fd; when the
     *  terminal is mapped, it writes "1", and when it's unmapped it
     *  writes "0".
     *
     *  The subprocess also writes single bytes down this fd; when the
     *  dpkg process is running it writes "1", and when it's suspended
     *  it writes "0".
     */
    int subprocess_map_signal_fd;

    log4cxx::LoggerPtr logger;
    log4cxx::LoggerPtr logger_backgrounding;

    /** \brief Handle input on the pipe to the subprocess. */
    bool handle_suspend_resume_event(Glib::IOCondition condition);

    // Forbid copying.
    DpkgTerminal(const DpkgTerminal &);

    /** \brief The code that initializes the dpkg invocation in the
     *  terminal.
     *
     *  \param dpkg_socket_name
     *              The socket on which to report the child's dpkg status.
     *  \param f    A callback to invoke after dpkg finishes running.
     */
    void child_process(const temp::name &dpkg_socket_name,
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

    /** \brief Emitted when the dpkg process is suspended or resumed.
     *
     *  The argument states whether the process is currently running:
     *  "true" if it is running, and "false" if it is suspended.  This
     *  may be emitted more than once in a row with the same argument.
     */
    sigc::signal<void, bool> subprocess_running_changed;

    /** \brief Start running dpkg in the encapsulated terminal.
     *
     *  This must be invoked from a foreground thread.
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


    /** \brief Invoke to tell the dpkg process that it is now a
     *  "foreground" or a "background" process.
     *
     *  \param foreground  \b true to put the dpkg process in the
     *                     foreground; \b false to put it in the
     *                     background.  If in the background, it will
     *                     be suspended when it requires input.
     */
    void set_foreground(bool foreground);
  };
}

#endif
