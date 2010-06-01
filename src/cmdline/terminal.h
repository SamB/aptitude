/** \brief terminal.h */    // -*-c++-*-


// Copyright (C) 2010 Daniel Burrows
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

#ifndef APTITUDE_CMDLINE_SCREEN_WIDTH_H
#define APTITUDE_CMDLINE_SCREEN_WIDTH_H

#include <boost/shared_ptr.hpp>

#include <iosfwd>

namespace aptitude
{
  namespace cmdline
  {
    /** \brief Abstraction of the I/O device used for the command-line
     *  code.
     *
     *  A virtual interface is used so that we can dummy it out for
     *  testing (see mocks/terminal.h).
     */
    class terminal
    {
    public:
      virtual ~terminal();

      /** \brief Check whether the output stream seems to be connected
       *  to a terminal.
       *
       *  Normally maps to isatty(1).
       */
      virtual bool output_is_a_terminal() = 0;

      /** \brief Write some text to the terminal. */
      virtual void write_text(const std::string &msg) = 0;

      /** \brief Return the cursor to the beginning of the current
       *  line.
       *
       *  Like writing '\r'.  Might not have an effect unless you call
       *  flush_output().
       */
      virtual void move_to_beginning_of_line() = 0;

      /** \brief Flush the output device.
       *
       *  Writing '\n' is assumed to implicitly flush it (as on a
       *  standard terminal), but this is necessary if you want to
       *  ensure that something like a prompt or a progress bar is
       *  printed.
       */
      virtual void flush() = 0;

      /** \brief Prompt for a line of input from the terminal device.
       *
       *  \param msg A message to display as the prompt string.  The
       *             cursor will be positioned immediately to the
       *             right of this message.
       *
       *  \return a single line of user input.
       *
       *  \throw StdinEOFException if EOF is encountered before the
       *  end of the current line.
       *
       *  \note The StdinEOFException is normally just passed along
       *  until it reaches main(), which prints an appropriate message
       *  and aborts the program.
       */
      virtual std::string prompt_for_input(const std::string &msg) = 0;

      /** \brief Retrieve the current screen width.
       *
       *  A default value is returned if the output doesn't appear to
       *  be a tty.
       */
      virtual unsigned int get_screen_width() = 0;
    };

    /** \brief Create a terminal object attached to the standard I/O
        streams.
     */
    boost::shared_ptr<terminal> create_terminal();
  }
}

#endif // APTITUDE_CMDLINE_SCREEN_WIDTH_H
