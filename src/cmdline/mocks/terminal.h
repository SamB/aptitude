// \file terminal.h                -*-c++-*-
//
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

#ifndef APTITUDE_CMDLINE_MOCKS_TERMINAL_H
#define APTITUDE_CMDLINE_MOCKS_TERMINAL_H

// Local includes:
#include <cmdline/terminal.h>

// System includes:
#include <boost/shared_ptr.hpp>

#include <gmock/gmock.h>

namespace aptitude
{
  namespace cmdline
  {
    namespace mocks
    {
      /** \brief A mock terminal object.
       *
       *  This combines calls to write_text() and provides a mocked
       *  output() method that is invoked when the terminal would
       *  normally flush its output.  write_text() is also provided as
       *  a mock, but normally client code will use output().
       *
       *  Calls to move_to_beginning_of_line() are rewritten to place
       *  '\r' on the output stream instead.  This is done so that
       *  client code can easily verify that the
       *  move_to_beginning_of_line() occurs in the right place
       *  relative to calls to write_to_text().
       */
      class terminal : public aptitude::cmdline::terminal
      {
        // The implementation is hidden so that its machinery doesn't
        // have to leak into the header.  It just sets the default
        // behavior of write_text() to generate appropriate calls to
        // output().
        class impl;
        friend boost::shared_ptr<terminal> create_terminal();

        terminal();

      public:
        MOCK_METHOD0(output_is_a_terminal, bool());
        MOCK_METHOD1(write_text, void(const std::string &));
        MOCK_METHOD0(move_to_beginning_of_line, void());
        MOCK_METHOD0(flush, void());
        MOCK_METHOD1(prompt_for_input, std::string(const std::string &));
        MOCK_METHOD0(get_screen_width, unsigned int());

        // This method is invoked when the terminal would flush its
        // output: specifically, for each newline that's written and
        // for each call to flush().
        //
        // If the terminal would flush, but there's no text to flush,
        // this isn't invoked.
        MOCK_METHOD1(output, void(const std::string &));
      };

      /** \brief Create a mock terminal. */
      boost::shared_ptr<terminal> create_terminal();
    }
  }
}

#endif
