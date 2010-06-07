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
       *  This object doesn't have any special behavior; it can be
       *  used to test other mocks that extend the terminal's behavior
       *  (such as the teletype mock).  To get a terminal that
       *  interprets calls to write_text(), use
       *  create_combining_terminal().
       */
      class terminal : public aptitude::cmdline::terminal
      {
        class combining_impl;
        friend boost::shared_ptr<terminal> create_combining_terminal();

      public:
        MOCK_METHOD0(output_is_a_terminal, bool());
        MOCK_METHOD1(write_text, void(const std::wstring &));
        MOCK_METHOD0(move_to_beginning_of_line, void());
        MOCK_METHOD0(flush, void());
        MOCK_METHOD1(prompt_for_input, std::wstring(const std::wstring &));
        MOCK_METHOD0(get_screen_width, unsigned int());

        // This method is invoked when the terminal would flush its
        // output: specifically, for each newline that's written and
        // for each call to flush().
        //
        // If the terminal would flush, but there's no text to flush,
        // this isn't invoked.
        MOCK_METHOD1(output, void(const std::wstring &));

        /** \brief Create a terminal object. */
        static boost::shared_ptr<terminal> create();
      };

      /** \brief Create a mock terminal that interprets calls to
       *  write_text() and flush(), invoking output() when
       *  appropriate.
       *
       *  Calls to move_to_beginning_of_line() are rewritten to place
       *  '\r' on the output stream instead.  This is done so that
       *  client code can easily verify that the
       *  move_to_beginning_of_line() occurs in the right place
       *  relative to calls to write_to_text().
       */
      boost::shared_ptr<terminal> create_combining_terminal();
    }
  }
}

#endif
