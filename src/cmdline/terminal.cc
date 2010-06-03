/** \file terminal.cc */


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



// Local includes:

#include "terminal.h"

#include "cmdline_prompt.h" // For StdinEOFException.  \todo move that
                            // into terminal.h.

// System includes:

#include <boost/make_shared.hpp>
#include <boost/shared_ptr.hpp>

#include <iostream>

#include <sys/ioctl.h>

using boost::make_shared;
using boost::shared_ptr;

namespace aptitude
{
  namespace cmdline
  {
    namespace
    {
      // \todo Consider using a signal handler to catch window resizes
      // -- watch out for what happens if the curses UI starts up,
      // we'll have to tear down the handler, and be sure to handle
      // memory barriers.
      class terminal_impl : public terminal
      {
      public:
        bool output_is_a_terminal();
        void write_text(const std::string &msg);
        void move_to_beginning_of_line();
        void flush();
        std::string prompt_for_input(const std::string &msg);
        unsigned int get_screen_width();
      };

      bool terminal_impl::output_is_a_terminal()
      {
        return isatty(1);
      }

      void terminal_impl::write_text(const std::string &msg)
      {
        std::cout << msg;
      }

      void terminal_impl::move_to_beginning_of_line()
      {
        std::cout << '\r';
      }

      void terminal_impl::flush()
      {
        std::cout << std::flush;
      }

      std::string terminal_impl::prompt_for_input(const std::string &msg)
      {
        std::cout << msg << std::flush;

        std::string rval;
        char buf[1024];
        std::cin.getline(buf, 1023);
        rval += buf;

        while(!std::cin && !std::cin.eof())
          {
            std::cin.getline(buf, 1023);
            rval += buf;
          }

        if(!std::cin)
          throw StdinEOFException();

        return rval;
      }

      unsigned int terminal_impl::get_screen_width()
      {
        // Ripped from apt-get, which ripped it from GNU ls
        winsize ws;

        if (ioctl(1, TIOCGWINSZ, &ws) != -1 && ws.ws_col >= 5)
          return ws.ws_col;
        else
          // \todo Should we distinguish between "can't read a
          // terminal size" and "read a tiny terminal size"?
          return 80;
      }
    }

    terminal::~terminal()
    {
    }

    shared_ptr<terminal> create_terminal()
    {
      return make_shared<terminal_impl>();
    }
  }
}
