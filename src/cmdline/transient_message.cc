/** \file transient_message.cc */

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

#include "transient_message.h"

#include "cmdline_common.h"

#include <boost/make_shared.hpp>

#include <cwidget/generic/util/transcode.h>

#include <iostream>

using boost::make_shared;
using boost::shared_ptr;
using cwidget::util::transcode;

namespace aptitude
{
  namespace cmdline
  {
    transient_message::~transient_message()
    {
    }

    namespace
    {
      class transient_message_impl : public transient_message
      {
        // The length of the last line we displayed.  Not
        // last_line.size() because it counts character width, not
        // bytes.
        std::size_t last_line_len;

        // The last string we displayed.
        std::string last_line;

        void clear_last_line();

      public:
        void set_text(const std::string &line);
      };


      void transient_message_impl::clear_last_line()
      {
        std::cout << '\r';
        for(std::size_t i = 0; i < last_line_len; ++i)
          std::cout << ' ';
        std::cout << '\r';

        last_line_len = 0;
        last_line.clear();
      }

      void transient_message_impl::set_text(const std::string &line)
      {
        if(last_line == line)
          // Don't clutter the terminal stream if there's nothing to
          // do.
          return;

        last_line = line;

        update_screen_width();


        // Display the message on a single line of the terminal.
        //
        // TODO: it would be nice to be able to properly wrap
        // multi-line messages and then clean them up.  Is it
        // possible to do that in a sane way?
        const std::wstring line_w = transcode(line);
        std::wstring::const_iterator display_end = line_w.begin();
        int display_width = 0;
        {
          while(display_end != line_w.end() && display_width < screen_width)
            {
              const wchar_t next = *display_end;
              const int next_width = wcwidth(next);

              if(next_width > screen_width)
                break;

              ++display_end;
              display_width += next_width;
            }
        }
        const std::wstring display(line_w.begin(), display_end);

        clear_last_line();
        std::cout << transcode(display) << std::flush;
        last_line_len = display_width;
      }
    }

    shared_ptr<transient_message> create_transient_message()
    {
      return make_shared<transient_message_impl>();
    }
  }
}
