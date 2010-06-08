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


// Local includes:
#include "transient_message.h"

#include "cmdline_common.h"
#include "terminal.h"


// System includes:
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
        std::wstring last_line;

        // The terminal used to output text.
        shared_ptr<terminal> term;

        // The locale to be used with that terminal.
        shared_ptr<terminal_locale> term_locale;

        void clear_last_line();

      public:
        transient_message_impl(const shared_ptr<terminal> &_term,
                               const shared_ptr<terminal_locale> &_term_locale)
          : last_line_len(0),
            term(_term),
            term_locale(_term_locale)
        {
        }

        void set_text(const std::wstring &line);
      };


      void transient_message_impl::clear_last_line()
      {
        static const std::wstring blank(L" ");

        term->move_to_beginning_of_line();
        for(std::size_t i = 0; i < last_line_len; ++i)
          term->write_text(blank);
        term->move_to_beginning_of_line();

        last_line_len = 0;
      }

      void transient_message_impl::set_text(const std::wstring &line)
      {
        if(last_line == line)
          // Don't clutter the terminal stream if there's nothing to
          // do.
          return;

        const unsigned int screen_width = term->get_screen_width();


        // Display the message on a single line of the terminal.
        //
        // TODO: it would be nice to be able to properly wrap
        // multi-line messages and then clean them up.  Is it
        // possible to do that in a sane way?
        std::wstring::const_iterator display_end = line.begin();
        unsigned int display_width = 0;
        {
          while(display_end != line.end() && display_width < screen_width)
            {
              const wchar_t next = *display_end;
              const int next_width = term_locale->wcwidth(next);

              if(next_width >= 0)
                {
                  const unsigned int new_display_width =
                    display_width + static_cast<unsigned int>(next_width);

                  if(new_display_width > screen_width)
                    break;

                  display_width = new_display_width;
                }

              ++display_end;
            }
        }
        const std::wstring display(line.begin(), display_end);

        clear_last_line();
        term->write_text(display);
        term->flush();
        last_line_len = display_width;
        last_line = line;
      }
    }

    shared_ptr<transient_message>
    create_transient_message(const shared_ptr<terminal> &term,
                             const shared_ptr<terminal_locale> &term_locale)
    {
      return make_shared<transient_message_impl>(term, term_locale);
    }
  }
}
