/** \file teletype.cc */         // -*-c++-*-

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
#include "teletype.h"

#include "terminal.h"

// System includes:
#include <boost/make_shared.hpp>

using boost::make_shared;
using boost::shared_ptr;
using testing::AnyNumber;
using testing::Invoke;
using testing::_;

namespace aptitude
{
  namespace cmdline
  {
    namespace mocks
    {
      teletype::~teletype()
      {
      }

      namespace
      {
        class teletype_with_terminal : public teletype
        {
          std::wstring last_line;
          unsigned int cursor_position;

          const shared_ptr<terminal> term;

          void scroll_line();

          void handle_output(const std::wstring &output);
          void handle_move_to_beginning_of_line();

        public:
          teletype_with_terminal(const shared_ptr<terminal> &_term)
            : cursor_position(0), term(_term)
          {
            ON_CALL(*term, output(_))
              .WillByDefault(Invoke(this, &teletype_with_terminal::handle_output));

            // TODO: need to defer this until flush() is called!
            ON_CALL(*term, move_to_beginning_of_line())
              .WillByDefault(Invoke(this, &teletype_with_terminal::handle_move_to_beginning_of_line));

            // Normally code using this interface will ignore output()
            // and flush, so set that as the default behavior.
            EXPECT_CALL(*term, output(_))
              .Times(AnyNumber());

            EXPECT_CALL(*term, flush())
              .Times(AnyNumber());

            EXPECT_CALL(*term, move_to_beginning_of_line())
              .Times(AnyNumber());
          }
        };

        void teletype_with_terminal::scroll_line()
        {
          last_line.clear();
          cursor_position = 0;
          newline();
        }

        void teletype_with_terminal::handle_output(const std::wstring &output)
        {
          // This is a minor hack to avoid generating an empty
          // set_last_line() call after newline().  It's set to true
          // when we emit a newline and to false when we see any
          // character that's not \r or \n.
          bool suppress_set_last_line = false;
          const unsigned int screen_width = term->get_screen_width();

          for(std::wstring::const_iterator it = output.begin();
              it != output.end(); ++it)
            {
              const char c = *it;

              switch(c)
                {
                case '\n':
                  if(!suppress_set_last_line)
                    set_last_line(last_line);
                  scroll_line();
                  suppress_set_last_line = true;
                  break;

                case '\r':
                  cursor_position = 0;
                  break;

                default:
                  if(cursor_position == screen_width)
                    {
                      if(!suppress_set_last_line)
                        set_last_line(last_line);
                      scroll_line();
                      suppress_set_last_line = true;

                      // Now that we've moved to the next line, we
                      // still need to output the character that
                      // caused the scroll.  This breaks if the screen
                      // width is 0 -- don't do that.
                      last_line.push_back(c);
                      ++cursor_position;
                    }
                  else
                    {
                      suppress_set_last_line = false;

                      if(cursor_position < last_line.size())
                        last_line[cursor_position] = c;
                      else
                        {
                          // Note that cursor_position should be at
                          // most last_line.size() + 1; this is just
                          // for safety's sake.
                          while(last_line.size() + 1 < cursor_position)
                            last_line.push_back(' ');

                          last_line.push_back(c);
                        }
                      ++cursor_position;
                    }
                }
            }

          if(!suppress_set_last_line)
            set_last_line(last_line);
        }

        void teletype_with_terminal::handle_move_to_beginning_of_line()
        {
          cursor_position = 0;
        }
      }

      shared_ptr<teletype>
      create_teletype(const boost::shared_ptr<terminal> &term)
      {
        return make_shared<teletype_with_terminal>(term);
      }
    }
  }
}
