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
          // The index of the character containing the cursor:
          unsigned int cursor_idx;
          // The character cell containing the cursor:
          unsigned int cursor_position;

          const shared_ptr<terminal> term;
          const shared_ptr<terminal_locale> term_locale;

          void scroll_line();

          void handle_output(const std::wstring &output);
          void handle_move_to_beginning_of_line();

          /** \brief Return the column width of the given character,
           *  or 0 if it's not printable (instead of -1).
           */
          int safe_wcwidth(wchar_t c)
          {
            const int rval = term_locale->wcwidth(c);
            if(rval < 0)
              return 0;
            else
              return rval;
          }

        public:
          teletype_with_terminal(const shared_ptr<terminal> &_term,
                                 const shared_ptr<terminal_locale> &_term_locale)
            : cursor_idx(0),
              cursor_position(0),
              term(_term),
              term_locale(_term_locale)
          {
            ON_CALL(*term, output(_))
              .WillByDefault(Invoke(this, &teletype_with_terminal::handle_output));

            // TODO: need to defer this until flush() is called!
            ON_CALL(*term, move_to_beginning_of_line())
              .WillByDefault(Invoke(this, &teletype_with_terminal::handle_move_to_beginning_of_line));

            // Normally code using this interface will ignore calls to
            // functions on the terminal, so set that as the default
            // behavior.
            EXPECT_CALL(*term, output(_))
              .Times(AnyNumber());

            EXPECT_CALL(*term, write_text(_))
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
          cursor_idx = 0;
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
              const wchar_t c = *it;

              switch(c)
                {
                case '\n':
                  if(!suppress_set_last_line)
                    set_last_line(last_line);
                  scroll_line();
                  suppress_set_last_line = true;
                  break;

                case '\r':
                  cursor_idx = 0;
                  cursor_position = 0;
                  break;

                default:
                  {
                    const int c_width = safe_wcwidth(c);

                    if(cursor_position + c_width > screen_width)
                      {
                        if(!suppress_set_last_line)
                          set_last_line(last_line);
                        scroll_line();
                        // Don't suppress the last line, since we're
                        // about to output a character (making it
                        // nonempty).
                        suppress_set_last_line = false;

                        // Now that we've moved to the next line, we
                        // still need to output the character that
                        // caused the scroll.  This breaks if the screen
                        // width is 0 -- don't do that.
                        last_line.push_back(c);
                        cursor_position += c_width;
                        ++cursor_idx;
                      }
                    else
                      {
                        suppress_set_last_line = false;

                        if(cursor_idx < last_line.size())
                          {
                            // This is an overwrite.  Find the character
                            // or characters that were overwritten and
                            // replace them with the new character.  If
                            // some of the cells containing the old
                            // character aren't filled, fill them with
                            // spaces.
                            //
                            // Note that we don't handle the case where
                            // we start writing in the middle of a
                            // character, since that can't happen (since
                            // the cursor starts at the left and only
                            // moves via writes).
                            int chars_to_replace = 0;
                            int replaced_width = 0;

                            while(replaced_width < c_width)
                              {
                                const int replace_idx =
                                  cursor_idx + chars_to_replace;
                                const int previous_width =
                                  safe_wcwidth(last_line[replace_idx]);

                                ++chars_to_replace;
                                replaced_width += previous_width;
                              }

                            std::wstring new_text;
                            new_text.push_back(c);

                            for(int i = c_width; i < replaced_width; ++i)
                              new_text.push_back(L' ');


                            last_line.replace(cursor_idx,
                                              chars_to_replace,
                                              new_text);
                          }
                        else
                          {
                            // Note that cursor_idx should be at
                            // most last_line.size() + 1; this is just
                            // for safety's sake.
                            while(last_line.size() + 1 < cursor_idx)
                              last_line.push_back(' ');

                            last_line.push_back(c);
                          }

                        cursor_position += c_width;
                        ++cursor_idx;
                      }
                  }
                }
            }

          if(!suppress_set_last_line)
            set_last_line(last_line);
        }

        void teletype_with_terminal::handle_move_to_beginning_of_line()
        {
          cursor_position = 0;
          cursor_idx = 0;
        }
      }

      shared_ptr<teletype>
      create_teletype(const boost::shared_ptr<terminal> &term,
                      const boost::shared_ptr<terminal_locale> &term_locale)
      {
        return make_shared<teletype_with_terminal>(term, term_locale);
      }
    }
  }
}
