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

// System includes:
#include <boost/make_shared.hpp>

using boost::make_shared;
using boost::shared_ptr;
using testing::AnyNumber;
using testing::Invoke;
using testing::Return;
using testing::_;

namespace aptitude
{
  namespace cmdline
  {
    namespace mocks
    {
      shared_ptr<terminal> terminal::create()
      {
        return make_shared<terminal>();
      }

      shared_ptr<terminal_locale> terminal_locale::create()
      {
        return make_shared<terminal_locale>();
      }

      class terminal::combining_impl : public terminal
      {
        std::wstring pending_writes;

        void do_write_text(const std::wstring &s)
        {
          std::wstring::size_type start = 0;
          for(std::wstring::size_type nl = s.find('\n', start);
              nl != s.npos; nl = s.find('\n', start))
            {
              pending_writes.append(s, start, (nl - start) + 1);
              start = nl + 1;

              output(pending_writes);
              pending_writes.clear();
            }

          pending_writes.append(s, start, s.npos);
        }

        void do_move_to_beginning_of_line()
        {
          do_write_text(L"\r");
        }

        void do_flush()
        {
          if(!pending_writes.empty())
            {
              output(pending_writes);
              pending_writes.clear();
            }
        }

      public:
        combining_impl()
        {
          ON_CALL(*this, write_text(_))
            .WillByDefault(Invoke(this, &combining_impl::do_write_text));

          ON_CALL(*this, move_to_beginning_of_line())
            .WillByDefault(Invoke(this, &combining_impl::do_move_to_beginning_of_line));

          EXPECT_CALL(*this, write_text(_))
            .Times(AnyNumber());

          EXPECT_CALL(*this, move_to_beginning_of_line())
            .Times(AnyNumber());
        }

        // This is overridden (rather than relying on ON_CALL) to
        // ensure that output is written before the mock's flush() is
        // called; otherwise, it looks like the output comes second,
        // which is surprising.
        void flush()
        {
          do_flush();
          terminal::flush();
        }
      };

      shared_ptr<terminal> create_combining_terminal()
      {
        return make_shared<terminal::combining_impl>();
      }

      terminal_locale::terminal_locale()
      {
        EXPECT_CALL(*this, wcwidth(_))
          .WillRepeatedly(Return(1));
      }
    }
  }
}
