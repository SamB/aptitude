/** \file test_terminal_mock.cc */

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


#include <cmdline/mocks/terminal.h>
#include <cmdline/terminal.h>

namespace mocks = aptitude::cmdline::mocks;

using aptitude::cmdline::terminal;
using boost::shared_ptr;
using testing::InSequence;
using testing::StrEq;
using testing::Test;
using testing::_;

namespace
{
  struct TerminalMock : public Test
  {
    boost::shared_ptr<mocks::terminal> terminal;

  public:
    TerminalMock()
      : terminal(mocks::create_combining_terminal())
    {
    }
  };
}


// The only behavior that we need to test on the mock is that it
// correctly combines and splits writes using the usual flushing
// behavior.


TEST_F(TerminalMock, WriteEmptyStringDoesNotOutput)
{
  EXPECT_CALL(*terminal, output(_))
    .Times(0);
  EXPECT_CALL(*terminal, flush());

  terminal->write_text(L"");
  terminal->flush();
}

TEST_F(TerminalMock, WritesMustBeFlushed)
{
  EXPECT_CALL(*terminal, output(_))
    .Times(0);
  EXPECT_CALL(*terminal, flush())
    .Times(0);

  // Nothing should be called by this:
  terminal->write_text(L"abc");
}

TEST_F(TerminalMock, MoveToBeginningOfLineMustBeFlushed)
{
  EXPECT_CALL(*terminal, output(_))
    .Times(0);

  terminal->move_to_beginning_of_line();
}

TEST_F(TerminalMock, WriteAndFlush)
{
  {
    InSequence dummy;

    EXPECT_CALL(*terminal, output(StrEq(L"abc")));
    EXPECT_CALL(*terminal, flush());
  }

  terminal->write_text(L"abc");
  terminal->flush();
}

TEST_F(TerminalMock, MoveToBeginningOfLineAndFlush)
{
  {
    InSequence dummy;

    EXPECT_CALL(*terminal, output(StrEq(L"\r")));
    EXPECT_CALL(*terminal, flush());
  }

  terminal->move_to_beginning_of_line();
  terminal->flush();
}

TEST_F(TerminalMock, NewlineIsImplicitFlush)
{
  EXPECT_CALL(*terminal, output(StrEq(L"abc\n")));
  EXPECT_CALL(*terminal, flush())
    .Times(0);

  terminal->write_text(L"abc\n");
}

TEST_F(TerminalMock, DoubleFlushDoesNotOutput)
{
  {
    InSequence dummy;

    EXPECT_CALL(*terminal, output(StrEq(L"def")));
    EXPECT_CALL(*terminal, flush());
    EXPECT_CALL(*terminal, flush());
  }

  terminal->write_text(L"def");
  terminal->flush();
  terminal->flush();
}

TEST_F(TerminalMock, DoubleNewlineOutputsTwice)
{
  EXPECT_CALL(*terminal, flush())
    .Times(0);

  {
    InSequence dummy;

    EXPECT_CALL(*terminal, output(StrEq(L"def\n")));
    EXPECT_CALL(*terminal, output(StrEq(L"\n")));
  }

  terminal->write_text(L"def\n\n");
}

TEST_F(TerminalMock, MultipleNewlines)
{
  {
    InSequence dummy;

    EXPECT_CALL(*terminal, output(StrEq(L"abc\n")));
    EXPECT_CALL(*terminal, output(StrEq(L"I like\n")));
    EXPECT_CALL(*terminal, output(StrEq(L"bunnies!\n")));
    EXPECT_CALL(*terminal, output(StrEq(L" -- Burble")));
    EXPECT_CALL(*terminal, flush());
  }

  terminal->write_text(L"abc\nI like\nbunnies!\n -- Burble");
  terminal->flush();
}

TEST_F(TerminalMock, FlushAfterNewlineDoesNotOutput)
{
  {
    InSequence dummy;

    EXPECT_CALL(*terminal, output(StrEq(L"xyz\n")));
    EXPECT_CALL(*terminal, flush());
  }

  terminal->write_text(L"xyz\n");
  terminal->flush();
}



TEST_F(TerminalMock, FlushCombinesWrites)
{
  {
    InSequence dummy;

    EXPECT_CALL(*terminal, output(StrEq(L"abcdef")));
    EXPECT_CALL(*terminal, flush());
  }

  terminal->write_text(L"abc");
  terminal->write_text(L"def");
  terminal->flush();
}

TEST_F(TerminalMock, FlushCombinesWritesWithMoveToBeginningOfLine)
{
  {
    InSequence dummy;

    EXPECT_CALL(*terminal, output(StrEq(L"abc\rdef\rghi")));
    EXPECT_CALL(*terminal, flush());
  }

  terminal->write_text(L"abc");
  terminal->move_to_beginning_of_line();
  terminal->write_text(L"def");
  terminal->move_to_beginning_of_line();
  terminal->write_text(L"ghi");
  terminal->flush();
}

TEST_F(TerminalMock, NewlineCombinesWrites)
{
  EXPECT_CALL(*terminal, flush())
    .Times(0);

  EXPECT_CALL(*terminal, output(StrEq(L"xyzzy\n")));

  terminal->write_text(L"xyz");
  terminal->write_text(L"zy\n");
}

TEST_F(TerminalMock, newlineCombinesWritesWithMoveToBeginningOfLine)
{
  EXPECT_CALL(*terminal, output(StrEq(L"abc\rdef\n")));
  EXPECT_CALL(*terminal, flush())
    .Times(0);

  terminal->write_text(L"abc");
  terminal->move_to_beginning_of_line();
  terminal->write_text(L"def");
  terminal->write_text(L"\n");
}

// Check that there's no weirdness when you need to combine and split
// at the same time.
TEST_F(TerminalMock, CombineAndSplit)
{
  {
    InSequence dummy;

    EXPECT_CALL(*terminal, output(StrEq(L"ab\n")));
    EXPECT_CALL(*terminal, output(StrEq(L"de\rfg\n")));
    EXPECT_CALL(*terminal, output(StrEq(L"hijklmn\n")));
    EXPECT_CALL(*terminal, output(StrEq(L"op")));
    EXPECT_CALL(*terminal, flush());
  }

  terminal->write_text(L"a");
  terminal->write_text(L"b\nde");
  terminal->move_to_beginning_of_line();
  terminal->write_text(L"fg\nhijk");
  terminal->write_text(L"lmn\nop");
  terminal->flush();
}

// Check that the transient message will work properly.
TEST_F(TerminalMock, SimulatedTransientMessage)
{
  {
    InSequence dummy;

    EXPECT_CALL(*terminal, output(StrEq(L"\r\rabc")));
    EXPECT_CALL(*terminal, output(StrEq(L"\r   \ra")));
  }

  terminal->move_to_beginning_of_line();
  terminal->move_to_beginning_of_line();
  terminal->write_text(L"abc");
  terminal->flush();

  terminal->move_to_beginning_of_line();
  terminal->write_text(L"   ");
  terminal->move_to_beginning_of_line();
  terminal->write_text(L"a");
  terminal->flush();
}
