/** \file test_teletype_mock */

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
#include <cmdline/mocks/teletype.h>
#include <cmdline/mocks/terminal.h>

// System includes:
#include <gmock/gmock.h>
#include <gtest/gtest.h>

namespace mocks = aptitude::cmdline::mocks;

using aptitude::cmdline::mocks::StrTrimmedEq;
using boost::shared_ptr;
using testing::InSequence;
using testing::Return;
using testing::Test;

namespace
{
  struct TeletypeTest : public Test
  {
    shared_ptr<mocks::terminal> term;
    shared_ptr<mocks::teletype> teletype;

    TeletypeTest()
      : term(mocks::create_combining_terminal()),
        teletype(mocks::create_teletype(term))
    {
      EXPECT_CALL(*term, get_screen_width())
        .WillRepeatedly(Return(80));
    }
  };
}

TEST_F(TeletypeTest, testOutputPartialLine)
{
  EXPECT_CALL(*teletype, set_last_line("abc"));

  term->output("abc");
}

TEST_F(TeletypeTest, testOutputLine)
{
  {
    InSequence dummy;

    EXPECT_CALL(*teletype, set_last_line("abc"));
    EXPECT_CALL(*teletype, newline());
  }


  term->output("abc\n");
  term->flush();
}

TEST_F(TeletypeTest, testOverwriteOneCharAtATime)
{
  {
    InSequence dummy;

    EXPECT_CALL(*teletype, set_last_line("abc"));
    EXPECT_CALL(*teletype, set_last_line("xbc"));
    EXPECT_CALL(*teletype, set_last_line("xyc"));
    EXPECT_CALL(*teletype, set_last_line("xyz"));
  }

  term->output("abc");
  term->move_to_beginning_of_line();
  term->flush();

  term->output("x");
  term->flush();

  term->output("y");
  term->flush();

  term->output("z");
  term->flush();
}

TEST_F(TeletypeTest, overwriteEverything)
{
  {
    InSequence dummy;

    EXPECT_CALL(*teletype, set_last_line("abc"));
    EXPECT_CALL(*teletype, set_last_line("xyz"));
  }

  term->output("abc");
  term->move_to_beginning_of_line();
  term->flush();
  term->output("xyz");
  term->flush();
}

TEST_F(TeletypeTest, overwritePastEverything)
{
  {
    InSequence dummy;

    EXPECT_CALL(*teletype, set_last_line("abc"));
    EXPECT_CALL(*teletype, set_last_line("xyzw"));
  }

  term->output("abc");
  term->move_to_beginning_of_line();
  term->flush();
  term->output("xyzw");
  term->flush();
}

TEST_F(TeletypeTest, testWritePastEOL)
{
  EXPECT_CALL(*term, get_screen_width())
    .WillRepeatedly(Return(5));

  {
    InSequence dummy;

    EXPECT_CALL(*teletype, set_last_line("abcde"));
    EXPECT_CALL(*teletype, newline());
    EXPECT_CALL(*teletype, set_last_line("fghij"));
  }

  term->output("abcdefghij");
}

TEST_F(TeletypeTest, testOverwritePastEOL)
{
  EXPECT_CALL(*term, get_screen_width())
    .WillRepeatedly(Return(5));

  {
    InSequence dummy;

    EXPECT_CALL(*teletype, set_last_line("12345"));
    EXPECT_CALL(*teletype, set_last_line("abcde"));
    EXPECT_CALL(*teletype, newline());
    EXPECT_CALL(*teletype, set_last_line("fghij"));
  }

  term->output("12345");
  term->move_to_beginning_of_line();
  term->output("abcdefghij");
}

TEST(TrimmedEqTest, testTrimmedEqExact)
{
  EXPECT_THAT("abc", StrTrimmedEq("abc"));
}

TEST(TrimmedEqTest, testTrimmedEqFirstLeftPadded)
{
  EXPECT_THAT("  abc", StrTrimmedEq("abc"));
}

TEST(TrimmedEqTest, testTrimmedEqFirstRightPadded)
{
  EXPECT_THAT("abc  ", StrTrimmedEq("abc"));
}

TEST(TrimmedEqTest, testTrimmedEqFirstBothPadded)
{
  EXPECT_THAT("  abc  ", StrTrimmedEq("abc"));
}

TEST(TrimmedEqTest, testTrimmedEqSecondLeftPadded)
{
  EXPECT_THAT("abc", StrTrimmedEq("  abc"));
}

TEST(TrimmedEqTest, testTrimmedEqSecondRightPadded)
{
  EXPECT_THAT("abc", StrTrimmedEq("abc  "));
}

TEST(TrimmedEqTest, testTrimmedEqSecondBothPadded)
{
  EXPECT_THAT("abc", StrTrimmedEq("  abc  "));
}

TEST(TrimmedEqTest, testTrimmedEqBothBothPadded)
{
  EXPECT_THAT(" abc  ", StrTrimmedEq("   abc    "));
}
