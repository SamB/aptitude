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
#include <cwidget/generic/util/transcode.h>

#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include <locale.h>

namespace mocks = aptitude::cmdline::mocks;

using aptitude::cmdline::mocks::StrTrimmedEq;
using aptitude::cmdline::mocks::StrTrimmedRightEq;
using boost::shared_ptr;
using testing::InSequence;
using testing::Not;
using testing::Return;
using testing::StrEq;
using testing::Test;

namespace
{
  // Note that I use a character that is *never* multi-column under
  // normal circumstances, to verify that the locale object is
  // actually being used.
  const wchar_t two_column_char = L'Z';

  struct TeletypeTest : public Test
  {
    // An arbitrary Unicode codepoint that I happen to know will take
    // two columns.  (TODO: does this make the test locale-dependent?
    // Although using some sort of locale mock is kind of a scary idea)
    const std::wstring widechar;

    // Stores the previous value of LC_CTYPE before we modified it for
    // the test.
    //
    // I really, really don't like having to do this.  But mocking out
    // the whole locale system would be utterly insane, and I can't
    // find a Unicode character that I can test regardless of the
    // value of CTYPE.
    std::string previous_lc_ctype;

    shared_ptr<mocks::terminal> term;
    shared_ptr<mocks::terminal_locale> term_locale;
    shared_ptr<mocks::teletype> teletype;

    static std::string safe_string(const char *c)
    {
      if(c == NULL)
        return std::string();
      else
        return c;
    }

    // narrow -> wide conversion function that doesn't depend on the
    // current locale.
    static std::wstring transcode(const std::string &s)
    {
      return cwidget::util::transcode(s, "UTF-8");
    }

    // wide -> narrow conversion function that doesn't depend on the
    // current locale.
    static std::string transcode(const std::wstring &s)
    {
      return cwidget::util::transcode(s, "UTF-8");
    }

    TeletypeTest()
      : widechar(1, two_column_char),
        term(mocks::create_combining_terminal()),
        term_locale(mocks::terminal_locale::create()),
        teletype(mocks::create_teletype(term, term_locale))
    {
      EXPECT_CALL(*term, get_screen_width())
        .WillRepeatedly(Return(80));

      EXPECT_CALL(*term_locale, wcwidth(two_column_char))
        .WillRepeatedly(Return(2));
    }

    void SetUp()
    {
      // Sanity-check the widechar string.
      EXPECT_EQ(1, widechar.size());
      if(widechar.size() >= 0)
        {
          EXPECT_EQ(two_column_char, widechar[0]);
          EXPECT_EQ(2, term_locale->wcwidth(widechar[0]));
        }
    }
  };
}

TEST_F(TeletypeTest, testOutputPartialLine)
{
  EXPECT_CALL(*teletype, set_last_line(StrEq(L"abc")));

  term->output(L"abc");
}

TEST_F(TeletypeTest, testOutputLine)
{
  {
    InSequence dummy;

    EXPECT_CALL(*teletype, set_last_line(StrEq(L"abc")));
    EXPECT_CALL(*teletype, newline());
  }


  term->output(L"abc\n");
  term->flush();
}

TEST_F(TeletypeTest, testOverwriteOneCharAtATime)
{
  {
    InSequence dummy;

    EXPECT_CALL(*teletype, set_last_line(StrEq(L"abc")));
    EXPECT_CALL(*teletype, set_last_line(StrEq(L"xbc")));
    EXPECT_CALL(*teletype, set_last_line(StrEq(L"xyc")));
    EXPECT_CALL(*teletype, set_last_line(StrEq(L"xyz")));
  }

  term->output(L"abc");
  term->move_to_beginning_of_line();
  term->flush();

  term->output(L"x");
  term->flush();

  term->output(L"y");
  term->flush();

  term->output(L"z");
  term->flush();
}

TEST_F(TeletypeTest, OverwriteNarrowCharWithWideChar)
{
  {
    InSequence dummy;
    EXPECT_CALL(*teletype, set_last_line(StrEq(L"abc")));
    EXPECT_CALL(*teletype, set_last_line(StrEq(widechar + L"c")));
  }

  term->output(L"abc");
  term->move_to_beginning_of_line();
  term->flush();

  term->output(widechar);
  term->flush();
}

TEST_F(TeletypeTest, OverwriteWideCharWithNarrowChar)
{
  {
    InSequence dummy;
    EXPECT_CALL(*teletype, set_last_line(StrEq(widechar + L"c")));
    // NB: this behavior isn't quite what a real terminal will do, but
    // it's close enough for testing. (the terminal would have a blank
    // space without an actual character, but it would be too
    // complicated to simulate that, and it doesn't really matter from
    // the point of view of seeing what the output looks like, which
    // is what this is for)
    EXPECT_CALL(*teletype, set_last_line(StrEq(L"a c")));
  }

  term->output(widechar + L"c");
  term->move_to_beginning_of_line();
  term->flush();

  term->output(L"a");
  term->flush();
}

TEST_F(TeletypeTest, OverwriteWideCharWithNarrowChars)
{
  {
    InSequence dummy;
    EXPECT_CALL(*teletype, set_last_line(StrEq(widechar + L"c")));
    EXPECT_CALL(*teletype, set_last_line(StrEq(L"abc")));
  }

  term->output(widechar + L"c");
  term->move_to_beginning_of_line();
  term->flush();

  term->output(L"ab");
  term->flush();
}

TEST_F(TeletypeTest, overwriteEverything)
{
  {
    InSequence dummy;

    EXPECT_CALL(*teletype, set_last_line(StrEq(L"abc")));
    EXPECT_CALL(*teletype, set_last_line(StrEq(L"xyz")));
  }

  term->output(L"abc");
  term->move_to_beginning_of_line();
  term->flush();
  term->output(L"xyz");
  term->flush();
}

TEST_F(TeletypeTest, overwritePastEverything)
{
  {
    InSequence dummy;

    EXPECT_CALL(*teletype, set_last_line(StrEq(L"abc")));
    EXPECT_CALL(*teletype, set_last_line(StrEq(L"xyzw")));
  }

  term->output(L"abc");
  term->move_to_beginning_of_line();
  term->flush();
  term->output(L"xyzw");
  term->flush();
}

TEST_F(TeletypeTest, testWritePastEOL)
{
  EXPECT_CALL(*term, get_screen_width())
    .WillRepeatedly(Return(5));

  {
    InSequence dummy;

    EXPECT_CALL(*teletype, set_last_line(StrEq(L"abcde")));
    EXPECT_CALL(*teletype, newline());
    EXPECT_CALL(*teletype, set_last_line(StrEq(L"fghij")));
  }

  term->output(L"abcdefghij");
}

TEST_F(TeletypeTest, WritePastEOLAfterWideChar)
{
  EXPECT_CALL(*term, get_screen_width())
    .WillRepeatedly(Return(4));

  {
    InSequence dummy;

    EXPECT_CALL(*teletype, set_last_line(StrEq(widechar + L"bc")));
    EXPECT_CALL(*teletype, newline());
    EXPECT_CALL(*teletype, set_last_line(StrEq(L"def")));
  }

  term->output(widechar + L"bcdef");
}

TEST_F(TeletypeTest, WriteWideCharPastEOL)
{
  EXPECT_CALL(*term, get_screen_width())
    .WillRepeatedly(Return(4));

  {
    InSequence dummy;

    EXPECT_CALL(*teletype, set_last_line(StrEq(widechar + widechar)));
    EXPECT_CALL(*teletype, newline());
    EXPECT_CALL(*teletype, set_last_line(StrEq(widechar)));
  }

  term->output(widechar + widechar + widechar);
}

TEST_F(TeletypeTest, WriteWideCharPastEOLWithSplit)
{
  EXPECT_CALL(*term, get_screen_width())
    .WillRepeatedly(Return(4));

  {
    InSequence dummy;
    EXPECT_CALL(*teletype, set_last_line(StrEq(L"a" + widechar)));
    EXPECT_CALL(*teletype, newline());
    EXPECT_CALL(*teletype, set_last_line(StrEq(widechar + L"a")));
  }

  term->output(L"a" + widechar + widechar + L"a");
}

TEST_F(TeletypeTest, testOverwritePastEOL)
{
  EXPECT_CALL(*term, get_screen_width())
    .WillRepeatedly(Return(5));

  {
    InSequence dummy;

    EXPECT_CALL(*teletype, set_last_line(StrEq(L"12345")));
    EXPECT_CALL(*teletype, set_last_line(StrEq(L"abcde")));
    EXPECT_CALL(*teletype, newline());
    EXPECT_CALL(*teletype, set_last_line(StrEq(L"fghij")));
  }

  term->output(L"12345");
  term->move_to_beginning_of_line();
  term->output(L"abcdefghij");
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

TEST(TrimmedEqTest, TrimmedEqWide)
{
  EXPECT_THAT(L" abc  ", StrTrimmedEq(L"   abc     "));
}

TEST(TrimmedEqTest, TrimmedEqWideAndNarrow)
{
  EXPECT_THAT(" abc  ", StrTrimmedEq(L"   abc     "));
}





TEST(TrimmedEqTest, testTrimmedRightEqExact)
{
  EXPECT_THAT(" abc", StrTrimmedRightEq(" abc"));
}

TEST(TrimmedEqTest, testTrimmedRightEqFirstLeftPadded)
{
  EXPECT_THAT("  abc", Not(StrTrimmedRightEq("abc")));
}

TEST(TrimmedEqTest, testTrimmedRightEqFirstRightPadded)
{
  EXPECT_THAT("abc  ", StrTrimmedRightEq("abc"));
}

TEST(TrimmedEqTest, testTrimmedRightEqFirstBothPadded)
{
  EXPECT_THAT("  abc  ", Not(StrTrimmedRightEq("abc")));
}

TEST(TrimmedEqTest, testTrimmedRightEqSecondLeftPadded)
{
  EXPECT_THAT("abc", Not(StrTrimmedRightEq("  abc")));
}

TEST(TrimmedEqTest, testTrimmedRightEqSecondRightPadded)
{
  EXPECT_THAT("abc", StrTrimmedRightEq("abc  "));
}

TEST(TrimmedEqTest, testTrimmedRightEqSecondBothPadded)
{
  EXPECT_THAT("abc", Not(StrTrimmedRightEq("  abc  ")));
}

TEST(TrimmedEqTest, testTrimmedRightEqBothBothPadded)
{
  EXPECT_THAT(" abc  ", Not(StrTrimmedRightEq("   abc    ")));
}

TEST(TrimmedEqTest, TrimmedRightEqWide)
{
  EXPECT_THAT(L" abc  ", Not(StrTrimmedRightEq(L"   abc     ")));
}

TEST(TrimmedEqTest, TrimmedRightEqWideAndNarrow)
{
  EXPECT_THAT(" abc  ", Not(StrTrimmedRightEq(L"   abc     ")));
}
