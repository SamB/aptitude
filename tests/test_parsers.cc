// test_parsers.cc
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

#include <generic/util/parsers.h>

#include <cppunit/extensions/HelperMacros.h>

#include <boost/lexical_cast.hpp>
#include <boost/numeric/conversion/cast.hpp>

using namespace parsers;

class ParsersTest : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(ParsersTest);

  CPPUNIT_TEST(testParseChar);
  CPPUNIT_TEST(testParseAnyChar);
  CPPUNIT_TEST(testWhitespace);
  CPPUNIT_TEST(testInteger);
  CPPUNIT_TEST(testIntegerInvalid);
  CPPUNIT_TEST(testEof);
  CPPUNIT_TEST(testStr);
  CPPUNIT_TEST(testVal);
  CPPUNIT_TEST(testAndThenBothMatch);
  CPPUNIT_TEST(testAndThenBothMatchChained);
  CPPUNIT_TEST(testAndThenOnlyFirstMatches);
  CPPUNIT_TEST(testAndThenOnlyFirstMatchesChained);
  CPPUNIT_TEST(testAndThenFirstFailsAndConsumesInput);
  CPPUNIT_TEST(testAndThenFirstFailsWithoutConsumingInput);
  CPPUNIT_TEST(testAndThenFirstFailsWithoutConsumingInputChained);
  CPPUNIT_TEST(testSetExpected);
  CPPUNIT_TEST(testForeachEmptyEndNotEOF);
  CPPUNIT_TEST(testForeachEmptyEndEOF);
  CPPUNIT_TEST(testForeachNotEmptyEndNotEOF);
  CPPUNIT_TEST(testForeachNotEmptyEndEOF);

  CPPUNIT_TEST_SUITE_END();

public:

  void testParseChar()
  {
    ch_p<char> comma(','), semicolon(';');

    std::string input(",;;,,,;a");

    std::string::const_iterator begin = input.begin(), end = input.end();

    CPPUNIT_ASSERT_THROW(semicolon(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL(0, begin - input.begin());

    CPPUNIT_ASSERT_EQUAL(',', comma(begin, end));
    CPPUNIT_ASSERT_EQUAL(1, begin - input.begin());

    CPPUNIT_ASSERT_THROW(comma(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL(1, begin - input.begin());

    CPPUNIT_ASSERT_EQUAL(';', semicolon(begin, end));
    CPPUNIT_ASSERT_EQUAL(2, begin - input.begin());

    CPPUNIT_ASSERT_EQUAL(';', semicolon(begin, end));
    CPPUNIT_ASSERT_EQUAL(3, begin - input.begin());

    CPPUNIT_ASSERT_EQUAL(',', comma(begin, end));
    CPPUNIT_ASSERT_EQUAL(4, begin - input.begin());

    CPPUNIT_ASSERT_THROW(semicolon(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL(4, begin - input.begin());

    CPPUNIT_ASSERT_EQUAL(',', comma(begin, end));
    CPPUNIT_ASSERT_EQUAL(5, begin - input.begin());

    CPPUNIT_ASSERT_EQUAL(',', comma(begin, end));
    CPPUNIT_ASSERT_EQUAL(6, begin - input.begin());

    CPPUNIT_ASSERT_EQUAL(';', semicolon(begin, end));
    CPPUNIT_ASSERT_EQUAL(7, begin - input.begin());

    CPPUNIT_ASSERT_THROW(comma(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL(7, begin - input.begin());

    CPPUNIT_ASSERT_THROW(semicolon(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL(7, begin - input.begin());
  }

  void testParseAnyChar()
  {
    anychar_p<char> any;
    std::string input = "abcdefg";
    std::string::const_iterator begin = input.begin(), end = input.end();

    for(std::string::size_type i = 0; i < input.size(); ++i)
      {
        CPPUNIT_ASSERT_EQUAL(i, boost::numeric_cast<std::string::size_type>((begin - input.begin())));
        CPPUNIT_ASSERT_EQUAL(input[i], any(begin, end));
      }

    CPPUNIT_ASSERT_EQUAL(input.size(), boost::numeric_cast<std::string::size_type>(begin - input.begin()));
  }

  void testWhitespace()
  {
    space_p sp(space());

    std::string input = " b";

    std::string::const_iterator begin = input.begin(), end = input.end();

    CPPUNIT_ASSERT_EQUAL(' ', sp(begin, end));
    CPPUNIT_ASSERT_EQUAL(1, begin - input.begin());

    CPPUNIT_ASSERT_THROW(sp(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL(1, begin - input.begin());
  }

  void testInteger()
  {
    integer_p integer;

    for(int i = -1000; i <= 1000; ++i)
      {
        std::string input = boost::lexical_cast<std::string>(i) + "Q";

        std::string::const_iterator begin = input.begin(), end = input.end();
        CPPUNIT_ASSERT_EQUAL(i, integer(begin, end));
        CPPUNIT_ASSERT_EQUAL(1, input.end() - begin);
      }

    {
      std::string input = boost::lexical_cast<std::string>(INT_MIN) + "Q";
      std::string::const_iterator begin = input.begin(), end = input.end();
      CPPUNIT_ASSERT_EQUAL(INT_MIN, integer(begin, end));
      CPPUNIT_ASSERT_EQUAL(1, input.end() - begin);
    }

    {
      std::string input = boost::lexical_cast<std::string>(INT_MAX) + "Q";
      std::string::const_iterator begin = input.begin(), end = input.end();
      CPPUNIT_ASSERT_EQUAL(INT_MAX, integer(begin, end));
      CPPUNIT_ASSERT_EQUAL(1, input.end() - begin);
    }
  }

  void testIntegerInvalid()
  {
    integer_p integer;

    // Try something that's just not an integer at all (but has
    // integer bits later in the string).
    {
      std::string input = "abc123";
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_THROW(integer(begin, end), ParseException);
      CPPUNIT_ASSERT_EQUAL(0, begin - input.begin());
    }

    // If we see a lone hyphen, we should eat it and fail.
    {
      std::string input = "-abc123";
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_THROW(integer(begin, end), ParseException);
      CPPUNIT_ASSERT_EQUAL(1, begin - input.begin());
    }

    // The only other failure mode would be an integer that's too
    // large or too small.  Ideally we would test INT_MIN-1 and
    // INT_MAX+1, but computing that would require some complexity.
    // Easier to just append zeroes.

    {
      std::string input = boost::lexical_cast<std::string>(INT_MIN) + "0";
      std::string::const_iterator begin = input.begin(), end = input.end();
      CPPUNIT_ASSERT_THROW(integer(begin, end), ParseException);
      CPPUNIT_ASSERT(begin != input.begin());
    }

    {
      std::string input = boost::lexical_cast<std::string>(INT_MAX) + "0";
      std::string::const_iterator begin = input.begin(), end = input.end();
      CPPUNIT_ASSERT_THROW(integer(begin, end), ParseException);
      CPPUNIT_ASSERT(begin != input.begin());
    }
  }

  void testEof()
  {
    std::string input = "abc";

    std::string::const_iterator begin = input.begin(), end = input.end();

    eof e;

    CPPUNIT_ASSERT_THROW(e(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL(0, begin - input.begin());

    ++begin;
    CPPUNIT_ASSERT_THROW(e(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL(1, begin - input.begin());

    ++begin;
    CPPUNIT_ASSERT_THROW(e(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL(2, begin - input.begin());

    ++begin;
    CPPUNIT_ASSERT_NO_THROW(e(begin, end));
    CPPUNIT_ASSERT_EQUAL(3, begin - input.begin());
  }

  void testStr()
  {
    std::string input = "abcdef";

    std::string::const_iterator begin = input.begin(), end = input.end();

    str abc("abc");
    str da("da");
    str xyz("xyz");

    CPPUNIT_ASSERT_NO_THROW(abc(begin, end));
    CPPUNIT_ASSERT_EQUAL(3, begin - input.begin());

    CPPUNIT_ASSERT_THROW(da(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL(4, begin - input.begin());

    CPPUNIT_ASSERT_THROW(xyz(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL(4, begin - input.begin());
  }

  void testVal()
  {
    std::string input = "o3qithkje5hgkjh";
    std::string::const_iterator begin = input.begin(), end = input.begin();

    val_p<std::string> v = val("abcdefg");

    CPPUNIT_ASSERT_EQUAL(std::string("abcdefg"), v(begin, end));
    CPPUNIT_ASSERT_EQUAL(0, begin - input.begin());
  }

  // Cases to test for the "and then" parser:
  //
  //  1. Both parsers match.  Check that "begin" is advanced
  //     past the second match and that the second value (not
  //     the first) is returned.
  //  2. Only the first parser matches.  Check that "begin" is
  //     advanced and an exception is thrown.
  //  3. The first parser fails after consuming input.
  //     Check that "begin" is advanced and an exception is
  //     thrown.
  //  4. The first parser fails without consuming input.
  //     Check that "begin" is NOT advanced and that an
  //     exception is thrown.

  // Case 1: both parsers match.
  void testAndThenBothMatch()
  {
    std::string input = "abcd";
    std::string::const_iterator begin = input.begin(), end = input.end();

    str ab("ab"), cd("cd");

    CPPUNIT_ASSERT_NO_THROW((ab >> cd)(begin, end));
    CPPUNIT_ASSERT_EQUAL(4, begin - input.begin());
  }

  // Case 1.5: chain together multiple parsers.
  void testAndThenBothMatchChained()
  {
    std::string input = "abcd";
    std::string::const_iterator begin = input.begin(), end = input.end();

    ch_p<char> a('a'), b('b'), c('c'), d('d');

    CPPUNIT_ASSERT_NO_THROW((a >> b >> c >> d)(begin, end));
    CPPUNIT_ASSERT_EQUAL(4, begin - input.begin());
  }

  // Case 2: only the first parser matches.
  void testAndThenOnlyFirstMatches()
  {
    std::string input = "abcd";
    std::string::const_iterator begin = input.begin(), end = input.end();

    str ab("ab"), yz("yz");

    CPPUNIT_ASSERT_THROW((ab >> yz)(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL(2, begin - input.begin());
  }

  // Case 2.5: only the first two parsers match, with several chained together.
  void testAndThenOnlyFirstMatchesChained()
  {
    std::string input = "abcd";
    std::string::const_iterator begin = input.begin(), end = input.end();

    ch_p<char> a('a'), b('b'), y('y'), z('z');

    CPPUNIT_ASSERT_THROW((a >> b >> y >> z)(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL(2, begin - input.begin());
  }

  // Case 3: the first parser fails after consuming input.
  void testAndThenFirstFailsAndConsumesInput()
  {
    std::string input = "abcd";
    std::string::const_iterator begin = input.begin(), end = input.end();

    str ax("ax"), yz("yz");

    CPPUNIT_ASSERT_THROW((ax >> yz)(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL(1, begin - input.begin());
  }

  // Case 4: the first parser fails without consuming input.
  void testAndThenFirstFailsWithoutConsumingInput()
  {
    std::string input = "abcd";
    std::string::const_iterator begin = input.begin(), end = input.end();

    str wx("wx"), yz("yz");

    CPPUNIT_ASSERT_THROW((wx >> yz)(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL(0, begin - input.begin());
  }

  // Case 4.5: the first parser fails without consuming input (chained).
  void testAndThenFirstFailsWithoutConsumingInputChained()
  {
    std::string input = "abcd";
    std::string::const_iterator begin = input.begin(), end = input.end();

    ch_p<char> w('w'), x('x'), y('y'), z('z');

    CPPUNIT_ASSERT_THROW((w >> x >> y >> z)(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL(0, begin - input.begin());
  }

  void testSetExpected()
  {
    ch_p<char> a('a');

    std::string new_expected("My hovercraft is full of eels");

    set_expected_p<ch_p<char> > a2 = a[new_expected];

    std::stringstream msg1;
    a.get_expected_description(msg1);
    CPPUNIT_ASSERT( msg1.str() != new_expected );

    std::stringstream msg2;
    a2.get_expected_description(msg2);
    CPPUNIT_ASSERT_EQUAL(new_expected, msg2.str());

    // Test that we can parse, too.
    std::string input("aba");
    std::string::const_iterator begin = input.begin(), end = input.end();

    CPPUNIT_ASSERT_EQUAL('a', a2(begin, end));
    CPPUNIT_ASSERT_EQUAL(1, begin - input.begin());

    CPPUNIT_ASSERT_THROW(a2(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL(1, begin - input.begin());
  }

  void testForeachEmptyEndNotEOF()
  {
    alpha_p letter = alpha();

    std::string input = "   abcde   ";
    std::string::const_iterator begin = input.begin(), end = input.end();

    std::string result;

    (foreach(letter, push_back_a(result)))(begin, end);

    CPPUNIT_ASSERT_EQUAL(std::string(""), result);
    CPPUNIT_ASSERT_EQUAL(0, begin - input.begin());
  }

  void testForeachEmptyEndEOF()
  {
    alpha_p letter = alpha();

    std::string input = "";
    std::string::const_iterator begin = input.begin(), end = input.end();

    std::string result;

    (foreach(letter, push_back_a(result)))(begin, end);

    CPPUNIT_ASSERT_EQUAL(std::string(""), result);
    CPPUNIT_ASSERT_EQUAL(0, begin - input.begin());
  }

  void testForeachNotEmptyEndNotEOF()
  {
    alpha_p letter = alpha();

    std::string input = "abcde   ";
    std::string::const_iterator begin = input.begin(), end = input.end();

    std::string result;

    (foreach(letter, push_back_a(result)))(begin, end);

    CPPUNIT_ASSERT_EQUAL(std::string("abcde"), result);
    CPPUNIT_ASSERT_EQUAL(5, begin - input.begin());
  }

  void testForeachNotEmptyEndEOF()
  {
    alpha_p letter = alpha();

    std::string input = "abcde";
    std::string::const_iterator begin = input.begin(), end = input.end();

    std::string result;

    (foreach(letter, push_back_a(result)))(begin, end);

    CPPUNIT_ASSERT_EQUAL(std::string("abcde"), result);
    CPPUNIT_ASSERT_EQUAL(5, begin - input.begin());
  }
};

CPPUNIT_TEST_SUITE_REGISTRATION(ParsersTest);
