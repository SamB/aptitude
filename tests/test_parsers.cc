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
};

CPPUNIT_TEST_SUITE_REGISTRATION(ParsersTest);
