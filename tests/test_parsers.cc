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

#include <boost/fusion/container/generation/make_vector.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/numeric/conversion/cast.hpp>

using namespace parsers;

typedef std::string::const_iterator::difference_type iter_difftype;

template<typename T>
std::ostream &operator<<(std::ostream &out, const boost::optional<T> &o)
{
  if(o)
    return out << "Just " << *o;
  else
    return out << "Nothing";
}

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
  CPPUNIT_TEST(testAndFirstBothMatch);
  CPPUNIT_TEST(testAndFirstBothMatchChained);
  CPPUNIT_TEST(testAndFirstOnlyFirstMatches);
  CPPUNIT_TEST(testAndFirstOnlyFirstMatchesChained);
  CPPUNIT_TEST(testAndFirstFirstFailsAndConsumesInput);
  CPPUNIT_TEST(testAndFirstFirstFailsWithoutConsumingInput);
  CPPUNIT_TEST(testAndFirstFirstFailsWithoutConsumingInputChained);
  CPPUNIT_TEST(testSetExpected);
  CPPUNIT_TEST(testForeachEmptyEndNotEOF);
  CPPUNIT_TEST(testForeachEmptyEndEOF);
  CPPUNIT_TEST(testForeachNotEmptyEndNotEOF);
  CPPUNIT_TEST(testForeachNotEmptyEndEOF);
  CPPUNIT_TEST(testForeachFailure);
  CPPUNIT_TEST(testSkipEmptyEndNotEOF);
  CPPUNIT_TEST(testSkipEmptyEndEOF);
  CPPUNIT_TEST(testSkipNotEmptyEndNotEOF);
  CPPUNIT_TEST(testSkipNotEmptyEndEOF);
  CPPUNIT_TEST(testSkipOneEmptyEndNotEOF);
  CPPUNIT_TEST(testSkipOneEmptyEndEOF);
  CPPUNIT_TEST(testSkipOneNotEmptyEndNotEOF);
  CPPUNIT_TEST(testSkipOneNotEmptyEndEOF);
  CPPUNIT_TEST(testManyEmptyEndNotEOF);
  CPPUNIT_TEST(testManyEmptyEndEOF);
  CPPUNIT_TEST(testManyNotEmptyEndNotEOF);
  CPPUNIT_TEST(testManyNotEmptyEndEOF);
  CPPUNIT_TEST(testManyFailure);
  CPPUNIT_TEST(testOrFirstBranchMatches);
  CPPUNIT_TEST(testOrFirstBranchFailsAndConsumesInput);
  CPPUNIT_TEST(testOrSecondBranchMatches);
  CPPUNIT_TEST(testOrSecondBranchFailsAndConsumesInput);
  CPPUNIT_TEST(testOrSecondBranchFailsAndConsumesNoInput);
  CPPUNIT_TEST(testOrChainedSuccess);
  CPPUNIT_TEST(testOrChainedFailure);
  CPPUNIT_TEST(testOrChainedCollapsesParsers);
  CPPUNIT_TEST(testMaybeFailure);
  CPPUNIT_TEST(testMaybeSuccess);
  CPPUNIT_TEST(testMaybeValue);
  CPPUNIT_TEST(testTupleSuccess);
  CPPUNIT_TEST(testTupleFailureWithoutConsumingInput);
  CPPUNIT_TEST(testTupleFailureWithConsumingInput);
  CPPUNIT_TEST(testApplySuccess);
  CPPUNIT_TEST(testApplyFailure);
  CPPUNIT_TEST(testFollowedBySuccess);
  CPPUNIT_TEST(testFollowedByFailure);
  CPPUNIT_TEST(testNotFollowedBySuccess);
  CPPUNIT_TEST(testNotFollowedByFailure);
  CPPUNIT_TEST(testPostAssertSuccess);
  CPPUNIT_TEST(testPostAssertFailure);
  CPPUNIT_TEST(testManyOneSuccess);
  CPPUNIT_TEST(testManyOneFailure);
  CPPUNIT_TEST(testOptionalSuccess);
  CPPUNIT_TEST(testOptionalFailure);
  CPPUNIT_TEST(testSepBySuccessEmpty);
  CPPUNIT_TEST(testSepBySuccessNonempty);
  CPPUNIT_TEST(testSepByFailureInFirstElement);
  CPPUNIT_TEST(testSepByFailureInSeparator);
  CPPUNIT_TEST(testSepByFailureInSecondElement);

  CPPUNIT_TEST_SUITE_END();

public:

  void testParseChar()
  {
    ch_p<char> comma(','), semicolon(';');

    std::string input(",;;,,,;a");

    std::string::const_iterator begin = input.begin(), end = input.end();

    CPPUNIT_ASSERT_THROW(semicolon.parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)0, begin - input.begin());

    CPPUNIT_ASSERT_EQUAL(',', comma.parse(begin, end));
    CPPUNIT_ASSERT_EQUAL((iter_difftype)1, begin - input.begin());

    CPPUNIT_ASSERT_THROW(comma.parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)1, begin - input.begin());

    CPPUNIT_ASSERT_EQUAL(';', semicolon.parse(begin, end));
    CPPUNIT_ASSERT_EQUAL((iter_difftype)2, begin - input.begin());

    CPPUNIT_ASSERT_EQUAL(';', semicolon.parse(begin, end));
    CPPUNIT_ASSERT_EQUAL((iter_difftype)3, begin - input.begin());

    CPPUNIT_ASSERT_EQUAL(',', comma.parse(begin, end));
    CPPUNIT_ASSERT_EQUAL((iter_difftype)4, begin - input.begin());

    CPPUNIT_ASSERT_THROW(semicolon.parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)4, begin - input.begin());

    CPPUNIT_ASSERT_EQUAL(',', comma.parse(begin, end));
    CPPUNIT_ASSERT_EQUAL((iter_difftype)5, begin - input.begin());

    CPPUNIT_ASSERT_EQUAL(',', comma.parse(begin, end));
    CPPUNIT_ASSERT_EQUAL((iter_difftype)6, begin - input.begin());

    CPPUNIT_ASSERT_EQUAL(';', semicolon.parse(begin, end));
    CPPUNIT_ASSERT_EQUAL((iter_difftype)7, begin - input.begin());

    CPPUNIT_ASSERT_THROW(comma.parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)7, begin - input.begin());

    CPPUNIT_ASSERT_THROW(semicolon.parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)7, begin - input.begin());
  }

  void testParseAnyChar()
  {
    anychar_p<char> any;
    std::string input = "abcdefg";
    std::string::const_iterator begin = input.begin(), end = input.end();

    for(std::string::size_type i = 0; i < input.size(); ++i)
      {
        CPPUNIT_ASSERT_EQUAL(i, boost::numeric_cast<std::string::size_type>((begin - input.begin())));
        CPPUNIT_ASSERT_EQUAL(input[i], any.parse(begin, end));
      }

    CPPUNIT_ASSERT_EQUAL(input.size(), boost::numeric_cast<std::string::size_type>(begin - input.begin()));
  }

  void testWhitespace()
  {
    space_p sp(space());

    std::string input = " b";

    std::string::const_iterator begin = input.begin(), end = input.end();

    CPPUNIT_ASSERT_EQUAL(' ', sp.parse(begin, end));
    CPPUNIT_ASSERT_EQUAL((iter_difftype)1, begin - input.begin());

    CPPUNIT_ASSERT_THROW(sp.parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)1, begin - input.begin());
  }

  void testInteger()
  {
    integer_p integer;

    for(int i = -1000; i <= 1000; ++i)
      {
        std::string input = boost::lexical_cast<std::string>(i) + "Q";

        std::string::const_iterator begin = input.begin(), end = input.end();
        CPPUNIT_ASSERT_EQUAL(i, integer.parse(begin, end));
        CPPUNIT_ASSERT_EQUAL((iter_difftype)1, input.end() - begin);
      }

    {
      std::string input = boost::lexical_cast<std::string>(INT_MIN) + "Q";
      std::string::const_iterator begin = input.begin(), end = input.end();
      CPPUNIT_ASSERT_EQUAL(INT_MIN, integer.parse(begin, end));
      CPPUNIT_ASSERT_EQUAL((iter_difftype)1, input.end() - begin);
    }

    {
      std::string input = boost::lexical_cast<std::string>(INT_MAX) + "Q";
      std::string::const_iterator begin = input.begin(), end = input.end();
      CPPUNIT_ASSERT_EQUAL(INT_MAX, integer.parse(begin, end));
      CPPUNIT_ASSERT_EQUAL((iter_difftype)1, input.end() - begin);
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

      CPPUNIT_ASSERT_THROW(integer.parse(begin, end), ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)0, begin - input.begin());
    }

    // If we see a lone hyphen, we should eat it and fail.
    {
      std::string input = "-abc123";
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_THROW(integer.parse(begin, end), ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)1, begin - input.begin());
    }

    // The only other failure mode would be an integer that's too
    // large or too small.  Ideally we would test INT_MIN-1 and
    // INT_MAX+1, but computing that would require some complexity.
    // Easier to just append zeroes.

    {
      std::string input = boost::lexical_cast<std::string>(INT_MIN) + "0";
      std::string::const_iterator begin = input.begin(), end = input.end();
      CPPUNIT_ASSERT_THROW(integer.parse(begin, end), ParseException);
      CPPUNIT_ASSERT(begin != input.begin());
    }

    {
      std::string input = boost::lexical_cast<std::string>(INT_MAX) + "0";
      std::string::const_iterator begin = input.begin(), end = input.end();
      CPPUNIT_ASSERT_THROW(integer.parse(begin, end), ParseException);
      CPPUNIT_ASSERT(begin != input.begin());
    }
  }

  void testEof()
  {
    std::string input = "abc";

    std::string::const_iterator begin = input.begin(), end = input.end();

    eof e;

    CPPUNIT_ASSERT_THROW(e.parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)0, begin - input.begin());

    ++begin;
    CPPUNIT_ASSERT_THROW(e.parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)1, begin - input.begin());

    ++begin;
    CPPUNIT_ASSERT_THROW(e.parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)2, begin - input.begin());

    ++begin;
    CPPUNIT_ASSERT_NO_THROW(e.parse(begin, end));
    CPPUNIT_ASSERT_EQUAL((iter_difftype)3, begin - input.begin());
  }

  void testStr()
  {
    std::string input = "abcdef";

    std::string::const_iterator begin = input.begin(), end = input.end();

    str abc("abc");
    str da("da");
    str xyz("xyz");

    CPPUNIT_ASSERT_NO_THROW(abc.parse(begin, end));
    CPPUNIT_ASSERT_EQUAL((iter_difftype)3, begin - input.begin());

    CPPUNIT_ASSERT_THROW(da.parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)4, begin - input.begin());

    CPPUNIT_ASSERT_THROW(xyz.parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)4, begin - input.begin());
  }

  void testVal()
  {
    std::string input = "o3qithkje5hgkjh";
    std::string::const_iterator begin = input.begin(), end = input.begin();

    val_p<std::string> v = val("abcdefg");

    CPPUNIT_ASSERT_EQUAL(std::string("abcdefg"), v.parse(begin, end));
    CPPUNIT_ASSERT_EQUAL((iter_difftype)0, begin - input.begin());
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

    str abc("abc");
    ch_p<char> d('d');

    CPPUNIT_ASSERT_EQUAL('d', (abc >> d).parse(begin, end));
    CPPUNIT_ASSERT_EQUAL((iter_difftype)4, begin - input.begin());
  }

  // Case 1.5: chain together multiple parsers.
  void testAndThenBothMatchChained()
  {
    std::string input = "abcd";
    std::string::const_iterator begin = input.begin(), end = input.end();

    ch_p<char> a('a'), b('b'), c('c'), d('d');

    CPPUNIT_ASSERT_EQUAL('d', (a >> b >> c >> d).parse(begin, end));
    CPPUNIT_ASSERT_EQUAL((iter_difftype)4, begin - input.begin());
  }

  // Case 2: only the first parser matches.
  void testAndThenOnlyFirstMatches()
  {
    std::string input = "abcd";
    std::string::const_iterator begin = input.begin(), end = input.end();

    str ab("ab"), yz("yz");

    CPPUNIT_ASSERT_THROW((ab >> yz).parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)2, begin - input.begin());
  }

  // Case 2.5: only the first two parsers match, with several chained together.
  void testAndThenOnlyFirstMatchesChained()
  {
    std::string input = "abcd";
    std::string::const_iterator begin = input.begin(), end = input.end();

    ch_p<char> a('a'), b('b'), y('y'), z('z');

    CPPUNIT_ASSERT_THROW((a >> b >> y >> z).parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)2, begin - input.begin());
  }

  // Case 3: the first parser fails after consuming input.
  void testAndThenFirstFailsAndConsumesInput()
  {
    std::string input = "abcd";
    std::string::const_iterator begin = input.begin(), end = input.end();

    str ax("ax"), yz("yz");

    CPPUNIT_ASSERT_THROW((ax >> yz).parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)1, begin - input.begin());
  }

  // Case 4: the first parser fails without consuming input.
  void testAndThenFirstFailsWithoutConsumingInput()
  {
    std::string input = "abcd";
    std::string::const_iterator begin = input.begin(), end = input.end();

    str wx("wx"), yz("yz");

    CPPUNIT_ASSERT_THROW((wx >> yz).parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)0, begin - input.begin());
  }

  // Case 4.5: the first parser fails without consuming input (chained).
  void testAndThenFirstFailsWithoutConsumingInputChained()
  {
    std::string input = "abcd";
    std::string::const_iterator begin = input.begin(), end = input.end();

    ch_p<char> w('w'), x('x'), y('y'), z('z');

    CPPUNIT_ASSERT_THROW((w >> x >> y >> z).parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)0, begin - input.begin());
  }

  // Cases to test for the "and first" parser:
  //
  //  1. Both parsers match.  Check that "begin" is advanced
  //     past the second match and that the first value is
  //     returned.
  //  2. Only the first parser matches.  Check that "begin" is
  //     advanced and an exception is thrown.
  //  3. The first parser fails after consuming input.
  //     Check that "begin" is advanced and an exception is
  //     thrown.
  //  4. The first parser fails without consuming input.
  //     Check that "begin" is NOT advanced and that an
  //     exception is thrown.

  // Case 1: both parsers match.
  void testAndFirstBothMatch()
  {
    std::string input = "abcd";
    std::string::const_iterator begin = input.begin(), end = input.end();

    ch_p<char> a('a');
    str bcd("bcd");

    CPPUNIT_ASSERT_EQUAL('a', (a << bcd).parse(begin, end));
    CPPUNIT_ASSERT_EQUAL((iter_difftype)4, begin - input.begin());
  }

  // Case 1.5: chain together multiple parsers.
  void testAndFirstBothMatchChained()
  {
    std::string input = "abcd";
    std::string::const_iterator begin = input.begin(), end = input.end();

    ch_p<char> a('a'), b('b'), c('c'), d('d');

    CPPUNIT_ASSERT_EQUAL('a', (a << b << c << d).parse(begin, end));
    CPPUNIT_ASSERT_EQUAL((iter_difftype)4, begin - input.begin());
  }

  // Case 2: only the first parser matches.
  void testAndFirstOnlyFirstMatches()
  {
    std::string input = "abcd";
    std::string::const_iterator begin = input.begin(), end = input.end();

    str ab("ab"), yz("yz");

    CPPUNIT_ASSERT_THROW((ab << yz).parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)2, begin - input.begin());
  }

  // Case 2.5: only the first two parsers match, with several chained together.
  void testAndFirstOnlyFirstMatchesChained()
  {
    std::string input = "abcd";
    std::string::const_iterator begin = input.begin(), end = input.end();

    ch_p<char> a('a'), b('b'), y('y'), z('z');

    CPPUNIT_ASSERT_THROW((a << b << y << z).parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)2, begin - input.begin());
  }

  // Case 3: the first parser fails after consuming input.
  void testAndFirstFirstFailsAndConsumesInput()
  {
    std::string input = "abcd";
    std::string::const_iterator begin = input.begin(), end = input.end();

    str ax("ax"), yz("yz");

    CPPUNIT_ASSERT_THROW((ax << yz).parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)1, begin - input.begin());
  }

  // Case 4: the first parser fails without consuming input.
  void testAndFirstFirstFailsWithoutConsumingInput()
  {
    std::string input = "abcd";
    std::string::const_iterator begin = input.begin(), end = input.end();

    str wx("wx"), yz("yz");

    CPPUNIT_ASSERT_THROW((wx << yz).parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)0, begin - input.begin());
  }

  // Case 4.5: the first parser fails without consuming input (chained).
  void testAndFirstFirstFailsWithoutConsumingInputChained()
  {
    std::string input = "abcd";
    std::string::const_iterator begin = input.begin(), end = input.end();

    ch_p<char> w('w'), x('x'), y('y'), z('z');

    CPPUNIT_ASSERT_THROW((w << x << y << z).parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)0, begin - input.begin());
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

    CPPUNIT_ASSERT_EQUAL('a', a2.parse(begin, end));
    CPPUNIT_ASSERT_EQUAL((iter_difftype)1, begin - input.begin());

    CPPUNIT_ASSERT_THROW(a2.parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)1, begin - input.begin());
  }

  void testForeachEmptyEndNotEOF()
  {
    alpha_p letter = alpha();

    std::string input = "   abcde   ";
    std::string::const_iterator begin = input.begin(), end = input.end();

    std::string result;

    (foreach(letter, push_back_a(result))).parse(begin, end);

    CPPUNIT_ASSERT_EQUAL(std::string(""), result);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)0, begin - input.begin());
  }

  void testForeachEmptyEndEOF()
  {
    alpha_p letter = alpha();

    std::string input = "";
    std::string::const_iterator begin = input.begin(), end = input.end();

    std::string result;

    (foreach(letter, push_back_a(result))).parse(begin, end);

    CPPUNIT_ASSERT_EQUAL(std::string(""), result);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)0, begin - input.begin());
  }

  void testForeachNotEmptyEndNotEOF()
  {
    alpha_p letter = alpha();

    std::string input = "abcde   ";
    std::string::const_iterator begin = input.begin(), end = input.end();

    std::string result;

    (foreach(letter, push_back_a(result))).parse(begin, end);

    CPPUNIT_ASSERT_EQUAL(std::string("abcde"), result);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)5, begin - input.begin());
  }

  void testForeachNotEmptyEndEOF()
  {
    alpha_p letter = alpha();

    std::string input = "abcde";
    std::string::const_iterator begin = input.begin(), end = input.end();

    std::string result;

    (foreach(letter, push_back_a(result))).parse(begin, end);

    CPPUNIT_ASSERT_EQUAL(std::string("abcde"), result);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)5, begin - input.begin());
  }

  void testForeachFailure()
  {
    std::string input = "2345 1234 234jf ";
    std::string::const_iterator begin = input.begin(), end = input.end();

    std::vector<int> result;

    CPPUNIT_ASSERT_THROW(foreach(integer() >> ch(' '),
                                 push_back_a(result)).parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)13, begin - input.begin());
  }


  void testSkipEmptyEndNotEOF()
  {
    alpha_p letter = alpha();

    std::string input = "   abcde   ";
    std::string::const_iterator begin = input.begin(), end = input.end();

    (skip(letter)).parse(begin, end);

    CPPUNIT_ASSERT_EQUAL((iter_difftype)0, begin - input.begin());
  }

  void testSkipEmptyEndEOF()
  {
    alpha_p letter = alpha();

    std::string input = "";
    std::string::const_iterator begin = input.begin(), end = input.end();

    (skip(letter)).parse(begin, end);

    CPPUNIT_ASSERT_EQUAL((iter_difftype)0, begin - input.begin());
  }

  void testSkipNotEmptyEndNotEOF()
  {
    alpha_p letter = alpha();

    std::string input = "abcde   ";
    std::string::const_iterator begin = input.begin(), end = input.end();

    (skip(letter)).parse(begin, end);

    CPPUNIT_ASSERT_EQUAL((iter_difftype)5, begin - input.begin());
  }

  void testSkipNotEmptyEndEOF()
  {
    alpha_p letter = alpha();

    std::string input = "abcde";
    std::string::const_iterator begin = input.begin(), end = input.end();

    (skip(letter)).parse(begin, end);

    CPPUNIT_ASSERT_EQUAL((iter_difftype)5, begin - input.begin());
  }

  void testSkipOneEmptyEndNotEOF()
  {
    alpha_p letter = alpha();

    std::string input = "   abcde   ";
    std::string::const_iterator begin = input.begin(), end = input.end();

    CPPUNIT_ASSERT_THROW((skipOne(letter)).parse(begin, end), ParseException);

    CPPUNIT_ASSERT_EQUAL((iter_difftype)0, begin - input.begin());
  }

  void testSkipOneEmptyEndEOF()
  {
    alpha_p letter = alpha();

    std::string input = "";
    std::string::const_iterator begin = input.begin(), end = input.end();

    CPPUNIT_ASSERT_THROW((skipOne(letter)).parse(begin, end), ParseException);

    CPPUNIT_ASSERT_EQUAL((iter_difftype)0, begin - input.begin());
  }

  void testSkipOneNotEmptyEndNotEOF()
  {
    alpha_p letter = alpha();

    std::string input = "abcde   ";
    std::string::const_iterator begin = input.begin(), end = input.end();

    (skipOne(letter)).parse(begin, end);

    CPPUNIT_ASSERT_EQUAL((iter_difftype)5, begin - input.begin());
  }

  void testSkipOneNotEmptyEndEOF()
  {
    alpha_p letter = alpha();

    std::string input = "abcde";
    std::string::const_iterator begin = input.begin(), end = input.end();

    (skipOne(letter)).parse(begin, end);

    CPPUNIT_ASSERT_EQUAL((iter_difftype)5, begin - input.begin());
  }

  void testManyEmptyEndNotEOF()
  {
    alpha_p letter = alpha();

    std::string input = "   abcde   ";
    std::string::const_iterator begin = input.begin(), end = input.end();

    boost::shared_ptr<std::string> result = (many_string(letter)).parse(begin, end);

    CPPUNIT_ASSERT_EQUAL(std::string(""), *result);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)0, begin - input.begin());
  }

  void testManyEmptyEndEOF()
  {
    alpha_p letter = alpha();

    std::string input = "";
    std::string::const_iterator begin = input.begin(), end = input.end();

    boost::shared_ptr<std::string> result = (many_string(letter)).parse(begin, end);

    CPPUNIT_ASSERT_EQUAL(std::string(""), *result);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)0, begin - input.begin());
  }

  void testManyNotEmptyEndNotEOF()
  {
    alpha_p letter = alpha();

    std::string input = "abcde   ";
    std::string::const_iterator begin = input.begin(), end = input.end();

    boost::shared_ptr<std::string> result = (many_string(letter)).parse(begin, end);

    CPPUNIT_ASSERT_EQUAL(std::string("abcde"), *result);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)5, begin - input.begin());
  }

  void testManyNotEmptyEndEOF()
  {
    alpha_p letter = alpha();

    std::string input = "abcde";
    std::string::const_iterator begin = input.begin(), end = input.end();

    boost::shared_ptr<std::string> result = (many_string(letter)).parse(begin, end);

    CPPUNIT_ASSERT_EQUAL(std::string("abcde"), *result);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)5, begin - input.begin());
  }

  void testManyFailure()
  {
    std::string input = "2345 1234 234jf ";
    std::string::const_iterator begin = input.begin(), end = input.end();

    CPPUNIT_ASSERT_THROW(many(integer() >> ch(' ')).parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)13, begin - input.begin());
  }

  void testOrFirstBranchMatches()
  {
    str ab("ab"), a("a");

    std::string input = "abskrj";
    std::string::const_iterator begin = input.begin(), end = input.end();

    CPPUNIT_ASSERT_NO_THROW((ab | a).parse(begin, end));
    CPPUNIT_ASSERT_EQUAL((iter_difftype)2, begin - input.begin());
  }

  void testOrFirstBranchFailsAndConsumesInput()
  {
    str ab("ab"), a("a");

    std::string input = "acskrj";
    std::string::const_iterator begin = input.begin(), end = input.end();

    CPPUNIT_ASSERT_THROW((ab | a).parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)1, begin - input.begin());
  }

  void testOrSecondBranchMatches()
  {
    str ab("ab"), cd("cd");

    std::string input = "cdlkwrj";
    std::string::const_iterator begin = input.begin(), end = input.end();

    CPPUNIT_ASSERT_NO_THROW((ab | cd).parse(begin, end));
    CPPUNIT_ASSERT_EQUAL((iter_difftype)2, begin - input.begin());
  }

  void testOrSecondBranchFailsAndConsumesInput()
  {
    str ab("ab"), cd("cd");

    std::string input = "cyzablksdfj";
    std::string::const_iterator begin = input.begin(), end = input.end();

    CPPUNIT_ASSERT_THROW((ab | cd).parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)1, begin - input.begin());
  }

  void testOrSecondBranchFailsAndConsumesNoInput()
  {
    str ab("ab"), cd("cd");

    std::string input = "yzablksdfj";
    std::string::const_iterator begin = input.begin(), end = input.end();

    CPPUNIT_ASSERT_THROW((ab | cd).parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)0, begin - input.begin());
  }

  void testOrChainedSuccess()
  {
    str ab("ab"), cd("cd"), ef("ef");

    {
      std::string input = "abcdef";
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_NO_THROW((ab | cd | ef).parse(begin, end));
      CPPUNIT_ASSERT_EQUAL((iter_difftype)2, begin - input.begin());
    }

    {
      std::string input = "cd";
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_NO_THROW((ab | cd | ef).parse(begin, end));
      CPPUNIT_ASSERT_EQUAL((iter_difftype)2, begin - input.begin());
    }

    {
      std::string input = "ef";
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_NO_THROW((ab | cd | ef).parse(begin, end));
      CPPUNIT_ASSERT_EQUAL((iter_difftype)2, begin - input.begin());
    }
  }

  void testOrChainedFailure()
  {
    str ab("ab"), cd("cd"), ef("ef"), a("a");

    {
      std::string input = "axyz";
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_THROW((ab | cd | ef).parse(begin, end), ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)1, begin - input.begin());
    }

    {
      std::string input = "axyz";
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_THROW((ab | a | a | a | a | cd | ef).parse(begin, end), ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)1, begin - input.begin());
    }

    {
      std::string input = "cxyz";
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_THROW((ab | cd | ef).parse(begin, end), ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)1, begin - input.begin());
    }

    {
      std::string input = "exyz";
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_THROW((ab | cd | ef).parse(begin, end), ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)1, begin - input.begin());
    }

    {
      std::string input = "xyz";
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_THROW((ab | cd | ef).parse(begin, end), ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)0, begin - input.begin());
    }
  }

  void testOrChainedCollapsesParsers()
  {
    ch_p<char> a('a'), b('b'), c('c'), d('d'), e('e'), f('f');

    CPPUNIT_ASSERT_EQUAL(6, (int)boost::fusion::size((a | b | c | d | e | f).get_values()));
  }

  void testMaybeSuccess()
  {
    std::string input("abcdefg");
    std::string::const_iterator begin = input.begin(), end = input.end();

    CPPUNIT_ASSERT_EQUAL(std::string("ab"),
                         std::string((maybe(str("ab") >> val("ab")) | (str("a") >> val("a"))).parse(begin, end)));
    CPPUNIT_ASSERT_EQUAL((iter_difftype)2, begin - input.begin());
  }

  void testMaybeFailure()
  {
    std::string input("abcdefg");
    std::string::const_iterator begin = input.begin(), end = input.end();

    CPPUNIT_ASSERT_EQUAL(std::string("a"),
                         std::string((maybe(str("ad") >> val("ad")) | (str("a") >> val("a"))).parse(begin, end)));
    CPPUNIT_ASSERT_EQUAL((iter_difftype)1, begin - input.begin());
  }

  void testMaybeValue()
  {
    std::string input("12345");
    std::string::const_iterator begin = input.begin(), end = input.end();

    CPPUNIT_ASSERT_EQUAL(12345, integer().parse(begin, end));
  }

  void testTupleSuccess()
  {
    {
      std::string input("765");
      std::string::const_iterator begin = input.begin(), end = input.end();
      CPPUNIT_ASSERT_EQUAL(boost::fusion::make_vector(765),
                           parsers::tuple(integer()).parse(begin, end));
      CPPUNIT_ASSERT_EQUAL((iter_difftype)3, begin - input.begin());
    }

    {
      std::string input("123,456,789");
      std::string::const_iterator begin = input.begin(), end = input.end();
      CPPUNIT_ASSERT_EQUAL(boost::fusion::make_vector(123, 456, 789),
                           (integer(), ch(',') >> integer(), ch(',') >> integer()).parse(begin, end));
      CPPUNIT_ASSERT_EQUAL((iter_difftype)11, begin - input.begin());

    }
  }

  void testTupleFailureWithoutConsumingInput()
  {
    {
      std::string input("abcdefg");
      std::string::const_iterator begin = input.begin(), end = input.end();
      CPPUNIT_ASSERT_THROW((integer(), str("abc"), str("def")).parse(begin, end), ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)0, begin - input.begin());
    }
  }

  void testTupleFailureWithConsumingInput()
  {
    // Cases tested: in a 3-tuple, check what happens if the first
    // parser fails, if the second parser fails, and if the third
    // fails, with input consumed in each case.

    {
      std::string input("123,456,789");
      std::string::const_iterator begin = input.begin(), end = input.end();
      CPPUNIT_ASSERT_THROW((digit() >> alpha(), integer(), integer()).parse(begin, end), ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)1, begin - input.begin());
    }

    {
      std::string input("123,456,789");
      std::string::const_iterator begin = input.begin(), end = input.end();
      CPPUNIT_ASSERT_THROW((integer(), ch(',') >> alpha(), integer()).parse(begin, end), ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)4, begin - input.begin());
    }

    {
      std::string input("123,456,789");
      std::string::const_iterator begin = input.begin(), end = input.end();
      CPPUNIT_ASSERT_THROW((integer(), ch(',') >> integer(), ch(',') >> integer() >> alpha()).parse(begin, end), ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)11, begin - input.begin());
    }
  }

  // Simple function object usable with apply().
  struct addOne
  {
    typedef int result_type;

    result_type operator()(int input) const
    {
      return input + 1;
    }
  };

  struct makeIntPair
  {
    typedef boost::fusion::vector<int, int> result_type;

    result_type operator()(int n1, int n2) const
    {
      return boost::fusion::make_vector(n1, n2);
    }
  };

  void testApplySuccess()
  {
    // Unary test via apply().
    {
      std::string input("123,456");
      std::string::const_iterator begin = input.begin(), end = input.end();
      CPPUNIT_ASSERT_EQUAL(124, apply(addOne(), integer()).parse(begin, end));
      CPPUNIT_ASSERT_EQUAL((iter_difftype)3, begin - input.begin());
    }

    // Unary test using a unary tuple.
    {
      std::string input("123,456");
      std::string::const_iterator begin = input.begin(), end = input.end();
      CPPUNIT_ASSERT_EQUAL(124, ::parsers::apply(addOne(), tuple(integer())).parse(begin, end));
      CPPUNIT_ASSERT_EQUAL((iter_difftype)3, begin - input.begin());
    }

    // Unary test without the apply() convenience routine..
    {
      std::string input("123,456");
      std::string::const_iterator begin = input.begin(), end = input.end();
      CPPUNIT_ASSERT_EQUAL(124, (apply_p<addOne, tuple_p<boost::fusion::vector<integer_p> > >(addOne(), tuple(integer())).parse(begin, end)));
      CPPUNIT_ASSERT_EQUAL((iter_difftype)3, begin - input.begin());
    }

    // Binary test.
    {
      std::string input("123,456");
      std::string::const_iterator begin = input.begin(), end = input.end();
      CPPUNIT_ASSERT_EQUAL(boost::fusion::make_vector(123, 456), (apply(makeIntPair(), (integer(), ch(',') >> integer())).parse(begin, end)));
      CPPUNIT_ASSERT_EQUAL((iter_difftype)7, begin - input.begin());
    }

    // Test that parsers::construct_f works as expected.
    {
      std::string input("123,456");
      std::string::const_iterator begin = input.begin(), end = input.end();
      CPPUNIT_ASSERT_EQUAL(boost::fusion::make_vector(123, 456), (apply(construct_f<boost::fusion::vector<int, int> >(),
                                                                        (integer(), ch(',') >> integer())).parse(begin, end)));
      CPPUNIT_ASSERT_EQUAL((iter_difftype)7, begin - input.begin());
    }
  }

  void testApplyFailure()
  {
    {
      std::string input("123,456");
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_THROW(apply(makeIntPair(), (integer(), integer())).parse(begin, end), ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)3, begin - input.begin());
    }
  }

  void testFollowedBySuccess()
  {
    {
      std::string input("123,456");
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_EQUAL(456, (integer() >> ch(',') >> followedBy(integer())).parse(begin, end));
      CPPUNIT_ASSERT_EQUAL((iter_difftype)4, begin - input.begin());
    }
  }

  void testFollowedByFailure()
  {
    {
      std::string input("123,abc");
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_THROW((integer() >> ch(',') >> followedBy(integer())).parse(begin, end), ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)4, begin - input.begin());
    }

    {
      std::string input("123,axy");
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_THROW((integer() >> ch(',') >> followedBy(str("abc"))).parse(begin, end), ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)4, begin - input.begin());
    }
  }

  void testNotFollowedBySuccess()
  {
    {
      std::string input("123,456");
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_NO_THROW((integer() >> notFollowedBy(str(",abc"))).parse(begin, end));
      CPPUNIT_ASSERT_EQUAL((iter_difftype)3, begin - input.begin());
    }
  }

  void testNotFollowedByFailure()
  {
    {
      std::string input("123,456");
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_THROW((integer() >> ch(',') >> notFollowedBy(integer())).parse(begin, end), ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)4, begin - input.begin());
    }

    {
      std::string input("123,abc");
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_THROW((integer() >> ch(',') >> notFollowedBy(str("abc"))).parse(begin, end), ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)4, begin - input.begin());
    }
  }

  class lessThan_f
  {
    int x;

  public:
    lessThan_f(int _x) : x(_x) { }

    bool operator()(int y) const { return y < x; }
  };

  void testPostAssertSuccess()
  {
    {
      std::string input("123456");
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_EQUAL(123456, postAssert(integer(), "integer below 500000", lessThan_f(500000)).parse(begin, end));
      CPPUNIT_ASSERT_EQUAL((iter_difftype)6, begin - input.begin());
    }
  }

  void testPostAssertFailure()
  {
    {
      std::string input("123456");
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_THROW(postAssert(integer(), "integer below 10", lessThan_f(10)).parse(begin, end), ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)6, begin - input.begin());
    }
  }

  void testManyOneSuccess()
  {
    {
      std::string input("57482adfb");
      std::string::const_iterator begin = input.begin(), end = input.end();

      manyOne_result<charif_p<char, digit_f>, std::string>::type
        p = manyOne_string(digit());
      boost::shared_ptr<std::string> ptr;
      CPPUNIT_ASSERT_NO_THROW(ptr = p.parse(begin, end));
      CPPUNIT_ASSERT_EQUAL(std::string("57482"), *ptr);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)5, begin - input.begin());
    }

    {
      std::string input("a34b15c999");
      std::string::const_iterator begin = input.begin(), end = input.end();

      typedef boost::fusion::vector<char, int> charint_vector;
      typedef std::vector<charint_vector> result_type;
      result_type expected;
      expected.push_back(charint_vector('a', 34));
      expected.push_back(charint_vector('b', 15));
      expected.push_back(charint_vector('c', 999));

      boost::shared_ptr<result_type> result;
      CPPUNIT_ASSERT_NO_THROW(result = manyOne( (anychar(), integer()) ).parse(begin, end));
      CPPUNIT_ASSERT_EQUAL((iter_difftype)10, begin - input.begin());

      CPPUNIT_ASSERT_EQUAL(expected.size(), result->size());
      for(result_type::size_type i = 0; i < expected.size(); ++i)
        {
          std::ostringstream msg;

          msg << "At index " << i;
          CPPUNIT_ASSERT_EQUAL(expected[i], (*result)[i]);
        }
    }
  }

  void testManyOneFailure()
  {
    {
      std::string input("abdsfa");
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_THROW(manyOne_string(digit()).parse(begin, end), ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)0, begin - input.begin());
    }

    {
      std::string input("a34b15c");
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_THROW( manyOne( (anychar(), integer()) ).parse(begin, end), ParseException );

      CPPUNIT_ASSERT_EQUAL((iter_difftype)7, begin - input.begin());
    }
  }

  void testOptionalSuccess()
  {
    {
      std::string input("123478zs");
      std::string::const_iterator begin = input.begin(), end = input.end();

      boost::optional<int> result;
      CPPUNIT_ASSERT_NO_THROW( result = optional(integer()).parse(begin, end) );
      CPPUNIT_ASSERT_EQUAL(boost::optional<int>(123478), result);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)6, begin - input.begin());
    }

    {
      std::string input("alsdkfj");
      std::string::const_iterator begin = input.begin(), end = input.end();

      boost::optional<int> result;
      CPPUNIT_ASSERT_NO_THROW( result = optional(integer()).parse(begin, end) );
      CPPUNIT_ASSERT_EQUAL(boost::optional<int>(), result);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)0, begin - input.begin());
    }
  }

  void testOptionalFailure()
  {
    {
      std::string input("234,abc");
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_THROW(optional( (integer(), integer()) ).parse(begin, end), ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)3, begin - input.begin());
    }
  }

  void testSepBySuccessEmpty()
  {
    {
      std::string input = "abcde";
      std::string::const_iterator begin = input.begin(), end = input.end();

      boost::shared_ptr<std::vector<int> > result;
      CPPUNIT_ASSERT_NO_THROW(result = sepBy(str(","), integer()).parse(begin, end));
      CPPUNIT_ASSERT_EQUAL(0, (int)result->size());
      CPPUNIT_ASSERT_EQUAL((iter_difftype)0, begin - input.begin());
    }
  }

  void testSepBySuccessNonempty()
  {
    {
      std::string input = "3094,124498,34saflk";
      std::string::const_iterator begin = input.begin(), end = input.end();

      boost::shared_ptr<std::vector<int> > result;
      CPPUNIT_ASSERT_NO_THROW(result = sepBy(str(","), integer()).parse(begin, end));
      CPPUNIT_ASSERT_EQUAL(3, (int)result->size());
      CPPUNIT_ASSERT_EQUAL(3094, (*result)[0]);
      CPPUNIT_ASSERT_EQUAL(124498, (*result)[1]);
      CPPUNIT_ASSERT_EQUAL(34, (*result)[2]);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)14, begin - input.begin());
    }
  }

  void testSepByFailureInFirstElement()
  {
    {
      std::string input = "abxyz,abcde,abcde";
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_THROW(sepBy(str(","), str("abcde")).parse(begin, end), ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)2, begin - input.begin());
    }
  }

  void testSepByFailureInSeparator()
  {
    {
      std::string input = "ab,,cd,ef";
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_THROW(sepBy(str(",,"), many(alpha())).parse(begin, end), ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)7, begin - input.begin());
    }
  }

  void testSepByFailureInSecondElement()
  {
    // Check that if we see a separator, we have to see an element
    // after it (i.e., the parser doesn't accept dangling separators
    // at the end of the list).
    {
      std::string input = "ab,cd,,ef";
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_THROW(sepBy(str(","), manyOne(alpha())).parse(begin, end), ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)6, begin - input.begin());
    }
  }
};

CPPUNIT_TEST_SUITE_REGISTRATION(ParsersTest);
