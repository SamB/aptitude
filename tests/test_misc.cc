// Miscellaneous tests.
//
//   Copyright (C) 2005, 2007 Daniel Burrows
//
//   This program is free software; you can redistribute it and/or
//   modify it under the terms of the GNU General Public License as
//   published by the Free Software Foundation; either version 2 of
//   the License, or (at your option) any later version.
//
//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//   General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with this program; see the file COPYING.  If not, write to
//   the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//   Boston, MA 02111-1307, USA.

#include <cppunit/extensions/HelperMacros.h>

#include <generic/util/util.h>

class MiscTest : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(MiscTest);

  CPPUNIT_TEST(testStripWS);
  CPPUNIT_TEST(testOrderlessLt);
  CPPUNIT_TEST(test_ssprintf);

  CPPUNIT_TEST_SUITE_END();
private:
  void assertStripWS(const std::string &s,
		     const std::string &expected)
  {
    std::string s2 = s;
    stripws(s2);

    CPPUNIT_ASSERT_EQUAL(expected, s2);
  }

  void testStripWS()
  {
    assertStripWS("    abc", "abc");
    assertStripWS("abc    ", "abc");
    assertStripWS("   abc   ", "abc");

    assertStripWS("    ", "");

    // double-check that there are no weird corner cases involving
    // singly letters.
    assertStripWS(" a", "a");
    assertStripWS("a ", "a");
    assertStripWS(" a ", "a");
  }

  void testOrderlessLt()
  {
    orderless_lt<int> cmp;

    std::pair<int, int> a(1, 2);
    std::pair<int, int> b(2, 1);
    std::pair<int, int> c(4, 1);
    std::pair<int, int> d(1, 4);
    std::pair<int, int> e(4, 6);

    CPPUNIT_ASSERT(!cmp(a, a));
    CPPUNIT_ASSERT(!cmp(b, b));
    CPPUNIT_ASSERT(!cmp(c, c));
    CPPUNIT_ASSERT(!cmp(d, d));
    CPPUNIT_ASSERT(!cmp(e, e));

    CPPUNIT_ASSERT(!cmp(a, b));
    CPPUNIT_ASSERT( cmp(b, c));
    CPPUNIT_ASSERT(!cmp(c, d));
    CPPUNIT_ASSERT( cmp(d, e));
    CPPUNIT_ASSERT(!cmp(e, a));

    CPPUNIT_ASSERT( cmp(a, c));
    CPPUNIT_ASSERT( cmp(b, d));
    CPPUNIT_ASSERT( cmp(c, e));
    CPPUNIT_ASSERT(!cmp(d, a));
    CPPUNIT_ASSERT(!cmp(e, b));

    CPPUNIT_ASSERT( cmp(a, d));
    CPPUNIT_ASSERT( cmp(b, e));
    CPPUNIT_ASSERT(!cmp(c, a));
    CPPUNIT_ASSERT(!cmp(d, b));
    CPPUNIT_ASSERT(!cmp(e, c));

    CPPUNIT_ASSERT( cmp(a, e));
    CPPUNIT_ASSERT(!cmp(b, a));
    CPPUNIT_ASSERT(!cmp(c, b));
    CPPUNIT_ASSERT(!cmp(d, c));
    CPPUNIT_ASSERT(!cmp(e, d));
  }

  void test_ssprintf()
  {
    // Test that inserting very long strings via ssprintf actually works.
    std::string horriblelongthing = "abcdefghijklmnopqrstuvwxyz";
    while(horriblelongthing.size() < 4096)
      horriblelongthing += horriblelongthing;

    CPPUNIT_ASSERT_EQUAL(horriblelongthing + " 20", ssprintf("%s %d", horriblelongthing.c_str(), 20));
  }
};

CPPUNIT_TEST_SUITE_REGISTRATION(MiscTest);
