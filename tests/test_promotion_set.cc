// test_dense_setset.cc
//
//   Copyright (C) 2005, 2009 Daniel Burrows
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

#include <generic/problemresolver/promotion_set.h>
#include <generic/problemresolver/dummy_universe.h>

#include <cppunit/extensions/HelperMacros.h>

#include <iostream>

namespace
{
  const char *dummy_universe_1 = "\
UNIVERSE [			  \
  PACKAGE a < v1 v2 v3 > v1	  \
  PACKAGE b < v1 v2 v3 > v1	  \
  PACKAGE c < v1 v2 v3 > v1	  \
				  \
  DEP a v1 -> < b v2 >		  \
  DEP b v2 -> < c v2 >		  \
				  \
  DEP a v2 -> < >		  \
  DEP a v3 -> < >		  \
]";
}

class Promotion_SetTest : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(Promotion_SetTest);

  CPPUNIT_TEST(testFindHighestPromotion);

  CPPUNIT_TEST_SUITE_END();

  static dummy_universe_ref parseUniverse(const std::string &s)
  {
    std::istringstream in(s);

    return parse_universe(in);
  }

  typedef dummy_universe_ref::package package;
  typedef dummy_universe_ref::version version;
  typedef dummy_universe_ref::dep dep;
  typedef generic_choice<dummy_universe_ref> choice;
  typedef promotion_set<dummy_universe_ref> dummy_promotion_set;
  typedef dummy_promotion_set::promotion promotion;

  static imm::set<promotion> get_promotions(const dummy_promotion_set &promotions)
  {
    imm::set<promotion> rval;
    for(dummy_promotion_set::const_iterator it = promotions.begin();
	it != promotions.end(); ++it)
      rval.insert(*it);
    return rval;
  }

  static unsigned int empirical_promotions_size(const dummy_promotion_set &promotions)
  {
    unsigned int rval = 0;
    for(dummy_promotion_set::const_iterator it = promotions.begin();
	it != promotions.end(); ++it)
      ++rval;
    return rval;
  }

  static void make_test_promotions(const dummy_universe_ref &u,
				   dummy_promotion_set &promotions)
  {
    // Otherwise the tests below will fail.
    CPPUNIT_ASSERT_EQUAL((unsigned int)0, promotions.size());

    package a(u.find_package("a"));
    package b(u.find_package("b"));
    package c(u.find_package("c"));

    version av1(a.version_from_name("v1"));
    version av2(a.version_from_name("v2"));
    version av3(a.version_from_name("v3"));

    version bv1(b.version_from_name("v1"));
    version bv2(b.version_from_name("v2"));
    version bv3(b.version_from_name("v3"));

    version cv1(c.version_from_name("v1"));
    version cv2(c.version_from_name("v2"));
    version cv3(c.version_from_name("v3"));

    dep av1d1(*av1.deps_begin());
    dep bv2d1(*bv2.deps_begin());
    dep av2d1(*av2.deps_begin());
    dep av3d1(*av3.deps_begin());

    // Insert promotions:
    // (Install(a v1 [a v2 -> <>])): 100
    // (Install(a v1, b v2, c v3)): 50
    // (Install(a v1, b v2)): 75
    // (Install(a v1, b v2, c v1)): 10
    // (Install(b v2)): 30
    // (Break(b v2 -> <c v2>)): 125
    // (Install(c v3), Break (b v2 -> <c v2>)): 500
    //
    // Note that the third entry should override the second one.  The
    // fourth entry shouldn't be stored at all.

    // Verify that the promotion set has the expected
    // entries at each point in its construction.
    imm::set<promotion> expected_promotions;
    CPPUNIT_ASSERT_EQUAL(expected_promotions.size(), promotions.size());
    CPPUNIT_ASSERT_EQUAL(expected_promotions.size(), empirical_promotions_size(promotions));
    CPPUNIT_ASSERT_EQUAL(expected_promotions, get_promotions(promotions));

    imm::set<choice> p1_choices;
    p1_choices.insert(choice::make_install_version_from_dep_source(av1, av2d1));
    promotion p1(p1_choices, 100);
    expected_promotions.insert(p1);
    promotions.insert(p1);
    CPPUNIT_ASSERT_EQUAL(expected_promotions.size(), promotions.size());
    CPPUNIT_ASSERT_EQUAL(expected_promotions.size(), empirical_promotions_size(promotions));
    CPPUNIT_ASSERT_EQUAL(expected_promotions, get_promotions(promotions));

    imm::set<choice> p2_choices;
    p2_choices.insert(choice::make_install_version(av1));
    p2_choices.insert(choice::make_install_version(bv2));
    p2_choices.insert(choice::make_install_version(cv3));
    promotion p2(p2_choices, 50);
    expected_promotions.insert(p2);
    promotions.insert(p2);
    CPPUNIT_ASSERT_EQUAL(expected_promotions.size(), promotions.size());
    CPPUNIT_ASSERT_EQUAL(expected_promotions.size(), empirical_promotions_size(promotions));
    CPPUNIT_ASSERT_EQUAL(expected_promotions, get_promotions(promotions));

    imm::set<choice> p3_choices;
    p3_choices.insert(choice::make_install_version(av1));
    p3_choices.insert(choice::make_install_version(bv2));
    promotion p3(p3_choices, 75);
    expected_promotions.insert(p3);
    expected_promotions.erase(p2);
    promotions.insert(p3);
    CPPUNIT_ASSERT_EQUAL(expected_promotions.size(), promotions.size());
    CPPUNIT_ASSERT_EQUAL(expected_promotions.size(), empirical_promotions_size(promotions));
    CPPUNIT_ASSERT_EQUAL(expected_promotions, get_promotions(promotions));

    imm::set<choice> p4_choices;
    p4_choices.insert(choice::make_install_version(av1));
    p4_choices.insert(choice::make_install_version(bv2));
    p4_choices.insert(choice::make_install_version(cv1));
    promotion p4(p4_choices, 10);
    promotions.insert(p4);
    CPPUNIT_ASSERT_EQUAL(expected_promotions.size(), promotions.size());
    CPPUNIT_ASSERT_EQUAL(expected_promotions.size(), empirical_promotions_size(promotions));
    CPPUNIT_ASSERT_EQUAL(expected_promotions, get_promotions(promotions));

    imm::set<choice> p5_choices;
    p5_choices.insert(choice::make_install_version(bv2));
    promotion p5(p5_choices, 30);
    expected_promotions.insert(p5);
    promotions.insert(p5);
    CPPUNIT_ASSERT_EQUAL(expected_promotions.size(), promotions.size());
    CPPUNIT_ASSERT_EQUAL(expected_promotions.size(), empirical_promotions_size(promotions));
    CPPUNIT_ASSERT_EQUAL(expected_promotions, get_promotions(promotions));

    imm::set<choice> p6_choices;
    p6_choices.insert(choice::make_break_soft_dep(bv2d1));
    promotion p6(p6_choices, 125);
    expected_promotions.insert(p6);
    promotions.insert(p6);
    CPPUNIT_ASSERT_EQUAL(expected_promotions.size(), promotions.size());
    CPPUNIT_ASSERT_EQUAL(expected_promotions.size(), empirical_promotions_size(promotions));
    CPPUNIT_ASSERT_EQUAL(expected_promotions, get_promotions(promotions));

    imm::set<choice> p7_choices;
    p7_choices.insert(choice::make_install_version(cv3));
    p7_choices.insert(choice::make_break_soft_dep(bv2d1));
    promotion p7(p7_choices, 500);
    expected_promotions.insert(p7);
    promotions.insert(p7);
  }

public:
  // Test searching for the highest promotion contained in a 
  void testFindHighestPromotion()
  {
    dummy_universe_ref u(parseUniverse(dummy_universe_1));
    dummy_promotion_set p(u);

    package a(u.find_package("a"));
    package b(u.find_package("b"));
    package c(u.find_package("c"));

    version av1(a.version_from_name("v1"));
    version av2(a.version_from_name("v2"));
    version av3(a.version_from_name("v3"));

    version bv1(b.version_from_name("v1"));
    version bv2(b.version_from_name("v2"));
    version bv3(b.version_from_name("v3"));

    version cv1(c.version_from_name("v1"));
    version cv2(c.version_from_name("v2"));
    version cv3(c.version_from_name("v3"));

    dep av1d1(*av1.deps_begin());
    dep bv2d1(*bv2.deps_begin());
    dep av2d1(*av2.deps_begin());
    dep av3d1(*av3.deps_begin());

    make_test_promotions(u, p);

    // First search: (Install(a v1), Install(b v3),
    //                Install(c v3), Break(b v2 -> <c v2>))
    //
    // Should turn up only (T500: Install(c v3), Break(b v2 -> <c v2>))
    imm::set<choice> search1;
    search1.insert(choice::make_install_version(av1));
    search1.insert(choice::make_install_version(bv3));
    search1.insert(choice::make_install_version(cv3));
    search1.insert(choice::make_break_soft_dep(bv2d1));

    imm::set<choice> expected1;
    expected1.insert(choice::make_install_version(cv3));
    expected1.insert(choice::make_break_soft_dep(bv2d1));

    dummy_promotion_set::const_iterator found = p.find_highest_promotion_for(search1);
    CPPUNIT_ASSERT(found != p.end());
    CPPUNIT_ASSERT_EQUAL(promotion(expected1, 500), *found);

    dummy_promotion_set::const_iterator found2 =
      p.find_highest_promotion_containing(search1, choice::make_install_version(av1));
    CPPUNIT_ASSERT(found2 == p.end());

    found2 = p.find_highest_promotion_containing(search1, choice::make_install_version(bv3));
    CPPUNIT_ASSERT(found2 == p.end());

    found2 = p.find_highest_promotion_containing(search1, choice::make_install_version(cv3));
    CPPUNIT_ASSERT(found2 != p.end());
    CPPUNIT_ASSERT_EQUAL(promotion(expected1, 500), *found2);

    found2 = p.find_highest_promotion_containing(search1, choice::make_break_soft_dep(bv2d1));
    CPPUNIT_ASSERT(found2 != p.end());
    CPPUNIT_ASSERT_EQUAL(promotion(expected1, 500), *found2);



    // Second search: (a v1, b v1)
  }
};

CPPUNIT_TEST_SUITE_REGISTRATION(Promotion_SetTest);
