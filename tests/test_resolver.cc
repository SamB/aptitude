// test_resolver.cc                       -*-c++-*-
//
//   Copyright (C) 2005 Daniel Burrows
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
//
// A test of the generic resolver layer.

#include <generic/problemresolver/dummy_universe.h>
#include <generic/problemresolver/problemresolver.h>

#include <cppunit/extensions/HelperMacros.h>

#include <sstream>

using namespace std;

typedef generic_solution<dummy_universe_ref> dummy_solution;

const char *dummy_universe_1 = "\
UNIVERSE [ \
  PACKAGE a < v1 v2 v3 > v1 \
  PACKAGE b < v1 v2 v3 > v1 \
  PACKAGE c < v1 v2 v3 > v1 \
\
  DEP a v1 -> < b v2 > \
  DEP b v2 -> < c v2 > \
\
  DEP a v2 -> < > \
  DEP a v3 -> < > \
]";

const char *dummy_universe_2 = "\
UNIVERSE [ \
  PACKAGE a < v1 v2 > v1 \
  PACKAGE b < v1 v2 > v1 \
  PACKAGE c < v1 v2 > v1 \
\
  DEP a v1 -> < b v2 > \
  DEP a v1 -> < c v2 > \
]";

// Done this way so meaningful line numbers are generated.
#define assertEqEquivalent(x1, x2) \
do { \
    CPPUNIT_ASSERT((x1) == (x2)); \
    CPPUNIT_ASSERT((x2) == (x1)); \
    CPPUNIT_ASSERT(!((x1) != (x2))); \
    CPPUNIT_ASSERT(!((x2) != (x1))); \
    CPPUNIT_ASSERT(!((x1) < (x2))); \
    CPPUNIT_ASSERT(!((x2) < (x1))); \
} while(0)

#define assertEqInequivalent(x1, x2) \
do { \
    CPPUNIT_ASSERT((x1) != (x2)); \
    CPPUNIT_ASSERT((x2) != (x1)); \
    CPPUNIT_ASSERT(!((x1) == (x2))); \
    CPPUNIT_ASSERT(!((x2) == (x1))); \
    CPPUNIT_ASSERT((x1) < (x2) || (x2) < (x1)); \
    CPPUNIT_ASSERT(!((x1) < (x2) && (x2) < (x1))); \
} while(0)

#define assertLtEquivalent(x1, x2, lt) \
do { \
    CPPUNIT_ASSERT(!(lt((x1), (x2)))); \
    CPPUNIT_ASSERT(!(lt((x2), (x1)))); \
} while(0)

#define assertLtInequivalent(x1, x2, lt) \
do { \
    CPPUNIT_ASSERT(lt((x1), (x2)) || lt((x2), (x1))); \
    CPPUNIT_ASSERT(!(lt((x1), (x2)) && lt((x2), (x1)))); \
} while(0)

class ResolverTest : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(ResolverTest);

  CPPUNIT_TEST(testActionCompare);
  CPPUNIT_TEST(testSolutionCompare);
  CPPUNIT_TEST(testRejections);
  CPPUNIT_TEST(testDropSolutionSupersets);

  CPPUNIT_TEST_SUITE_END();

private:
  static dummy_universe_ref parseUniverse(const std::string &s)
  {
    std::istringstream in(s);

    return parse_universe(in);
  }

  /** Test that the comparison operators on actions work. */
  void testActionCompare()
  {
    dummy_universe_ref u = parseUniverse(dummy_universe_1);

    // Grab two arbitrary deps.
    dummy_universe::dep_iterator di = u.deps_begin();
    CPPUNIT_ASSERT(!di.end());
    dummy_universe::dep d1 = *di;
    ++di;
    CPPUNIT_ASSERT(!di.end());
    dummy_universe::dep d2 = *di;

    dummy_solution::action a1(u.find_package("a").version_from_name("v1"),
			      d1, false, 49);
    dummy_solution::action a2(u.find_package("a").version_from_name("v1"),
			      d2, false, 21);

    assertEqEquivalent(a1, a1);
    assertEqEquivalent(a1, a2);

    dummy_solution::action a3(u.find_package("a").version_from_name("v2"),
			      d1, false, 49);

    assertEqEquivalent(a2, a2);
    assertEqEquivalent(a3, a3);

    assertEqInequivalent(a1, a3);
    assertEqInequivalent(a2, a3);

    dummy_solution::action a4(u.find_package("c").version_from_name("v3"),
			      d2, false, 21);

    assertEqEquivalent(a4, a4);
    assertEqInequivalent(a1, a4);
    assertEqInequivalent(a2, a4);
    assertEqInequivalent(a3, a4);
  }

  /** Generate a successor solution that just contains the given
   *  information by using internal constructors of the solution
   *  class.  Used when testing the solution class itself.
   */
  template<class a_iter, class u_iter>
  dummy_solution unsafe_successor(const dummy_solution &parent,
				  a_iter abegin, a_iter aend,
				  u_iter ubegin, u_iter uend)
  {
    imm::map<dummy_universe::package, dummy_solution::action> actions = parent.get_actions();
    imm::set<dummy_universe::dep> unresolved = parent.get_unresolved_soft_deps();

    for(a_iter ai = abegin; ai != aend; ++ai)
      actions.put((*ai).ver.get_package(), *ai);

    for(u_iter ui = ubegin; ui != uend; ++ui)
      unresolved.insert(*ui);

    return dummy_solution(new dummy_solution::solution_rep(actions,
							   parent.get_broken(),
							   unresolved,
							   parent.get_forbidden_versions(),
							   parent.get_score(),
							   parent.get_action_score()));
  }

  /** Tests that the comparison operations on solutions work. */
  void testSolutionCompare()
  {
    dummy_universe_ref u = parseUniverse(dummy_universe_1);

    // Grab two arbitrary deps.
    dummy_universe::dep_iterator di = u.deps_begin();
    CPPUNIT_ASSERT(!di.end());
    dummy_universe::dep d1 = *di;
    ++di;
    CPPUNIT_ASSERT(!di.end());
    dummy_universe::dep d2 = *di;

    solution_weights weights(0, 0, 0, 0, u.get_version_count());

    imm::set<dummy_universe::dep> u_broken;
    for(dummy_universe::broken_dep_iterator bi = u.broken_begin();
	!bi.end(); ++bi)
      u_broken.insert(*bi);

    dummy_solution::action a1(u.find_package("a").version_from_name("v1"),
			      d1, false, 49);
    dummy_solution::action a2(u.find_package("a").version_from_name("v1"),
			      d2, false, 21);
    dummy_solution::action a3(u.find_package("a").version_from_name("v2"),
			      d1, false, 49);

    dummy_solution::action a4(u.find_package("c").version_from_name("v3"),
			      d2, false, 21);


    // Generate some meaningless solutions to check that equivalency
    // is correctly calculated according to the version mappings and
    // the set of unsolved soft deps.
    dummy_solution s0 = dummy_solution::root_node(u_broken,
						  u, weights);
    dummy_solution s1
      = unsafe_successor(s0, &a1, &a1+1,
			 (dummy_universe::dep *) 0,
			 (dummy_universe::dep *) 0);
    dummy_solution s2
      = unsafe_successor(s0, &a2, &a2+1,
			 (dummy_universe::dep *) 0,
			 (dummy_universe::dep *) 0);
    dummy_solution s3
      = unsafe_successor(s0, &a3, &a3+1,
			 (dummy_universe::dep *) 0,
			 (dummy_universe::dep *) 0);
    dummy_solution s4
      = unsafe_successor(s0, &a4, &a4+1,
			 (dummy_universe::dep *) 0,
			 (dummy_universe::dep *) 0);

    // the following two should be equal.
    dummy_solution s5
      = unsafe_successor(s1, &a4, &a4+1,
			 (dummy_universe::dep *) 0,
			 (dummy_universe::dep *) 0);
    dummy_solution s6
      = unsafe_successor(s4, &a1, &a1+1,
			 (dummy_universe::dep *) 0,
			 (dummy_universe::dep *) 0);

    // and this should not equal any other solution.
    dummy_solution s7
      = unsafe_successor(s0,
			 (dummy_solution::action *) 0,
			 (dummy_solution::action *) 0,
			 &d1, &d1+1);

    dummy_resolver::solution_contents_compare solcmp;

    assertLtEquivalent(s0, s0, solcmp);
    assertLtInequivalent(s0, s1, solcmp);
    assertLtInequivalent(s0, s2, solcmp);
    assertLtInequivalent(s0, s3, solcmp);
    assertLtInequivalent(s0, s4, solcmp);
    assertLtInequivalent(s0, s5, solcmp);
    assertLtInequivalent(s0, s6, solcmp);
    assertLtInequivalent(s0, s7, solcmp);

    assertLtEquivalent(s1, s1, solcmp);
    assertLtEquivalent(s1, s2, solcmp);
    assertLtInequivalent(s1, s3, solcmp);
    assertLtInequivalent(s1, s4, solcmp);
    assertLtInequivalent(s1, s5, solcmp);
    assertLtInequivalent(s1, s6, solcmp);
    assertLtInequivalent(s1, s7, solcmp);

    assertLtEquivalent(s2, s2, solcmp);
    assertLtInequivalent(s2, s3, solcmp);
    assertLtInequivalent(s2, s4, solcmp);
    assertLtInequivalent(s2, s5, solcmp);
    assertLtInequivalent(s2, s6, solcmp);
    assertLtInequivalent(s2, s7, solcmp);

    assertLtEquivalent(s3, s3, solcmp);
    assertLtInequivalent(s3, s4, solcmp);
    assertLtInequivalent(s3, s5, solcmp);
    assertLtInequivalent(s3, s6, solcmp);
    assertLtInequivalent(s3, s7, solcmp);

    assertLtEquivalent(s4, s4, solcmp);
    assertLtInequivalent(s4, s5, solcmp);
    assertLtInequivalent(s4, s6, solcmp);
    assertLtInequivalent(s4, s7, solcmp);

    assertLtEquivalent(s5, s5, solcmp);
    assertLtEquivalent(s5, s6, solcmp);
    assertLtInequivalent(s5, s7, solcmp);

    assertLtEquivalent(s6, s6, solcmp);
    assertLtInequivalent(s6, s7, solcmp);

    assertLtEquivalent(s7, s7, solcmp);
  }

  // Check that rejections of versions don't block out all versions of
  // the package (this actually happened once).  As installing version
  // 2 of everything is a solution, rejecting the third versions
  // should be OK.
  void testRejections()
  {
    dummy_universe_ref u = parseUniverse(dummy_universe_1);
    dummy_resolver r(10, -300, -100, 100000, 0, 50000, u);

    r.reject_version(u.find_package("a").version_from_name("v3"));
    r.reject_version(u.find_package("b").version_from_name("v3"));
    r.reject_version(u.find_package("c").version_from_name("v3"));

    try
      {
	r.find_next_solution(1000000);
      }
    catch(NoMoreSolutions)
      {
	CPPUNIT_FAIL("Expected at least one solution, got none.");
      }
  }

  // Test that the resolver ignores already-generated solutions when
  // generating successors.  Also tests that the resolver generates
  // solutions in the expected order in a simple situation.
  void testDropSolutionSupersets()
  {
    dummy_universe_ref u = parseUniverse(dummy_universe_2);
    dummy_resolver r(10, -300, -100, 100000, 0, 50000, u);

    dummy_universe::package a = u.find_package("a");
    dummy_universe::version av1 = a.version_from_name("v1");
    dummy_universe::version av2 = a.version_from_name("v2");

    r.add_version_score(av2, 100);

    dummy_solution s;

    try
      {
	s = r.find_next_solution(100);
      }
    catch(NoMoreSolutions)
      {
	CPPUNIT_FAIL("Expected at least one solution, got none.");
      }

    CPPUNIT_ASSERT(s.version_of(a) == av2);
    CPPUNIT_ASSERT_EQUAL(s.get_actions().size(), 1U);

    try
      {
	s = r.find_next_solution(100);
      }
    catch(NoMoreSolutions)
      {
	CPPUNIT_FAIL("Expected at least two solutions, got only one.");
      }

    // Note that the only solutions to this problem are (a) install
    // a:v2 and do anything else, or (b) leave a:v1 installed and
    // install b:v2 and c:v2.  The search algorithm will "see" the
    // option of installing a:v2 to resolve the second dep of a:v1
    // after seeing the solution <a:=v2>.  If the bug we're testing
    // for is present, it'll try to install a:v2 again; otherwise
    // it'll reject that solution as containing a conflict.
    CPPUNIT_ASSERT(s.version_of(a) != av2);
  }
};

CPPUNIT_TEST_SUITE_REGISTRATION(ResolverTest);
