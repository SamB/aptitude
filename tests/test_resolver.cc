// test_resolver.cc                       -*-c++-*-
//
//   Copyright (C) 2005, 2007-2009 Daniel Burrows
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
  do {									\
    std::stringstream ____x1_ss, ____x2_ss;				\
    ____x1_ss << (x1); ____x2_ss << (x2);				\
    std::string ____x1_s(____x1_ss.str()), ____x2_s(____x2_ss.str());	\
    CPPUNIT_ASSERT_MESSAGE(____x1_s + " does not equal " + ____x2_s,	\
			   (x1) == (x2));				\
    CPPUNIT_ASSERT_MESSAGE(____x2_s + " does not equal " + ____x1_s,	\
			   (x2) == (x1));				\
    CPPUNIT_ASSERT_MESSAGE(____x1_s + " is inequal to " + ____x2_s,	\
			   !((x1) != (x2)));				\
    CPPUNIT_ASSERT_MESSAGE(____x2_s + " is inequal to " + ____x1_s,	\
			   !((x2) != (x1)));				\
    CPPUNIT_ASSERT_MESSAGE(____x1_s + " is less than " + ____x2_s,	\
			   !((x1) < (x2)));				\
    CPPUNIT_ASSERT_MESSAGE(____x2_s + " is less than " + ____x1_s,	\
			   !((x2) < (x1)));				\
  } while(0)

#define assertEqInequivalent(x1, x2) \
  do {									\
    std::stringstream ____x1_ss, ____x2_ss;				\
    ____x1_ss << (x1); ____x2_ss << (x2);				\
    std::string ____x1_s(____x1_ss.str()), ____x2_s(____x2_ss.str());	\
    CPPUNIT_ASSERT_MESSAGE(____x1_s + " is not inequal to " + ____x2_s,		\
			   (x1) != (x2));				\
    CPPUNIT_ASSERT_MESSAGE(____x2_s + " is not inequal to " + ____x1_s,		\
			   (x2) != (x1));				\
    CPPUNIT_ASSERT_MESSAGE(____x1_s + " equals " + ____x2_s,	\
			   !((x1) == (x2)));				\
    CPPUNIT_ASSERT_MESSAGE(____x2_s + " equals " + ____x1_s,	\
			   !((x2) == (x1)));				\
    CPPUNIT_ASSERT_MESSAGE(____x1_s + " is unrelated to " + ____x2_s + " under <", \
			   (x1) < (x2) || (x2) < (x1));			\
    CPPUNIT_ASSERT_MESSAGE(____x1_s + " is both less than and greater than " + ____x2_s, \
			   !((x1) < (x2) && (x2) < (x1)));		\
  } while(0)

#define assertLtEquivalent(x1, x2, lt)					\
  do {									\
    std::stringstream ____x1_ss, ____x2_ss;				\
    ____x1_ss << (x1); ____x2_ss << (x2);				\
    std::string ____x1_s(____x1_ss.str()), ____x2_s(____x2_ss.str());	\
    CPPUNIT_ASSERT_MESSAGE(____x1_s + " is less than " + ____x2_s,	\
			   !(lt((x1), (x2))));				\
    CPPUNIT_ASSERT_MESSAGE(____x2_s + " is less than " + ____x1_s,	\
			   !(lt((x2), (x1))));				\
  } while(0)

#define assertLtInequivalent(x1, x2, lt) \
  do {									\
    std::stringstream ____x1_ss, ____x2_ss;				\
    ____x1_ss << (x1); ____x2_ss << (x2);				\
    std::string ____x1_s(____x1_ss.str()), ____x2_s(____x2_ss.str());	\
    CPPUNIT_ASSERT_MESSAGE(____x1_s + " is not related under < to " + ____x2_s, \
			   lt((x1), (x2)) || lt((x2), (x1)));		\
    CPPUNIT_ASSERT_MESSAGE(____x1_s + " is both less than and greater than " + ____x2_s, \
			   !(lt((x1), (x2)) && lt((x2), (x1))));	\
  } while(0)

class ResolverTest : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(ResolverTest);

  CPPUNIT_TEST(testRejections);
  CPPUNIT_TEST(testTiers);
  CPPUNIT_TEST(testInitialState);
  CPPUNIT_TEST(testJointScores);
  CPPUNIT_TEST(testDropSolutionSupersets);

  CPPUNIT_TEST_SUITE_END();

private:
  typedef dummy_universe_ref::package package;
  typedef dummy_universe_ref::version version;
  typedef dummy_universe_ref::dep dep;
  typedef dummy_universe_ref::tier tier;

  typedef generic_solution<dummy_universe_ref> solution;
  typedef generic_choice_set<dummy_universe_ref> choice_set;
  typedef generic_choice<dummy_universe_ref> choice;

  static dummy_universe_ref parseUniverse(const std::string &s)
  {
    std::istringstream in(s);

    return parse_universe(in);
  }

  // Check that the first set is a (perhaps) more general version of
  // the second, but that there are no missing items.
  static void assertSameEffect(const choice_set &s1, const choice_set &s2)
  {
    std::ostringstream tmp1, tmp2;
    tmp1 << s1;
    tmp2 << s2;
    std::string str1(tmp1.str()), str2(tmp2.str());

    CPPUNIT_ASSERT_EQUAL_MESSAGE("The sets " + str1 + " and " + str2 + " are not the same size.",
				 s1.size(), s2.size());

    CPPUNIT_ASSERT_MESSAGE("The set " + str1 + " does not contain " + str2 + ".",
			   s1.contains(s2));
  }

#if 0
  // Old routines that need to be adapted to the new resolver code.

  /** Generate a successor solution that just contains the given
   *  information by using internal constructors of the solution
   *  class.  Used when testing the solution class itself.
   */
  template<class c_iter>
  dummy_solution unsafe_successor(const dummy_solution &parent,
				  c_iter cbegin, c_iter cend)
  {
    choice_set choices(parent.get_choices());

    for(c_iter ci = cbegin; ci != cend; ++ci)
      choices.insert_or_narrow(*ci);

    return dummy_solution(new dummy_solution::solution_rep(choices,
							   parent.get_broken(),
							   parent.get_forbidden_versions(),
							   parent.get_initial_state(),
							   parent.get_score(),
							   parent.get_action_score(),
							   parent.get_tier()));
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

    resolver_initial_state<dummy_universe_ref> initial_state(imm::map<dummy_universe::package, dummy_universe::version>(), u.get_package_count());

    solution_weights<dummy_universe_ref> weights(0, 0, 0, 0, u.get_version_count(),
						 initial_state);

    imm::set<dummy_universe::dep> u_broken;
    for(dummy_universe::broken_dep_iterator bi = u.broken_begin();
	!bi.end(); ++bi)
      u_broken.insert(*bi);

    choice c1(choice::make_install_version(u.find_package("a").version_from_name("v1"),
					   false, d1, 49));
    choice c2(choice::make_install_version(u.find_package("a").version_from_name("v1"),
					   false, d2, 21));
    choice c3(choice::make_install_version(u.find_package("a").version_from_name("v2"),
					   false, d1, 49));

    choice c4(choice::make_install_version(u.find_package("c").version_from_name("v3"),
					   false, d2, 21));

    choice c5(choice::make_break_soft_dep(d1, 443));


    // Generate some meaningless solutions to check that equivalency
    // is correctly calculated according to the version mappings and
    // the set of unsolved soft deps.
    dummy_solution s0 = dummy_solution::root_node(u_broken,
						  u, weights, initial_state,
						  tier(0));
    dummy_solution s1
      = unsafe_successor(s0, &c1, &c1 + 1);
    dummy_solution s2
      = unsafe_successor(s0, &c2, &c2 + 1);
    dummy_solution s3
      = unsafe_successor(s0, &c3, &c3 + 1);
    dummy_solution s4
      = unsafe_successor(s0, &c4, &c4 + 1);

    // the following two should be equal.
    dummy_solution s5
      = unsafe_successor(s1, &c4, &c4 + 1);
    dummy_solution s6
      = unsafe_successor(s4, &c1, &c1 + 1);

    // and this should not equal any other solution.
    dummy_solution s7
      = unsafe_successor(s0, &c5, &c5+1);

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
    assertLtInequivalent(s1, s2, solcmp);
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
#endif

  // Check that rejections of versions don't block out all versions of
  // the package (this actually happened once).  As installing version
  // 2 of everything is a solution, rejecting the third versions
  // should be OK.
  void testRejections()
  {
    dummy_universe_ref u = parseUniverse(dummy_universe_1);
    dummy_resolver r(10, -300, -100, 100000, 50000, 50,
		     imm::map<dummy_universe::package, dummy_universe::version>(),
		     u);

    r.reject_version(u.find_package("a").version_from_name("v3"));
    r.reject_version(u.find_package("b").version_from_name("v3"));
    r.reject_version(u.find_package("c").version_from_name("v3"));

    try
      {
	r.find_next_solution(1000000, NULL);
      }
    catch(NoMoreSolutions)
      {
	CPPUNIT_FAIL("Expected at least one solution, got none.");
      }
  }

  void testTiers()
  {
    log4cxx::LoggerPtr logger(log4cxx::Logger::getLogger("test.resolver.testTiers"));
    LOG_TRACE(logger, "Entering testTiers");

    dummy_universe_ref u = parseUniverse(dummy_universe_2);

    package a = u.find_package("a");
    package b = u.find_package("b");
    package c = u.find_package("c");
    version av1 = a.version_from_name("v1");
    version av2 = a.version_from_name("v2");
    version bv1 = b.version_from_name("v1");
    version bv2 = b.version_from_name("v2");
    version cv1 = c.version_from_name("v1");
    version cv2 = c.version_from_name("v2");

    choice_set av1_choices;
    av1_choices.insert_or_narrow(choice::make_install_version(bv2, 0));
    av1_choices.insert_or_narrow(choice::make_install_version(cv2, 0));

    choice_set av2_choices;
    av2_choices.insert_or_narrow(choice::make_install_version(av2, 0));

    LOG_TRACE(logger, "Verifying that without a tier the shortest solution is produced first and there are two solutions.");

    // Verify that without a tier we get the shorter solution first.
    // Without this we aren't testing anything!
    {
      dummy_resolver r(10, -300, -100, 100000, 50000, 50,
		       imm::map<dummy_universe::package, dummy_universe::version>(),
		       u);
      r.set_version_score(av2, 10000);
      r.set_version_score(bv2, -100);
      r.set_version_score(cv2, -100);

      solution sol;
      try
	{
	  sol = r.find_next_solution(1000000, NULL);
	}
      catch(NoMoreSolutions)
	{
	  CPPUNIT_FAIL("Expected two solutions, got none.");
	}

      LOG_TRACE(logger, "Got first solution: " << sol);

      assertSameEffect(av2_choices, sol.get_choices());

      try
	{
	  sol = r.find_next_solution(1000000, NULL);
	}
      catch(NoMoreSolutions)
	{
	  CPPUNIT_FAIL("Expected two solutions, got only one.");
	}

      LOG_TRACE(logger, "Got second solution: " << sol);

      assertSameEffect(av1_choices, sol.get_choices());

      bool done = false;
      try
	{
	  sol = r.find_next_solution(1000000, NULL);
	}
      catch(NoMoreSolutions)
	{
	  done = true;
	}

      if(!done)
	LOG_ERROR(logger, "Got unexpected third solution: " << sol);

      CPPUNIT_ASSERT_MESSAGE("Expected two solutions, got more.", done);
    }

    LOG_TRACE(logger, "Checking that adjusting tiers changes the output.");

    // Now check that adjusting tiers changes the output.
    {
      dummy_resolver r(10, -300, -100, 100000, 50000, 50,
		       imm::map<dummy_universe::package, dummy_universe::version>(),
		       u);
      r.set_version_score(av2, 1000);
      r.set_version_score(bv2, -100);
      r.set_version_score(cv2, -100);
      r.set_version_min_tier(av2, tier(100));

      solution sol;
      try
	{
	  sol = r.find_next_solution(1000000, NULL);
	}
      catch(NoMoreSolutions)
	{
	  CPPUNIT_FAIL("Expected two solutions, got none.");
	}

      LOG_TRACE(logger, "Got first solution: " << sol);

      assertSameEffect(av1_choices, sol.get_choices());

      try
	{
	  sol = r.find_next_solution(1000000, NULL);
	}
      catch(NoMoreSolutions)
	{
	  CPPUNIT_FAIL("Expected two solutions, got only one.");
	}

      LOG_TRACE(logger, "Got second solution: " << sol);

      assertSameEffect(av2_choices, sol.get_choices());

      bool done = false;
      try
	{
	  sol = r.find_next_solution(1000000, NULL);
	}
      catch(NoMoreSolutions)
	{
	  done = true;
	}

      if(!done)
	LOG_ERROR(logger, "Got an unexpected third solution: " << sol);

      CPPUNIT_ASSERT_MESSAGE("Expected two solutions, got more.", done);
    }
  }

  // Check that initial states work.
  void testInitialState()
  {
    log4cxx::LoggerPtr logger(log4cxx::Logger::getLogger("test.resolver.testInitialState"));
    LOG_TRACE(logger, "Entering testInitialState");

    dummy_universe_ref u = parseUniverse(dummy_universe_2);

    package a = u.find_package("a");
    package b = u.find_package("b");
    package c = u.find_package("c");
    version av1 = a.version_from_name("v1");
    version av2 = a.version_from_name("v2");
    version bv1 = b.version_from_name("v1");
    version bv2 = b.version_from_name("v2");
    version cv1 = c.version_from_name("v1");
    version cv2 = c.version_from_name("v2");


    imm::map<package, version> initial_state;
    initial_state.put(a, av2);
    initial_state.put(c, cv2);

    dummy_resolver r(10, -300, -100, 10000000, 500, 1,
		     initial_state,
		     u);

    CPPUNIT_ASSERT_EQUAL(r.get_initial_state().version_of(a), av2);
    CPPUNIT_ASSERT_EQUAL(r.get_initial_state().version_of(c), cv2);

    // I use an immset here because it has a decent operator<< for
    // printing error messages, and to smooth out differences in order
    // (which shouldn't happen in this implementation, but shouldn't
    // be wrong either).
    imm::set<version> expected_initial_state;
    expected_initial_state.insert(av2);
    expected_initial_state.insert(cv2);

    {
      std::set<version> initial_state;
      r.get_initial_state().get_initial_versions(initial_state);
      imm::set<version> initial_state_set;
      for(std::set<version>::const_iterator it = initial_state.begin();
	  it != initial_state.end(); ++it)
	initial_state_set.insert(*it);
      CPPUNIT_ASSERT_EQUAL(initial_state_set,
			   expected_initial_state);
    }

    try
      {
	LOG_TRACE(logger, "Checking that the empty solution is the first solution.");
	dummy_solution sol = r.find_next_solution(1000000, NULL);

	CPPUNIT_ASSERT_MESSAGE("There are no broken deps, so only the empty solution should be returned.",
			       sol.get_choices().size() == 0);

	{
	  std::set<version> initial_state;
	  sol.get_initial_state().get_initial_versions(initial_state);
	  imm::set<version> initial_state_set;
	  for(std::set<version>::const_iterator it = initial_state.begin();
	      it != initial_state.end(); ++it)
	    initial_state_set.insert(*it);
	  CPPUNIT_ASSERT_EQUAL(initial_state_set,
			       expected_initial_state);
	}

	LOG_TRACE(logger, "Checking that the empty solution is the only solution.");
	bool out_of_solutions = false;
	try
	  {
	    sol = r.find_next_solution(1000000, NULL);
	  }
	catch(NoMoreSolutions&)
	  {
	    out_of_solutions = true;
	  }
	CPPUNIT_ASSERT_MESSAGE("There should be only one solution.",
			       out_of_solutions);
      }
    catch(NoMoreTime&)
      {
	CPPUNIT_FAIL("Unable to solve the solution in the ridiculous amount of time I allocated.");
      }
    catch(NoMoreSolutions&)
      {
	CPPUNIT_FAIL("The empty solution should be returned.");
      }
  }

  // Check that joint scores work.
  void testJointScores()
  {
    dummy_universe_ref u = parseUniverse(dummy_universe_2);
    dummy_resolver r(10, -300, -100, 10000000, 500, 500,
		     imm::map<package, version>(),
		     u);
    // Disable this to debug the resolver test.
    //r.set_debug(true);
    // Score the combination of b, v1 and a, v2 highly.
    package a = u.find_package("a");
    package b = u.find_package("b");
    package c = u.find_package("c");
    version av2 = a.version_from_name("v2");
    version bv1 = b.version_from_name("v1");
    version bv2 = b.version_from_name("v2");
    version cv2 = c.version_from_name("v2");

    imm::set<version> choice_pair, choice_pair2;
    choice_pair.insert(av2);
    choice_pair.insert(bv1);
    choice_pair2.insert(bv2);
    choice_pair2.insert(cv2);

    const int test_score = 100000;
    const int test_score2 = -200000;
    r.add_joint_score(choice_pair, test_score);
    r.add_joint_score(choice_pair2, test_score2);

    bool saw_one_positive = false;
    bool saw_one_negative = false;
    bool saw_one_positive2 = false;
    while(1)
      {
	try
	  {
	    dummy_solution sol = r.find_next_solution(1000000, NULL);

	    if(sol.version_of(a) == av2 &&
	       sol.version_of(b) == bv1)
	      {
		saw_one_positive = true;
		// Check that we *don't* apply the joint score in
		// this case, since b starts out at v1.
		CPPUNIT_ASSERT_EQUAL((int)(sol.get_choices().size()) * r.get_step_score() + r.get_full_solution_score(),
				     sol.get_score());
	      }
	    else if(sol.version_of(b) == bv2 &&
		    sol.version_of(c) == cv2)
	      {
		saw_one_positive2 = true;
		saw_one_negative = true;
		CPPUNIT_ASSERT_EQUAL(test_score2 + ((int)sol.get_choices().size()) * r.get_step_score() + r.get_full_solution_score(),
				     sol.get_score());
	      }
	    else
	      {
		saw_one_negative = true;
		CPPUNIT_ASSERT_EQUAL((int)(sol.get_choices().size() * r.get_step_score()) + r.get_full_solution_score(),
				     sol.get_score());
	      }
	  }
	catch(NoMoreTime&)
	  {
	    CPPUNIT_FAIL("Unable to solve the solution in the ridiculous amount of time I allocated.");
	  }
	catch(NoMoreSolutions&)
	  {
	    if(!saw_one_positive)
	      CPPUNIT_FAIL("Expected at least one solution containing a, version 2 and b, version 1");
	    else if(!saw_one_negative)
	      CPPUNIT_FAIL("Expected at least one solution containing a, version 1 or b, version 2");
	    else if(!saw_one_positive2)
	      CPPUNIT_FAIL("Expected at least one solution containing b, version 2 and c, version 2");

	    break;
	  }
      }
  }

  // Test that the resolver ignores already-generated solutions when
  // generating successors.  Also tests that the resolver generates
  // solutions in the expected order in a simple situation.
  void testDropSolutionSupersets()
  {
    dummy_universe_ref u = parseUniverse(dummy_universe_2);
    dummy_resolver r(10, -300, -100, 100000, 50000, 500000,
		     imm::map<dummy_universe::package, dummy_universe::version>(),
		     u);

    dummy_universe::package a = u.find_package("a");
    dummy_universe::version av1 = a.version_from_name("v1");
    dummy_universe::version av2 = a.version_from_name("v2");

    r.add_version_score(av2, 100);

    dummy_solution s;

    try
      {
	s = r.find_next_solution(100, NULL);
      }
    catch(NoMoreSolutions)
      {
	CPPUNIT_FAIL("Expected at least one solution, got none.");
      }

    CPPUNIT_ASSERT(s.version_of(a) == av2);
    CPPUNIT_ASSERT_EQUAL(s.get_choices().size(), 1U);

    try
      {
	s = r.find_next_solution(100, NULL);
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

    try
      {
	s = r.find_next_solution(100, NULL);
	CPPUNIT_FAIL("Expected exactly two solutions, got more than two.");
      }
    catch(NoMoreSolutions)
      {
      }
  }
};

CPPUNIT_TEST_SUITE_REGISTRATION(ResolverTest);
