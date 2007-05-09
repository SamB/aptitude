// test.cc
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
//   General Public License for more details.  You should have
//   received a copy of the GNU General Public License along with this
//   program; see the file COPYING.  If not, write to the Free
//   Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
//   MA 02111-1307, USA.
//
// Demonstrates the use of the problem-resolver and (eventually) runs
// tests on it.  The convoluted adaptor classes are the way they are
// in order to keep the APT end of things reasonably thin and
// efficient.

#include "dummy_universe.h"
#include "problemresolver.h"
#include "sanity_check_universe.h"

#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <vector>

#include <generic/util/eassert.h>

using namespace std;

// To make things easier, the tests are specified as plaintext files.
// The syntax is quite simple: it consists of whitespace-separated
// words, of the form:
//
// SCRIPT ::= "UNIVERSE" "[" UNIVERSE "]" TEST ...
// UNIVERSE ::= (PACKAGE | DEP) ...
// PACKAGE ::= "PACKAGE" pkgname "<" vername1 ... ">" currentver
// DEP ::= "DEP" pkgname1 vername1 "->" "<" pkgname2 vername2 ... ">"
//       | "SOFTDEP" pkgname1 vername1 "->" "<" pkgname2 vername2 ... ">"
//       | "DEP" pkgname1 vername1 "!!" "<" pkgname2 vername2 ... ">"
//
// TEST ::= "TEST" step_score broken_score "{" SCORE ... "}" "EXPECT" "(" SOLN ... ")"
// SCORE ::= "SCORE" pkgname "<" vername1 score1 ... ">"
// SOLN ::= step_count "ANY"
//       |  step_count "<" pkgname1 vername1 ... ">"
//
// The second DEP form is a Conflicts, and is implicitly converted to
// dependency form internally.

/** Reads the list of scores into the resolver. */
void read_scores(istream &f,
		 dummy_universe_ref universe, dummy_resolver &resolver)
{
  if(!universe)
    throw ParseError("Internal error: NULL universe in read_scores");

  f >> ws;

  while(f)
    {
      string s;

      f >> s >> ws;

      if(s == "}")
	return;
      else if(s == "SCORE")
	{
	  if(f.eof())
	    throw ParseError("Expected package name following SCORE, got "+s);

	  string pkgname;

	  f >> pkgname >> ws;

	  dummy_universe::package pkg=universe.find_package(pkgname);

	  if(f.eof())
	    throw ParseError("Expected '<' following package name, got EOF");

	  f >> s >> ws;

	  if(s != "<")
	    throw ParseError("Expected '<' following package name, got "+s);

	  if(f.eof())
	    throw ParseError("Expected '>' or version name, got EOF");

	  while(f)
	    {
	      string vername;
	      int score;

	      f >> vername >> ws;

	      if(vername == ">")
		break;

	      dummy_universe::version ver=pkg.version_from_name(vername);

	      if(f.eof())
		throw ParseError("Expected score, got EOF");

	      f >> score >> ws;

	      if(f.eof())
		throw ParseError("Expected '>' or version name, got EOF");

	      if(!f)
		throw ParseError("Error reading score of " + pkgname + " " + vername);

	      resolver.set_version_score(ver, score);
	    }

	  if(f.eof())
	    throw ParseError("Expected '}' or SCORE, got EOF");
	}
      else
	throw ParseError("Expected 'SCORE' or '}', got "+s);
    }

  throw ParseError("Unexpected error reading score list.");
}

/** Reads the tail of a non-ANY SOLN form. */
map<dummy_universe::package, dummy_resolver::version> read_solution(istream &f, dummy_universe_ref universe)
{
  if(!universe)
    throw ParseError("Internal error: NULL universe passed to read_solution");

  map<dummy_universe::package, dummy_resolver::version> rval;

  f >> ws;

  while(f)
    {
      string s;

      if(f.eof())
	throw ParseError("Expected '>' or package, got EOF");

      f >> s >> ws;

      if(s == ">")
	return rval;
      else
	{
	  if(f.eof())
	    throw ParseError("Expected version, got EOF");

	  dummy_universe::package pkg=universe.find_package(s);

	  f >> s >> ws;

	  if(f.eof())
	    throw ParseError("Expected '>' or package name, got EOF");

	  if(s == ">")
	    throw ParseError("Expected version name, got '>'");

	  dummy_universe::version ver=pkg.version_from_name(s);

	  if(rval.find(pkg) != rval.end())
	    throw ParseError("Package "+pkg.get_name()+" bound twice in solution");

	  rval[pkg]=ver;
	}
    }

  throw ParseError("Unexpected error reading solution list");
}

void run_test_file(istream &f, bool show_world)
{
  dummy_universe_ref universe=NULL;

  f >> ws;

  while(f)
    {
      string s;

      if(f.eof())
	// This is the only place where EOF is valid.
	return;

      f >> s >> ws;

      if(s == "UNIVERSE")
	{
	  if(f.eof())
	    throw ParseError("Expected '[' following UNIVERSE, got EOF.");

	  f >> s >> ws;

	  if(s != "[")
	    throw ParseError("Expected '[' following UNIVERSE, got " + s);

	  universe=parse_universe_tail(f);

	  sanity_check_universe(universe);

	  if(show_world)
	    {
	      cout << "Input universe:" << endl;
	      dump_universe(universe, cout);
	    }
	}
      else if(s == "TEST")
	{
	  if(!universe)
	    throw ParseError("Expected UNIVERSE before TEST");

	  if(f.eof())
	    throw ParseError("Expected step_score and broken_score following 'TEST', got EOF");

	  int step_score;
	  int broken_score;
	  int unfixed_soft_score;
	  int infinity;
	  int max_successors;
	  int goal_score;

	  f >> step_score >> broken_score >> unfixed_soft_score >> infinity >> max_successors >> goal_score;

	  if(f.eof())
	    throw ParseError("Expected '{' following broken_score, got EOF");

	  if(!f)
	    throw ParseError("Error reading step_score, broken_score, unfixed_soft_score, infinity, max_succ, and goal_score after 'TEST'");

	  f >> s >> ws;

	  if(s != "{")
	    throw ParseError("Expected '{' following TEST, got "+s);

	  dummy_resolver resolver(step_score, broken_score,
				  unfixed_soft_score,
				  infinity, max_successors,
				  goal_score, universe);

	  resolver.set_debug(true);

	  read_scores(f, universe, resolver);

	  if(f.eof())
	    throw ParseError("Expected 'EXPECT', got EOF");

	  f >> s >> ws;

	  if(s != "EXPECT")
	    throw ParseError("Expected 'EXPECT', got "+s);

	  if(f.eof())
	    throw ParseError("Expected '(' following EXPECT, got EOF");

	  f >> s >> ws;

	  if(s != "(")
	    throw ParseError("Expected '(' following EXPECT, got "+s);

	  while(f)
	    {
	      if(f.eof())
		throw ParseError("Expected ')' or package name, got EOF");

	      f >> s >> ws;

	      if(s == ")")
		break;

	      int step_count=atoi(s.c_str());

	      if(step_count<=0)
		throw ParseError("step_count must be a positive integer, not "+s);

	      f >> s >> ws;

	      if(s == "ANY")
		{
		  try
		    {
		      dummy_resolver::solution next_soln=resolver.find_next_solution(step_count);

		      cout << "Next solution is ";
		      next_soln.dump(cout);

		      cout << " (ignored)" << endl;
		    }
		  catch(NoMoreTime)
		    {
		      cout << "Ran out of steps (ignored)" << endl;
		    }
		  catch(NoMoreSolutions)
		    {
		      cout << "Ran out of solutions (ignored)" << endl;
		    }
		}
	      else if(s == "<")
		{
		  try
		    {
		      map<dummy_universe::package, dummy_resolver::version> expected=read_solution(f, universe);

		      dummy_resolver::solution next_soln=resolver.find_next_solution(step_count);


		      cout << "Next solution is ";
		      next_soln.dump(cout);

		      bool equal=true;

		      std::map<dummy_universe::package, dummy_universe::version>::const_iterator expect_iter=expected.begin();
		      imm::map<dummy_universe::package, dummy_resolver::action>::const_iterator soln_iter=next_soln.get_actions().begin();

		      while(equal &&
			    expect_iter != expected.end() &&
			    soln_iter != next_soln.get_actions().end())
			{
			  if(expect_iter->first != soln_iter->first ||
			     expect_iter->second != soln_iter->second.ver)
			    equal=false;
			}

		      if(equal)
			cout << " (OK)" << endl;
		      else
			{
			  cout << " (FAILED)" << endl;
			  cout << "Expected <";
			  for(map<dummy_universe::package, dummy_universe::version>::const_iterator i=expected.begin();
			      i!=expected.end(); ++i)
			    cout << i->first.get_name()
				 << ":="
				 << i->second.get_name();
			  cout << ">" << endl;
			}
		    }
		  catch(NoMoreSolutions)
		    {
		      cout << "Ran out of solutions (FAILED)" << endl;
		    }
		  catch(NoMoreTime)
		    {
		      cout << "Ran out of time (FAILED)" << endl;
		    }
		}
	      else
		throw ParseError("Expected ANY or '<', got "+s);
	    }
	}
      else
	throw ParseError("Expected UNIVERSE or TEST, got "+s);
    }
}

int main(int argc, char **argv)
{
  int rval=0;
  bool show_world=false;

  for(int i=1; i<argc; ++i)
    {
      // lame man's command line
      if(!strcmp(argv[0], "--dump"))
	show_world=true;

      ifstream f(argv[i]);

      if(!f)
	{
	  cerr << "Couldn't read from file " << argv[i] << "." << endl;
	  rval=-1;
	}

      try
	{
	  f >> ws;
	  run_test_file(f, show_world);
	}
      catch(const Exception &e)
	{
	  cerr << "Error reading " << argv[i] << ": " << e.errmsg() << endl;
	  rval=-1;
	}
    }

  return rval;
}

