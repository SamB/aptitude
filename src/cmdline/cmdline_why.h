// cmdline_why.h                            -*-c++-*-
//
//   Copyright (C) 2007 Daniel Burrows
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

#ifndef CMDLINE_WHY_H
#define CMDLINE_WHY_H

/** \brief Explain why a package is installed or conflicted against.
 *
 *  aptitude why A1 [A2 ...] B
 *     --> show the shortest strongest justification from (some) A to B.
 *          A and B may NOT have version/archive tags; any A may be a pattern,
 *          but B must not be.
 *  aptitude why-not A1 [A2 ...] B
 *     --> show the shortest strongest justification from (some) A1
 *         to a conflict on B. A and B may NOT have version/archive tags;
 *         any A may be a pattern, but B must not be.
 *  aptitude why B
 *    --> Equivalent to aptitude why ~i!~M B
 *  aptitude why-not B
 *    --> Equivalent to aptitude why-not ~i!~M B
 *
 *  If -v is passed on the command-line, aptitude displays all the
 *  justifications it can find, rather than stopping at the shortest
 *  one.
 *
 *  \return 0 if a justification was found, -1 if an error occurred, and
 *  1 if no justification could be found.
 */
int cmdline_why(int argc, char *argv[],
		const char *status_fname, int verbosity,
		bool why_not);

#endif
