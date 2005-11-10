// exceptions.h                                     -*-c++-*-
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

#ifndef EXCEPTIONS_H
#define EXCEPTIONS_H

#include <generic/util/exception.h>

/** Generic errors in the problem resolver. */
class ProblemResolverError : public Exception {
};

/** An exception indicating that no more solutions are available. */
class NoMoreSolutions : public ProblemResolverError {
  std::string errmsg() const
  {
    return "No more solutions to this dependency problem.";
  }
};

/** An exception indicating that the resolver ran out of time to
 *  find a solution.
 */
class NoMoreTime:public ProblemResolverError {
  std::string errmsg() const
  {
    return "No more time to solve this dependency problem.";
  }
};

/** An exception indicating that the resolver was interrupted from
 *  another thread.
 */
class InterruptedException : public ProblemResolverError {
  std::string errmsg() const
  {
    return "Dependency solution was interrupted.";
  }
};

/** An exception indicating that an internal error occurred. */
class ResolverInternalErrorException : public ProblemResolverError {
  std::string msg;
public:
  ResolverInternalErrorException(const std::string &_msg)
    : msg(_msg)
  {
  }

  std::string errmsg() const
  {
    return msg;
  }
};

/** An exception indicating that two threads tried to run the resolver
 *  at once.
 */
class DoubleRunException : public ProblemResolverError {
  std::string errmsg() const
  {
    return "Internal error: Multiple threads of execution tried to enter the resolver at once.";
  }
};

#endif // EXCEPTIONS_H
