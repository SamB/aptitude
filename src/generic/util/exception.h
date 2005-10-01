// exception.h                     -*-c++-*-
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
// A generic exception class supporting std::string error messages
// (unlike std::exception, which only supports const char* error
// messages).  If execinfo is available and ENABLE_DYNAMIC_BACKTRACE
// is defined, it also generates information about the state of the
// stack at the time of its instantiation (if not, then
// get_backtrace() returns an empty string).

#ifndef EXCEPTION_H
#define EXCEPTION_H

#include <string>
#include <vector>

class Exception
{
  /** A textual listing of the symbols on the stack at the time that
   *  the exception is created.
   */
  std::string bt;
public:
  Exception();

  std::string get_backtrace() const { return bt; }
  virtual std::string errmsg() const = 0;
  virtual ~Exception() {}
};

#endif
