// cmdline_spinner.h                             -*-c++-*-
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
//  A generic spinner (for use with the background resolver).

#ifndef CMDLINE_SPINNER_H
#define CMDLINE_SPINNER_H

#include <string>

/** A spinner for command-line output. */
class cmdline_spinner
{
  std::string msg;

  /** The number of spins. */
  int count;

  /** The quietness of this spinner. */
  int quiet_level;
public:
  cmdline_spinner(int _quiet_level);

  /** Display the current spinner state, overwriting the current
   *  terminal line.
   */
  void display() const;

  /** Return a spin character for the given tick count. */
  virtual char spin_char() const;

  /** Advance the spinner one position. */
  void tick()
  {
    ++count;
  }

  /** Set the message displayed to the left of the spinner. */
  void set_msg(const std::string &new_msg)
  {
    msg = new_msg;
  }
};

#endif
