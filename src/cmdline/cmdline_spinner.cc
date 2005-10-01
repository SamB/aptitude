// cmdline_spinner.cc                               -*-c++-*-
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

#include "cmdline_spinner.h"

#include "cmdline_common.h"

#include <iostream>

cmdline_spinner::cmdline_spinner(int _quiet_level)
  : count(0), quiet_level(_quiet_level)
{
}

char cmdline_spinner::spin_char() const
{
  switch(count % 4)
    {
    case 0:
      return '.';
    case 1:
      return 'o';
    case 2:
      return 'O';
    case 3:
      return 'o';
    default:
      return '?';
    }
}

void cmdline_spinner::display() const
{
  if(quiet_level > 0)
    return;

  update_screen_width();

  // Build the string to output.
  std::string out(msg, 0, screen_width - 2);

  out.append(screen_width - 1 - out.size(), ' ');
  if(out.size() < screen_width)
    out += spin_char();

  std::cout << '\r' << out << std::flush;
}
