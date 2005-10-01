// style.cc
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

#include "style.h"

// Be lazy, use the standard map.
#include <map>

using namespace std;

map<string, style> styles;

const style &get_style(const std::string &name)
{
  static style null_style;
  map<string, style>::const_iterator found=styles.find(name);

  if(found == styles.end())
    return null_style;
  else
    return found->second;
}

void set_style(const std::string &name, const style &style)
{
  styles[name]=style;
}
