// columnify.h    -*-c++-*-
//
//  Copyright 2000, 2005 Daniel Burrows
//  
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; see the file COPYING.  If not, write to
//  the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//  Boston, MA 02111-1307, USA.
//
//  Support for creating column-formatted strings.  Columns which exceed their
// size limit are truncated.  Columns are collected into 'groups', each of
// which starts at a particular location.  This allows things such as
// right-justification of text.  If one column group overlaps another, the
// conflict is resolved by adjusting the column which is situated farthest to
// the right (by moving it farther to the right).
//
//  Right-justification is now handled (somewhat hackily) by columnify; to
// use it, set the start_off to something negative.  The group will be aligned
// so the the right edge of the column is start_off+1 characters from the
// right-hand screen edge -- so to align something exactly with the right-hand
// edge of the screen, set start_off to -1.

#ifndef COLUMNIFY_H
#define COLUMNIFY_H

#include <list>
#include <string>
#include <generic/util/eassert.h>

struct column_disposition
{
  std::wstring text;  // The contents of the columns
  int minx;          // The minimum x value to start this column at (useful for
                     // indenting stuff in trees)

  column_disposition(const std::wstring &_text, int _minx):text(_text), minx(_minx) {}

  /** Generate a column from the multibyte string _text. */
  column_disposition(const std::string &_text, int _minx,
		     const char *encoding=NULL);
};

struct column
{
  column_disposition info;
  int width;
  bool expand, shrink;

  column(const column_disposition &_info, int _width, bool _expand, bool _shrink)
    :info(_info), width(_width), expand(_expand), shrink(_shrink)
  {
    eassert(_width>=0);
  }
};

typedef std::list<column> column_list;

typedef std::list<column> layout;

/* \return a string formatted as requested.  The string will be no
 * wider than width columns.  I could probably use printf-style
 * strings, but with the groups it would probably turn into a major
 * pain..
 */
std::wstring columnify(const layout &format, int width);

#endif
