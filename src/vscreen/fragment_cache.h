// fragment_cache.h                           -*-c++-*-
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
// A special fragment class that caches its contents.

#ifndef FRAGMENT_CACHE_H
#define FRAGMENT_CACHE_H

#include "fragment.h"

/** A fragment that caches its contents; a cached result is used if
 *  the same width is passed to the layout routine twice in a row.
 *  Obviously this should only be done if you know that the contents
 *  are static.
 */
class fragment_cache:public fragment
{
  fragment *contents;

  /** The last cached result. */
  mutable fragment_contents cached_lines;

  /** If cached_lines is valid, it was formatted in this style: */
  mutable style cached_lines_style;

  /** If cached_lines is valid, cached_lines was formatted for these
   *  widths.
   */
  mutable size_t cached_lines_first_width, cached_lines_rest_width;

  /** The cached max_width value. */
  mutable size_t cached_max_width;

  /** For what indents is cached_max_width valid? */
  mutable size_t cached_max_width_first_indent, cached_max_width_rest_indent;

  /** The cached trailing_width value. */
  mutable size_t cached_trailing_width;

  /** For what indents is cached_trailing_width valid? */
  mutable size_t cached_trailing_width_first_indent, cached_trailing_width_rest_indent;

  /** The cached final_newline value. */
  mutable bool cached_final_nl:1;

  /** If \b true, the corresponding property is valid. */
  mutable bool cached_lines_valid:1, cached_max_width_valid:1;
  /** If \b true, the corresponding property is valid. */
  mutable bool cached_trailing_width_valid:1, cached_final_nl_valid:1;
public:
  fragment_cache(fragment *_contents);
  ~fragment_cache();

  void invalidate();

  fragment_contents layout(size_t firstw, size_t restw,
			   const style &st);

  void set_attr(int attr);

  size_t max_width(size_t first_indent, size_t rest_indent) const;
  size_t trailing_width(size_t first_indent, size_t rest_indent) const;

  bool final_newline() const;
};


#endif
