// fragment_cache.cc
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

#include "fragment_cache.h"

fragment_cache::fragment_cache(fragment *_contents)
  :contents(_contents), cached_lines_valid(false),
   cached_max_width_valid(false), cached_trailing_width_valid(false),
   cached_final_nl_valid(false)
{
}

fragment_cache::~fragment_cache()
{
  delete contents;
}

void fragment_cache::invalidate()
{
  cached_lines_valid=cached_max_width_valid=false;
  cached_trailing_width_valid=cached_final_nl=false;
}

fragment_contents fragment_cache::layout(size_t firstw, size_t restw,
					 const style &st)
{
  if(!cached_lines_valid ||
     cached_lines_first_width != firstw ||
     cached_lines_rest_width != restw ||
     cached_lines_style != st)
    {
      cached_lines=contents->layout(firstw, restw, st);
      cached_lines_first_width=firstw;
      cached_lines_rest_width=restw;
      cached_lines_style=st;
      cached_lines_valid=true;
    }

  return cached_lines;
}

size_t fragment_cache::max_width(size_t first_indent, size_t rest_indent) const
{
  if(!cached_max_width_valid ||
     first_indent != cached_max_width_first_indent ||
     rest_indent != cached_max_width_rest_indent)
    {
      cached_max_width=contents->max_width(first_indent, rest_indent);
      cached_max_width_first_indent=first_indent;
      cached_max_width_rest_indent=rest_indent;
      cached_max_width_valid=true;
    }

  return cached_max_width;
}

size_t fragment_cache::trailing_width(size_t first_indent, size_t rest_indent) const
{
  if(!cached_trailing_width_valid ||
     first_indent != cached_trailing_width_first_indent ||
     rest_indent != cached_trailing_width_rest_indent)
    {
      cached_trailing_width=contents->trailing_width(first_indent, rest_indent);
      cached_trailing_width_first_indent=first_indent;
      cached_trailing_width_rest_indent=rest_indent;
      cached_trailing_width_valid=true;
    }

  return cached_trailing_width;
}

bool fragment_cache::final_newline() const
{
  if(!cached_final_nl_valid)
    {
      cached_final_nl=contents->final_newline();
      cached_final_nl_valid=true;
    }

  return cached_final_nl;
}
