// changelog_parse.h                        -*-c++-*-
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

#ifndef CHANGELOG_PARSE_H
#define CHANGELOG_PARSE_H

#include <apt-pkg/pkgcache.h>

class fragment;
namespace temp {class name;}

/** Parse the contents of the given file as a Debian changelog.  If
 *  for some reason the file cannot be parsed, returns \b NULL.
 *
 *  \param file a temporary file object containing the changelog.
 *  \param verstr the name of the currently installed version of
 *                this package (empty if there is no installed
 *                version)
 */
fragment *make_changelog_fragment(const temp::name &file,
				  const std::string &curver);

#endif
