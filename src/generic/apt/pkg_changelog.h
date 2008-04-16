// pkg_changelog.h    -*-c++-*-
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


#ifndef PKG_CHANGELOG_H
#define PKG_CHANGELOG_H

#include <string>

#include <generic/util/temp.h>

#include <apt-pkg/pkgcache.h>

#include <sigc++/slot.h>

/** \brief Routines to download a Debian changelog for a given package.
 *
 *  \file pkg_changelog.h
 */

class download_manager;

/** Generate a download process object that retrieves the given
 *  package version's changelog.  When the download is complete, the
 *  given slot will be invoked with the file to which the changelog
 *  was downloaded as an argument.
 */
download_manager *get_changelog(pkgCache::VerIterator ver,
				const sigc::slot1<void, temp::name> &k);

/** Generate a download process object that retrieves a changelog for
 *  the given source package.
 *
 *  \param srcpkg the source package name
 *  \param ver the version of the source package
 *  \param section the section of the source package
 *  \param name the name of the package that the user provided
 *              (e.g., the binary package that the changelog command
 *               was executed on)
 */
download_manager *get_changelog_from_source(const std::string &srcpkg,
					    const std::string &ver,
					    const std::string &section,
					    const std::string &name,
					    const sigc::slot1<void, temp::name> &k);

#endif
