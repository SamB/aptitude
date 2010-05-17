// cmdline_show.h                                      -*-c++-*-
//
//  Copyright 2004 Daniel Burrows

#ifndef CMDLINE_SHOW_H
#define CMDLINE_SHOW_H

#include <iosfwd>
#include <string>

#include <apt-pkg/pkgcache.h>

/** \file cmdline_show.h
 */

namespace cwidget
{
  class fragment;
  class fragment_contents;
}

/** \brief Render the description of a single version as found in a
 *  particular index file.
 *
 *  \param ver  The version to render.
 *  \param vf   The index file in which to find information about ver.
 *  \param verbose  How much information to render (scale of 0 to 2 inclusive).
 */
cwidget::fragment *version_file_fragment(const pkgCache::VerIterator &ver,
					 const pkgCache::VerFileIterator &vf,
					 int verbose);

/** Run the "show" operation on a single argument, presented as a string. */
bool do_cmdline_show(std::string s, int verbose);

/** The "show" user command. */
int cmdline_show(int argc, char *argv[], int verbose);

std::ostream &operator<<(std::ostream &out, const cwidget::fragment_contents &contents);

#endif // CMDLINE_SHOW_H
