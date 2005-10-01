// view_changelog.h
//
//   Copyright 2004 Daniel Burrows
//
// A utility function to view a package's changelog.  Inserts widgets
// into the main UI as appropriate.

#ifndef VIEW_CHANGELOG_H
#define VIEW_CHANGELOG_H

#include <apt-pkg/pkgcache.h>

void view_changelog(pkgCache::VerIterator ver);

#endif
