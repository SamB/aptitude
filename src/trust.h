// trust.h
//
//  Copyright 2004 Daniel Burrows
//
//  Various routines dealing with messages about package trust.

#ifndef TRUST_H
#define TRUST_H

#include <apt-pkg/pkgcache.h>

class fragment;

/** Create a new fragment, suitable as a BIG FAT WARNING to the user
 *  that a single package is not trusted.
 *
 *  \param ver the version to which the fragment refers
 *  \return the new fragment, or NULL if ver is trusted.
 */
fragment *make_untrusted_warning(const pkgCache::VerIterator &ver);

#endif
