// reason_fragment.h
//
//  Copyright 2004 Daniel Burrows
//
// Code to convert a list of reasons to text fragments.

#ifndef REASON_FRAGMENT_H
#define REASON_FRAGMENT_H

#include <apt-pkg/pkgcache.h>

#include <generic/apt/infer_reason.h>

namespace cwidget
{
  class fragment;
}

/** Generate a cwidget::fragment containing a (multi-line) description of why
 *  the package is in its present state.
 *
 *  If pkg.end() is \b true, returns nopackage().
 *
 *  \param pkg the package to examine
 *  \param breakage this parameter will be set to \b true if there
 *                 is "interesting" breakage involving the package,
 *                 false otherwise.  "interesting" breakage indicates that
 *                 either the package itself is broken, or it breaks
 *                 one of its reverse dependencies.
 *  \return the new text fragment
 */
fragment *reason_fragment(const pkgCache::PkgIterator &pkg,
			  bool &breakage);

/** Generate a cwidget::fragment containing a (multi-line) description of why
 *  the package is in its present state.
 *
 *  If pkg.end() is \b true, returns nopackage().
 *
 *  \param pkg the package to examine
 *  \return the new text fragment
 */
fragment *reason_fragment(const pkgCache::PkgIterator &pkg);

/** \return a string explaining that no package is selected. */
fragment *nopackage();

#endif
