// pkg_sortpolicy.h    (-*-c++-*-)
//
//  Copyright 2001 Daniel Burrows
//
// Package sorting policies.  Currently they just handle PkgIterator/VerIterator
// pairs and a wrapper class integrates them with vs_tree.  If this can handle
// everything useful, I'd like to keep it as is, since it's simpler than the
// alternatives.

#ifndef PKG_SORTPOLICY_H
#define PKG_SORTPOLICY_H

#include <apt-pkg/pkgcache.h>
#include <vscreen/vs_treeitem.h>

class pkg_tree_node;

class pkg_sortpolicy
{
  pkg_sortpolicy *chain;

  bool reversed;
protected:
  const pkg_sortpolicy *get_chain() const {return chain;}
  bool get_reversed() const {return reversed;}
public:
  pkg_sortpolicy(pkg_sortpolicy *_chain, bool _reversed)
    :chain(_chain), reversed(_reversed) {}

  virtual ~pkg_sortpolicy() {delete chain;}

  virtual int compare(const pkgCache::PkgIterator &pkg1, const pkgCache::VerIterator &ver1,
		      const pkgCache::PkgIterator &pkg2, const pkgCache::VerIterator &ver2) const=0;
};

// This is an experiment..I'm using factories to avoid a massively oversized
// header file.  We'll see how it works..
//
// Each of these instantiates a corresponding class and returns an instance of
// it.
pkg_sortpolicy *pkg_sortpolicy_name(pkg_sortpolicy *chain, bool reversed);
pkg_sortpolicy *pkg_sortpolicy_installed_size(pkg_sortpolicy *chain, bool reversed);
pkg_sortpolicy *pkg_sortpolicy_priority(pkg_sortpolicy *chain, bool reversed);

// ewwwwwwww.
//
//  IMPORTANT: this does NOT, repeat NOT delete its "contents".  It wouldn't
// work if it did without all sorts of evil.  You have been warned.
//
//  Having the operator() be virtual is a bit of an ick..
class pkg_sortpolicy_wrapper:public sortpolicy
{
  pkg_sortpolicy *chain;
public:
  pkg_sortpolicy_wrapper(pkg_sortpolicy *_chain):chain(_chain) {}

  int compare(vs_treeitem *item1, vs_treeitem *item2) const;
  bool operator()(vs_treeitem *item1, vs_treeitem *item2)
  {
    return (compare(item1, item2)<0);
  }
};

#endif
