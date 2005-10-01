// pkg_sortpolicy.cc

#include "pkg_sortpolicy.h"
#include "pkg_item.h"

#include <vscreen/vs_subtree.h>

// Blah, this is the easiest way to define trivial subclasses:
// (not that far from lambda, actually)
// Yes, I hate typing more than I have to.
#define PKG_SORTPOLICY_SUBCLASS(name,code)	\
class name##_impl:public pkg_sortpolicy		\
{						\
public:						\
  name##_impl(pkg_sortpolicy *_chain, bool _reversed)\
  :pkg_sortpolicy(_chain, _reversed) {}		\
						\
						\
  inline int do_compare(const pkgCache::PkgIterator &pkg1, const pkgCache::VerIterator &ver1, \
	                const pkgCache::PkgIterator &pkg2, const pkgCache::VerIterator &ver2) const \
  {						\
    code					\
  }						\
						\
  int compare(const pkgCache::PkgIterator &pkg1, const pkgCache::VerIterator &ver1, \
	      const pkgCache::PkgIterator &pkg2, const pkgCache::VerIterator &ver2) const \
  {						\
    int rval=do_compare(pkg1, ver1, pkg2, ver2); \
    if(rval==0 && get_chain())			\
      return get_chain()->compare(pkg1, ver1, pkg2, ver2);\
    else					\
      return get_reversed()?-rval:rval;		\
  }						\
};						\
						\
pkg_sortpolicy *name(pkg_sortpolicy *chain, bool reversed)	\
{						\
  return new name##_impl(chain, reversed);	\
}						\


int pkg_sortpolicy_wrapper::compare(vs_treeitem *item1,
				    vs_treeitem *item2) const
{
  // erk.  RTTI is nasty.  Is there any way around it here?
  const pkg_item *pitem1=dynamic_cast<const pkg_item *>(item1);
  const pkg_item *pitem2=dynamic_cast<const pkg_item *>(item2);

  if(!pitem1 || !pitem2)
    return wcscmp(item1->tag(), item2->tag());
  else if(chain)
    return chain->compare(pitem1->get_package(), pitem1->visible_version(),
			  pitem2->get_package(), pitem2->visible_version());
  else
    return 0; // punt!
}

// The old by-name sorting
PKG_SORTPOLICY_SUBCLASS(pkg_sortpolicy_name,
			return strcmp(pkg1.Name(), pkg2.Name()););

// installed-size-sorting, treats virtual packages as 0-size
PKG_SORTPOLICY_SUBCLASS(pkg_sortpolicy_installed_size,
			if(ver1.end() && ver2.end())
			  return 0;
			else if(ver1.end())
			  return 1;
			else if(ver2.end())
			  return -1;
			else if(ver1->InstalledSize<ver2->InstalledSize)
			  return -1;
			else if(ver1->InstalledSize>ver2->InstalledSize)
			  return 1;
			else
			  return 0;);

// Priority sorting
PKG_SORTPOLICY_SUBCLASS(pkg_sortpolicy_priority,
			int pri1=ver1.end()?0:ver1->Priority;
			int pri2=ver2.end()?0:ver2->Priority;
			if(pri1<pri2)
			  return -1;
			  else if(pri1==pri2)
			return 0;
			  else // if(pri1>pri2)
			return 1;);
