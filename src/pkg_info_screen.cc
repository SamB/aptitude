// pkg_info_screen.cc
//
//  Copyright 2000-2002, 2004-2005 Daniel Burrows
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
//  Gathers information about a package into one
// spot (pkg_grouppolicy_info*) and provides dedicated code to display
// it (pkg_info_screen)

#include "aptitude.h"

#include "pkg_info_screen.h"

#include "vscreen/fragment.h"
#include "vscreen/vs_layout_item.h"

#include "dep_item.h"
#include "desc_parse.h"
#include "pkg_item_with_subtree.h"
#include "pkg_subtree.h"
#include "pkg_ver_item.h"
#include "trust.h"

#include <generic/apt/apt.h>

#include <apt-pkg/pkgrecords.h>
#include <apt-pkg/strutl.h>

class pkg_grouppolicy_info:public pkg_grouppolicy
{
public:
  pkg_grouppolicy_info(pkg_signal *_sig,
		       desc_signal *_desc_sig)
    :pkg_grouppolicy(_sig, _desc_sig) {}

  void add_package(const pkgCache::PkgIterator &pkg, pkg_subtree *root);

  static void setup_package_info(const pkgCache::PkgIterator &pkg,
				 const pkgCache::VerIterator &ver,
				 pkg_item_with_generic_subtree *tree,
				 pkg_signal *sig);
};

void pkg_grouppolicy_info::add_package(const pkgCache::PkgIterator &pkg,
				       pkg_subtree *root)
{
  pkg_item_with_generic_subtree *newtree=new pkg_item_with_generic_subtree(pkg,
									   get_sig(),
									   true);

  setup_package_info(pkg, pkg_item::visible_version(pkg), newtree, get_sig());

  root->add_child(newtree);
}

void pkg_grouppolicy_info::setup_package_info(const pkgCache::PkgIterator &pkg,
					      const pkgCache::VerIterator &ver,
					      pkg_item_with_generic_subtree *tree,
					      pkg_signal *sig)
{
  char buf[256];
  
  if(!ver.end())
    {
      pkgRecords::Parser &rec=apt_package_records->Lookup(ver.FileList());

      std::wstring desc(get_long_description(ver));
      std::wstring shortdesc(desc, 0, desc.find(L'\n'));

      vector<fragment*> frags;

      fragment *untrusted_warning=make_untrusted_warning(ver);
      if(untrusted_warning != NULL)
	{
	  frags.push_back(untrusted_warning);
	  frags.push_back(newline_fragment());
	}

      // Avoid creating new strings to translate.
      frags.push_back(clipbox(fragf("%B%s%b%ls%n",
				    _("Description: "), shortdesc.c_str())));
      frags.push_back(indentbox(2, 2, make_desc_fragment(desc)));

      fragment *tags = make_tags_fragment(pkg);
      if(tags != NULL)
	frags.push_back(fragf("%n%F", tags));

      // Can I use something other than a clipbox below?

      if((pkg->Flags&pkgCache::Flag::Essential)==pkgCache::Flag::Essential ||
	 (pkg->Flags&pkgCache::Flag::Important)==pkgCache::Flag::Important)
	frags.push_back(clipbox(fragf("%B%s%b%s",
				      _("Essential: "), _("yes"))));

      frags.push_back(clipbox(fragf("%B%s%b%s%n"
				    "%B%s%b%s%n"
				    "%B%s%b%s%n"
				    "%B%s%b%s%n"
				    "%B%s%b%s%n"
				    "%B%s%b%s%n",
				    _("Priority: "),pkgCache::VerIterator(ver).PriorityType()?pkgCache::VerIterator(ver).PriorityType():_("Unknown"),
				    _("Section: "),pkg.Section()?pkg.Section():_("Unknown"),
				    _("Maintainer: "),rec.Maintainer().c_str(),
				    _("Compressed size: "), SizeToStr(ver->Size).c_str(),
				    _("Uncompressed size: "), SizeToStr(ver->InstalledSize).c_str(),
				    _("Source Package: "),
				    rec.SourcePkg().empty()?pkg.Name():rec.SourcePkg().c_str())));

      tree->add_child(new vs_layout_item(sequence_fragment(frags)));

      setup_package_deps<pkg_item_with_generic_subtree>(pkg, ver, tree, sig);

      // Note: reverse provides show up in the version list
      if(!ver.ProvidesList().end())
	{
	  snprintf(buf, 256, _("Package names provided by %s"), pkg.Name());
	  pkg_subtree *prvtree=new pkg_subtree(transcode(buf));

	  for(pkgCache::PrvIterator prv=ver.ProvidesList(); !prv.end(); ++prv)
	    prvtree->add_child(new pkg_item(prv.ParentPkg(), sig));

	  tree->add_child(prvtree);
	}
    }

  snprintf(buf, 256, _("Packages which depend on %s"), pkg.Name());
  pkg_subtree *revtree=new pkg_subtree(transcode(buf));
  setup_package_deps<pkg_subtree>(pkg, ver, revtree, sig, true);
  tree->add_child(revtree);

  pkg_vertree_generic *newtree=new pkg_vertree_generic(transcode(_("Versions")), true);
  setup_package_versions(pkg, newtree, sig);
  tree->add_child(newtree);
}

pkg_info_screen::pkg_info_screen(const pkgCache::PkgIterator &pkg,
				 const pkgCache::VerIterator &ver)
  :apt_info_tree(pkg.Name(), ver.end()?"":ver.VerStr())
{
  set_root(setup_new_root(pkg, ver), true);
}

vs_treeitem *pkg_info_screen::setup_new_root(const pkgCache::PkgIterator &pkg,
					     const pkgCache::VerIterator &ver)
{
  pkg_item_with_generic_subtree *tree=new pkg_item_with_generic_subtree(pkg,
									get_sig(),
									true);
  pkg_grouppolicy_info::setup_package_info(pkg, ver, tree, get_sig());
  return tree;
}
