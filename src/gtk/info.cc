// -*-c++-*-

// info.h
//
//  Copyright 1999-2008 Daniel Burrows
//  Copyright 2008 Obey Arthur Liu
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

#include "info.h"
#include "aptitude.h"

#undef OK
#include <gtkmm.h>

#include <apt-pkg/pkgcache.h>
#include <apt-pkg/pkgsystem.h>
#include <apt-pkg/version.h>

#include <gtk/gui.h>

#include <gtk/gui.h>
#include <gtk/packagesview.h>

#include <cwidget/generic/util/ssprintf.h>

namespace gui
{
  class DependsViewGenerator : public PackagesTreeModelGenerator
  {
    Glib::RefPtr<Gtk::TreeStore> store;
    PackagesColumns *packages_columns;

  private:
    DependsViewGenerator(PackagesColumns *_packages_columns)
    {
      // FIXME: Hack while finding a nonblocking thread join.
      finished = false;
      packages_columns = _packages_columns;
      store = Gtk::TreeStore::create(*packages_columns);
    }

  public:
    /** \brief Create a preview tab generator.
     *
     *  \param packages_columns  The columns of the new store.
     *
     *  \note This is mainly a workaround for the fact that either
     *  sigc++ doesn't provide convenience functors for constructors
     *  or I can't find them.
     */
    static DependsViewGenerator *create(PackagesColumns *packages_columns)
    {
      return new DependsViewGenerator(packages_columns);
    }

    void add(const pkgCache::PkgIterator &currpkg, const pkgCache::VerIterator &currver,
             std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator> * reverse_packages_store)
    {
      for(pkgCache::DepIterator dep = currver.DependsList();
      !dep.end(); ++dep)
      {
        pkgCache::DepIterator start, end;
        surrounding_or(dep, start, end);
        bool first = true;

        Gtk::TreeModel::iterator tree;
        Gtk::TreeModel::Row row;

        for(pkgCache::DepIterator todisp = start;
        todisp != end; ++todisp)
        {
          Gtk::TreeModel::iterator tree2;
          Gtk::TreeModel::Row row2;

          const bool is_or_continuation = !first;
          first = false;

          if(!is_or_continuation)
          {
            tree = store->append();
            row = *tree;
            packages_columns->fill_row(row, pkgCache::PkgIterator(), pkgCache::VerIterator());
            row[packages_columns->Name] = Glib::ustring(todisp.DepType()) + ": " + Glib::ustring(todisp.TargetPkg().Name());
          }
          else
          {
            row[packages_columns->Name] = row[packages_columns->Name] + " | " + Glib::ustring(todisp.ParentPkg().Name());
          }

          tree2 = store->append(tree->children());
          row2 = *tree2;

          packages_columns->fill_row(row2, pkgCache::PkgIterator(), pkgCache::VerIterator());

          row2[packages_columns->Name] = todisp.TargetPkg().Name();

          if(todisp->CompareOp != pkgCache::Dep::NoOp &&
              todisp.TargetVer() != NULL)
          {
            row2[packages_columns->Version] = Glib::ustring(todisp.CompType())+" "+Glib::ustring(todisp.TargetVer());
          }
          else
          {
            row2[packages_columns->Version] = "N/A";
          }

          bool resolvable = false;

          // Insert the various resolutions of this dep.  First direct
          // resolutions:
          {
            std::vector<pkgCache::VerIterator> direct_resolutions;
            for(pkgCache::VerIterator ver = todisp.TargetPkg().VersionList();
            !ver.end(); ++ver)
            {
              if(_system->VS->CheckDep(ver.VerStr(), todisp->CompareOp, todisp.TargetVer()))
              {
                resolvable = true;
                direct_resolutions.push_back(ver);
              }
            }

            if(!direct_resolutions.empty())
            {
              for(std::vector<pkgCache::VerIterator>::const_iterator it = direct_resolutions.begin();
              it != direct_resolutions.end(); ++it)
              {
                Gtk::TreeModel::iterator tree3 = store->append(tree2->children());
                Gtk::TreeModel::Row row3 = *tree3;
                reverse_packages_store->insert(std::make_pair(it->ParentPkg(), tree3));
                packages_columns->fill_row(row3, it->ParentPkg(), *it);
              }
            }
          }

          // Check for resolutions through virtual deps.
          {
            std::vector<pkgCache::VerIterator> virtual_resolutions;

            for(pkgCache::PrvIterator prv = todisp.TargetPkg().ProvidesList();
            !prv.end(); ++prv)
            {
              if(_system->VS->CheckDep(prv.ProvideVersion(), todisp->CompareOp, todisp.TargetVer()))
              {
                resolvable = true;
                virtual_resolutions.push_back(currver);
              }
            }


            if(!virtual_resolutions.empty())
            {
              for(std::vector<pkgCache::VerIterator>::const_iterator it = virtual_resolutions.begin();
              it != virtual_resolutions.end(); ++it)
              {
                Gtk::TreeModel::iterator tree3 = store->append(tree2->children());
                Gtk::TreeModel::Row row3 = *tree3;
                reverse_packages_store->insert(std::make_pair(it->ParentPkg(), tree3));
                packages_columns->fill_row(row3, it->ParentPkg(), *it);
              }
            }
          }

          if(!resolvable)
          {
            Gtk::TreeModel::iterator tree3 = store->append(tree2->children());
            Gtk::TreeModel::Row row3 = *tree3;

            packages_columns->fill_row(row3, pkgCache::PkgIterator(), pkgCache::VerIterator());

            row3[packages_columns->Name] = "Not available";
          }
        }
      }
    }

    void finish()
    {
      store->set_sort_column(packages_columns->Name, Gtk::SORT_ASCENDING);
      // FIXME: Hack while finding a nonblocking thread join.
      finished = true;
    }

    Glib::RefPtr<Gtk::TreeModel> get_model()
    {
      return store;
    }
  };

  class VersionsViewGenerator : public PackagesTreeModelGenerator
  {
    Glib::RefPtr<Gtk::ListStore> store;
    PackagesColumns *packages_columns;

  private:
    VersionsViewGenerator(PackagesColumns *_packages_columns)
    {
      // FIXME: Hack while finding a nonblocking thread join.
      finished = false;
      packages_columns = _packages_columns;
      store = Gtk::ListStore::create(*packages_columns);
    }

  public:
    /** \brief Create a preview tab generator.
     *
     *  \param packages_columns  The columns of the new store.
     *
     *  \note This is mainly a workaround for the fact that either
     *  sigc++ doesn't provide convenience functors for constructors
     *  or I can't find them.
     */
    static VersionsViewGenerator *create(PackagesColumns *packages_columns)
    {
      return new VersionsViewGenerator(packages_columns);
    }

    void add(const pkgCache::PkgIterator &currpkg, const pkgCache::VerIterator &currver,
             std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator> * reverse_packages_store)
    {
      for (pkgCache::VerIterator ver = currpkg.VersionList(); ver.end() == false; ver++)
      {
        Gtk::TreeModel::iterator iter = store->append();
        Gtk::TreeModel::Row row = *iter;

        reverse_packages_store->insert(std::make_pair(currpkg, iter));

	packages_columns->fill_row(row, currpkg, ver);
      }
    }

    void finish()
    {
      store->set_sort_column(packages_columns->Name, Gtk::SORT_ASCENDING);
      // FIXME: Hack while finding a nonblocking thread join.
      finished = true;
    }

    Glib::RefPtr<Gtk::TreeModel> get_model()
    {
      return store;
    }
  };

  InfoTab::InfoTab(Glib::ustring label)
  : Tab(Info, label, Gnome::Glade::Xml::create(glade_main_file, "main_info_scrolledwindow"), "main_info_scrolledwindow")
  {
    get_xml()->get_widget("main_info_textview", textview);
    get_widget()->show();
  }

  void InfoTab::insert_deps(pkgCache::VerIterator ver,
			    const Glib::RefPtr<Gtk::TextBuffer> &buffer,
			    Gtk::TextBuffer::iterator where)
  {
    const std::string bullet = cwidget::util::transcode(L"\x2022 ");

    Glib::RefPtr<Gtk::TextBuffer::Tag> dependency_head = Gtk::TextBuffer::Tag::create();
    dependency_head->property_pixels_above_lines() = 10;
    buffer->get_tag_table()->add(dependency_head);

    Glib::RefPtr<Gtk::TextBuffer::Tag> or_continuation = Gtk::TextBuffer::Tag::create();
    or_continuation->property_indent() = 5;
    buffer->get_tag_table()->add(or_continuation);

    Glib::RefPtr<Gtk::TextBuffer::Tag> dep_resolution = Gtk::TextBuffer::Tag::create();
    dep_resolution->property_indent() = 40;
    buffer->get_tag_table()->add(dep_resolution);

    Glib::RefPtr<Gtk::TextBuffer::Tag> dep_resolution_item = Gtk::TextBuffer::Tag::create();
    dep_resolution_item->property_indent() = 60;
    buffer->get_tag_table()->add(dep_resolution_item);

    Glib::RefPtr<Gtk::TextBuffer::Tag> dep_resolution_item_end = Gtk::TextBuffer::Tag::create();
    dep_resolution_item->property_indent() = 60;
    dep_resolution_item_end->property_pixels_below_lines() = 10;
    buffer->get_tag_table()->add(dep_resolution_item_end);

    for(pkgCache::DepIterator dep = ver.DependsList();
	!dep.end(); ++dep)
      {
	pkgCache::DepIterator start, end;
	surrounding_or(dep, start, end);
	bool first = true;

	for(pkgCache::DepIterator todisp = start;
	    todisp != end; ++todisp)
	{
	  const bool is_or_continuation = !first;
	  first = false;

	  Glib::RefPtr<Gtk::TextBuffer::Tag> head_tag =
	    is_or_continuation ? or_continuation : dependency_head;

	  if(!is_or_continuation)
	    {
	      where = buffer->insert_with_tag(where, todisp.ParentPkg().Name(), head_tag);
	      where = buffer->insert_with_tag(where, " ", head_tag);
	      where = buffer->insert_with_tag(where, todisp.DepType(), head_tag);
	      where = buffer->insert_with_tag(where, " ", head_tag);
	    }
	  else
	    where = buffer->insert_with_tag(where, " or ", head_tag);



	  std::string targetstr = todisp.TargetPkg().Name();
	  if(todisp->CompareOp != pkgCache::Dep::NoOp &&
	     todisp.TargetVer() != NULL)
	    {
	      targetstr += "(";
	      targetstr += todisp.CompType();
	      targetstr += " ";
	      targetstr += todisp.TargetVer();
	      targetstr += ")";
	    }

	  where = buffer->insert_with_tag(where, targetstr.c_str(), head_tag);
	  where = buffer->insert(where, "\n");

	  bool resolvable = false;

	  // Insert the various resolutions of this dep.  First direct
	  // resolutions:
	  {
	    std::vector<pkgCache::VerIterator> direct_resolutions;
	    for(pkgCache::VerIterator ver = todisp.TargetPkg().VersionList();
		!ver.end(); ++ver)
	      {
		if(_system->VS->CheckDep(ver.VerStr(), todisp->CompareOp, todisp.TargetVer()))
		  {
		    resolvable = true;
		    direct_resolutions.push_back(ver);
		  }
	      }

	    if(!direct_resolutions.empty())
	      {
		using cwidget::util::ssprintf;
		where = buffer->insert_with_tag(where,
					       ssprintf(_("Versions of %s satisfying this dependency:\n"),
							todisp.TargetPkg().Name()),
					       dep_resolution);

		for(std::vector<pkgCache::VerIterator>::const_iterator it = direct_resolutions.begin();
		    it != direct_resolutions.end(); ++it)
		  {
		    where = buffer->insert_with_tag(where, bullet, dep_resolution_item);

		    where = buffer->insert_with_tag(where, it->VerStr(), dep_resolution_item);
		    where = buffer->insert_with_tag(where, "\n", dep_resolution_item);
		  }
	      }
	  }

	  // Check for resolutions through virtual deps.
	  {
	    std::vector<pkgCache::VerIterator> virtual_resolutions;

	    for(pkgCache::PrvIterator prv = todisp.TargetPkg().ProvidesList();
		!prv.end(); ++prv)
	      {
		if(_system->VS->CheckDep(prv.ProvideVersion(), todisp->CompareOp, todisp.TargetVer()))
		  {
		    resolvable = true;
		    virtual_resolutions.push_back(ver);
		  }
	      }


	    if(!virtual_resolutions.empty())
	      {
		using cwidget::util::ssprintf;
		where = buffer->insert_with_tag(where,
						ssprintf(_("Packages providing %s:\n"),
							 targetstr.c_str()),
						dep_resolution);

		for(std::vector<pkgCache::VerIterator>::const_iterator it = virtual_resolutions.begin();
		    it != virtual_resolutions.end(); ++it)
		  {
		    where = buffer->insert_with_tag(where, bullet, dep_resolution_item);
		    where = buffer->insert_with_tag(where, it->ParentPkg().Name(), dep_resolution_item);
		    where = buffer->insert_with_tag(where, " ", dep_resolution_item);
		    where = buffer->insert_with_tag(where, it->VerStr(), dep_resolution_item);
		    where = buffer->insert_with_tag(where, "\n", dep_resolution_item);
		  }
	      }
	  }

	  if(!resolvable)
	    {
	      using cwidget::util::ssprintf;
	      where = buffer->insert_with_tag(where,
					      ssprintf(_("%s is not available.\n"),
						       targetstr.c_str()),
					      dep_resolution_item);
	    }
	}
      }
  }

  void InfoTab::disp_package(pkgCache::PkgIterator pkg, pkgCache::VerIterator ver)
  {
    Glib::RefPtr<Gtk::TextBuffer> buffer = textview->get_buffer();
    set_label("Info: " + Glib::ustring(pkg.Name()));

    Glib::RefPtr<Gtk::TextBuffer::Tag> refTagMatch = Gtk::TextBuffer::Tag::create();
    refTagMatch->property_scale() = 2;

    buffer->get_tag_table()->add(refTagMatch);

    buffer->insert_with_tag(buffer->end(), "Package: " + Glib::ustring(pkg.Name()), refTagMatch);
    buffer->insert(buffer->end(), "\nVersion: " + Glib::ustring(ver.VerStr()));

    buffer->insert_with_tag(buffer->end(), "\nDependencies:\n", refTagMatch);

    // Disabling this for now
    //insert_deps(ver, buffer, buffer->end());

    pDependsView = new PackagesView(sigc::ptr_fun(DependsViewGenerator::create),
        Gnome::Glade::Xml::create(glade_main_file, "main_packages_treeview"),
        pkg, ver);
    pDependsView->get_treeview()->get_column(0)->set_fixed_width(80);
    pDependsView->get_treeview()->expand_all();

    Glib::RefPtr<Gtk::TextChildAnchor> DependsViewAnchor = buffer->create_child_anchor(buffer->end());
    textview->add_child_at_anchor(*(pDependsView->get_treeview()), DependsViewAnchor);

    pVersionsView = new PackagesView(sigc::ptr_fun(VersionsViewGenerator::create),
        Gnome::Glade::Xml::create(glade_main_file, "main_packages_treeview"),
        pkg, ver);

    buffer->insert_with_tag(buffer->end(), "\nVersions:\n", refTagMatch);
    Glib::RefPtr<Gtk::TextChildAnchor> VersionsViewAnchor = buffer->create_child_anchor(buffer->end());
    textview->add_child_at_anchor(*(pVersionsView->get_treeview()), VersionsViewAnchor);

  }

}
