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
              packages_columns->fill_header(row, Glib::ustring(todisp.DepType()) + ": ");
              row[packages_columns->Name] =row[packages_columns->Name] + Glib::ustring(todisp.TargetPkg().Name());
            }
          else
            {
              row[packages_columns->Name] =row[packages_columns->Name]+ " | " + Glib::ustring(todisp.TargetPkg().Name());
            }

          tree2 = store->append(tree->children());
          row2 = *tree2;

          packages_columns->fill_header(row2, todisp.TargetPkg().Name());

          if(todisp->CompareOp != pkgCache::Dep::NoOp &&
              todisp.TargetVer() != NULL)
          {
            row2[packages_columns->Version] = Glib::Markup::escape_text(Glib::ustring(todisp.CompType())+" "+Glib::ustring(todisp.TargetVer()));
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
                packages_columns->fill_row(row3, it->ParentPkg(), *it, true);
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
                virtual_resolutions.push_back(prv.OwnerVer());
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
                packages_columns->fill_row(row3, it->ParentPkg(), *it, true);
              }
            }
          }

          if(!resolvable)
          {
            Gtk::TreeModel::iterator tree3 = store->append(tree2->children());
            Gtk::TreeModel::Row row3 = *tree3;

            // This should call something like fill_header,
            // but it's not really a header, so we're keeping this for now.
            packages_columns->fill_row(row3, pkgCache::PkgIterator(), pkgCache::VerIterator());

            row3[packages_columns->Name] = "Not available";
          }
        }
      }
    }

    void finish()
    {
      store->set_sort_column(packages_columns->Version, Gtk::SORT_ASCENDING);
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

	packages_columns->fill_row(row, currpkg, ver, true);
      }
    }

    void finish()
    {
      store->set_sort_column(packages_columns->Version, Gtk::SORT_ASCENDING);
      // FIXME: Hack while finding a nonblocking thread join.
      finished = true;
    }

    Glib::RefPtr<Gtk::TreeModel> get_model()
    {
      return store;
    }
  };

  InfoTab::InfoTab(Glib::ustring label)
  : Tab(Info, label, Gnome::Glade::Xml::create(glade_main_file, "main_info_hpaned"), "main_info_hpaned")
  {
    get_xml()->get_widget("main_info_textview", textview);
    get_widget()->show();
  }

  void InfoTab::disp_package(pkgCache::PkgIterator pkg, pkgCache::VerIterator ver)
  {
    Glib::RefPtr<Gtk::TextBuffer> textBuffer = textview->get_buffer();
    set_label("Info: " + Glib::ustring(pkg.Name()));

    Glib::RefPtr<Gtk::TextBuffer::Tag> nameTag = textBuffer->create_tag();
    nameTag->property_size() = 20 * Pango::SCALE;

    Glib::RefPtr<Gtk::TextBuffer::Tag> fieldNameTag = textBuffer->create_tag();
    fieldNameTag->property_weight() = 2 * Pango::SCALE;

    textBuffer->insert_with_tag(textBuffer->end(), pkg.Name(), nameTag);
    textBuffer->insert(textBuffer->end(), "\n");

    // TODO: insert a horizontal rule here (how?)

    textBuffer->insert(textBuffer->end(), "\n");

    //pkgRecords::Parser &rec=apt_package_records->Lookup(ver.FileList());

    textBuffer->insert_with_tag(textBuffer->end(), _("Version: "), fieldNameTag);
    textBuffer->insert(textBuffer->end(), ver.VerStr());

    textBuffer->insert(textBuffer->end(), "\n");
    textBuffer->insert(textBuffer->end(), "\n");

    std::wstring longdesc = get_long_description(ver, apt_package_records);

    textBuffer->insert_with_tag(textBuffer->end(), _("Description: "), fieldNameTag);

    textBuffer->insert(textBuffer->end(), cwidget::util::transcode(longdesc, "UTF-8"));


    pVersionsView = new PackagesView(sigc::ptr_fun(VersionsViewGenerator::create),
        get_xml(), "main_info_versionsview", pkg, ver);
    pVersionsView->get_treeview()->get_column(2)->set_fixed_width(154);

    pDependsView = new PackagesView(sigc::ptr_fun(DependsViewGenerator::create),
        get_xml(), "main_info_dependsview", pkg, ver);
    pDependsView->get_treeview()->get_column(0)->set_fixed_width(80);
    pDependsView->get_treeview()->get_column(2)->set_fixed_width(280);
    Gtk::TreeModel::Children dependsChildren = pDependsView->get_treeview()->get_model()->children();
    for(Gtk::TreeModel::iterator it = dependsChildren.begin();
	it != dependsChildren.end(); ++it)
      {
	// Expand all the top-level entries, which we magically know
	// contain the individual components of a dependency.
	Gtk::TreeModel::Path path = pDependsView->get_treeview()->get_model()->get_path(it);
	pDependsView->get_treeview()->expand_row(path, false);
      }
  }

  void InfoTab::show_tab(const pkgCache::PkgIterator &pkg,
			 const pkgCache::VerIterator &ver)
  {
    InfoTab * infotab = new InfoTab(_("Info:"));
    tab_add(infotab);
    infotab->disp_package(pkg, ver);
  }
}
