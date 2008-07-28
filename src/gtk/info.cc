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

#include <gtk/gui.h>

#include <gtk/gui.h>
#include <gtk/packagesview.h>

namespace gui
{
  class DependsViewGenerator : public PackagesTreeModelGenerator
  {
    Glib::RefPtr<Gtk::ListStore> store;
    PackagesColumns *packages_columns;

  private:
    DependsViewGenerator(PackagesColumns *_packages_columns)
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
    static DependsViewGenerator *create(PackagesColumns *packages_columns)
    {
      return new DependsViewGenerator(packages_columns);
    }

    void add(const pkgCache::PkgIterator &currpkg, const pkgCache::VerIterator &currver,
             std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator> * reverse_packages_store)
    {
      for (pkgCache::DepIterator dep = currver.DependsList(); dep.end() == false; dep++)
      {
        Gtk::TreeModel::iterator iter = store->append();
        Gtk::TreeModel::Row row = *iter;

        reverse_packages_store->insert(std::make_pair(dep.TargetPkg(), iter));

        row[packages_columns->PkgIterator] = dep.TargetPkg();
        row[packages_columns->VerIterator] = dep.TargetPkg().CurrentVer();
        row[packages_columns->CurrentStatus] = dep.TargetPkg().CurrentVer()?current_state_string(dep.TargetPkg(), dep.TargetPkg().CurrentVer()):"";
        row[packages_columns->SelectedStatus] = dep.TargetPkg().CurrentVer()?selected_state_string(dep.TargetPkg(), dep.TargetPkg().CurrentVer()):"";
        row[packages_columns->Name] = dep.TargetPkg().Name()?dep.TargetPkg().Name():"";
        row[packages_columns->Section] = dep.TargetPkg().Section()?dep.TargetPkg().Section():"";
        row[packages_columns->Version] = dep.TargetVer()?dep.TargetVer():"";
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

        row[packages_columns->PkgIterator] = currpkg;
        row[packages_columns->VerIterator] = ver;
        row[packages_columns->CurrentStatus] = current_state_string(currpkg, ver);
        row[packages_columns->SelectedStatus] = selected_state_string(currpkg, ver);
        row[packages_columns->Name] = currpkg.Name()?currpkg.Name():"";
        row[packages_columns->Section] = currpkg.Section()?currpkg.Section():"";
        row[packages_columns->Version] = ver.VerStr();
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

  void InfoTab::disp_package(pkgCache::PkgIterator pkg, pkgCache::VerIterator ver)
  {
    Glib::RefPtr<Gtk::TextBuffer> buffer = textview->get_buffer();
    set_label("Info: " + Glib::ustring(pkg.Name()));

    Glib::RefPtr<Gtk::TextBuffer::Tag> refTagMatch = Gtk::TextBuffer::Tag::create();
    refTagMatch->property_scale() = 2;

    buffer->get_tag_table()->add(refTagMatch);

    buffer->insert_with_tag(buffer->end(), "Package: " + Glib::ustring(pkg.Name()), refTagMatch);
    buffer->insert(buffer->end(), "\nVersion: " + Glib::ustring(ver.VerStr()));

    pDependsView = new PackagesView(sigc::ptr_fun(DependsViewGenerator::create),
        Gnome::Glade::Xml::create(glade_main_file, "main_packages_treeview"),
        "~n"+Glib::ustring(pkg.Name()));

    buffer->insert_with_tag(buffer->end(), "\nDependencies:\n", refTagMatch);
    Glib::RefPtr<Gtk::TextChildAnchor> DependsViewAnchor = buffer->create_child_anchor(buffer->end());
    textview->add_child_at_anchor(*(pDependsView->get_treeview()), DependsViewAnchor);

    pVersionsView = new PackagesView(sigc::ptr_fun(VersionsViewGenerator::create),
        Gnome::Glade::Xml::create(glade_main_file, "main_packages_treeview"),
        "~n"+Glib::ustring(pkg.Name()));

    buffer->insert_with_tag(buffer->end(), "\nVersions:\n", refTagMatch);
    Glib::RefPtr<Gtk::TextChildAnchor> VersionsViewAnchor = buffer->create_child_anchor(buffer->end());
    textview->add_child_at_anchor(*(pVersionsView->get_treeview()), VersionsViewAnchor);

  }

}
