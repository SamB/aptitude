// packagestab.cc
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

#include "packagestab.h"
#include "aptitude.h"

#undef OK
#include <gtkmm.h>

#include <apt-pkg/strutl.h>

#include <generic/util/util.h>

#include <gtk/gui.h>
#include <gtk/packagesview.h>

namespace gui
{

  class PackagesTabGenerator : public PackagesTreeModelGenerator
  {
    Glib::RefPtr<Gtk::ListStore> store;
    PackagesColumns *packages_columns;

  private:
    PackagesTabGenerator(PackagesColumns *_packages_columns)
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
    static PackagesTabGenerator *create(PackagesColumns *packages_columns)
    {
      return new PackagesTabGenerator(packages_columns);
    }

    void add(const pkgCache::PkgIterator &pkg, const pkgCache::VerIterator &ver,
             std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator> * reverse_packages_store)
    {
      Gtk::TreeModel::iterator iter = store->append();
      Gtk::TreeModel::Row row = *iter;

      reverse_packages_store->insert(std::make_pair(pkg, iter));

      row[packages_columns->PkgIterator] = pkg;
      row[packages_columns->VerIterator] = ver;
      row[packages_columns->CurrentStatus] = current_state_string(pkg, ver);
      row[packages_columns->SelectedStatus] = selected_state_string(pkg, ver);
      row[packages_columns->Name] = pkg.Name()?pkg.Name():"";
      row[packages_columns->Section] = pkg.Section()?pkg.Section():"";
      row[packages_columns->Version] = ver.VerStr();
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

  PackagesTab::PackagesTab(const Glib::ustring &label) :
    Tab(Packages, label, Gnome::Glade::Xml::create(glade_main_file, "main_packages_vbox"), "main_packages_vbox")
  {
    get_xml()->get_widget("main_packages_textview", pPackagesTextView);
    get_xml()->get_widget("main_notebook_packages_limit_entry", pLimitEntry);
    pLimitEntry->signal_activate().connect(sigc::mem_fun(*this, &PackagesTab::repopulate_model));
    get_xml()->get_widget("main_notebook_packages_limit_button", pLimitButton);
    pLimitButton->signal_clicked().connect(sigc::mem_fun(*this, &PackagesTab::repopulate_model));

    pPackagesView = new PackagesView(sigc::ptr_fun(PackagesTabGenerator::create), get_xml());

    pPackagesView->get_treeview()->signal_selection.connect(sigc::mem_fun(*this, &PackagesTab::activated_package_handler));
    pPackagesView->get_treeview()->signal_cursor_changed().connect(sigc::mem_fun(*this, &PackagesTab::activated_package_handler));

    get_widget()->show();
  }

  // TODO: Should be moved into PackagesView for use with PackagesView::signal_on_package_selection.
  void PackagesTab::activated_package_handler()
  {
    Gtk::TreeModel::Path path;
    Gtk::TreeViewColumn * focus_column;
    pPackagesView->get_treeview()->get_cursor(path, focus_column);
    if (pPackagesView->get_treeview()->get_selection()->is_selected(path))
    {
      Gtk::TreeModel::iterator iter = pPackagesView->get_packages_store()->get_iter(path);
      pkgCache::PkgIterator pkg = (*iter)[pPackagesView->get_packages_columns()->PkgIterator];
      pkgCache::VerIterator ver = (*iter)[pPackagesView->get_packages_columns()->VerIterator];
      display_desc(pkg, ver);
    }
    else
    {
      pPackagesTextView->get_buffer()->set_text("");
    }
  }

  void PackagesTab::repopulate_model()
  {
    pPackagesView->relimit_packages_view(pLimitEntry->get_text());
    set_label(_("Packages: ") + pLimitEntry->get_text());
  }

  void PackagesTab::display_desc(pkgCache::PkgIterator pkg, pkgCache::VerIterator ver)
  {
    if (ver)
    {
      pkgRecords::Parser &rec=apt_package_records->Lookup(ver.FileList());
      string misc = ssprintf("%s%s\n"
          "%s%s\n"
          "%s%s\n"
          "%s%s\n"
          "%s%s\n"
          "%s%s\n"
          "%s%s\n",
          _("Name: "), pkg.Name(),
          _("Priority: "),pkgCache::VerIterator(ver).PriorityType()?pkgCache::VerIterator(ver).PriorityType():_("Unknown"),
              _("Section: "),pkg.Section()?pkg.Section():_("Unknown"),
                  _("Maintainer: "),rec.Maintainer().c_str(),
                  _("Compressed size: "), SizeToStr(ver->Size).c_str(),
                  _("Uncompressed size: "), SizeToStr(ver->InstalledSize).c_str(),
                  _("Source Package: "),
                  rec.SourcePkg().empty()?pkg.Name():rec.SourcePkg().c_str());
      string desc = cwidget::util::transcode(get_long_description(ver, apt_package_records), "UTF-8");
      pPackagesTextView->get_buffer()->set_text(misc + _("Description: ") + desc);
    }
    else
    {
      pPackagesTextView->get_buffer()->set_text(ssprintf("%s%s\n", _("Name: "), pkg.Name()));
    }
  }

}
