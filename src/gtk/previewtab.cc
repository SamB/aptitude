// previewtab.cc
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

#include "previewtab.h"
#include "aptitude.h"

#undef OK
#include <gtkmm.h>

#include <apt-pkg/strutl.h>

#include <generic/util/util.h>

#include <gtk/gui.h>
#include <gtk/packagesview.h>

namespace gui
{

  class PreviewTabGenerator : public PackagesTreeModelGenerator
  {
    Glib::RefPtr<Gtk::TreeStore> store;
    PackagesColumns *packages_columns;

    // \todo Swiped from pkg_grouppolicy_mode; should be pushed into
    // low-level code.
    const static char * const child_names[];

    std::map<int, Gtk::TreeStore::iterator> state_trees;

  private:
    PreviewTabGenerator(PackagesColumns *_packages_columns)
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
    static PreviewTabGenerator *create(PackagesColumns *packages_columns)
    {
      return new PreviewTabGenerator(packages_columns);
    }

    void add(const pkgCache::PkgIterator &pkg, const pkgCache::VerIterator &ver,
             std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator> * reverse_packages_store)
    {
      int group = find_pkg_state(pkg, *apt_cache_file);
      if(group != pkg_unchanged)
        {
          const std::map<int, Gtk::TreeModel::iterator>::const_iterator found =
            state_trees.find(group);

          Gtk::TreeModel::iterator tree;
          if(found == state_trees.end())
            {
              tree = store->append();
              Gtk::TreeModel::Row tree_row = *tree;
              packages_columns->fill_header(tree_row, _(child_names[group]));
              state_trees[group] = tree;
            }
          else
            tree = found->second;

          Gtk::TreeModel::iterator iter = store->append(tree->children());
          Gtk::TreeModel::Row row = *iter;

          reverse_packages_store->insert(std::make_pair(pkg, iter));

	  packages_columns->fill_row(row, pkg, ver);
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


  // \todo This is proof-of-concept only; the child_names list should
  // be in common code.
  const char * const PreviewTabGenerator::child_names[num_pkg_action_states]=
    {
      N_("Packages with unsatisfied dependencies\n The dependency requirements of these packages will be unmet after the install is complete.\n .\n The presence of this tree probably indicates that something is broken, either on your system or in the Debian archive."),
      N_("Packages being removed because they are no longer used\n These packages are being deleted because they were automatically installed to fulfill dependencies, and the planned action will result in no installed package declaring an 'important' dependency on them.\n"),
      N_("Packages being automatically held in their current state\n These packages could be upgraded, but they have been kept in their current state to avoid breaking dependencies."),
      N_("Packages being automatically installed to satisfy dependencies\n These packages are being installed because they are required by another package you have chosen for installation."),
      N_("Packages being deleted due to unsatisfied dependencies\n These packages are being deleted because one or more of their dependencies is no longer available, or because another package conflicts with them."),
      N_("Packages to be downgraded\n An older version of these packages than is currently installed will be installed."),
      N_("Packages being held back\n These packages could be upgraded, but you have asked for them to be held at their current version."),
      N_("Packages to be reinstalled\n These packages will be reinstalled."),
      N_("Packages to be installed\n These packages have been manually selected for installation on your computer."),
      N_("Packages to be removed\n These packages have been manually selected for removal."),
      N_("Packages to be upgraded\n These packages will be upgraded to a newer version."),
      N_("Packages that are partially installed\n These packages are not fully installed and configured; an attempt will be made to complete their installation."),
    };

  PreviewTab::PreviewTab(const Glib::ustring &label) :
    Tab(Preview, label, Gnome::Glade::Xml::create(glade_main_file, "main_packages_vbox"), "main_packages_vbox")
  {
    get_xml()->get_widget("main_packages_textview", pPackagesTextView);
    get_xml()->get_widget("main_notebook_packages_limit_entry", pLimitEntry);
    pLimitEntry->signal_activate().connect(sigc::mem_fun(*this, &PreviewTab::repopulate_model));
    get_xml()->get_widget("main_notebook_packages_limit_button", pLimitButton);
    pLimitButton->signal_clicked().connect(sigc::mem_fun(*this, &PreviewTab::repopulate_model));

    pPackagesView = new PackagesView(sigc::ptr_fun(PreviewTabGenerator::create), get_xml());;

    pPackagesView->get_treeview()->signal_selection.connect(sigc::mem_fun(*this, &PreviewTab::activated_package_handler));

    pPackagesView->get_treeview()->expand_all();

    get_widget()->show();
  }

  // TODO: Should be moved into PackagesView for use with PackagesView::signal_on_package_selection.
  void PreviewTab::activated_package_handler()
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

  void PreviewTab::repopulate_model()
  {
    pPackagesView->relimit_packages_view(pLimitEntry->get_text());
    pPackagesView->get_treeview()->expand_all();
    set_label(_("Preview: ") + pLimitEntry->get_text());
  }

  void PreviewTab::display_desc(pkgCache::PkgIterator pkg, pkgCache::VerIterator ver)
  {
    if(pkg.end())
      pPackagesTextView->get_buffer()->set_text("");
    else if (!ver.end())
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
