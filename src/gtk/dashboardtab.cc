// dashboardtab.cc
//
//   Copyright (C) 2008-2009 Obey Arthur Liu
//   Copyright (C) 2008 Daniel Burrows
//
//   This program is free software; you can redistribute it and/or
//   modify it under the terms of the GNU General Public License as
//   published by the Free Software Foundation; either version 2 of
//   the License, or (at your option) any later version.
//
//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//   General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with this program; see the file COPYING.  If not, write to
//   the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//   Boston, MA 02111-1307, USA.

#include "dashboardtab.h"

#include "changelog.h"
#include "hyperlink.h"
#include "info.h"
#include "packagestab.h" // For PackageSearchEntry.
#include "pkgview.h"
#include "progress.h"

#include <aptitude.h>

#include <generic/apt/apt.h>
#include <generic/apt/matching/pattern.h>

#include <cwidget/generic/util/ssprintf.h>

namespace cw = cwidget;

namespace gui
{
  void DashboardTab::do_search()
  {
    pMainWindow->add_packages_tab(package_search_entry->get_text());
  }

  void DashboardTab::do_upgrade()
  {
    do_mark_upgradable();
    // TODO: run a "safe-upgrade" here first, and if it fails
    // display a notification saying "unable to upgrade everything,
    // try harder?" that falls back to this (full upgrade) logic.
    pMainWindow->do_preview();
  }

  bool DashboardTab::get_edit_columns_available()
  {
    return true;
  }

  void DashboardTab::dispatch_edit_columns()
  {
    upgrades_pkg_view->show_edit_columns_dialog();
  }

  namespace
  {
    struct sort_versions_by_package_name
    {
      bool operator()(const pkgCache::VerIterator &v1,
		      const pkgCache::VerIterator &v2)
      {
	return strcmp(v1.ParentPkg().Name(),
		      v2.ParentPkg().Name()) < 0;
      }
    };
  }

  // Download all the changelogs and show the new entries.
  void DashboardTab::create_upgrade_summary()
  {
    if(apt_cache_file == NULL)
      return;

    Glib::RefPtr<Gtk::TextBuffer> text_buffer = Gtk::TextBuffer::create();

    Glib::RefPtr<Gtk::TextBuffer::Tag> header_tag = text_buffer->create_tag();
    header_tag->property_weight() = Pango::WEIGHT_BOLD;
    header_tag->property_weight_set() = true;
    header_tag->property_scale() = Pango::SCALE_X_LARGE;
    header_tag->property_scale_set() = true;

    cwidget::util::ref_ptr<guiOpProgress> p(guiOpProgress::create());

    std::vector<pkgCache::VerIterator> versions;

    {
      int i = 0;
      p->OverallProgress(i, (*apt_cache_file)->Head().PackageCount, 1,
			 _("Preparing to download changelogs"));

      for(pkgCache::PkgIterator pkg = (*apt_cache_file)->PkgBegin();
	  !pkg.end(); ++pkg)
	{
	  if(pkg->CurrentState == pkgCache::State::Installed &&
	     (*apt_cache_file)[pkg].Upgradable())
	    {
	      pkgCache::VerIterator candver = (*apt_cache_file)[pkg].CandidateVerIter(*apt_cache_file);

	      if(!candver.end())
		versions.push_back(candver);
	    }

	  ++i;
	  p->Progress(i);
	}

      p->Done();
    }

    std::sort(versions.begin(), versions.end(),
	      sort_versions_by_package_name());

    std::vector<changelog_download_job> changelogs;
    for(std::vector<pkgCache::VerIterator>::const_iterator it =
	  versions.begin(); it != versions.end(); ++it)
      {
	// Write out the header: PACKAGE: VERSION -> VERSION
	pkgCache::VerIterator candver = *it;
	Gtk::TextBuffer::iterator where = text_buffer->end();

	Glib::RefPtr<Gtk::TextBuffer::Mark> header_begin_mark =
	  text_buffer->create_mark(where);

	where = text_buffer->insert(where,
				    candver.ParentPkg().Name());

	where = text_buffer->insert(where, " ");

	pkgCache::VerIterator currver = candver.ParentPkg().CurrentVer();
	if(currver.end() ||
	   currver.VerStr() == NULL) // Just defensive; should never happen.
	  where = text_buffer->insert(where, "???");
	else
	  where = add_hyperlink(text_buffer, where,
				currver.VerStr(),
				sigc::bind(sigc::ptr_fun(&InfoTab::show_tab),
					   currver.ParentPkg(), currver));

	where = text_buffer->insert(where, " -> ");

	where = add_hyperlink(text_buffer, where,
			      candver.VerStr(),
			      sigc::bind(sigc::ptr_fun(&InfoTab::show_tab),
					 candver.ParentPkg(), candver));

	where = text_buffer->insert(where, "\n");

	const Gtk::TextBuffer::iterator header_begin =
	  text_buffer->get_iter_at_mark(header_begin_mark);
	text_buffer->apply_tag(header_tag, header_begin, where);

	where = text_buffer->insert(where, "\n");

	const Gtk::TextBuffer::iterator changelog_begin_iter = where;
	const Glib::RefPtr<Gtk::TextBuffer::Mark> changelog_begin =
	  text_buffer->create_mark(changelog_begin_iter);

	changelog_download_job job(changelog_begin,
				   text_buffer,
				   candver,
				   true);

	changelogs.push_back(job);

	where = text_buffer->insert(where, "\n\n");
      }

    upgrades_summary_textview->set_buffer(text_buffer);

    fetch_and_show_changelogs(changelogs);
  }

  void DashboardTab::handle_cache_closed()
  {
    available_upgrades_label->set_text(_("Available upgrades:"));
  }

  void DashboardTab::handle_upgrades_store_reloaded()
  {
    // Slightly lame: we know that the upgrade view will have as
    // many rows as there are upgrades, so just count the number of
    // rows (rather than re-calculating that and maybe being slow or
    // inconsistent).
    int num_upgrades = upgrades_pkg_view->get_model()->children().size();
    const char *text(ngettext("%d available upgrade:",
			      "%d available upgrades:",
			      num_upgrades));
    std::string formatted_text = cw::util::ssprintf(text, num_upgrades);
    available_upgrades_label->set_text(formatted_text);
  }

  DashboardTab::DashboardTab(Glib::ustring label)
    : Tab(Dashboard, label,
	  Gnome::Glade::Xml::create(glade_main_file, "dashboard_main"),
	  "dashboard_main")
  {
    get_xml()->get_widget("dashboard_upgrades_selected_package_textview",
			  upgrades_changelog_view);

    Gtk::Entry *search_entry;
    Gtk::Button *search_button;
    Gtk::Label *search_errors;

    get_xml()->get_widget("dashboard_search_entry",
			  search_entry);
    get_xml()->get_widget("dashboard_search_button",
			  search_button);
    get_xml()->get_widget("dashboard_search_errors",
			  search_errors);

    get_xml()->get_widget("dashboard_upgrade_button",
			  upgrade_button);
    get_xml()->get_widget("dashboard_available_upgrades_label",
			  available_upgrades_label);
    get_xml()->get_widget("dashboard_upgrades_summary_textview",
			  upgrades_summary_textview);

    package_search_entry = PackageSearchEntry::create(search_entry,
						      search_errors,
						      search_button);
    package_search_entry->activated.connect(sigc::hide(sigc::mem_fun(*this, &DashboardTab::do_search)));

    upgrades_pkg_view = cwidget::util::ref_ptr<PkgView>(new PkgView(get_xml(), "dashboard_upgrades_treeview",
								    _("Dashboard"), ""));
    upgrades_pkg_view->get_treeview()->signal_selection.connect(sigc::mem_fun(*this, &DashboardTab::activated_upgrade_package_handler));
    upgrades_pkg_view->get_treeview()->signal_cursor_changed().connect(sigc::mem_fun(*this, &DashboardTab::activated_upgrade_package_handler));

    cache_closed.connect(sigc::bind(sigc::mem_fun(*upgrade_button, &Gtk::Widget::set_sensitive),
				    false));
    cache_closed.connect(sigc::mem_fun(*this,
				       &DashboardTab::handle_cache_closed));
    cache_reloaded.connect(sigc::bind(sigc::mem_fun(*upgrade_button, &Gtk::Widget::set_sensitive),
				      true));
    upgrades_pkg_view->store_reloaded.connect(sigc::mem_fun(*this,
							    &DashboardTab::handle_upgrades_store_reloaded));


    upgrades_pkg_view->set_limit(aptitude::matching::pattern::make_upgradable());

    cache_reloaded.connect(sigc::mem_fun(*this, &DashboardTab::create_upgrade_summary));
    create_upgrade_summary();

    upgrade_button->set_image(*manage(new Gtk::Image(Gtk::Stock::GO_UP, Gtk::ICON_SIZE_BUTTON)));
    upgrade_button->signal_clicked().connect(sigc::mem_fun(*this, &DashboardTab::do_upgrade));

    get_widget()->show_all();

    upgrade_button->set_sensitive(apt_cache_file != NULL);

    // TODO: start an "update" when the program starts, or not?

    // TODO: start downloading changelogs and display them when
    // packages are selected.

    // TODO: customize the displayed columns to display version /
    // size / archive information.
  }

  void DashboardTab::activated_upgrade_package_handler()
  {
    if(apt_cache_file == NULL)
      {
	upgrades_changelog_view->get_buffer()->set_text("");
	return;
      }

    Gtk::TreeModel::Path path;
    Gtk::TreeViewColumn * focus_column;
    upgrades_pkg_view->get_treeview()->get_cursor(path, focus_column);
    // It's important that we create a new buffer in each case,
    // because it prevents any stale downloads from corrupting the
    // changelog display.  \todo if the user clicks on several
    // packages while downloads are in process, we'll render some
    // extra changelogs and throw the renders away; ideally, we
    // would just disconnect the rendering job completely, but that
    // will require some changes to how changelogs are downloaded.
    if (upgrades_pkg_view->get_treeview()->get_selection()->is_selected(path))
      {
	Gtk::TreeModel::iterator iter = upgrades_pkg_view->get_model()->get_iter(path);
	using cwidget::util::ref_ptr;
	ref_ptr<Entity> ent = (*iter)[upgrades_pkg_view->get_columns()->EntObject];
	ref_ptr<PkgEntity> pkg_ent = ent.dyn_downcast<PkgEntity>();

	pkgCache::PkgIterator pkg = pkg_ent->get_pkg();
	pkgCache::VerIterator candver = (*apt_cache_file)[pkg].CandidateVerIter(*apt_cache_file);
	const Glib::RefPtr<Gtk::TextBuffer> buffer =
	  Gtk::TextBuffer::create();

	upgrades_changelog_view->set_buffer(buffer);

	if(candver.end())
	  buffer->set_text(ssprintf(_("The package %s has no candidate version and will not be upgraded; unable to show its changelog."),
				    pkg.Name()));
	else
	  fetch_and_show_changelog(candver, buffer, buffer->end());
      }
    else
      {
	upgrades_changelog_view->set_buffer(Gtk::TextBuffer::create());
      }
  }
}


