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
#include "info.h"

#undef OK
#include <gtkmm.h>

#include <apt-pkg/strutl.h>

#include <generic/util/util.h>

#include <gtk/gui.h>
#include <gtk/packagesview.h>
#include <gtk/packageinformation.h>

#include <cwidget/generic/util/ssprintf.h>

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

      packages_columns->fill_row(row, pkg, ver);
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
    Tab(Packages, label, Gnome::Glade::Xml::create(glade_main_file, "main_packages_hpaned"), "main_packages_hpaned")
  {
    get_xml()->get_widget("main_packages_textview", pPackagesTextView);
    get_xml()->get_widget("main_notebook_packages_limit_entry", pLimitEntry);
    pLimitEntry->signal_activate().connect(sigc::mem_fun(*this, &PackagesTab::repopulate_model));
    get_xml()->get_widget("main_notebook_packages_limit_button", pLimitButton);
    pLimitButton->signal_clicked().connect(sigc::mem_fun(*this, &PackagesTab::repopulate_model));

    pPackagesView = new PackagesView(sigc::ptr_fun(PackagesTabGenerator::create), get_xml(), "main_packages_treeview");

    // Ask the user to enter a search pattern.
    //
    // TODO: a similar message should appear when a search produces no
    // matches.
    {
      Glib::RefPtr<Gtk::ListStore> store = Gtk::ListStore::create(*pPackagesView->get_packages_columns());

      Gtk::TreeModel::iterator iter = store->append();
      Gtk::TreeModel::Row row = *iter;
      pPackagesView->get_packages_columns()->fill_header(row,
							 "Enter a search and click \"Find\" to display packages.");

      pPackagesView->get_treeview()->set_model(store);
    }

    pPackagesView->get_treeview()->set_fixed_height_mode(true);

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

  namespace
  {
    Glib::RefPtr<Gtk::Button> insert_button(Gtk::Container *parent,
					    const Glib::ustring &buttonText,
					    Gtk::StockID stockId,
					    pkgCache::PkgIterator pkg,
					    pkgCache::VerIterator ver,
					    PackagesAction action)
    {
      Gtk::Button *button = new Gtk::Button(buttonText);
      button->set_image(*new Gtk::Image(stockId, Gtk::ICON_SIZE_BUTTON));
      button->signal_clicked().connect(sigc::bind(sigc::ptr_fun(&dispatch_action),
						  pkg, ver, action));

      parent->add(*button);
      return Glib::RefPtr<Gtk::Button>(button);
    }

    void update_package_button_states(pkgCache::PkgIterator pkg,
				      // Covers installation, upgrades, and
				      // downgrades.
				      Glib::RefPtr<Gtk::Button> installButton,
				      Glib::RefPtr<Gtk::Button> removeButton,
				      Glib::RefPtr<Gtk::Button> purgeButton,
				      Glib::RefPtr<Gtk::Button> keepButton,
				      Glib::RefPtr<Gtk::Button> holdButton)
    {
      using cwidget::util::ssprintf;

      if(pkg.end())
	return;

      std::set<PackagesAction> actions;
      add_actions(pkg, actions);

      pkgDepCache::StateCache &state = (*apt_cache_file)[pkg];
      pkgCache::VerIterator candver = state.CandidateVerIter(*apt_cache_file);

      // Don't show the user the option to install the package unless
      // it might at some point be installable.
      if(candver.end() || state.Status == 0)
	installButton->hide();
      else
	{
	  installButton->show();

	  if(state.Status == 1)
	    {
	      installButton->set_label(ssprintf(_("Upgrade to %s version %s"),
						pkg.Name(),
						candver.VerStr()));
	      installButton->set_image(*new Gtk::Image(Gtk::Stock::GO_UP,
						       Gtk::ICON_SIZE_BUTTON));
	    }
	  else if(state.Status == 2)
	    {
	      installButton->set_label(ssprintf(_("Install %s version %s"),
						pkg.Name(),
						candver.VerStr()));
	      installButton->set_image(*new Gtk::Image(Gtk::Stock::ADD,
						       Gtk::ICON_SIZE_BUTTON));
	    }
	  else if(state.Status == -1)
	    {
	      installButton->set_label(ssprintf(_("Downgrade to %s version %s"),
						pkg.Name(),
						candver.VerStr()));
	      installButton->set_image(*new Gtk::Image(Gtk::Stock::GO_DOWN,
						       Gtk::ICON_SIZE_BUTTON));
	    }
	}

      if(state.Keep())
	{
	  if((*apt_cache_file)->get_ext_state(pkg).selection_state == pkgCache::State::Hold)
	    keepButton->set_label(ssprintf(_("Don't hold %s at its current version."),
					   pkg.Name()));
	  else
	    keepButton->set_label(ssprintf(_("Cancel any actions on %s."), pkg.Name()));
	}
      else if(state.Delete())
	{
	  if(state.iFlags & pkgDepCache::Purge)
	    keepButton->set_label(ssprintf(_("Cancel the purge of %s."), pkg.Name()));
	  else
	    keepButton->set_label(ssprintf(_("Cancel the removal of %s."), pkg.Name()));
	}
      else if(state.Install())
	{
	  if(state.Status == 1)
	    keepButton->set_label(ssprintf(_("Cancel the upgrade of %s."), pkg.Name()));
	  else if(state.Status == 2)
	    keepButton->set_label(ssprintf(_("Cancel the installation of %s."), pkg.Name()));
	  else if(state.Status == -1)
	    keepButton->set_label(ssprintf(_("Cancel the downgrade of %s."), pkg.Name()));
	  else if(state.Status == 1 && (state.iFlags & pkgDepCache::ReInstall))
	    keepButton->set_label(ssprintf(_("Cancel the reinstallation of %s."), pkg.Name()));
	  else
	    keepButton->set_label(ssprintf(_("Cancel any actions on %s."), pkg.Name()));
	}
      else
	keepButton->set_label(ssprintf(_("Cancel any actions on %s."), pkg.Name()));

      // If the package isn't installed, the "remove" and "purge"
      // buttons shouldn't appear, since they're never going to be
      // available.
      removeButton->property_visible() = (state.Status != 2);
      purgeButton->property_visible() = (state.Status != 2);

      installButton->property_sensitive() =
	(actions.find(Upgrade) != actions.end() ||
	 actions.find(Downgrade) != actions.end() ||
	 actions.find(Install) != actions.end());

      removeButton->property_sensitive() = (actions.find(Remove) != actions.end());
      purgeButton->property_sensitive() = (actions.find(Purge) != actions.end());
      keepButton->property_sensitive() = (actions.find(Keep) != actions.end());
      holdButton->property_sensitive() = (actions.find(Hold) != actions.end());
    }

    void maybe_update_package_button_states(const std::set<pkgCache::PkgIterator> *changed_packages,
					    pkgCache::PkgIterator pkg,
					    // Covers installation, upgrades, and
					    // downgrades.
					    Glib::RefPtr<Gtk::Button> installButton,
					    Glib::RefPtr<Gtk::Button> removeButton,
					    Glib::RefPtr<Gtk::Button> purgeButton,
					    Glib::RefPtr<Gtk::Button> keepButton,
					    Glib::RefPtr<Gtk::Button> holdButton)
    {
      if(changed_packages != NULL &&
	 changed_packages->find(pkg) != changed_packages->end())
	update_package_button_states(pkg,
				     installButton,
				     removeButton,
				     purgeButton,
				     keepButton,
				     holdButton);
    }
  }

  void PackagesTab::display_desc(pkgCache::PkgIterator pkg, pkgCache::VerIterator ver)
  {
    Glib::RefPtr<Gtk::TextBuffer> textBuffer = Gtk::TextBuffer::create();

    if(pkg.end())
      {
	textBuffer->set_text("");
      }
    else
      {
        PackageInformation info(pkg, ver);

	Glib::RefPtr<Gtk::TextBuffer::Tag> nameTag = textBuffer->create_tag();
	nameTag->property_size() = 20 * Pango::SCALE;

	Glib::RefPtr<Gtk::TextBuffer::Tag> fieldNameTag = textBuffer->create_tag();
	fieldNameTag->property_weight() = 2 * Pango::SCALE;

	textBuffer->insert_with_tag(textBuffer->end(),
				    info.Name(),
				    nameTag);
	textBuffer->insert(textBuffer->end(), " ");
	add_hyperlink(textBuffer, textBuffer->end(), _("(more info...)"),
		      sigc::bind(sigc::ptr_fun(&InfoTab::show_tab),
				 pkg, ver));
	textBuffer->insert(textBuffer->end(), "\n");
        textBuffer->insert(textBuffer->end(), info.ShortDescription());
        textBuffer->insert(textBuffer->end(), "\n");

	// TODO: insert a horizontal rule here (how?)

	textBuffer->insert(textBuffer->end(), "\n");

	if (ver)
	  {
	    //pkgRecords::Parser &rec=apt_package_records->Lookup(ver.FileList());

	    textBuffer->insert_with_tag(textBuffer->end(), _("Version: "), fieldNameTag);
	    textBuffer->insert(textBuffer->end(), info.Version());

	    textBuffer->insert(textBuffer->end(), "\n");
	    textBuffer->insert(textBuffer->end(), "\n");

	    std::wstring longdesc = get_long_description(ver, apt_package_records);

	    textBuffer->insert_with_tag(textBuffer->end(), _("Description: "), fieldNameTag);

	    textBuffer->insert(textBuffer->end(), info.LongDescription());
	  }
	else
	  {
	  }
      }

    pPackagesTextView->set_buffer(textBuffer);

    // This all has to be done after we set the buffer anyway.
    //
    // TODO: better layout is probably good.  Is it?
    if(!pkg.end())
      {
	textBuffer->insert(textBuffer->end(), "\n");
	Glib::RefPtr<Gtk::TextBuffer::ChildAnchor> button_box_anchor =
	  textBuffer->create_child_anchor(textBuffer->end());

	Gtk::ButtonBox *button_box = new Gtk::VButtonBox;

	Glib::RefPtr<Gtk::Button> installButton = insert_button(button_box,
								"The user should never see this text.", Gtk::Stock::DIALOG_ERROR,
								pkg, ver, Install);
	Glib::RefPtr<Gtk::Button> removeButton = insert_button(button_box,
							       ssprintf(_("Remove %s"), pkg.Name()),
							       Gtk::Stock::REMOVE,
							       pkg, ver, Remove);
	Glib::RefPtr<Gtk::Button> purgeButton = insert_button(button_box,
							      ssprintf(_("Purge %s"), pkg.Name()),
							      Gtk::Stock::CLEAR,
							      pkg, ver, Purge);
	Glib::RefPtr<Gtk::Button> keepButton = insert_button(button_box,
							     "The user should not see this text.",
							     Gtk::Stock::MEDIA_PAUSE,
							     pkg, ver, Keep);
	Glib::RefPtr<Gtk::Button> holdButton = insert_button(button_box,
							     ssprintf(_("Hold %s at its current version."),
								      pkg.Name()),
							     Gtk::Stock::MEDIA_REWIND,
							     pkg, ver, Hold);

	button_box->show_all();
	update_package_button_states(pkg,
				     installButton,
				     removeButton,
				     purgeButton,
				     keepButton,
				     holdButton);

	pPackagesTextView->add_child_at_anchor(*button_box, button_box_anchor);

	(*apt_cache_file)->package_states_changed.connect(sigc::bind(sigc::ptr_fun(&maybe_update_package_button_states), pkg, installButton, removeButton, purgeButton, keepButton, holdButton));
      }
  }

}
