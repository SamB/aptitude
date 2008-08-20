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
#include <gtk/packageinformation.h>
#include <gtk/pkgview.h>

#include <cwidget/generic/util/ssprintf.h>

namespace gui
{
  namespace
  {
    void repopulate_searchable_view(PkgView &packages_view,
				    Gtk::Entry &search_entry,
				    const sigc::slot0<void> &after_repopulate_hook)
    {
      std::string search_term = search_entry.get_text();
      packages_view.set_limit(search_term);

      if(packages_view.get_model()->children().size() == 0)
	{
	  Glib::RefPtr<Gtk::ListStore> store = Gtk::ListStore::create(*packages_view.get_columns());

	  Gtk::TreeModel::iterator iter = store->append();
	  Gtk::TreeModel::Row row = *iter;
	  (new HeaderEntity(ssprintf(_("No packages matched \"%s\"."), search_term.c_str())))->fill_row(packages_view.get_columns(), row);

	  packages_view.set_model(store);
	}

      after_repopulate_hook();
    }
  }

  void setup_searchable_view(Gtk::Entry *search_entry,
			     Gtk::Button *search_button,
			     const cwidget::util::ref_ptr<PkgView> packages_view,
			     const sigc::slot0<void> &after_repopulate_hook)
  {
    sigc::slot0<void> repopulate_hook = sigc::bind(sigc::ptr_fun(&repopulate_searchable_view),
						   packages_view.weak_ref(),
						   sigc::ref(*search_entry),
						   after_repopulate_hook);

    search_entry->signal_activate().connect(repopulate_hook);
    search_button->signal_clicked().connect(repopulate_hook);

    // Ask the user to enter a search pattern.
    //
    // TODO: a similar message should appear when a search produces no
    // matches.
    {
      Glib::RefPtr<Gtk::ListStore> store = Gtk::ListStore::create(*packages_view->get_columns());

      Gtk::TreeModel::iterator iter = store->append();
      Gtk::TreeModel::Row row = *iter;
      (new HeaderEntity(_("Enter a search and click \"Find\" to display packages.")))->fill_row(packages_view->get_columns(), row);

      packages_view->set_model(store);
    }
  }

  PackagesTab::PackagesTab(const Glib::ustring &label) :
    Tab(Packages, label, Gnome::Glade::Xml::create(glade_main_file, "main_packages_hpaned"), "main_packages_hpaned")
  {
    get_xml()->get_widget("main_packages_textview", pPackagesTextView);
    get_xml()->get_widget("main_notebook_packages_limit_entry", pLimitEntry);
    get_xml()->get_widget("main_notebook_packages_limit_button", pLimitButton);

    using cwidget::util::ref_ptr;
    pPkgView = ref_ptr<PkgView>(new PkgView(get_xml(), "main_packages_treeview"));

    // TODO: We prevent the tab from closing itself, but we should rather make closing
    //       the tab gracefully stop the generator from doing whatever it's doing
    pPkgView->store_reloading.connect(sigc::bind(sigc::mem_fun(*get_label_button(), &Gtk::Widget::set_sensitive), false));
    pPkgView->store_reloaded.connect(sigc::bind(sigc::mem_fun(*get_label_button(), &Gtk::Widget::set_sensitive), true));

    setup_searchable_view(pLimitEntry, pLimitButton, pPkgView,
			  sigc::mem_fun(this, &PackagesTab::after_repopulate_model));

    pPkgView->get_treeview()->set_fixed_height_mode(true);

    pPkgView->get_treeview()->signal_selection.connect(sigc::mem_fun(*this, &PackagesTab::activated_package_handler));
    pPkgView->get_treeview()->signal_cursor_changed().connect(sigc::mem_fun(*this, &PackagesTab::activated_package_handler));

    get_widget()->show();
  }

  // TODO: Should be moved into PackagesView for use with PackagesView::signal_on_package_selection.
  void PackagesTab::activated_package_handler()
  {
    Gtk::TreeModel::Path path;
    Gtk::TreeViewColumn * focus_column;
    pPkgView->get_treeview()->get_cursor(path, focus_column);
    if (pPkgView->get_treeview()->get_selection()->is_selected(path))
    {
      Gtk::TreeModel::iterator iter = pPkgView->get_model()->get_iter(path);
      using cwidget::util::ref_ptr;
      ref_ptr<Entity> ent = (*iter)[pPkgView->get_columns()->EntObject];
      ref_ptr<PkgEntity> pkg_ent = ent.dyn_downcast<PkgEntity>();
      display_desc(ent);
    }
    else
    {
      pPackagesTextView->get_buffer()->set_text("");
    }
  }

  void PackagesTab::after_repopulate_model()
  {
    set_label(_("Packages: ") + pLimitEntry->get_text());
  }

  namespace
  {
    Gtk::Button *insert_button(Gtk::Container *parent,
			       const Glib::ustring &buttonText,
			       Gtk::StockID stockId,
			       const cwidget::util::ref_ptr<Entity> &entity,
			       PackagesAction action)
    {
      Gtk::Button *button = manage(new Gtk::Button(buttonText));
      button->set_image(*manage(new Gtk::Image(stockId, Gtk::ICON_SIZE_BUTTON)));
      button->signal_clicked().connect(sigc::bind(sigc::mem_fun(entity.unsafe_get_ref(), &Entity::dispatch_action),
						  action));

      parent->add(*button);
      return button;
    }

    void update_package_button_states(Entity &entBare,
				      // Covers installation, upgrades, and
				      // downgrades.
				      Gtk::Button &installButton,
				      Gtk::Button &removeButton,
				      Gtk::Button &purgeButton,
				      Gtk::Button &keepButton,
				      Gtk::Button &holdButton)
    {
      using cwidget::util::ssprintf;
      cwidget::util::ref_ptr<Entity> ent(&entBare);
      cwidget::util::ref_ptr<PkgEntity> pkg_ent = ent.dyn_downcast<PkgEntity>();
      pkgCache::PkgIterator pkg;
      pkgCache::VerIterator ver;

      if(pkg_ent.valid())
	{
	  pkg = pkg_ent->get_pkg();
	  ver = pkg_ent->get_ver();
	}

      if(!pkg_ent.valid() || pkg.end())
	return;

      std::set<PackagesAction> actions;
      ent->add_actions(actions);

      pkgDepCache::StateCache &state = (*apt_cache_file)[pkg];
      pkgCache::VerIterator candver = state.CandidateVerIter(*apt_cache_file);

      // Don't show the user the option to install the package unless
      // it might at some point be installable.
      if(candver.end() || state.Status == 0)
	installButton.hide();
      else
	{
	  installButton.show();

	  if(state.Status == 1)
	    {
	      installButton.set_label(ssprintf(_("Upgrade to %s version %s"),
					       pkg.Name(),
					       candver.VerStr()));
	      installButton.set_image(*manage(new Gtk::Image(Gtk::Stock::GO_UP,
							     Gtk::ICON_SIZE_BUTTON)));
	    }
	  else if(state.Status == 2)
	    {
	      installButton.set_label(ssprintf(_("Install %s version %s"),
					       pkg.Name(),
					       candver.VerStr()));
	      installButton.set_image(*manage(new Gtk::Image(Gtk::Stock::ADD,
							     Gtk::ICON_SIZE_BUTTON)));
	    }
	  else if(state.Status == -1)
	    {
	      installButton.set_label(ssprintf(_("Downgrade to %s version %s"),
					       pkg.Name(),
					       candver.VerStr()));
	      installButton.set_image(*manage(new Gtk::Image(Gtk::Stock::GO_DOWN,
							     Gtk::ICON_SIZE_BUTTON)));
	    }
	}

      if(state.Keep())
	{
	  if((*apt_cache_file)->get_ext_state(pkg).selection_state == pkgCache::State::Hold)
	    keepButton.set_label(ssprintf(_("Don't hold %s at its current version."),
					  pkg.Name()));
	  else
	    keepButton.set_label(ssprintf(_("Cancel any actions on %s."), pkg.Name()));
	}
      else if(state.Delete())
	{
	  if(state.iFlags & pkgDepCache::Purge)
	    keepButton.set_label(ssprintf(_("Cancel the purge of %s."), pkg.Name()));
	  else
	    keepButton.set_label(ssprintf(_("Cancel the removal of %s."), pkg.Name()));
	}
      else if(state.Install())
	{
	  if(state.Status == 1)
	    keepButton.set_label(ssprintf(_("Cancel the upgrade of %s."), pkg.Name()));
	  else if(state.Status == 2)
	    keepButton.set_label(ssprintf(_("Cancel the installation of %s."), pkg.Name()));
	  else if(state.Status == -1)
	    keepButton.set_label(ssprintf(_("Cancel the downgrade of %s."), pkg.Name()));
	  else if(state.Status == 1 && (state.iFlags & pkgDepCache::ReInstall))
	    keepButton.set_label(ssprintf(_("Cancel the reinstallation of %s."), pkg.Name()));
	  else
	    keepButton.set_label(ssprintf(_("Cancel any actions on %s."), pkg.Name()));
	}
      else
	keepButton.set_label(ssprintf(_("Cancel any actions on %s."), pkg.Name()));

      // If the package isn't installed, the "remove" and "purge"
      // buttons shouldn't appear, since they're never going to be
      // available.
      removeButton.property_visible() = (state.Status != 2);
      purgeButton.property_visible() = (state.Status != 2);

      installButton.property_sensitive() =
	(actions.find(Upgrade) != actions.end() ||
	 actions.find(Downgrade) != actions.end() ||
	 actions.find(Install) != actions.end());

      removeButton.property_sensitive() = (actions.find(Remove) != actions.end());
      purgeButton.property_sensitive() = (actions.find(Purge) != actions.end());
      keepButton.property_sensitive() = (actions.find(Keep) != actions.end());
      holdButton.property_sensitive() = (actions.find(Hold) != actions.end());
    }

    // TODO: it would be nice to fold the arguments into a single
    // parameter.  The reason this isn't done is that I want the
    // connection to "go away" when the buttons are destroyed; in
    // order for that to work I have to bind directly to the pointers
    // (sigc++ will detect that they implement sigc::trackable).
    void maybe_update_package_button_states(const std::set<pkgCache::PkgIterator> *changed_packages,
					    Entity &entBare,
					    // Covers installation, upgrades, and
					    // downgrades.
					    Gtk::Button &installButton,
					    Gtk::Button &removeButton,
					    Gtk::Button &purgeButton,
					    Gtk::Button &keepButton,
					    Gtk::Button &holdButton)
    {
      cwidget::util::ref_ptr<Entity> ent(&entBare);
      cwidget::util::ref_ptr<PkgEntity> pkg_ent = ent.dyn_downcast<PkgEntity>();

      if(pkg_ent.valid() &&
	 changed_packages != NULL &&
	 changed_packages->find(pkg_ent->get_pkg()) != changed_packages->end())
	update_package_button_states(entBare,
				     installButton,
				     removeButton,
				     purgeButton,
				     keepButton,
				     holdButton);
    }
  }

  struct compare_provider_lists_by_name
  {
    bool operator()(const std::pair<pkgCache::PkgIterator, std::vector<pkgCache::VerIterator> > &p1,
		    const std::pair<pkgCache::PkgIterator, std::vector<pkgCache::VerIterator> > &p2) const
    {
      return strcmp(p1.first.Name(), p2.first.Name()) < 0;
    }
  };

  void PackagesTab::display_desc(const cwidget::util::ref_ptr<Entity> &ent)
  {
    Glib::RefPtr<Gtk::TextBuffer> textBuffer = Gtk::TextBuffer::create();

    cwidget::util::ref_ptr<PkgEntity> pkg_ent = ent.dyn_downcast<PkgEntity>();
    pkgCache::PkgIterator pkg;
    pkgCache::VerIterator ver;

    if(pkg_ent.valid())
      {
	pkg = pkg_ent->get_pkg();
	ver = pkg_ent->get_ver();
      }

    // \todo We should gracefully handle missing versions.
    if(!pkg_ent.valid() || pkg.end() || ver.end())
      {
	// \todo This should be a generic function in gui.h.  In fact,
	// maybe the collation should be a generic function in
	// generic/apt/apt.h.
	if(pkg_ent.valid() && !pkg.end() && !pkg.ProvidesList().end())
	  {
	    textBuffer->insert(textBuffer->end(), ssprintf(_("%s is a virtual package provided by:\n"),
							   pkg.Name()));
	    // Collate the providers by the providing package.
	    std::map<pkgCache::PkgIterator, std::vector<pkgCache::VerIterator> > providers;
	    for(pkgCache::PrvIterator prv = pkg.ProvidesList(); !prv.end(); ++prv)
	      {
		providers[prv.OwnerPkg()].push_back(prv.OwnerVer());
	      }

	    // Put them in alphabetical order.
	    std::vector<std::pair<pkgCache::PkgIterator, std::vector<pkgCache::VerIterator> > >
	      provider_pairs(providers.begin(), providers.end());

	    std::sort(provider_pairs.begin(), provider_pairs.end(),
		      compare_provider_lists_by_name());

	    for(std::vector<std::pair<pkgCache::PkgIterator, std::vector<pkgCache::VerIterator> > >::const_iterator
		  it = provider_pairs.begin(); it != provider_pairs.end(); ++it)
	      {
		using cwidget::util::transcode;
		textBuffer->insert(textBuffer->end(), transcode(L"\t\x2022 "));
		add_hyperlink(textBuffer, textBuffer->end(),
			      it->first.Name(),
			      sigc::bind(sigc::ptr_fun(&InfoTab::show_tab),
					 it->first,
					 PkgEntity::get_ver(it->first)));

		textBuffer->insert(textBuffer->end(), "  (");
		bool first = true;
		for(std::vector<pkgCache::VerIterator>::const_iterator verIt = it->second.begin();
		    verIt != it->second.end(); ++verIt)
		  {
		    if(first)
		      first = false;
		    else
		      textBuffer->insert(textBuffer->end(), ", ");

		    add_hyperlink(textBuffer, textBuffer->end(),
				  verIt->VerStr(),
				  sigc::bind(sigc::ptr_fun(&InfoTab::show_tab),
					     verIt->ParentPkg(),
					     *verIt));
		  }

		textBuffer->insert(textBuffer->end(), ")\n");
	      }
	  }
	else
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

	    textBuffer->insert(textBuffer->end(), "\n");
	    add_debtags(textBuffer, textBuffer->end(),
			pkg, fieldNameTag);
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
	textBuffer->insert(textBuffer->end(), "\n\n");
	Glib::RefPtr<Gtk::TextBuffer::ChildAnchor> button_box_anchor =
	  textBuffer->create_child_anchor(textBuffer->end());

	Gtk::ButtonBox *button_box = manage(new Gtk::VButtonBox);

	Gtk::Button *installButton = insert_button(button_box,
						   "The user should never see this text.", Gtk::Stock::DIALOG_ERROR,
						   ent, Install);
	Gtk::Button *removeButton = insert_button(button_box,
						  ssprintf(_("Remove %s"), pkg.Name()),
						  Gtk::Stock::REMOVE,
						  ent, Remove);
	Gtk::Button *purgeButton = insert_button(button_box,
						 ssprintf(_("Purge %s"), pkg.Name()),
						 Gtk::Stock::CLEAR,
						 ent, Purge);
	Gtk::Button *keepButton = insert_button(button_box,
						"The user should not see this text.",
						Gtk::Stock::MEDIA_REWIND,
						ent, Keep);
	Gtk::Button *holdButton = insert_button(button_box,
						ssprintf(_("Hold %s at its current version."),
							 pkg.Name()),
						Gtk::Stock::MEDIA_PAUSE,
						ent, Hold);

	button_box->show_all();
	update_package_button_states(*ent.unsafe_get_ref(),
				     *installButton,
				     *removeButton,
				     *purgeButton,
				     *keepButton,
				     *holdButton);

	pPackagesTextView->add_child_at_anchor(*button_box, button_box_anchor);

	// Note the use of sigc::ref / ent.weak_ref() to ensure this
	// connection is removed when the buttons are destroyed.
	(*apt_cache_file)->package_states_changed.connect(sigc::bind(sigc::ptr_fun(&maybe_update_package_button_states), ent.weak_ref(), sigc::ref(*installButton), sigc::ref(*removeButton), sigc::ref(*purgeButton), sigc::ref(*keepButton), sigc::ref(*holdButton)));
      }
  }

}
