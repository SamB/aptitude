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

#include <generic/apt/config_signal.h>
#include <generic/apt/matching/compare_patterns.h>
#include <generic/apt/matching/parse.h>
#include <generic/apt/matching/pattern.h>
#include <generic/util/undo.h>
#include <generic/util/util.h>

#include <gtk/hyperlink.h>
#include <gtk/gui.h>
#include <gtk/packageinformation.h>
#include <gtk/pkgview.h>

#include <cwidget/generic/util/ssprintf.h>

namespace gui
{
  namespace
  {
    class limit_combobox_columns : public Gtk::TreeModel::ColumnRecord
    {
    public:
      Gtk::TreeModelColumn<Glib::ustring> name;
      Gtk::TreeModelColumn<cwidget::util::ref_ptr<aptitude::matching::pattern> > pattern;

      limit_combobox_columns()
      {
	add(name);
	add(pattern);
      }
    };

    // Stuff to manage the property.
    Glib::Quark limit_combobox_columns_property("aptitude-packages-tab-limit-combobox-columns");

    void limit_columns_destructor(gpointer limit_columns_void)
    {
      limit_combobox_columns *cols =
	static_cast<limit_combobox_columns *>(limit_columns_void);

      delete cols;
    }

    limit_combobox_columns *get_combobox_limit_columns(Gtk::ComboBox *combo)
    {
      return static_cast<limit_combobox_columns *>(combo->get_data(limit_combobox_columns_property));
    }

    void set_combobox_limit_columns(Gtk::ComboBox *combo,
				    limit_combobox_columns *limit_columns)
    {
      combo->set_data(limit_combobox_columns_property, limit_columns,
		      &limit_columns_destructor);
    }



    void repopulate_searchable_view(PkgView &packages_view,
				    Gtk::Entry &search_entry,
				    Gtk::ComboBox &limit_combobox,
				    const sigc::slot0<void> &after_repopulate_hook)
    {
      limit_combobox_columns *limit_columns =
	get_combobox_limit_columns(&limit_combobox);
      if(limit_columns == NULL)
	{
	  limit_columns = new limit_combobox_columns;
	  set_combobox_limit_columns(&limit_combobox, limit_columns);
	}
      std::string search_term = search_entry.get_text();
      const cwidget::util::ref_ptr<aptitude::matching::pattern> p(aptitude::matching::parse(search_term));
      cwidget::util::ref_ptr<aptitude::matching::pattern> final_pattern;

      Gtk::TreeModel::const_iterator active_limit_iter = limit_combobox.get_active();
      if(!active_limit_iter)
	final_pattern = p;
      else if(p.valid())
	{
	  Gtk::TreeModel::Row active_limit_row = *active_limit_iter;
	  cwidget::util::ref_ptr<aptitude::matching::pattern> active_limit_pattern =
	    active_limit_row[limit_columns->pattern];

	  if(active_limit_pattern.valid())
	    final_pattern = aptitude::matching::pattern::make_and(p, active_limit_pattern);
	  else
	    final_pattern = p;
	}
      packages_view.set_limit(final_pattern);

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

    // By convention, a blank string for a name means that a row is a
    // separator.
    bool row_is_separator(const Glib::RefPtr<Gtk::TreeModel> &model,
			  const Gtk::TreeModel::iterator &iterator,
			  const limit_combobox_columns *limit_columns)
    {
      Gtk::TreeModel::Row row = *iterator;
      Glib::ustring name = row[limit_columns->name];
      return name.empty();
    }
  }

  void setup_searchable_view(Gtk::Entry *search_entry,
			     Gtk::Button *search_button,
			     Gtk::ComboBox *limit_combobox,
			     const cwidget::util::ref_ptr<PkgView> packages_view,
			     const sigc::slot0<void> &after_repopulate_hook)
  {
    using aptitude::matching::pattern;
    using cwidget::util::ref_ptr;

    limit_combobox_columns *limit_columns = new limit_combobox_columns;

    set_combobox_limit_columns(limit_combobox, limit_columns);

    sigc::slot0<void> repopulate_hook = sigc::bind(sigc::ptr_fun(&repopulate_searchable_view),
						   packages_view.weak_ref(),
						   sigc::ref(*search_entry),
						   sigc::ref(*limit_combobox),
						   after_repopulate_hook);

    search_entry->signal_activate().connect(repopulate_hook);
    search_button->signal_clicked().connect(repopulate_hook);

    Glib::RefPtr<Gtk::ListStore> limit_model = Gtk::ListStore::create(*limit_columns);
    // The "All Packages" option is always there.
    {
      Gtk::TreeModel::iterator all_packages_iter = limit_model->append();
      Gtk::TreeModel::Row all_packages_row = *all_packages_iter;
      all_packages_row[limit_columns->name] = "All Packages";
      all_packages_row[limit_columns->pattern] = pattern::make_true();
    }

    std::map<std::string, ref_ptr<pattern> > limits;
    // Patterns that exist by default.
    //
    // \todo This is a mess for i18n.  How can I do this so that it
    // gets translated by default, but can still be overridden?  Maybe
    // I should make the name a property of the group rather than the
    // tag?
    limits["Installed Packages"] = aptitude::matching::parse("?installed");
    limits["Not Installed Packages"] = aptitude::matching::parse("?not(?installed)");
    limits["New Packages"] = aptitude::matching::parse("?new");
    limits["Virtual Packages"] = aptitude::matching::parse("?virtual");
    // Add in all the entries in the "Aptitude::Limits" group.
    for(const Configuration::Item *it = aptcfg->Tree(PACKAGE "::Limits");
	it != NULL; it = it->Next)
      {
	ref_ptr<pattern> p =
	  aptitude::matching::parse(it->Tag);

	limits[it->Tag] = p;
      }

    for(std::map<std::string, ref_ptr<pattern> >::const_iterator
	  it = limits.begin(); it != limits.end(); ++it)
      {
	if(it->second.valid())
	  {
	    // Output an hrule to separate the limits from "All
	    // Packages" if this is the first limit.
	    if(it == limits.begin())
	      {
		Gtk::TreeModel::iterator pattern_iter = limit_model->append();
		Gtk::TreeModel::Row pattern_row = *pattern_iter;
		pattern_row[limit_columns->name] = "";
		pattern_row[limit_columns->pattern] = ref_ptr<pattern>();
	      }

	    Gtk::TreeModel::iterator pattern_iter = limit_model->append();
	    Gtk::TreeModel::Row pattern_row = *pattern_iter;
	    pattern_row[limit_columns->name] = it->first;
	    pattern_row[limit_columns->pattern] = it->second;
	  }
      }

    limit_combobox->set_row_separator_func(sigc::bind(sigc::ptr_fun(&row_is_separator),
						      limit_columns));
    limit_combobox->pack_start(limit_columns->name);
    if(limit_combobox->get_cells().begin() != limit_combobox->get_cells().end())
    {
      Gtk::CellRenderer *renderer = *limit_combobox->get_cells().begin();
      Gtk::CellRendererText *renderer_text = dynamic_cast<Gtk::CellRendererText *>(renderer);
      renderer_text->property_ellipsize() = Pango::ELLIPSIZE_END;
    }
    limit_combobox->set_model(limit_model);
    limit_combobox->set_active(0);

    limit_combobox->signal_changed().connect(repopulate_hook);

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
    get_xml()->get_widget("main_notebook_packages_show_only_combo_box", pLimitComboBox);

    using cwidget::util::ref_ptr;
    pPkgView = ref_ptr<PkgView>(new PkgView(get_xml(), "main_packages_treeview",
					    get_label(), ""));

    // TODO: We prevent the tab from closing itself, but we should rather make closing
    //       the tab gracefully stop the generator from doing whatever it's doing
    pPkgView->store_reloading.connect(sigc::bind(sigc::mem_fun(*get_label_button(), &Gtk::Widget::set_sensitive), false));
    pPkgView->store_reloaded.connect(sigc::bind(sigc::mem_fun(*get_label_button(), &Gtk::Widget::set_sensitive), true));

    setup_searchable_view(pLimitEntry, pLimitButton, pLimitComboBox, pPkgView,
			  sigc::mem_fun(this, &PackagesTab::after_repopulate_model));

    pPkgView->get_treeview()->set_fixed_height_mode(true);

    pPkgView->get_treeview()->signal_selection.connect(sigc::mem_fun(*this, &PackagesTab::activated_package_handler));
    pPkgView->get_treeview()->signal_cursor_changed().connect(sigc::mem_fun(*this, &PackagesTab::activated_package_handler));

    pPkgView->package_menu_actions_changed.connect(package_menu_actions_changed.make_slot());
    apt_undos->changed.connect(undo_available_changed.make_slot());

    Glib::RefPtr<Gtk::SizeGroup> size_group = Gtk::SizeGroup::create(Gtk::SIZE_GROUP_HORIZONTAL);
    Gtk::Label *path_label;
    get_xml()->get_widget("main_notebook_packages_limit_label", path_label);
    size_group->add_widget (*path_label);
    get_xml()->get_widget("main_notebook_packages_show_only_label", path_label);
    size_group->add_widget (*path_label);

    get_widget()->show();
  }

  bool PackagesTab::get_undo_available()
  {
    return apt_undos->size() > 0;
  }

  void PackagesTab::dispatch_undo()
  {
    apt_undos->undo();
  }

  bool PackagesTab::get_edit_columns_available()
  {
    return true;
  }

  void PackagesTab::dispatch_edit_columns()
  {
    pPkgView->show_edit_columns_dialog();
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
    const std::string title = _("Packages: ") + pLimitEntry->get_text();
    set_label(title);
    pPkgView->edit_columns_dialog_parent_title_changed(get_label());
  }

  class InfoTabButtons : public Gtk::VButtonBox
  {
    cwidget::util::ref_ptr<PkgEntity> e;

    Gtk::Button installButton;
    Gtk::Button removeButton;
    Gtk::Button purgeButton;
    Gtk::Button keepButton;
    Gtk::Button holdButton;
    Gtk::Button autoButton;
    Gtk::Button manualButton;

    void do_dispatch_action(PackagesAction action)
    {
      std::auto_ptr<undo_group> undo(new undo_group);
      {
	aptitudeDepCache::action_group group(*apt_cache_file, undo.get());
	e->dispatch_action(action, true);
	e->dispatch_action(action, false);
      }
      if(!undo.get()->empty())
	apt_undos->add_item(undo.release());
    }

    void insert_button(Gtk::Button *button,
		       const Glib::ustring &buttonText,
		       Gtk::StockID stockId,
		       PackagesAction action)
    {
      button->set_image(*manage(new Gtk::Image(stockId, Gtk::ICON_SIZE_BUTTON)));
      button->signal_clicked().connect(sigc::bind(sigc::mem_fun(*this, &InfoTabButtons::do_dispatch_action),
						  action));
      button->set_label(buttonText);

      add(*button);
      button->show();
    }

    void update_package_button_states(const std::set<pkgCache::PkgIterator> *changed_packages)
    {
      using cwidget::util::ssprintf;
      pkgCache::PkgIterator pkg;
      pkgCache::VerIterator ver;

      if(e.valid())
	{
	  pkg = e->get_pkg();
	  ver = e->get_ver();
	}

      if(!e.valid() || pkg.end())
	return;

      if(changed_packages->find(pkg) == changed_packages->end())
	return;

      std::set<PackagesAction> actions;
      e->add_actions(actions);

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

      autoButton.property_visible() = (actions.find(MakeAutomatic) != actions.end());
      manualButton.property_visible() = (actions.find(MakeManual) != actions.end());
    }

  public:
    InfoTabButtons(const cwidget::util::ref_ptr<PkgEntity> &_e)
      : e(_e)
    {
      (*apt_cache_file)->package_states_changed.connect(sigc::mem_fun(*this, &InfoTabButtons::update_package_button_states));

      std::string name(!e->get_pkg().end()
		       ? e->get_pkg().Name()
		       : "??");

      insert_button(&installButton,
		    "The user should never see this text.", Gtk::Stock::DIALOG_ERROR,
		    Install);
      insert_button(&removeButton,
		    ssprintf(_("Remove %s"), name.c_str()),
		    Gtk::Stock::REMOVE,
		    Remove);
      insert_button(&purgeButton,
		    ssprintf(_("Purge %s"), name.c_str()),
		    Gtk::Stock::CLEAR,
		    Purge);
      insert_button(&keepButton,
		    "The user should not see this text.",
		    Gtk::Stock::MEDIA_REWIND,
		    Keep);
      insert_button(&holdButton,
		    ssprintf(_("Hold %s at its current version."),
			     name.c_str()),
		    Gtk::Stock::MEDIA_PAUSE,
		    Hold);
      insert_button(&autoButton,
		    ssprintf(_("Mark %s as automatically installed."),
			     name.c_str()),
		    Gtk::StockID(),
		    MakeAutomatic);
      insert_button(&manualButton,
		    ssprintf(_("Mark %s as manually installed."),
			     name.c_str()),
		    Gtk::StockID(),
		    MakeManual);

      std::set<pkgCache::PkgIterator> dummy;
      dummy.insert(e->get_pkg());
      update_package_button_states(&dummy);
    }
  };

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
    if(pkg_ent.valid())
      {
	textBuffer->insert(textBuffer->end(), "\n\n");
	Glib::RefPtr<Gtk::TextBuffer::ChildAnchor> button_box_anchor =
	  textBuffer->create_child_anchor(textBuffer->end());

	Gtk::ButtonBox *button_box = manage(new InfoTabButtons(pkg_ent));

	button_box->show();

	pPackagesTextView->add_child_at_anchor(*button_box, button_box_anchor);

	// Note the use of sigc::ref / ent.weak_ref() to ensure this
	// connection is removed when the buttons are destroyed.
      }
  }

  std::set<PackagesAction> PackagesTab::get_package_menu_actions()
  {
    return pPkgView->get_package_menu_actions();
  }

  void PackagesTab::dispatch_package_menu_action(PackagesAction action)
  {
    pPkgView->apply_action_to_selected(action);
  }
}
