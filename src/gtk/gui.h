// -*-c++-*-

// gui.h
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

#ifndef GUI_H_
#define GUI_H_

#undef OK
#include <gtkmm.h>
#include <libglademm/xml.h>

#include <set>

#include <sigc++/slot.h>

#include <apt-pkg/pkgcache.h>

#include <gtk/errortab.h>
#include <gtk/notify.h>

namespace gui
{
  // Local forward declarations:
  class AptitudeWindow;
  class TabsManager;
  class Tab;

  //This is a list of global and unique base widgets and other related stuff
  extern AptitudeWindow * pMainWindow;
  extern Glib::RefPtr<Gnome::Glade::Xml> refXml;
  extern std::string glade_main_file;
  extern bool want_to_quit;

  /** \brief Dispatch the given thunk to the main loop. */
  void post_event(const sigc::slot0<void> &thunk);

  void gtk_update();

  /** \brief Invoke tab_add on pMainWindow. */
  void tab_add(Tab *tab);

  /** \brief Invoke tab_del on pMainWindow. */
  void tab_del(Tab *tab);

  /** \brief Add a hyperlink to a Gtk::TextBuffer.
   *
   *  \param buffer      The buffer to which the link should be added.
   *  \param where       The location in the buffer at which the link should be added.
   *  \param link_text   The text that the user will see (will be
   *                     displayed in a standard "link style").
   *  \param link_action A callback invoked when the user clicks the link.
   *
   *  \return an iterator pointing to the end of the newly inserted text.
   */
  Gtk::TextBuffer::iterator add_hyperlink(const Glib::RefPtr<Gtk::TextBuffer> &buffer,
					  Gtk::TextBuffer::iterator where,
					  const Glib::ustring &link_text,
					  const sigc::slot0<void> &link_action);

  /** \brief Insert the tags of the given package into a TextBuffer.
   *
   *  Each tag will be a link that pops up a new packages view.
   *
   *  \param buffer The buffer into which the tags should be inserted.
   *  \param pkg    The package whose tags should be displayed.
   *  \param headerTag  A text-tag used to display the header that
   *                    leads off the tag list.
   *
   *  \return an iterator pointing to the end of the newly inserted text.
   */
  Gtk::TextBuffer::iterator add_debtags(const Glib::RefPtr<Gtk::TextBuffer> &buffer,
					Gtk::TextBuffer::iterator where,
					const pkgCache::PkgIterator &pkg,
					const Glib::RefPtr<Gtk::TextBuffer::Tag> &headerTag);

  /**
   * This is the main Aptitude custom window widget.
   */
  class AptitudeWindow : public Gtk::Window
  {
    private:
      Gtk::ToolButton * pToolButtonDashboard;
      Gtk::ToolButton * pToolButtonUpdate;
      Gtk::ToolButton * pToolButtonPackages;
      Gtk::ToolButton * pToolButtonPreview;
      Gtk::ToolButton * pToolButtonResolver;
      Gtk::ToolButton * pToolButtonInstallRemove;

      Gtk::ImageMenuItem * pMenuFilePackageRun;
      Gtk::ImageMenuItem * pMenuFileUpdateLists;
      Gtk::ImageMenuItem * pMenuFileMarkUpgradable;
      Gtk::ImageMenuItem * pMenuFileForgetNew;
      Gtk::ImageMenuItem * pMenuFileKeepAll;
      Gtk::ImageMenuItem * pMenuFileClean;
      Gtk::ImageMenuItem * pMenuFileAutoclean;
      Gtk::ImageMenuItem * pMenuFileReloadCache;
      Gtk::ImageMenuItem * pMenuFileSweep;
      Gtk::ImageMenuItem * pMenuFileSuToRoot;
      Gtk::ImageMenuItem * pMenuFileExit;

      Gtk::ProgressBar * pProgressBar;
      Gtk::Statusbar * pStatusBar;
      NotifyView * pNotifyView;
      TabsManager * pNotebook;

      // The "global" singleton for storing apt errors.
      ErrorStore errorStore;

      // If this is not NULL, it points at the currently active
      // errors-view.
      ErrorTab *activeErrorTab;

      // Nulls out the error tab when it's closed.
      void apt_error_tab_closed();

      void show_dependency_chains_tab();

      // Register the resolver-sensitivity callback.
      void update_resolver_sensitivity_callback();
      // Update the resolver sensistivity.
      void update_resolver_sensitivity();

    public:
      /**
       * Glade::Xml derived widget constructor.
       */
      AptitudeWindow(BaseObjectType* cobject, const Glib::RefPtr<Gnome::Glade::Xml>& refGlade)/* : Gtk::Window(cobject)*/;

    /** \brief Show the apt errors tab.
     *
     *  \todo This manually handles finding the active tab if there is
     *  one; perhaps this logic should be part of the tab manager?
     */
    void show_apt_errors();

    Gtk::ProgressBar * get_progress_bar() const { return pProgressBar; }
    Gtk::Statusbar * get_status_bar() const { return pStatusBar; }
    NotifyView *get_notifyview() const { return pNotifyView; }
    TabsManager * get_notebook() const { return pNotebook; }

    /** \brief Add a tab to the main notebook of this window. */
    void tab_add(Tab *tab);

    /** \brief Remove a tab from the main notebook of this window. */
    void tab_del(Tab *tab);

    /** \brief Open a resolver tab, as if it had been triggered from the menu.
     */
    void do_resolver();

    /** \brief Open a preview tab, as if it had been triggered from the menu. */
    void do_preview();

    /** \brief Open a list of currently broken packages. */
    void do_show_broken();
  };

  void main(int argc, char *argv[]);

  /** \brief Start an install/remove run, as if it had been triggered
   *  from the menu.
   */
  void do_installremove();

  /** \return \b true if an install/remove run can be started. */
  void do_installremove_allowed();

  /** \brief Start a list update, as if it had been triggered from the menu.
   */
  void do_update();

  /** @{
   *  Constants giving the textual and iconic representation of the
   *  current and selected states of packages.
   */

  /** \brief Represents information about a state that the packag can be in. */
  class entity_state_info
  {
    std::string flag;
    std::string description;
    Gtk::StockID icon;

  public:
    entity_state_info()
      : flag(), description(), icon()
    {
    }

    entity_state_info(const std::string &_flag,
		      const std::string &_description,
		      const Gtk::StockID &_icon)
      : flag(_flag), description(_description), icon(_icon)
    {
    }

    const std::string &get_flag() const { return flag; }
    const std::string &get_description() const { return description; }
    const Gtk::StockID &get_icon() const { return icon; }
  };

  extern const entity_state_info virtual_columns;
  extern const entity_state_info not_installed_columns;
  extern const entity_state_info unpacked_columns;
  extern const entity_state_info half_configured_columns;
  extern const entity_state_info half_installed_columns;
  extern const entity_state_info config_files_columns;
  extern const entity_state_info triggers_awaited_columns;
  extern const entity_state_info triggers_pending_columns;
  extern const entity_state_info installed_columns;
  extern const entity_state_info error_columns;

  extern const entity_state_info install_columns;
  extern const entity_state_info reinstall_columns;
  extern const entity_state_info upgrade_columns;
  extern const entity_state_info downgrade_columns;
  extern const entity_state_info remove_columns;
  extern const entity_state_info purge_columns;
  extern const entity_state_info hold_columns;
  extern const entity_state_info forbid_columns;
  extern const entity_state_info broken_columns;

  /** @} */
}

#endif /*GUI_H_*/
