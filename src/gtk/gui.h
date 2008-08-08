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

#include "errortab.h"

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
      TabsManager * pNotebook;

      // The "global" singleton for storing apt errors.
      ErrorStore errorStore;

      // If this is not NULL, it points at the currently active
      // errors-view.
      ErrorTab *activeErrorTab;

      // Nulls out the error tab when it's closed.
      void apt_error_tab_closed();

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
    TabsManager * get_notebook() const { return pNotebook; }

    /** \brief Add a tab to the main notebook of this window. */
    void tab_add(Tab *tab);

    /** \brief Remove a tab from the main notebook of this window. */
    void tab_del(Tab *tab);
  };

  void main(int argc, char *argv[]);

}

#endif /*GUI_H_*/
