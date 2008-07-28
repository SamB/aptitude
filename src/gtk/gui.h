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

namespace gui
{
  // Local forward declarations:
  class AptitudeWindow;
  class TabsManager;

  //This is a list of global and unique base widgets and other related stuff
  extern AptitudeWindow * pMainWindow;
  extern Glib::RefPtr<Gnome::Glade::Xml> refXml;
  extern std::string glade_main_file;
  extern bool want_to_quit;

  /**
   * Signal called when a package status change happens, usually when marking it.
   * Used by PackagesView for global updating.
   * TODO: Check if it doesn't already exists.
   */
  extern sigc::signal<void, std::set<pkgCache::PkgIterator> > signal_on_changed_packages;

  void gtk_update();

  template <class Tab_Type> Tab_Type * tab_add(Glib::ustring label);

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
    public:
      /**
       * Glade::Xml derived widget constructor.
       */
      AptitudeWindow(BaseObjectType* cobject, const Glib::RefPtr<Gnome::Glade::Xml>& refGlade)/* : Gtk::Window(cobject)*/;

    Gtk::ProgressBar * get_progress_bar() const { return pProgressBar; }
    Gtk::Statusbar * get_status_bar() const { return pStatusBar; }
    TabsManager * get_notebook() const { return pNotebook; }
  };

  void main(int argc, char *argv[]);

}

#endif /*GUI_H_*/
