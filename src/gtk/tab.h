// -*-c++-*-

// tab.h
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

#ifndef TAB_H_
#define TAB_H_

#undef OK
#include <gtkmm.h>
#include <libglademm/xml.h>

namespace gui
{
  /**
   * This is a list of tab types.
   */
  enum TabType
  {
    Dashboard, Download, Packages, Info, Preview, Resolver, InstallRemove, Error
  };

  /**
   * \brief A Tab contains a widget and some metadata for inserting into the notebook.
   *
   *  \todo To delete tabs, we rely on the clicked() signal from the
   *  close button.  This is not ideal, because the underlying widget
   *  could be deleted some other way.
   */
  class Tab : public sigc::trackable
  {
    private:
      TabType type;
      Glib::ustring label;
      Glib::RefPtr<Gnome::Glade::Xml> xml;
      Gtk::Label * label_label;
      Gtk::Button * label_button;
      Gtk::Widget * label_widget;
      Gtk::Widget * widget;

      /** \brief Tabs are not copy-constructible.
       *
       *  Copy-constructing a tab could lead to confusion when it's deleted.
       */
      Tab(const Tab &);
    public:
      /** \brief Construct a new tab.
       *
       *  \param _type The type of the new tab.
       *  \param _label The label of the new tab.
       *  \param _xml  The XML object from which to take the widget
       *               of the new tab.
       *  \param widgetName  The name of the new tab's associated
       *                     widget within the given XML tree.
       */
      Tab(TabType _type, const Glib::ustring &_label,
          const Glib::RefPtr<Gnome::Glade::Xml> &_xml, const std::string &widgetName);
      virtual ~Tab();
      Glib::ustring get_label() { return label; }
      Gtk::Widget * get_label_widget() { return label_widget; }
      Gtk::Button * get_label_button() { return label_button; }
      void set_label(Glib::ustring);
      TabType get_type() { return type; }
      Gtk::Widget * get_widget() const { return widget; }
      const Glib::RefPtr<Gnome::Glade::Xml> &get_xml() { return xml; }

      /** \brief Invoked by the tab manager to determine the state of
       *  the status button.
       *
       *  \param visible    Set to true if the status button should be visible.
       *  \param sensitive  Set to true if the status button should be sensitive.
       *  \param label      Set to the new label of the status button.
       *  \param image      Set to the image of the status button or to NULL.
       *                    The caller takes possession of this pointer and
       *                    will destroy it.
       *
       *  The default implementation returns false, false, "", and NULL.
       */
      virtual void get_status_button(bool &visible,
				     bool &sensitive,
				     Glib::ustring &label,
				     Gtk::Widget *&image);

      /** \brief Invoked by the tab manager when the status button is clicked.
       *
       *  The default implementation does nothing.
       */
      virtual void status_button_clicked();

      /** \brief Emitted when the result of get_status_button() may have changed. */
      sigc::signal0<void> status_button_changed;

      /** \brief A signal invoked when the tab's "close" button is clicked.
       *
       *  The TabManager uses this to actually close the tab.
       */
      sigc::signal0<void> close_clicked;

      /** \brief A signal invoked when the tab is closed in the notebook. */
      sigc::signal0<void> closed;
  };

  /**
   * This is a custom widget that handles placement of tabs
   */
  class TabsManager : public Gtk::Notebook
  {
    private:
      /**
       * Gives the position for the next tab of given type
       * @param type type of tab
       * @return position of the next tab of this type
       */
      int next_position(TabType type);
      /**
       * Gives the number of tabs of given type
       * @param type type of tab
       * @return number of tabs of this type
       */
      int number_of(TabType type);

      /** Called when a page is removed from the notebook.
       *
       *  Technically this doesn't need to be a member, but
       *  it might be useful in some future expansions.
       */
      void page_removed(Gtk::Widget *widget, int page);

      void do_switch_page(GtkNotebookPage *page, guint page_idx);
    public:
      /**
       * Glade::Xml derived widget constructor.
       */
      TabsManager(BaseObjectType* cobject, const Glib::RefPtr<Gnome::Glade::Xml>& refGlade);
      /**
       * Appends a tab to the notebook
       * @param tab tab to append
       * @return position of the appended tab
       */
      int append_page(Tab &tab);
      /**
       * Remove a tab from the notebook
       * @param tab tab to remove
       */
      void remove_page(Tab &tab);

      /** \brief Get the currently active tab. */
      Tab *get_current_tab();

      /** \brief Emitted when a new tab is selected. */
      sigc::signal1<void, Tab *> tab_selected;
  };

}

#endif /* TAB_H_ */
