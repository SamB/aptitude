// -*-c++-*-

#ifndef GUI_H_
#define GUI_H_

#undef OK
#include <gtkmm.h>
#include <libglademm/xml.h>

#include <generic/apt/apt.h>

namespace gui
{

  /**
   * This is a list of tab types.
   */
  enum TabType
  {
    Dashboard, Download, Packages, Info, Preview, Resolver
  };

  /**
   * A Tab contains a widget and some metadata for inserting into the notebook.
   */
  class Tab
  {
    private:
      TabType type;
      Glib::ustring label;
      Glib::RefPtr<Gnome::Glade::Xml> xml;
      Gtk::Widget * widget;

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
      Glib::ustring get_label() { return label; }
      TabType get_type() { return type; }
      Gtk::Widget * get_widget() const { return widget; }
      const Glib::RefPtr<Gnome::Glade::Xml> &get_xml() { return xml; }
  };

  class PackagesTab;

  /**
   * The PackagesMarker marks packages belonging to a PackagesTab
   */
  class PackagesMarker
  {
    private:
      PackagesTab * tab;
    public:
      /** \brief Construct a packages marker for tab.
       *
       *  \param tab The tab on which the marking takes place.
       */
      PackagesMarker(PackagesTab * tab);
      void install();
      void remove();
      void purge();
      void keep();
      void hold();
  };

  /**
   * The context menu for packages in PackagesTab
   */
  class PackagesContextMenu
  {
    private:
      Gtk::Menu * pMenu;
      Gtk::ImageMenuItem * pMenuInstall;
      Gtk::ImageMenuItem * pMenuRemove;
      Gtk::ImageMenuItem * pMenuPurge;
      Gtk::ImageMenuItem * pMenuKeep;
      Gtk::ImageMenuItem * pMenuHold;
      PackagesTab * tab;
      PackagesMarker * marker;
    public:
      /** \brief Construct a context menu for tab.
       *
       *  \param tab The tab who owns the context menu.
       *  \param marker The marker to use to execute the actions.
       */
    PackagesContextMenu(PackagesTab * tab, PackagesMarker * marker);
    Gtk::Menu * get_menu() const { return pMenu; }
  };

  class PackagesColumns : public Gtk::TreeModel::ColumnRecord
  {
    public:
      Gtk::TreeModelColumn<pkgCache::PkgIterator> PkgIterator;
      Gtk::TreeModelColumn<pkgCache::VerIterator> VerIterator;
      Gtk::TreeModelColumn<Glib::ustring> CurrentStatus;
      Gtk::TreeModelColumn<Glib::ustring> SelectedStatus;
      Gtk::TreeModelColumn<Glib::ustring> Name;
      Gtk::TreeModelColumn<Glib::ustring> Section;
      Gtk::TreeModelColumn<Glib::ustring> Version;

      PackagesColumns();
  };

  class PackagesTab : public Tab
  {
    public:
      Glib::RefPtr<Gtk::ListStore> packages_store;
      PackagesColumns packages_columns;
      Gtk::TreeView * pPackagesTreeView;
      Gtk::TextView * pPackagesTextView;
      PackagesContextMenu * pPackagesContextMenu;
      PackagesMarker * pPackagesMarker;
      PackagesTab(const Glib::ustring &label);
      void createstore();
      void on_button_press_event(GdkEventButton* event);
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
      int append_page(Tab& tab);
  };

  /**
   * This is the main Aptitude custom window widget.
   */
  class AptitudeWindow : public Gtk::Window
  {
    private:
      Gtk::ToolButton * pToolButtonDashboard;
      Gtk::ToolButton * pToolButtonPackages;
      Gtk::ToolButton * pToolButtonUpdate;
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
