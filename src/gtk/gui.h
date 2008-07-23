// -*-c++-*-

#ifndef GUI_H_
#define GUI_H_

#undef OK
#include <gtkmm.h>
#include <libglademm/xml.h>

#include <generic/apt/apt.h>

#include <sigc++/slot.h>

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
   * This is a list of packages actions.
   * TODO: This probably already exist. Find it.
   * FIXME: Description shouldn't be here.
   */
  enum PackagesAction
  {
    Install, Remove, Purge, Keep, Hold, Description
  };

  class guiOpProgress : public OpProgress
  { // must derive to read protected member..
    private:
      float sanitizePercentFraction(float percent);
    public:
      ~guiOpProgress();
      void Update();
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
      Gtk::Label * label_label;
      Gtk::Widget * label_widget;
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
      Gtk::Widget * get_label_widget() { return label_widget; }
      void set_label(Glib::ustring);
      TabType get_type() { return type; }
      Gtk::Widget * get_widget() const { return widget; }
      const Glib::RefPtr<Gnome::Glade::Xml> &get_xml() { return xml; }
  };

  class PackagesTab;
  class PackagesView;

  /**
   * The PackagesMarker marks packages belonging to a PackagesTab
   */
  class PackagesMarker
  {
    private:
      PackagesView * view;
      void dispatch(pkgCache::PkgIterator pkg, pkgCache::VerIterator ver, PackagesAction action);
      void callback(const Gtk::TreeModel::iterator& iter, PackagesAction action);
    public:
      /** \brief Construct a packages marker for tab.
       *
       *  \param tab The tab on which the marking takes place.
       */
      PackagesMarker(PackagesView * view);
      void select(PackagesAction action);
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
    public:
      /** \brief Construct a context menu for tab.
       *
       *  \param tab The tab who owns the context menu.
       *  \param marker The marker to use to execute the actions.
       */
    PackagesContextMenu(PackagesView * view);
    Gtk::Menu * get_menu() const { return pMenu; };
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

  class PackagesTreeView : public Gtk::TreeView
  {
    public:
      PackagesTreeView(BaseObjectType* cobject, const Glib::RefPtr<Gnome::Glade::Xml>& refGlade);
      bool on_button_press_event(GdkEventButton* event);
      sigc::signal<void, GdkEventButton*> signal_context_menu;
      sigc::signal<void> signal_selection;
  };

  class PackagesView
  {
  public:
    typedef sigc::slot3<Glib::RefPtr<Gtk::TreeModel>,
			PackagesColumns *,
			std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator> *,
			Glib::ustring> build_store_func;

    private:
      PackagesTreeView * treeview;
      PackagesColumns * packages_columns;
      Glib::RefPtr<Gtk::TreeModel> packages_store;
      std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator> * reverse_packages_store;
      PackagesContextMenu * context;
      PackagesMarker * marker;
      build_store_func build_store;
    public:
    PackagesView(build_store_func build_store,
		   Glib::RefPtr<Gnome::Glade::Xml> refGlade);
      void context_menu_handler(GdkEventButton * event);
      void refresh_packages_view(std::set<pkgCache::PkgIterator> changed_packages);
      void relimit_packages_view(Glib::ustring limit);
      void update_stores(Glib::RefPtr<Gtk::TreeModel> packages_store,
			 std::multimap<pkgCache::PkgIterator,
			               Gtk::TreeModel::iterator> * reverse_packages_store);
      sigc::signal<void, pkgCache::PkgIterator, pkgCache::VerIterator> signal_on_package_selection;
      PackagesTreeView * get_treeview() { return treeview; };
      PackagesColumns * get_packages_columns() { return packages_columns; };
      PackagesMarker * get_marker() { return marker; };
      Glib::RefPtr<Gtk::TreeModel> get_packages_store() { return packages_store; };
      std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator> * get_reverse_packages_store() { return reverse_packages_store; };
  };

  class PackagesTab : public Tab
  {
    private:
      PackagesView * pPackagesView;
      Gtk::TextView * pPackagesTextView;
      Gtk::Entry * pLimitEntry;
      Gtk::Button * pLimitButton;
    public:
      PackagesTab(const Glib::ustring &label);
      Glib::RefPtr<Gtk::ListStore> create_store();
      std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator> * create_reverse_store();
      void repopulate_model();
      static Glib::RefPtr<Gtk::TreeModel> build_store(PackagesColumns * packages_columns,
						      std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator> * reverse_packages_store,
						      Glib::ustring limit);
      void display_desc(pkgCache::PkgIterator pkg, pkgCache::VerIterator ver);
      PackagesView * get_packages_view() { return pPackagesView; };
  };

  // FIXME: Stopgap solution to the functor issue.
  /*void PackagesTab_populate_model(PackagesColumns * packages_columns,
      Glib::RefPtr<Gtk::ListStore> packages_store,
      std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator> * reverse_packages_store,
      Glib::ustring limit);*/

  // TODO: This needs to share more code with PackagesTab.
  //       A PreviewTab is really a PackagesTab with a TreeStore.
  class PreviewTab : public Tab
  {
    private:
      PackagesView * pPackagesView;
      Gtk::TextView * pPackagesTextView;
      Gtk::Entry * pLimitEntry;
      Gtk::Button * pLimitButton;
    public:
      PreviewTab(const Glib::ustring &label);
      Glib::RefPtr<Gtk::TreeStore> create_store();
      std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator> * create_reverse_store();
      void repopulate_model();
      static Glib::RefPtr<Gtk::TreeModel> build_store(PackagesColumns * packages_columns,
						      std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator> * reverse_packages_store,
						      Glib::ustring limit);
      void display_desc(pkgCache::PkgIterator pkg, pkgCache::VerIterator ver);
      PackagesView * get_packages_view() { return pPackagesView; };
  };

  // FIXME: Stopgap solution to the functor issue.
  /*void PreviewTab_populate_model(PackagesColumns * packages_columns,
      Glib::RefPtr<Gtk::TreeStore> packages_store,
      std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator> * reverse_packages_store,
      Glib::ustring limit);*/

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
      /**
       * Remove a tab from the notebook
       * @param tab tab to remove
       */
      void remove_page(Tab& tab);
  };

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
