#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <vector>

#include "aptitude.h"

#undef OK
#include <gtkmm.h>
#include <libglademm/xml.h>

namespace gui
{
  //This is a list of global and unique base widgets and other related stuff
  Glib::RefPtr<Gnome::Glade::Xml> refXml;

 /**
  * This is a list of tab types.
  */
  enum TabType
  {
    DashboardTab, DownloadTab, PackagesTab, InfoTab, PreviewTab, ResolverTab
  };

  /**
   * A Tab is a widget with some metadata for inserting into the notebook.
   */
  class Tab : public Gtk::Widget
  {
    private:
      Glib::ustring label;
      TabType type;
    public:
      /**
       * Glade::Xml derived widget constructor.
       */
      Tab(BaseObjectType* cobject, const Glib::RefPtr<Gnome::Glade::Xml>& refGlade) : Gtk::Widget(cobject)
      {
        label = "";
        type = DashboardTab;
      }
      void set_metadata(Glib::ustring new_label, TabType new_type)
      {
        label = new_label;
        type = new_type;
      }
      Glib::ustring get_label()
      {
        return label;
      }
      TabType get_type()
      {
        return type;
      }
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
      int next_position(TabType type)
      {
        // TODO: implement something more elaborate and workflow-wise intuitive
        return get_n_pages();
      }
      /**
       * Gives the number of tabs of given type
       * @param type type of tab
       * @return number of tabs of this type
       */
      int number_of(TabType type)
      {
        return 0;
      }
    public:
      /**
       * Glade::Xml derived widget constructor.
       */
      TabsManager(BaseObjectType* cobject, const Glib::RefPtr<Gnome::Glade::Xml>& refGlade) : Gtk::Notebook(cobject)
      {
        ;;
      }
      /**
       * Appends a tab to the notebook
       * @param tab tab to append
       * @return position of the appended tab
       */
      int append_page(Tab& tab)
      {
        int rval;
        switch (tab.get_type())
        {
        case DashboardTab:
          // No more than one DashboardTab at once
          if (number_of(DashboardTab) == 0)
          {
            rval = insert_page(tab, _("Dashboard"), 0);
          }
          break;
        // TODO: handle other kinds of tabs
        default:
          rval = insert_page(tab, "generic tab: " + tab.get_label(), next_position(tab.get_type()));
        }
        return rval;
      }
  };

  TabsManager * pMainNotebook;

  /**
   * This is the main Aptitude custom window widget.
   */
  class AptitudeWindow : public Gtk::Window
  {
    public:
      Gtk::ToolButton * pToolButtonDashboard;
      /**
       * Glade::Xml derived widget constructor.
       */
      AptitudeWindow(BaseObjectType* cobject, const Glib::RefPtr<Gnome::Glade::Xml>& refGlade) : Gtk::Window(cobject)
      {
        refGlade->get_widget("main_toolbutton_dashboard", pToolButtonDashboard);
      }
  };

  AptitudeWindow * pMainWindow;

  /**
   * Adds a dashboard tab to the interface.
   * TODO: Get this one out of here!
   */
  void tab_add_dashboard()
  {
    Tab * dashboard;
    refXml->get_widget_derived("label1", dashboard);
    dashboard->set_metadata("truc", DashboardTab);
    pMainNotebook->set_current_page(pMainNotebook->append_page(*dashboard));

  }

  void main(int argc, char *argv[])
  {
    Gtk::Main kit(argc, argv);
    // Use the basename of argv0 to find the Glade file.
    // TODO: note that the .glade file will ultimately
    //       go to /usr/share/aptitude/glade or something,
    //       so a more general solution will be needed.
    std::string argv0(argv[0]);
    std::string argv0_path;
    std::string::size_type last_slash = argv0.rfind('/');
    if(last_slash != std::string::npos)
      {
        while(last_slash > 0 && argv0[last_slash - 1] == '/')
          --last_slash;
        argv0_path = std::string(argv0, 0, last_slash);
      }
    else
      argv0_path = '.';

    const std::string glade_main_file = argv0_path + "/gtk/ui-main.glade";

    //Loading the .glade file and widgets
    refXml = Gnome::Glade::Xml::create(glade_main_file);
    refXml->get_widget_derived("main_window", pMainWindow);
    pMainWindow->pToolButtonDashboard->signal_clicked().connect(&tab_add_dashboard);

    refXml->get_widget_derived("main_notebook", pMainNotebook);

    //This is the loop
    Gtk::Main::run(*pMainWindow);
  }
}
