// -*-c++-*-

#ifndef GUI_H_
#define GUI_H_

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
    Dashboard, Download, Packages, Info, Preview, Resolver
  };

  /**
   * A Tab contains a widget and some metadata for inserting into the notebook.
   */
  class Tab
  {
    private:
      Glib::ustring label;
      TabType type;
    protected:
      void set_type(TabType type);
    public:
      Gtk::Widget * pWidget;
      /**
       * Glade::Xml derived widget constructor.
       */
      Tab();
      void set_label(Glib::ustring label);
      Glib::ustring get_label();
      TabType get_type();
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
      Gtk::ToolButton * pToolButtonUpdate;
    public:
      Gtk::ProgressBar * pProgressBar;
      Gtk::Statusbar * pStatusBar;
      TabsManager * pNotebook;
      /**
       * Glade::Xml derived widget constructor.
       */
      AptitudeWindow(BaseObjectType* cobject, const Glib::RefPtr<Gnome::Glade::Xml>& refGlade)/* : Gtk::Window(cobject)*/;
  };

  void main(int argc, char *argv[]);

}

#endif /*GUI_H_*/
