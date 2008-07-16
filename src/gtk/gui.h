#ifndef GUI_H_
#define GUI_H_

#undef OK
#include <gtkmm.h>
#include <libglademm/xml.h>

namespace gui
{

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
      /**
       * Glade::Xml derived widget constructor.
       */
      AptitudeWindow(BaseObjectType* cobject, const Glib::RefPtr<Gnome::Glade::Xml>& refGlade)/* : Gtk::Window(cobject)*/;
  };

  void main(int argc, char *argv[]);

}

#endif /*GUI_H_*/
