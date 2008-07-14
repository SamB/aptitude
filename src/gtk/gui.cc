#include "gui.h"

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#undef OK
#include <gtkmm.h>
#include <libglademm/xml.h>

namespace gui
{
  //This is a list of global and unique widgets
  Gtk::Window * pMainWindow;

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
    Glib::RefPtr<Gnome::Glade::Xml> refXml = Gnome::Glade::Xml::create(glade_main_file);
    refXml->get_widget("main_window", pMainWindow);
    Gtk::Main::run(*pMainWindow);
  }
}
