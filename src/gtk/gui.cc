#include "gui.h"

#undef OK
#include <gtkmm.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif


using namespace std;

void gui_init()
{
  Gtk::Main kit();
  Gtk::Window window;
  Gtk::Main::run(window);
}

void gui_main()
{
  //
}
