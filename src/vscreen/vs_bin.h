// vs_bin.h        -*-c++-*-
//
//  Generic stuff for a container that can only handle one child.

#ifndef VS_BIN_H
#define VS_BIN_H

#include "vs_passthrough.h"

#include <sigc++/connection.h>

class vs_bin:public vs_passthrough
{
  vs_widget_ref subwidget;

  // These are unfortunate necessities; when a widget is /removed/
  // (but not destroyed), it is necessary to delete the connections to
  // it.  :-(
  sigc::connection show_conn, hide_conn;

  // right now these just show or hide the bin itself
  void show_widget(const vs_widget_ref &w);
  void hide_widget(const vs_widget_ref &w);

  void show_widget_bare(vscreen_widget &w);
  void hide_widget_bare(vscreen_widget &w);

protected:
  vs_bin();

public:
  virtual ~vs_bin();

  void set_subwidget(const ref_ptr<vscreen_widget> &w);
  void set_subwidget(vscreen_widget &w)
  {
    set_subwidget(ref_ptr<vscreen_widget>(&w));
  }

  vs_widget_ref get_subwidget() {return subwidget;}

  void destroy();

  virtual void show_all();

  virtual void add_widget(const vs_widget_ref &w);
  virtual void rem_widget(const vs_widget_ref &w);

  vs_widget_ref get_focus();

  void paint(const style &st);
};

#endif
