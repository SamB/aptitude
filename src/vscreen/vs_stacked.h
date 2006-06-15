// vs_stacked.h        -*-c++-*-
//
//  Manages a set of overlapping widgets, displaying them in a consistent
// order (it is possible to change the stacking order)
//
//  The size of the widget is unrelated to the sizes of its components.
//  (why? why not size it in a more flexible way?)

#ifndef VS_STACKED_H
#define VS_STACKED_H

#include "vs_passthrough.h"

#include <sigc++/connection.h>

class vs_stacked:public vs_passthrough
{
  // bleach, but we need somewhere to store the info on what the signals to
  // disconnect are :(
  struct child_info
  {
    vs_widget_ref w;

    sigc::connection shown_conn, hidden_conn;

    child_info(const vs_widget_ref &_w,
	       SigC::Connection &_shown_conn,
	       SigC::Connection &_hidden_conn)
      :w(_w), shown_conn(_shown_conn),
       hidden_conn(_hidden_conn)
    {
    }
  };

  typedef std::list<child_info> childlist;

  childlist children;

  int req_w, req_h;

  void layout_me();

  void hide_widget();
protected:
  void paint(const style &st);

  // The size passed in is used as a preferred size.  (what we get might be
  // larger or smaller)
  vs_stacked(int w, int h);
public:
  ~vs_stacked();

  void destroy();

  static ref_ptr<vs_stacked> create(int w=0, int h=0)
  {
    ref_ptr<vs_stacked> rval(new vs_stacked(w, h));
    rval->decref();
    return rval;
  }

  void add_widget(const vs_widget_ref &w);
  void rem_widget(const vs_widget_ref &w);
  void raise_widget(const vs_widget_ref &w);
  void lower_widget(const vs_widget_ref &w);

  void raise_widget_bare(vscreen_widget &w)
  {
    raise_widget(vs_widget_ref(&w));
  }
  void lower_widget_bare(vscreen_widget &w)
  {
    lower_widget(vs_widget_ref(&w));
  }

  void dispatch_mouse(short id, int x, int y, int z, mmask_t bstate);

  vs_widget_ref get_focus();

  void show_all();

  int width_request();
  int height_request(int w);
};

typedef ref_ptr<vs_stacked> vs_stacked_ref;

#endif
