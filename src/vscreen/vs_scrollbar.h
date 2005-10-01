// vs_scrollbar.h                         -*-c++-*-
//
//  Guess. >=)
//
//  Scrollbars in text-mode are, of course, only a visual effect, but they're
// useful nonetheless and a requested feature..

#ifndef VS_SCROLLBAR_H
#define VS_SCROLLBAR_H

#include "vscreen_widget.h"

class vs_scrollbar:public vscreen_widget
{
public:
  enum direction {HORIZONTAL, VERTICAL};

private:
  direction dir;

  int max, val;
  // The current slider maximum and value (FIXME: use floats?)

  /** Get the current X or Y location of the slider in the widget. 
   *
   *  \return the slider location, or -1 if it is not visible.
   */
  int get_slider();
protected:
  vs_scrollbar(direction _dir, int _val, int _max)
    :dir(_dir), max(_max), val(_val) {}

  vs_scrollbar(direction _dir)
    :dir(_dir), max(0), val(0) {}
public:
  static
  ref_ptr<vs_scrollbar> create(direction dir, int val, int max)
  {
    ref_ptr<vs_scrollbar> rval(new vs_scrollbar(dir, val, max));
    rval->decref();
    return rval;
  }

  static
  ref_ptr<vs_scrollbar> create(direction dir)
  {
    ref_ptr<vs_scrollbar> rval(new vs_scrollbar(dir));
    rval->decref();
    return rval;
  }

  void paint(const style &st);

  int width_request();
  int height_request(int w);

  bool get_cursorvisible();
  point get_cursorloc();
  void dispatch_mouse(short id, int x, int y, int z, mmask_t bstate);

  void set_slider(int newval, int newmax);

  /** This signal is emitted if the user "pages up" or "pages down"
   *  via the scrollbar.  Its argument is \b true for a "page up" and
   *  \b false for a "page down".
   */
  sigc::signal1<void, bool> scrollbar_interaction;
};

typedef ref_ptr<vs_scrollbar> vs_scrollbar_ref;

#endif
