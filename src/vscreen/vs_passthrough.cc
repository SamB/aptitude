// vs_passthrough.cc

#include "vs_passthrough.h"

#include <sigc++/functors/mem_fun.h>

vs_passthrough::vs_passthrough()
{
  focussed.connect(sigc::mem_fun(*this, &vs_passthrough::gained_focus));
  unfocussed.connect(sigc::mem_fun(*this, &vs_passthrough::lost_focus));
}

vs_widget_ref vs_passthrough::get_active_widget()
{
  return get_focus();
}

void vs_passthrough::defocus()
{
  lost_focus();
}

void vs_passthrough::refocus()
{
  gained_focus();
}

void vs_passthrough::gained_focus()
{
  vs_widget_ref tmpref(this);

  vs_widget_ref w = get_focus();

  if(w.valid())
    w->focussed();
}

void vs_passthrough::lost_focus()
{
  vs_widget_ref tmpref(this);

  vs_widget_ref w = get_focus();

  if(w.valid())
    w->unfocussed();
}

bool vs_passthrough::focus_me()
{
  vs_widget_ref tmpref(this);

  vs_widget_ref w = get_focus();

  if(w.valid() && w->focus_me())
    return true;
  else
    return vs_container::focus_me();
}

bool vs_passthrough::get_cursorvisible()
{
  vs_widget_ref tmpref(this);

  vs_widget_ref w = get_focus();

  return w.valid() && w->get_cursorvisible();
}

point vs_passthrough::get_cursorloc()
{
  vs_widget_ref tmpref(this);

  vs_widget_ref w = get_focus();

  if(w.valid())
    {
      point p=w->get_cursorloc();
      p.x+=w->get_startx();
      p.y+=w->get_starty();

      return p;
    }
  else
    return point(0, 0);
}

bool vs_passthrough::handle_key(const key &k)
{
  vs_widget_ref tmpref(this);

  vs_widget_ref w = get_focus();

  if(w.valid() && w->get_visible() && w->focus_me())
    return w->dispatch_key(k) || vs_container::handle_key(k);
  else
    return vs_container::handle_key(k);
}

void vs_passthrough::dispatch_mouse(short id, int x, int y, int z,
				    mmask_t bstate)
{
  vs_widget_ref tmpref(this);

  vs_widget_ref w = get_focus();

  if(w.valid() && w->get_visible())
    w->dispatch_mouse(id, x-w->get_startx(), y-w->get_starty(), z, bstate);
}
