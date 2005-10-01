// vs_transient.cc
//
// Copyright 2005 Daniel Burrows

#include "vs_transient.h"

#include <sigc++/functors/mem_fun.h>

vs_transient::vs_transient(const vs_widget_ref &w)
{
  set_subwidget(w);

  do_layout.connect(sigc::mem_fun(*this, &vs_transient::layout_me));
}

void vs_transient::layout_me()
{
  vs_widget_ref w=get_subwidget();

  if(w.valid())
    {
      if(w->get_visible())
	w->alloc_size(0, 0, getmaxx(), getmaxy());
      else
	w->alloc_size(0, 0, 0, 0);
    }
}

int vs_transient::width_request()
{
  vs_widget_ref w=get_subwidget();

  if(w.valid())
    return w->width_request();
  else
    return 0;
}

int vs_transient::height_request(int width)
{
  vs_widget_ref w=get_subwidget();

  if(w.valid())
    return w->height_request(width);
  else
    return 0;
}

bool vs_transient::focus_me()
{
  return true;
}

bool vs_transient::handle_char(chtype ch)
{
  destroy();
  return true;
}
