// vs_size_box.cc
//
// Copyright 2004 Daniel Burrows

#include "vs_size_box.h"

#include <sigc++/functors/mem_fun.h>

#include <utility>

using namespace std;

vs_size_box::vs_size_box(size s, const vs_widget_ref &w):min_size(s)
{
  set_subwidget(w);
  set_opaque(false);

  do_layout.connect(sigc::mem_fun(*this, &vs_size_box::layout_me));
}

int vs_size_box::width_request()
{
  vs_widget_ref tmpref(this);

  vs_widget_ref child = get_subwidget();

  if(child.valid())
    return max(child->width_request(), min_size.w);
  else
    return min_size.w;
}

int vs_size_box::height_request(int w)
{
  vs_widget_ref tmpref(this);

  vs_widget_ref child = get_subwidget();

  if(child.valid())
    return max(child->height_request(w), min_size.h);
  else
    return min_size.h;
}

void vs_size_box::layout_me()
{
  vs_widget_ref tmpref(this);

  vs_widget_ref child = get_subwidget();

  if(child.valid())
    {
      if(child->get_visible())
	child->alloc_size(0, 0, getmaxx(), getmaxy());
      else
	child->alloc_size(0, 0, 0, 0);
    }
}
