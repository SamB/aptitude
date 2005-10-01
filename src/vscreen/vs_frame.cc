// vs_frame.cc

#include "vs_frame.h"

#include "config/colors.h"

#include <sigc++/functors/mem_fun.h>

vs_frame::vs_frame(const vs_widget_ref &w)
  :vs_bin()
{
  set_subwidget(w);

  do_layout.connect(sigc::mem_fun(*this, &vs_frame::layout_me));
}

int vs_frame::width_request()
{
  vs_widget_ref tmpref(this);

  vs_widget_ref subwidget = get_subwidget();

  if(subwidget.valid() && subwidget->get_visible())
    return subwidget->width_request()+2;
  else
    return 2;
}

int vs_frame::height_request(int width)
{
  vs_widget_ref tmpref(this);

  if(width<2)
    return 0;
  else
    {
      vs_widget_ref subwidget = get_subwidget();

      if(subwidget.valid() && subwidget->get_visible())
	return subwidget->height_request(width-2)+2;
      else
	return 2;
    }
}

void vs_frame::layout_me()
{
  vs_widget_ref tmpref(this);

  vs_widget_ref subwidget = get_subwidget();

  if(subwidget.valid())
    {
      if(subwidget->get_visible())
	subwidget->alloc_size(1, 1, getmaxx()-2, getmaxy()-2);
      else
	subwidget->alloc_size(0, 0, 0, 0);
    }
}

void vs_frame::paint(const style &st)
{
  vs_widget_ref tmpref(this);

  border(0,0,0,0,0,0,0,0);

  vs_widget_ref subwidget = get_subwidget();

  if(subwidget.valid() && subwidget->get_visible())
    subwidget->display(st);
}
