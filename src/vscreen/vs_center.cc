// vs_center.cc

#include "vs_center.h"

#include <sigc++/functors/mem_fun.h>

vs_center::vs_center(const vs_widget_ref &w)
{
  set_subwidget(w);
  set_opaque(false);

  do_layout.connect(sigc::mem_fun(*this, &vs_center::layout_me));
}

int vs_center::width_request()
{
  vs_widget_ref tmpref(this);

  vs_widget_ref subwidget = get_subwidget();

  if(subwidget.valid() && subwidget->get_visible())
    return subwidget->width_request();
  else
    return 0;
}

int vs_center::height_request(int width)
{
  vs_widget_ref tmpref(this);

  vs_widget_ref subwidget = get_subwidget();

  if(subwidget.valid() && subwidget->get_visible())
    return subwidget->height_request(width);
  else
    return 0;
}

void vs_center::layout_me()
{
  vs_widget_ref tmpref(this);

  vs_widget_ref child=get_subwidget();

  if(child.valid())
    {
      if(child->get_visible())
	{
	  int child_w=child->width_request();
	  if(child_w>getmaxx())
	    child_w=getmaxx();

	  int child_h=child->height_request(child_w);
	  if(child_h>getmaxy())
	    child_h=getmaxy();
	  child->alloc_size((getmaxx()-child_w)/2, (getmaxy()-child_h)/2, child_w, child_h);
	}
      else
	child->alloc_size(0, 0, 0, 0);
    }
}
