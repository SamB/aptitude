// vs_scrollbar.cc			-*-c++-*-

#include "vs_scrollbar.h"

#include <vscreen.h>

int vs_scrollbar::get_slider()
{
  vs_widget_ref tmpref(this);

  int width = dir==HORIZONTAL?getmaxx():getmaxy();
  return max==0?-1:(width-1)*val/max;
}

void vs_scrollbar::paint(const style &st)
{
  vs_widget_ref tmpref(this);

  if(dir==HORIZONTAL)
    {
      int width=getmaxx();
      int thumbloc=get_slider();

      for(int x=0; x<width; x++)
	if(x==thumbloc)
	  mvadd_wch(0, x, L'#');
	else
	  mvadd_wch(0, x, WACS_CKBOARD);
    }
  else
    {
      int height=getmaxy();
      int thumbloc=get_slider();

      for(int y=0; y<height; y++)
	if(y==thumbloc)
	  mvadd_wch(y, 0, L'#');
	else
	  mvadd_wch(y, 0, WACS_CKBOARD);
    }
}

int vs_scrollbar::width_request()
{
  return 1;
}

int vs_scrollbar::height_request(int w)
{
  return 1;
}

void vs_scrollbar::set_slider(int newval, int newmax)
{
  if(max!=newmax || val!=newval)
    {
      max=newmax;
      val=newval;

      vscreen_update();
    }
}

bool vs_scrollbar::get_cursorvisible()
{
  return false;
}

point vs_scrollbar::get_cursorloc()
{
  return point(0, 0);
}

void vs_scrollbar::dispatch_mouse(short id,
				  int x, int y, int z,
				  mmask_t bstate)
{
  vs_widget_ref tmpref(this);

  int slider_loc=get_slider();
  int mloc = dir==HORIZONTAL?x:y;

  if(slider_loc!=-1)
    {
      if(mloc>slider_loc)
	scrollbar_interaction(false);
      else
	scrollbar_interaction(true);
    }
}
