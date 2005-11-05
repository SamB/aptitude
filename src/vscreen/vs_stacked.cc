// vs_stacked.cc

#include "vscreen.h"
#include "vs_stacked.h"

#include <sigc++/adaptors/bind.h>
#include <sigc++/functors/mem_fun.h>

vs_stacked::vs_stacked(int w, int h)
  :req_w(w), req_h(h)
{
  do_layout.connect(sigc::mem_fun(*this, &vs_stacked::layout_me));
}

vs_stacked::~vs_stacked()
{
  eassert(children.empty());
}

void vs_stacked::destroy()
{
  vs_widget_ref tmpref(this);

  while(!children.empty())
    children.front().w->destroy();

  vs_passthrough::destroy();
}

void vs_stacked::add_widget(const vs_widget_ref &w)
{
  vs_widget_ref tmpref(this);

  sigc::connection shown_conn=w->shown_sig.connect(sigc::bind(sigc::mem_fun(*this, &vs_stacked::raise_widget_bare), w.weak_ref()));
  sigc::connection hidden_conn=w->hidden_sig.connect(sigc::mem_fun(*this, &vs_stacked::hide_widget));

  defocus();

  children.push_back(child_info(w, shown_conn, hidden_conn));

  w->set_owner(this);

  refocus();

  if(w->get_visible())
    vscreen_update();
}

void vs_stacked::hide_widget()
{
  vscreen_update();
}

void vs_stacked::rem_widget(const vs_widget_ref &wBare)
{
  vs_widget_ref tmpref(this);

  vs_widget_ref w(wBare);

  for(childlist::iterator i=children.begin();
      i!=children.end();
      i++)
    {
      if(i->w==w)
	{
	  i->shown_conn.disconnect();
	  i->hidden_conn.disconnect();

	  children.erase(i);
	  w->set_owner(NULL);
	  if(w->get_visible())
	    vscreen_update();

	  w->unfocussed();
	  refocus();

	  return;
	}
    }
}

void vs_stacked::raise_widget(const vs_widget_ref &w)
{
  vs_widget_ref tmpref(this);

  for(childlist::iterator i=children.begin();
      i!=children.end();
      i++)
    if(i->w==w)
      {
	defocus();

	children.push_front(*i);
	children.erase(i);

	refocus();

	vscreen_update();
	return;
      }
}

void vs_stacked::lower_widget(const vs_widget_ref &w)
{
  vs_widget_ref tmpref(this);

  for(childlist::iterator i=children.begin();
      i!=children.end();
      i++)
    if(i->w==w)
      {
	defocus();

	children.push_back(*i);
	children.erase(i);

	refocus();

	vscreen_update();
	return;
      }
}

void vs_stacked::paint(const style &st)
{
  vs_widget_ref tmpref(this);

  // Go through the children back-to-front (reverse order)
  for(childlist::reverse_iterator i=children.rbegin();
      i!=children.rend();
      i++)
    if(i->w->get_visible())
      i->w->display(st);
}

void vs_stacked::dispatch_mouse(short id, int x, int y, int z, mmask_t bstate)
{
  vs_widget_ref tmpref(this);

  for(childlist::iterator i=children.begin();
      i!=children.end();
      ++i)
    if(i->w->get_visible() && i->w->enclose(y, x))
      {
	i->w->dispatch_mouse(id, x-i->w->get_startx(), y-i->w->get_starty(),
			     z, bstate);
	return;
      }
}

vs_widget_ref vs_stacked::get_focus()
{
  vs_widget_ref tmpref(this);

  for(childlist::iterator i=children.begin();
      i!=children.end();
      i++)
    if(i->w->get_visible() && i->w->focus_me())
      return i->w;
    else
      return NULL;

  return NULL;
}

void vs_stacked::show_all()
{
  vs_widget_ref tmpref(this);

  defocus();

  for(childlist::iterator i=children.begin();
      i!=children.end();
      i++)
    {
      i->shown_conn.disconnect();

      i->w->show_all();

      i->shown_conn=i->w->shown_sig.connect(sigc::bind(sigc::mem_fun(*this, &vs_stacked::raise_widget_bare), i->w.weak_ref()));
    }

  refocus();
}

int vs_stacked::width_request()
{
  return req_w;
}

int vs_stacked::height_request(int w)
{
  return req_h;
}

void vs_stacked::layout_me()
{
  vs_widget_ref tmpref(this);

  for(childlist::iterator i=children.begin(); i!=children.end(); i++)
    i->w->alloc_size(0, 0, getmaxx(), getmaxy());
}
