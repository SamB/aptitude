// vs_bin.cc

#include "vs_bin.h"

#include "vscreen.h"

#include <sigc++/adaptors/bind.h>
#include <sigc++/functors/mem_fun.h>

vs_bin::vs_bin()
  :vs_passthrough(), subwidget(NULL)
{
}

vs_bin::~vs_bin()
{
  if(subwidget.valid())
    set_subwidget(NULL);
}

vs_widget_ref vs_bin::get_focus()
{
  vs_widget_ref tmpref(this);

  vs_widget_ref w = subwidget;

  if(w.valid() && w->get_visible())
    return w;
  else
    return NULL;
}

void vs_bin::set_subwidget(const vs_widget_ref &w)
{
  vs_widget_ref tmpref(this);

  if(subwidget.valid())
    {
      subwidget->set_owner(NULL);
      subwidget->unfocussed();
      subwidget=NULL;
      show_conn.disconnect();
      hide_conn.disconnect();
    }

  subwidget = w;

  if(w.valid())
    {
      show_conn = w->shown_sig.connect(sigc::bind(sigc::mem_fun(*this, &vs_bin::show_widget_bare), w.weak_ref()));
      hide_conn = w->hidden_sig.connect(sigc::bind(sigc::mem_fun(*this, &vs_bin::hide_widget_bare), w.weak_ref()));
      w->set_owner(this);
      if(get_isfocussed())
	w->focussed();
    }

  vscreen_queuelayout();
}

void vs_bin::destroy()
{
  vs_widget_ref tmpref(this);

  if(subwidget.valid())
    subwidget->destroy();
  eassert(!subwidget.valid());

  vs_container::destroy();
}

void vs_bin::add_widget(const vs_widget_ref &w)
{
  vs_widget_ref tmpref(this);

  eassert(!subwidget.valid());
  eassert(w.valid());

  set_subwidget(w);

  // I assume that we're hidden right now.
  if(w->get_visible())
    show();

  if(get_isfocussed())
    w->focussed();
}

void vs_bin::rem_widget(const vs_widget_ref &w)
{
  vs_widget_ref tmpref(this);

  eassert(w == subwidget);
  set_subwidget(NULL);

  if(get_visible())
    hide();

  if(get_isfocussed())
    w->unfocussed();
}

void vs_bin::show_all()
{
  vs_widget_ref tmpref(this);

  if(subwidget.valid())
    subwidget->show_all();

  show();
}

void vs_bin::show_widget(const vs_widget_ref &w)
{
  vs_widget_ref tmpref(this);

  eassert(w==subwidget);

  show();
}

void vs_bin::show_widget_bare(vscreen_widget &w)
{
  vs_widget_ref tmpref(this);

  show_widget(vs_widget_ref(&w));
}

void vs_bin::hide_widget(const vs_widget_ref &w)
{
  vs_widget_ref tmpref(this);

  eassert(w==subwidget);
  hide();
}

void vs_bin::hide_widget_bare(vscreen_widget &w)
{
  vs_widget_ref tmpref(this);

  hide_widget(vs_widget_ref(&w));
}

void vs_bin::paint(const style &st)
{
  vs_widget_ref tmpref(this);

  if(subwidget.valid() && subwidget->get_visible())
    subwidget->display(st);
}
