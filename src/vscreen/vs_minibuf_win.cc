// vs_minibuf_win.cc
//
//  Copyright 2000-2005 Daniel Burrows
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; see the file COPYING.  If not, write to
//  the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//  Boston, MA 02111-1307, USA.

#include "vs_minibuf_win.h"
#include "vs_label.h"
#include "vs_multiplex.h"

#include <sigc++/adaptors/bind.h>
#include <sigc++/functors/mem_fun.h>

using namespace std;

vs_minibuf_win::vs_minibuf_win()
  :vs_passthrough(), main_widget(NULL)
{
  do_layout.connect(sigc::mem_fun(*this, &vs_minibuf_win::layout_me));

  status=vs_multiplex::create();
  status_lbl=vs_label::create("");
  status_lbl->set_bg_style(get_style("Status"));
  status->add_widget(status_lbl);
  header=vs_label::create("");
  header->set_bg_style(get_style("Header"));

  status->set_owner(this);
  header->set_owner(this);
  status_lbl->show();
  status->show();
  header->show();
}

vs_minibuf_win::~vs_minibuf_win()
{
}

void vs_minibuf_win::destroy()
{
  vs_widget_ref tmpref(this);

  if(main_widget.valid())
    main_widget->destroy();
  eassert(!main_widget.valid());

  header->destroy();
  status->destroy();

  eassert(!header.valid());
  eassert(!status.valid());

  vs_container::destroy();
}

void vs_minibuf_win::set_main_widget(const vs_widget_ref &w)
{
  vs_widget_ref tmpref(this);

  defocus();

  if(main_widget.valid())
    {
      main_destroy_conn.disconnect();
      main_widget->set_owner(NULL);
    }

  main_widget=w;

  if(main_widget.valid())
    {
      main_widget->set_owner(this);
      main_destroy_conn=main_widget->destroyed.connect(sigc::bind(sigc::mem_fun(*this, &vs_minibuf_win::set_main_widget), (vscreen_widget *) NULL));
    }
  refocus();

  vscreen_queuelayout();
  vscreen_update();
}

int vs_minibuf_win::width_request()
{
  vs_widget_ref tmpref(this);

  int w=0;

  if(status.valid())
    w=max(w, status->width_request());

  if(header.valid())
    w=max(w, header->width_request());

  if(main_widget.valid())
    w=max(w, main_widget->width_request());

  return w;
}

int vs_minibuf_win::height_request(int w)
{
  vs_widget_ref tmpref(this);

  int h=2;

  if(main_widget.valid())
    h=max(h, main_widget->height_request(w));
  return h;
}

void vs_minibuf_win::layout_me()
{
  vs_widget_ref tmpref(this);

  if(header.valid())
    header->alloc_size(0, 0, getmaxx(), 1);

  if(getmaxy()>1)
    {
      if(getmaxy()>2 && main_widget.valid())
	main_widget->alloc_size(0, 1, getmaxx(), getmaxy()-2);

      if(status.valid())
	status->alloc_size(0, getmaxy()-1, getmaxx(), 1);
    }
}

void vs_minibuf_win::paint(const style &st)
{
  vs_widget_ref tmpref(this);

  if(main_widget.valid() && main_widget->get_visible())
    main_widget->display(st);

  if(status.valid())
    status->display(st);
  if(header.valid())
    header->display(st);
}

void vs_minibuf_win::set_header(string new_header)
{
  vs_widget_ref tmpref(this);

  header->set_text(new_header);
}

void vs_minibuf_win::set_status(string new_status)
{
  vs_widget_ref tmpref(this);

  status_lbl->set_text(new_status);
}

void vs_minibuf_win::add_widget(const vs_widget_ref &widget)
{
  vs_widget_ref tmpref(this);

  defocus();
  status->add_widget(widget);
  refocus();
}

void vs_minibuf_win::rem_widget(const vs_widget_ref &widget)
{
  vs_widget_ref tmpref(this);

  eassert(widget.valid());

  if(widget == header)
    {
      header->set_owner(NULL);
      header = NULL;
    }
  else if(widget == status)
    {
      status->set_owner(NULL);
      status = NULL;
    }
  else if(widget == main_widget)
    {
      main_widget->set_owner(NULL);
      main_widget = NULL;
    }
  else
    {
      defocus();
      status->rem_widget(widget);
      refocus();
    }
}

void vs_minibuf_win::show_all()
{
  vs_widget_ref tmpref(this);

  if(main_widget.valid())
    main_widget->show_all();
  status->show();
  header->show();
}

vs_widget_ref vs_minibuf_win::get_focus()
{
  vs_widget_ref tmpref(this);

  if(status.valid() && status->focus_me())
    return status;
  else if(main_widget.valid() && main_widget->get_visible() && main_widget->focus_me())
    return main_widget;
  else
    return NULL;
}

void vs_minibuf_win::display_error(string err)
{
  add_widget(vs_transientlabel::create(err, get_style("Error")));
}
