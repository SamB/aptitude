// vs_radiogroup.cc

#include "vs_radiogroup.h"

#include "vs_togglebutton.h"

#include <sigc++/adaptors/bind.h>
#include <sigc++/functors/mem_fun.h>

vs_radiogroup::vs_radiogroup()
  :selected(items.max_size())
{
}

vs_radiogroup::~vs_radiogroup()
{
}

bool vs_radiogroup::selection_valid()
{
  return selected != items.max_size();
}

int vs_radiogroup::get_selected()
{
  return items[selected].id;
}

void vs_radiogroup::button_pressed(itemlist::size_type index)
{
  eassert(index<items.size());

  if(selected!=items.max_size())
    items[selected].b->set_checked(false);
  selected=index;

  if(index!=items.max_size())
    {
      items[index].b->set_checked(true);
      item_selected(items[index].id);
    }
}

void vs_radiogroup::add_button(const vs_togglebutton_ref &b, int id)
{
  eassert(id>=0);

  for(itemlist::iterator i=items.begin(); i!=items.end(); i++)
    eassert(i->b!=b);

  items.push_back(item(b, id,
		       b->destroyed.connect(sigc::bind(sigc::mem_fun(*this, &vs_radiogroup::rem_button_bare), b.weak_ref())),
		       b->pressed.connect(sigc::bind(sigc::mem_fun(*this, &vs_radiogroup::button_pressed), items.size()))));

  if(selected==items.max_size() || b->get_checked())
    button_pressed(items.size()-1);
}

void vs_radiogroup::rem_button(const vs_togglebutton_ref &b)
{
  for(itemlist::size_type i=0; i<items.size(); i++)
    if(items[i].b==b)
      {
	items[i].destroyed_conn.disconnect();
	items[i].pressed_conn.disconnect();
	if(selected==i)
	  {
	    if(i>0)
	      button_pressed(i-1);
	    else if(i+1<items.size())
	      button_pressed(i+1);
	    else
	      eassert(items.size() == 1);
	  }

	if(i==items.size()-1)
	  items.pop_back();
	else
	  {
	    items[i]=items[items.size()-1];
	    if(selected==items.size()-1)
	      selected=i;
	    items.pop_back();
	    items[i].pressed_conn.disconnect();
	    items[i].pressed_conn=items[i].b->pressed.connect(sigc::bind(sigc::mem_fun(*this, &vs_radiogroup::button_pressed), i));
	  }

	return;
      }
}

void vs_radiogroup::rem_button_bare(vs_togglebutton &b)
{
  rem_button(vs_togglebutton_ref(&b));
}

void vs_radiogroup::select(int id)
{
  for(itemlist::size_type i=0; i<items.size(); i++)
    if(items[i].id==id)
      {
	button_pressed(i);

	return;
      }

  // Er, no buttons match the given ID.
  abort();
}

void vs_radiogroup::destroy()
{
  delete this;
}
