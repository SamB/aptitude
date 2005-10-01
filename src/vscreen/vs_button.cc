// vs_button.cc

#include "vs_button.h"

#include "fragment.h"
#include "fragment_cache.h"
#include "vscreen.h"

#include "config/keybindings.h"

#include <sigc++/functors/mem_fun.h>

vs_button::vs_button(fragment *_label)
  :label(new fragment_cache(_label))
{
  focussed.connect(sigc::mem_fun(*this, &vs_button::accept_focus));
  unfocussed.connect(sigc::mem_fun(*this, &vs_button::lose_focus));
}

vs_button::vs_button(const std::wstring &_label)
  :label(new fragment_cache(text_fragment(_label)))
{
  focussed.connect(sigc::mem_fun(*this, &vs_button::accept_focus));
  unfocussed.connect(sigc::mem_fun(*this, &vs_button::lose_focus));
}

vs_button::vs_button(const std::string &_label)
  :label(new fragment_cache(text_fragment(_label)))
{
  focussed.connect(sigc::mem_fun(*this, &vs_button::accept_focus));
  unfocussed.connect(sigc::mem_fun(*this, &vs_button::lose_focus));
}

vs_button::~vs_button()
{
  delete label;
}

bool vs_button::focus_me()
{
  return true;
}

bool vs_button::get_cursorvisible()
{
  return get_isfocussed();
}

point vs_button::get_cursorloc()
{
  return point(0,0);
}

void vs_button::accept_focus()
{
  vscreen_update();
}

void vs_button::lose_focus()
{
  vscreen_update();
}

void vs_button::paint(const style &st)
{
  vs_widget_ref tmpref(this);

  size_t labelw=getmaxx()>=4?getmaxx()-4:0;

  const style my_style=
    get_isfocussed()?st+style_attrs_flip(A_REVERSE):st;

  apply_style(my_style);

  fragment_contents lines=label->layout(labelw, labelw, my_style);

  // TODO: create a "bracebox" fragment that places left&right braces
  // automatically.
  for(size_t i=0; i<lines.size(); ++i)
    {
      move(i, 0);

      if(lines.size() == 1)
	add_wch(L'[');
      else if(i==0)
	add_wch(WACS_ULCORNER);
      else if(i+1==lines.size())
	add_wch(WACS_LLCORNER);
      else
	add_wch(WACS_VLINE);

      add_wch(L' ');

      const fragment_line &l=lines[i];
      addstr(l);
      int w=2+l.width();

      while(w+1<getmaxx())
	{
	  ++w;
	  add_wch(L' ');
	}

      if(lines.size() == 1)
	add_wch(L']');
      else if(i==0)
	add_wch(WACS_URCORNER);
      else if(i+1==lines.size())
	add_wch(WACS_LRCORNER);
      else
	add_wch(WACS_VLINE);
    }
}

void vs_button::dispatch_mouse(short id, int x, int y, int z, mmask_t bmask)
{
  vs_widget_ref tmpref(this);

  if(bmask & (BUTTON1_CLICKED | BUTTON2_CLICKED |
	      BUTTON3_CLICKED | BUTTON4_CLICKED |
	      BUTTON1_RELEASED | BUTTON2_RELEASED |
	      BUTTON3_RELEASED | BUTTON4_RELEASED))
    pressed();
}

bool vs_button::handle_key(const key &k)
{
  vs_widget_ref tmpref(this);

  if(global_bindings.key_matches(k, "PushButton") ||
     global_bindings.key_matches(k, "Confirm"))
    {
      pressed();
      return true;
    }
  else
    return vscreen_widget::handle_key(k);
}

int vs_button::width_request()
{
  return label->max_width(0, 0)+4;
}

int vs_button::height_request(int width)
{
  size_t label_width=(width>=4)?width-4:0;

  return label->layout(label_width, label_width, style()).size();
}
