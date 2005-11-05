// vscreen_widget.cc

#include "vscreen_widget.h"
#include "vs_container.h"

// needed for queuelayout:
#include "vscreen.h"

#include <set>

#include <sigc++/adaptors/bind.h>
#include <sigc++/functors/mem_fun.h>

#include <config/colors.h>
#include <config/keybindings.h>

#include <generic/util/eassert.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

using namespace std;

// Queues of things that Need To Be Done
static list<vscreen_widget *> toresize;

vscreen_widget::vscreen_widget()
  :win(NULL),
   timeout_value(0),
   owner(NULL),
   geom(0,0,0,0),
   refcount(1),
   visible(false),
   isfocussed(false),
   pre_display_erase(true),
   is_destroyed(false)
{
  focussed.connect(sigc::bind(sigc::mem_fun(*this, &vscreen_widget::set_isfocussed), true));
  unfocussed.connect(sigc::bind(sigc::mem_fun(*this, &vscreen_widget::set_isfocussed), false));
}

vscreen_widget::~vscreen_widget()
{
  eassert(!owner);
  eassert(is_destroyed);
}

void vscreen_widget::set_bg_style(const style &new_style)
{
  bg_style=new_style;
}

void vscreen_widget::apply_style(const style &st)
{
  bkgdset(st.get_attrs());
  attrset(st.get_attrs());
}

void vscreen_widget::set_isfocussed(bool _isfocussed)
{
  isfocussed=_isfocussed;
}

void vscreen_widget::set_owner_window(cwindow _win, int x, int y, int w, int h)
{
  vs_widget_ref tmpref(this);

  if(_win)
    {
      geom.x=x;
      geom.y=y;
      geom.w=w;
      geom.h=h;

      if(geom.h==0 || geom.w==0)
	win=NULL;
      else
	{
	  eassert(!is_destroyed);

	  win=_win.derwin(geom.h,
			  geom.w,
			  geom.y,
			  geom.x);
	  win.keypad(true);
	}
    }
  else
    win=NULL;

  // NOT queue_layout()! (that would be just Dumb[tm])
  do_layout();
}

void vscreen_widget::alloc_size(int x, int y, int w, int h)
{
  if(owner)
    set_owner_window(owner->win, x, y, w, h);
  else
    set_owner_window(NULL, x, y, w, h);
}

void vscreen_widget::set_owner(vs_container *_owner)
{
  owner=_owner;
  alloc_size(0,0,0,0);
}

int vscreen_widget::timeout(int msecs)
{
  int prevval=timeout_value;
  timeout_value=msecs;
  if(win)
    win.timeout(msecs);
  return prevval;
}

void vscreen_widget::cleanup()
{
  eassert(is_destroyed);
  delete this;
}

void vscreen_widget::destroy()
{
  eassert(refcount > 0);

  if(is_destroyed)
    return;

  // Make sure we don't die during the destroy routine.
  vs_widget_ref this_ref = this;

  hide();

  if(owner)
    {
      owner->rem_widget(this);
      eassert(!win);
    }

  // This must be done after hide() and rem_widget(), because
  // otherwise some of the vscreen_widget manipulators become NOPs,
  // which causes confusion and problems for code that accesses them.
  // For instance, the multiplex rem_widget code expects hide_widget
  // to do something sensible.  I could try to fix this, but it's much
  // more straightforward to just leave the widget in a non-destroyed
  // state until after it's disconnected from everything.
  is_destroyed = true;

  destroyed();
}

ref_ptr<vs_container> vscreen_widget::get_owner()
{
  return owner;
}

void vscreen_widget::show()
{
  vs_widget_ref tmpref(this);

  if(is_destroyed)
    return;

  visible=true;

  shown_sig();
}

void vscreen_widget::show_all()
{
  vs_widget_ref tmpref(this);

  if(is_destroyed)
    return;

  show();
}

void vscreen_widget::hide()
{
  vs_widget_ref tmpref(this);

  if(is_destroyed)
    return;

  visible=false;

  hidden_sig();
}

void vscreen_widget::display(const style &st)
{
  vs_widget_ref tmpref(this);

  if(is_destroyed)
    return;

  // Erase our window, using the composition of the surrounding style
  // and our background style.
  style basic_st=st+bg_style;
  int bgattr=basic_st.get_attrs();

  if(pre_display_erase)
    {
      bkgd(bgattr);
      erase();
    }

  attrset(bgattr);
  paint(basic_st);
}

bool vscreen_widget::focus_me()
{
  if(is_destroyed)
    return false;

  return !auxillary_bindings.empty();
}

bool vscreen_widget::handle_key(const key &k)
{
  vs_widget_ref tmpref(this);

  if(is_destroyed)
    return false;

  bool rval=false;

  for(key_connection i=auxillary_post_bindings.begin();
      i!=auxillary_post_bindings.end();
      i++)
    if(i->bindings->key_matches(k, i->keyname))
      {
	i->slot();
	rval=true;
      }

  return rval;
}

bool vscreen_widget::dispatch_key(const key &k)
{
  vs_widget_ref tmpref(this);

  if(is_destroyed)
    return false;

  bool rval=false;

  for(key_connection i=auxillary_bindings.begin();
      i!=auxillary_bindings.end();
      i++)
    if(i->bindings->key_matches(k, i->keyname))
      {
	i->slot();
	rval=true;
      }

  return rval || handle_key(k);
}

void vscreen_widget::dispatch_mouse(short id, int x, int y, int z,
				    mmask_t bmask)
{
}

vscreen_widget::key_connection vscreen_widget::connect_key(const string &key,
							   keybindings *bindings,
							   const sigc::slot0<void> &slot)
{
  auxillary_bindings.push_back(binding_connection(key, bindings, slot));

  key_connection rval=auxillary_bindings.end();
  --rval;

  return rval;
}

vscreen_widget::key_connection vscreen_widget::connect_key_post(const string &key,
								keybindings *bindings,
								const sigc::slot0<void> &slot)
{
  auxillary_post_bindings.push_back(binding_connection(key, bindings, slot));

  key_connection rval=auxillary_bindings.end();
  --rval;

  return rval;
}

void vscreen_widget::disconnect_key(key_connection key)
{
  auxillary_bindings.erase(key);
}

void vscreen_widget::disconnect_key_post(key_connection key)
{
  auxillary_post_bindings.erase(key);
}
