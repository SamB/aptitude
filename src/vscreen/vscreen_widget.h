// vscreen_widget.h  -*-c++-*-
//
//   Copyright (C) 2000-2005 Daniel Burrows
//
//   This program is free software; you can redistribute it and/or
//   modify it under the terms of the GNU General Public License as
//   published by the Free Software Foundation; either version 2 of
//   the License, or (at your option) any later version.
//
//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//   General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with this program; see the file COPYING.  If not, write to
//   the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//   Boston, MA 02111-1307, USA.
//
// "widgets" are sort of what they sound like -- entities that get
// drawn on the screen in some stacked order and can grab keyboard
// input.  By default, the widget currently on the top of the stack
// has keyboard "focus".  (overriding this may be an option eventually
// but isn't right now) (the widget with keyboard focus gets to
// determine cursor information)
//
//
// Lifetime of a widget: a widget is potentially active and visible
// until it is destroyed (via the destroy() method).  Once a widget
// has been destroyed, you cannot un-destroy it, but further calls to
// destroy() are allowed until the widget is deleted for good.
// Widgets should generally be referred to using ref_ptr objects; to
// enforce this, widgets can only be allocated via W::create(...).  Of
// course, as with any refcounting scheme, it's the user's
// responsibility to prevent cycles by using unsafe_get_ref()
// appropriately.  For instance, pointers to the widget's parent have
// to be created this way.

#ifndef VSCREEN_WIDGET_H
#define VSCREEN_WIDGET_H

#include <list>
#include <vector>

#include <sigc++/signal.h>
#include <sigc++/trackable.h>

#include "curses++.h"
#include "config/style.h"
#include "ref_ptr.h"

#include <generic/util/eassert.h>

class vs_container;
class key;

struct size
{
  int w, h;
  size(int _w, int _h):w(_w), h(_h) {}
};

struct point
{
  int x, y;
  point(int _x, int _y):x(_x), y(_y) {}
};

struct rect
{
  int x, y, w, h;
  rect(int _x, int _y, int _w, int _h):x(_x), y(_y), w(_w), h(_h) {}
  void set_size(const size &s) {w=s.w; h=s.h;}
  size get_size() {return size(w, h);}
};

class keybindings;

/** The basic widget interface.  Note that due to the current
 *  reference counting implementation, this is not presently
 *  threadsafe.
 */
class vscreen_widget:virtual public sigc::trackable
{
  friend class vs_container;

  // Too many friends..
  friend bool vscreen_poll();
  friend void vscreen_mainloop(int);
  friend void vscreen_redraw();
  friend ref_ptr<vscreen_widget> vscreen_settoplevel(const ref_ptr<vscreen_widget> &);
  friend void vscreen_suspend_without_signals();
  friend void vscreen_resume();
  friend void vscreen_updatecursornow();
  friend void vscreen_handleresize();

  // Used to store info on externally/explicitly set bindings.
  struct binding_connection
  {
    std::string keyname;

    keybindings *bindings;

    sigc::slot0<void> slot;

    binding_connection():bindings(NULL) {}
    binding_connection(const std::string &_keyname, keybindings *_bindings, const sigc::slot0<void> &_slot)
      :keyname(_keyname), bindings(_bindings), slot(_slot) {}
  };

  // Bindings set via connect_key() and connect_key_post()
  std::list<binding_connection> auxillary_bindings, auxillary_post_bindings;

  cwindow win;

  int timeout_value;

  vs_container *owner;

  // Needed for recreating the window when the vscreen's window gets switched.
  // This stores the CURRENT size of the widget.
  rect geom;

  /** The basic style attached to this widget. */
  style bg_style;

  /** The number of live references to this object.  This is initially
   *  1, so that it's safe to take references in the constructor.  It
   *  is expected that subclasses will only permit construction via
   *  static methods that remove this initial reference.
   */
  mutable int refcount;

  // Whether the widget is visible (distinct from whether it has a window;
  // answers the question "should this widget have a window?")
  bool visible:1;

  // Tracks whether or not we have the focus.
  bool isfocussed:1;

  /** If \b true (the default setting), clear this widget with the
   *  background color before displaying it.
   */
  bool pre_display_erase:1;

  bool is_destroyed:1;

  // Used to set the owner-window without setting the owner.  Used only
  // to handle the toplevel widget (which has a window but no owner)
  // Like alloc_size
  void set_owner_window(cwindow _win, int x, int y, int w, int h);

  // Used to update the "focussed" state
  void set_isfocussed(bool _isfocussed);
protected:
  cwindow get_win() {return win;}

  /** Display this widget.
   *
   *  \param st the style environment in which the widget is to be
   *            displayed.
   */
  virtual void paint(const style &st)=0;

  /** Handles a keypress in this widget.
   *
   *  \param k the key that was pressed (see keybindings.h).
   *
   *  \return \b true if the key was consumed; if \b false is
   *  returned, further processing of the key will be performed.
   */
  virtual bool handle_key(const key &k);

  /** Handle cleanup when the reference count goes to 0. */
  void cleanup();
protected:
  vscreen_widget();

public:
  void incref()
  {
    eassert(refcount > 0);

    ++refcount;
  }

  void decref()
  {
    eassert(refcount > 0);

    --refcount;
    if(refcount == 0)
      cleanup();
  }

  static void handle_pending_deletes();

  // show() and hide() do the expected.  show_all() makes a container show
  // all of its "children".  (err..it doesn't make sense for menubars to show
  // their menus, but aside from that..)
  void show();
  virtual void show_all();
  void hide();
  void toggle_visible()
  {
    if(visible)
      hide();
    else
      show();
  }
  void set_visible(bool _visible)
  {
    if(visible!=_visible)
      {
	if(_visible)
	  show();
	else
	  hide();
      }
  }

  virtual ~vscreen_widget();

  // This should be called when an arbitrary widget is to have a
  // keypress sent to it.
  bool dispatch_key(const key & k);

  // This should be called when an arbitrary widget is to have a mouse event
  // sent to it.  Override it to change mousing behavior.
  virtual void dispatch_mouse(short id, int x, int y, int z, mmask_t bstate);



  // The following methods deal with handling widget layout.  Widget
  // layout is a two-shot affair: first, all widgets are allocated
  // space in the X dimension; then, all widgets are allocated space
  // in the Y dimension.  Doing allocation in this asymmetrical way
  // allows widgets with complex interdependencies between their
  // dimensions to be handled (for instance: a widget that wraps the
  // text it displays, and will have to request more height if its
  // width decreases).
  //
  // You can assume that the widget's state is unchanged between a
  // call to width_request() and a call to height_request().

  /** \return the desired width of the widget. */
  virtual int width_request()=0;

  /** Calculate the desired height of the widget, given its width.
   *
   *  \param width the width of this widget
   *  \return the desired height
   */
  virtual int height_request(int width)=0;

  /** Set the size and location in the parent of this widget.  This
   *  routine should be called by the parent to actually resize and/or
   *  move the widget around.  There is no guarantee that the new
   *  width and height bear any relation to what the _request
   *  functions asked for, although the parent is expected to make a
   *  best-effort attempt to give a widget its desired size.
   *
   *  As a special case, calling alloc_size(0, 0, 0, 0) indicates that
   *  the widget has no allocation at all.
   *
   *  \param x the new x location within the parent
   *  \param y the new y location within the parent
   *  \param w the new width of the widget
   *  \param h the new height of the widget
   */
  void alloc_size(int x, int y, int w, int h);




  virtual bool focus_me();
  // Returns true if this widet wants the keyboard focus (used in, eg, dialog
  // boxes)

  bool get_isfocussed() {return isfocussed;}

  void set_owner(vs_container *w);
  // Makes this widget a child of the given widget (incidentally will delete
  // any allocated size; setting the owner to NULL hides the widget for now)

  /** Display this widget in the given style environment.  This is an
   *  interface function only; subclasses should override paint() to
   *  control behavior (but should call display() on subwidgets).
   *
   *  \param st the style environment in which this widget should be
   *  displayed.
   */
  void display(const style &st);

  int timeout(int msecs);

  /** Destroys the visible representation of this widget and
   *  disconnects it from any children that it may have.
   */
  virtual void destroy();

  ref_ptr<vs_container> get_owner();

  virtual bool get_cursorvisible()=0;
  virtual point get_cursorloc()=0;

  int get_startx() {return geom.x;}
  int get_starty() {return geom.y;}
  int get_width() {return geom.w;}
  int get_height() {return geom.h;}

  bool get_visible() {return visible;}

  // Should NOT be overridden -- that was a thinko
  void sync() {if(win) {win.touch(); win.noutrefresh();}}

  // from the original vscreen
  int scroll(int n=1) {return win?win.scroll(n):0;}

  int addch(chtype ch) {return win?win.addch(ch):0;}
  int mvaddch(int y, int x, chtype ch) {return win?win.mvaddch(y,x,ch):0;}

  int add_wch(wchar_t wch)
  {
    return win?win.add_wch(wch):0;
  }

  int add_wch(const cchar_t *cch)
  {
    return win?win.add_wch(cch):0;
  }

  int mvadd_wch(int y, int x, wchar_t wch)
  {
    return win?win.mvadd_wch(y, x, wch):0;
  }

  int mvadd_wch(int y, int x, const cchar_t *cch)
  {
    return win?win.mvadd_wch(y, x, cch):0;
  }

  int addstr(const char *str) {return win?win.addstr(str):0;}
  int addnstr(const char *str, int n) {return win?win.addnstr(str, n):0;}
  int mvaddstr(int y, int x, const char *str) {return win?win.mvaddstr(y, x, str):0;}
  int mvaddnstr(int y, int x, const char *str, int n) {return win?win.mvaddnstr(y, x, str, n):0;}

  int addstr(const wchar_t *str) {return win?win.addstr(str):0;}
  int addnstr(const wchar_t *str, int n) {return win?win.addnstr(str, n):0;}
  int mvaddstr(int y, int x, const wchar_t *str) {return win?win.mvaddstr(y, x, str):0;}
  int mvaddnstr(int y, int x, const wchar_t *str, int n) {return win?win.mvaddnstr(y, x, str, n):0;}

  int addstr(const std::wstring &str) {return win?win.addstr(str):0;}
  int addnstr(const std::wstring &str, int n) {return win?win.addnstr(str, n):0;}
  int mvaddstr(int y, int x, const std::wstring &str) {return win?win.mvaddstr(y, x, str):0;}
  int mvaddnstr(int y, int x, const std::wstring &str, int n) {return win?win.mvaddnstr(y, x, str, n):0;}

  int addstr(const wchstring &str) {return win?win.addstr(str):0;}
  int addnstr(const wchstring &str, int n) {return win?win.addnstr(str, n):0;}
  int mvaddstr(int y, int x, const wchstring &str) {return win?win.mvaddstr(y, x, str):0;}
  int mvaddnstr(int y, int x, const wchstring &str, int n) {return win?win.mvaddnstr(y, x, str, n):0;}

  int addstr(const chstring &str) {return win?win.addstr(str):0;}
  int addnstr(const chstring &str, int n) {return win?win.addnstr(str, n):0;}
  int mvaddstr(int y, int x, const chstring &str) {return win?win.mvaddstr(y, x, str):0;}
  int mvaddnstr(int y, int x, const chstring &str, int n) {return win?win.mvaddnstr(y, x, str, n):0;}

  int attroff(int attrs) {return win?win.attroff(attrs):0;}
  int attron(int attrs) {return win?win.attron(attrs):0;}
  int attrset(int attrs) {return win?win.attrset(attrs):0;}

  void bkgdset(const chtype ch) {if(win) win.bkgdset(ch);}
  int bkgd(const chtype ch) {return win?win.bkgd(ch):0;}
  chtype getbkgd() {return win?win.getbkgd():0;}

  int border(chtype ls, chtype rs, chtype ts, chtype bs, chtype tl, chtype tr, chtype bl, chtype br)
    {return win?win.border(ls,rs,ts,bs,tl,tr,bl,br):0;}
  int box(chtype verch, chtype horch) {return win?win.box(verch,horch):0;}
  int hline(chtype ch, int n) {return win?win.hline(ch,n):0;}
  int vline(chtype ch, int n) {return win?win.vline(ch,n):0;}
  int mvhline(int y, int x, chtype ch, int n) {return win?win.mvhline(y, x, ch, n):0;}
  int mvvline(int y, int x, chtype ch, int n) {return win?win.mvvline(y,x,ch,n):0;}

  int delch() {return win?win.delch():0;}
  int mvdelch(int y, int x) {return win?win.mvdelch(y, x):0;}

  int deleteln() {return win?win.deleteln():0;}
  int insdelln(int n) {return win?win.insdelln(n):0;}
  int insertln() {return win?win.insertln():0;}

  int echochar(chtype ch) {return win?win.echochar(ch):0;}

  int move(int y, int x) {return win?win.move(y,x):0;}
  void getyx(int &y, int &x) {if(win) win.getyx(y,x); else y=x=0;}
  void getbegyx(int &y, int &x) {if(win) win.getbegyx(y,x); else y=x=0;}
  void getmaxyx(int &y, int &x) {if(win) win.getmaxyx(y,x); else y=x=0;}
  int getmaxy() {return win?win.getmaxy():0;}
  int getmaxx() {return win?win.getmaxx():0;}

  void show_string_as_progbar(int x, int y, const std::wstring &s,
			      const style &st1, const style &st2,
			      int size1, int totalsize)
  {
    if(win)
      win.show_string_as_progbar(x, y, s,
				 st1.get_attrs(),
				 st2.get_attrs(),
				 size1, totalsize);
  }

  void display_header(std::wstring s, const style &st) {if(win) win.display_header(s, st.get_attrs());}
  void display_status(std::wstring s, const style &st) {if(win) win.display_status(s, st.get_attrs());}

  int erase() {return win?win.erase():0;}
  int clear() {return win?win.clear():0;}
  int clrtobot() {return win?win.clrtobot():0;}
  int clrtoeol() {return win?win.clrtoeol():0;}

  // FIXME: we should preserve these settings ourselves and restore them on
  // set_win().  ?
  int keypad(bool bf) {return win?win.keypad(bf):0;}
  int meta(bool bf) {return win?win.meta(bf):0;}

  bool enclose(int y, int x)
  {
    if(win)
      return y>=geom.y && y<geom.y+geom.h && x>=geom.x && x<geom.x+geom.w;
    else
      return false;
  }

  /** Enable or disable clearing the background before displaying the
   *  widget.
   *
   *  \param opaque if \b true (the default setting), the widget's
   *  entire area will be overwritten with its background style prior
   *  to displaying it.
   */
  void set_opaque(bool opaque)
  {
    pre_display_erase=opaque;
  }

  /** Update this widget's basic style to the given value.  The style
   *  stack must be empty.
   */
  void set_bg_style(const style &new_style);

  /** Set the display attributes of our associated window directly
   *  from the given style.  (it is expected that subclasses will use
   *  this to control what is output to the window)
   */
  void apply_style(const style &st);

  typedef std::list<binding_connection>::iterator key_connection;
  // This can be used to connect to a pseudo-signal for keypresses.
  // Most useful for stuff like setting up hotkeys and keyboard accelerators..
  key_connection connect_key(const std::string &key,
			     keybindings *bindings,
			     const sigc::slot0<void> &slot);
  // Same, but the key is tested for after all other possibilities are
  // exhausted.
  key_connection connect_key_post(const std::string &key,
				  keybindings *bindings,
				  const sigc::slot0<void> &slot);

  // The opposite..
  void disconnect_key(key_connection c);
  // Eww, do I really need two of these?
  void disconnect_key_post(key_connection c);

  // Signals:
  //
  // I use signals for events that an external object (eg,
  // a container) might want to act on.  For instance, when an object is
  // hidden, its parent might want to rearrange its children.
  //
  // In contrast, virtual methods are used for specific behaviors of this
  // class: for instance, displaying the widget itself.

  sigc::signal0<void> shown_sig;
  // Emitted when the object is shown.  (not to be confused with the obsolete
  // show_sig, which was a request by the object to be shown)

  sigc::signal0<void> hidden_sig;
  // similarly

  sigc::signal0<void> destroyed;
  // Sent before a widget is destroyed.
  // A widget is always hidden before being destroyed

  sigc::signal0<void> do_layout;
  // Sent when the widget's layout needs to be recalculated and child windows
  // need to be re-updated (mainly when the size is altered)
  // This should not be called directly by the user.  Use vscreen_queuelayout()
  // instead.

  sigc::signal0<void> focussed;
  sigc::signal0<void> unfocussed;
  // Sent when we gain or lose the keyboard focus.
};

typedef ref_ptr<vscreen_widget> vs_widget_ref;

#endif
