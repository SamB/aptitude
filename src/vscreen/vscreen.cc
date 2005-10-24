// vscreen.cc
//
//  Copyright 1999-2005 Daniel Burrows
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
//
//  Implementation of vscreen stuff.

// We need this for recursive mutexes.  Should all files be compiled
// with this defined??
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include "vscreen.h"
#include "curses++.h"
#include "vscreen_widget.h"

#include "transcode.h"
#include "vs_editline.h"
#include "vs_menu.h"
#include "vs_menubar.h"
#include "vs_pager.h"
#include "vs_statuschoice.h"
#include "vs_table.h"
#include "vs_text_layout.h"
#include "vs_tree.h"

#include "config/keybindings.h"
#include "config/style.h"

#include <generic/util/event_queue.h>
#include <generic/util/threads.h>

// For _()
#include "../aptitude.h"

#include <signal.h>

#include <assert.h>
#include <sys/time.h>

#include <map>

threads::recursive_mutex vscreen_mutex;

inline
threads::mutex &vscreen_get_mutex()
{
  return vscreen_mutex;
}

sigc::signal0<void> main_hook;


static threads::event_queue<vscreen_event *> eventq;

using namespace std;

bool curses_avail=false;
bool should_exit=false;

static bool suspended_with_signals = false;
static struct sigaction oldsigcont, oldsigtstp;

// Used to queue and merge update requests
//
// The global event queue isn't used for this so that
// vscreen_update(); vscreen_tryupdate() works as desired.  However,
// threading magic is used to ensure that background threads can post
// update requests.
struct update_state
{
  bool layout;
  bool update;
  bool cursorupdate;

  update_state()
    :layout(false), update(false), cursorupdate(false)
  {
  }
};

threads::recursive_mutex pending_updates_mutex;
update_state pending_updates;


vscreen_event::~vscreen_event()
{
}

void slot_event::dispatch()
{
  the_slot();
}

static vs_widget_ref toplevel = NULL;
// The widget which is displayed as the root of everything

// Cleanly shutdown (eg, restore screen settings if possible)
//
// Called on SIGTERM, SIGINT, SIGSEGV, SIGABRT, and SIGQUIT
//
// FIXME: revert to the /previous/ handler, not just SIG_DFL?
static void sigkilled(int sig)
{
  endwin();

  switch(sig)
    {
    case SIGTERM:
      fprintf(stderr, _("Ouch!  Got SIGTERM, dying..\n"));
      break;
    case SIGSEGV:
      fprintf(stderr, _("Ouch!  Got SIGSEGV, dying..\n"));
      break;
    case SIGABRT:
      fprintf(stderr, _("Ouch!  Got SIGABRT, dying..\n"));
      break;
    case SIGQUIT:
      fprintf(stderr, _("Ouch!  Got SIGQUIT, dying..\n"));
      break;
    }

  signal(sig, SIG_DFL);
  raise(sig);
}

///////////////////////////////////////////////////////////////////////////////
// The following function comes from the glibc documentation.
// Subtract the `struct timeval' values X and Y,
// storing the result in RESULT.
// Return 1 if the difference is negative, otherwise 0.

static int
timeval_subtract (timeval *result, timeval *x, timeval *y)
{
  /* Perform the carry for the later subtraction by updating Y. */
  if (x->tv_usec < y->tv_usec)
    {
      int nsec = (y->tv_usec - x->tv_usec) / 1000000 + 1;
      y->tv_usec -= 1000000 * nsec;
      y->tv_sec += nsec;
    }
  if (x->tv_usec - y->tv_usec > 1000000)
    {
      int nsec = (x->tv_usec - y->tv_usec) / 1000000;
      y->tv_usec += 1000000 * nsec;
      y->tv_sec -= nsec;
    }

  /* Compute the time remaining to wait.
     `tv_usec' is certainly positive. */
  result->tv_sec = x->tv_sec - y->tv_sec;
  result->tv_usec = x->tv_usec - y->tv_usec;

  /* Return 1 if result is negative. */
  return x->tv_sec < y->tv_sec;
}
///////////////////////////////////////////////////////////////////////////////

vs_widget_ref vscreen_settoplevel(const vs_widget_ref &w)
{
  if(toplevel.valid())
    toplevel->unfocussed();

  vs_widget_ref oldw = toplevel;

  toplevel = w;

  if(curses_avail)
    {
      toplevel->set_owner_window(rootwin, 0, 0, rootwin.getmaxx(), rootwin.getmaxy());
      toplevel->show_all();
      toplevel->focussed();
      vscreen_redraw();
    }

  return oldw;
}

//////////////////////////////////////////////////////////////////////
void vscreen_post_event(vscreen_event *ev)
{
  eventq.put(ev);
}

//////////////////////////////////////////////////////////////////////
// Event management threads

/** This thread is responsible for posting wget_wch() calls. */
class input_thread
{
  class key_input_event : public vscreen_event
  {
    key k;
  public:
    key_input_event (const key &_k)
      :k(_k)
    {
    }

    void dispatch()
    {
      if(global_bindings.key_matches(k, "Refresh"))
	vscreen_redraw();
      else
	toplevel->dispatch_key(k);
    }
  };

  class mouse_input_event : public vscreen_event
  {
    MEVENT ev;

  public:
    mouse_input_event(const MEVENT &_ev)
      :ev(_ev)
    {
    }

    void dispatch()
    {
      if(toplevel.valid())
	toplevel->dispatch_mouse(ev.id, ev.x, ev.y, ev.z, ev.bstate);
    }
  };

  static input_thread instance;

  static threads::mutex instance_mutex;
  static threads::thread *instancet;
public:
  static void start()
  {
    threads::mutex::lock l(instance_mutex);

    if(instancet == NULL)
      instancet = new threads::thread(instance);
  }

  static void stop()
  {
    threads::mutex::lock l(instance_mutex);

    if(instancet != NULL)
      {
	instancet->cancel();
	instancet->join();
	delete instancet;
	instancet = NULL;
      }
  }

  void operator()() const
  {
    // NB: use the GLOBAL getch function to avoid weirdness
    // referencing the state of toplevel.
    while(1)
      {
	wint_t wch = 0;
	int status;

	do
	  {
	    status = get_wch(&wch);
	  } while(status == KEY_CODE_YES && wch == KEY_RESIZE);

	key k(wch, status == KEY_CODE_YES);

	if(status == ERR)
	  return; // ???

	if(wch == KEY_MOUSE)
	  {
	    MEVENT ev;
	    getmouse(&ev);

	    vscreen_post_event(new mouse_input_event(ev));
	  }
	else
	  vscreen_post_event(new key_input_event(k));
      }
  }
};

threads::mutex input_thread::instance_mutex;
threads::thread *input_thread::instancet = NULL;
input_thread input_thread::instance;

class signal_thread
{
  class signal_event : public vscreen_event
  {
    int signal;
  public:
    signal_event(int _signal)
      :signal(_signal)
    {
    }

    void dispatch()
    {
      switch(signal)
	{
	case SIGWINCH:
	  vscreen_handleresize();
	  break;
	default:
	  vscreen_exitmain();
	  break;
	}
    }
  };

  static signal_thread instance;
  static threads::thread *t;
public:
  static void start()
  {
    if(t == NULL)
      t = new threads::thread(instance);
  }

  static void stop()
  {
    if(t != NULL)
      {
	t->cancel();
	t->join();
	delete t;
	t = NULL;
      }
  }

  void operator()() const
  {
    sigset_t s;

    sigemptyset(&s);
    sigaddset(&s, SIGWINCH);

    while(1)
      {
	int signum;

	int result = sigwait(&s, &signum);

	if(result == 0)
	  vscreen_post_event(new signal_event(signum));
      }
  }
};

signal_thread signal_thread::instance;
threads::thread *signal_thread::t = NULL;

class timeout_thread
{
  class SingletonViolationException
  {
  public:
    string errmsg() const
    {
      return "Attempt to run a singleton thread twice!";
    }
  };


  /** Information about a single time-out. */
  struct timeout_info
  {
    vscreen_event *ev;
    timeval activate_time; // tells when this timeout should be triggered
    timeout_info(vscreen_event *_ev,
		 const timeval &_activate_time)
      :ev(_ev), activate_time(_activate_time)
    {
    }

    timeout_info()
    {
      activate_time.tv_sec = 0;
      activate_time.tv_usec = 0;
    }
  };


  // The set of active timeouts.
  map<int, timeout_info> timeouts;

  /** If \b true, the thread should stop. */
  bool cancelled;

  // A lock for the set of timeouts and the cancelled flag.
  threads::mutex timeouts_mutex;

  // A condition to be broadcast when the set of timeouts is expanded
  // by add_timeout.
  threads::condition timeout_added;

  /** The thread that is currently executing in this object. */
  threads::box<threads::thread *> running_thread;



  /** Post messages about any timeouts that occurred. Should be called
   *  with timeouts_mutex locked.
   */
  void check_timeouts()
  {
    map<int, timeout_info>::iterator i,j;
    for(i = timeouts.begin(); i != timeouts.end(); i = j)
      {
	j = i;
	j++;
	timeval result,curtime;
	gettimeofday(&curtime, 0);

	if(timeval_subtract(&result, &i->second.activate_time, &curtime) == 1 ||
	   result.tv_sec == 0 && result.tv_usec <= 10)
	  {
	    vscreen_post_event(i->second.ev);
	    timeouts.erase(i);
	  }
      }
  }

  /** timeouts_mutex should be locked when this is called.
   *
   *  \param tv_out the time at which the first active timeout should trigger.
   *  \return \b true if a timeout was found.
   */
  bool first_timeout(timeval &tv_out)
  {
    bool found_one = false;
    timeval mintime;
    mintime.tv_sec = INT_MAX/1000;
    mintime.tv_usec = (INT_MAX % 1000) * 1000;

    timeval curtime;
    gettimeofday(&curtime, 0);

    map<int, timeout_info>::iterator i,j;
    for(i = timeouts.begin(); i != timeouts.end(); i = j)
      {
	j = i;
	j++;
	timeval diff;
	if(timeval_subtract(&diff, &i->second.activate_time, &curtime) == 1 ||
	   diff.tv_sec == 0 && diff.tv_usec <= 10)
	  {
	    tv_out = curtime;
	    return true;
	  }
	else
	  {
	    if(diff.tv_sec < mintime.tv_sec ||
	       (diff.tv_sec == mintime.tv_sec && diff.tv_usec < mintime.tv_usec))
	      {
		found_one = true;
		mintime = i->second.activate_time;
	      }
	  }
      }
    if(found_one)
      {
	tv_out = mintime;
	return true;
      }
    else
      return false;
  }


  timeout_thread(const timeout_thread &other);
  timeout_thread &operator=(const timeout_thread &other);

  timeout_thread()
    : cancelled(false), running_thread(NULL)
  {
  }

  // The global instance; this is a Singleton.
  static timeout_thread instance;

  // Unfortunately, technical considerations in the threading code
  // mean that the actual thread object is expected to be copyable.
  // Hence this proxy:
  class timeout_proxy
  {
    timeout_thread &real_thread;
  public:
    timeout_proxy(timeout_thread &_real_thread)
      : real_thread(_real_thread)
    {
    }

    void operator()() const
    {
      real_thread();
    }
  };
public:
  static timeout_thread &get_instance()
  {
    return instance;
  }

  static void start()
  {
    timeout_thread &instance = get_instance();

    threads::thread *running = instance.running_thread.take();
    if(running != NULL)
      {
	instance.running_thread.put(running);
	throw SingletonViolationException();
      }

    instance.running_thread.put(new threads::thread(timeout_proxy(instance)));
  }

  static void stop()
  {
    timeout_thread &instance = get_instance();

    threads::thread *running = instance.running_thread.take();
    threads::mutex::lock l(instance.timeouts_mutex);

    instance.cancelled = true;
    instance.timeout_added.wake_all();

    l.release();

    running->join();

    instance.running_thread.put(NULL);
  }

  void operator()()
  {
    threads::mutex::lock l(timeouts_mutex);

    while(!cancelled)
      {
	timeval next_timeout;

	if(first_timeout(next_timeout))
	  {
	    timespec until;
	    until.tv_sec = next_timeout.tv_sec;
	    until.tv_nsec = next_timeout.tv_usec * 1000;

	    timeout_added.timed_wait(l, until);

	    // Probably don't need to do this, but it won't hurt.
	    check_timeouts();
	  }
	else
	  timeout_added.wait(l);
      }
  }

  /** Add a timeout to the set of active timeouts.
   *
   *  \param slot a callback to activate when the timeout triggers
   *  \param msecs the number of milliseconds in which to activate the
   *               timeout.
   *  \return the ID number of the new timeout; can be used later to
   *          remove the timeout before it triggers.
   */
  int add_timeout(vscreen_event *ev, int msecs)
  {
    threads::mutex::lock l(timeouts_mutex);

    timeval activate_time;
    gettimeofday(&activate_time, 0);
    activate_time.tv_sec  += msecs/1000;
    activate_time.tv_usec += (msecs%1000)*1000;
    while(activate_time.tv_usec > 1000 * 1000)
      // Should only run through once
      {
	activate_time.tv_sec++;
	activate_time.tv_usec -= 1000 * 1000;
      }

    // Since the timeouts are sorted by ID, the back element is the
    // maximum ID in use.
    int rval;

    if(timeouts.empty())
      rval = 0;
    else
      rval = timeouts.rbegin()->first + 1;

    timeouts[rval] = timeout_info(ev, activate_time);

    timeout_added.wake_all();

    return rval;
  }

  void del_timeout(int id)
  {
    threads::mutex::lock l(timeouts_mutex);

    timeouts.erase(id);
  }
};

timeout_thread timeout_thread::instance;

void vscreen_init()
{
  keybinding upkey, downkey, leftkey, rightkey, quitkey, homekey, endkey;
  keybinding historynextkey, historyprevkey;
  keybinding delfkey, delbkey, ppagekey, npagekey;
  keybinding undokey, helpkey, researchkey;
  keybinding menutogglekey, cancelkey;

  upkey.push_back(key(KEY_UP, true));
  upkey.push_back(key(L'k', false));
  downkey.push_back(key(KEY_DOWN, true));
  downkey.push_back(key(L'j', false));
  leftkey.push_back(key(KEY_LEFT, true));
  leftkey.push_back(key(L'h', false));
  rightkey.push_back(key(KEY_RIGHT, true));
  rightkey.push_back(key(L'l', false));
  quitkey.push_back(key(L'q', false));

  historyprevkey.push_back(key(KEY_UP, true));
  historyprevkey.push_back(KEY_CTRL(L'p'));
  historynextkey.push_back(key(KEY_DOWN, true));
  historynextkey.push_back(KEY_CTRL(L'n'));

  homekey.push_back(key(KEY_HOME, true));
  homekey.push_back(KEY_CTRL(L'a'));
  endkey.push_back(key(KEY_END, true));
  endkey.push_back(KEY_CTRL(L'e'));

  delfkey.push_back(key(KEY_DC, true));
  delfkey.push_back(KEY_CTRL(L'd'));

  delbkey.push_back(key(KEY_BACKSPACE, true));
  delbkey.push_back(KEY_CTRL(L'h'));

  ppagekey.push_back(key(KEY_PPAGE, true));
  ppagekey.push_back(KEY_CTRL(L'b'));

  npagekey.push_back(key(KEY_NPAGE, true));
  npagekey.push_back(KEY_CTRL(L'f'));

  undokey.push_back(KEY_CTRL(L'u'));
  undokey.push_back(KEY_CTRL(L'_'));

  helpkey.push_back(key(L'?', false));
  helpkey.push_back(KEY_CTRL(L'h'));
  helpkey.push_back(key(KEY_F(1), true));

  menutogglekey.push_back(KEY_CTRL(L't'));
  menutogglekey.push_back(key(KEY_F(10), true));
  menutogglekey.push_back(KEY_CTRL(L' '));

  cancelkey.push_back(KEY_CTRL(L'g'));
  cancelkey.push_back(key(L'\e', true));
  cancelkey.push_back(KEY_CTRL(L'['));

  researchkey.push_back(key(L'n', false));

  init_curses();

  mousemask(ALL_MOUSE_EVENTS, NULL);

  curses_avail=true;
  cbreak();
  rootwin.keypad(true);

  global_bindings.set("Quit", quitkey);
  global_bindings.set("Cycle", key(L'\t', false));
  global_bindings.set("Refresh", KEY_CTRL(L'l'));

  global_bindings.set("Up", upkey);
  global_bindings.set("Down", downkey);
  global_bindings.set("LevelDown", key(L'J', false));
  global_bindings.set("LevelUp", key(L'K', false));
  global_bindings.set("Left", leftkey);
  global_bindings.set("Right", rightkey);
  global_bindings.set("HistoryNext", historynextkey);
  global_bindings.set("HistoryPrev", historyprevkey);
  global_bindings.set("Parent", key(L'^', false));
  global_bindings.set("PrevPage", ppagekey);
  global_bindings.set("NextPage", npagekey);
  global_bindings.set("Begin", homekey);
  global_bindings.set("End", endkey);
  global_bindings.set("Search", key(L'/', false));
  global_bindings.set("SearchBack", key(L'\\', false));
  global_bindings.set("ReSearch", researchkey);
  global_bindings.set("DelBack", delbkey);
  global_bindings.set("DelForward", delfkey);

  global_bindings.set("DelEOL", KEY_CTRL(L'k'));
  global_bindings.set("DelBOL", KEY_CTRL(L'u'));

  global_bindings.set("Confirm", key(KEY_ENTER, true));
  global_bindings.set("Cancel", cancelkey);
  global_bindings.set("Undo", undokey);
  global_bindings.set("Help", helpkey);
  global_bindings.set("ToggleMenuActive", menutogglekey);
  global_bindings.set("PushButton", key(L' ', false));
  global_bindings.set("Yes", key(transcode(_("yes_key"))[0], false));
  global_bindings.set("No", key(transcode(_("no_key"))[0], false));

  global_bindings.set("ToggleExpanded", key(KEY_ENTER, true));
  global_bindings.set("ExpandAll", key(L'[', false));
  global_bindings.set("CollapseAll", key(L']', false));
  global_bindings.set("SelectParent", key(L'^', false));

  vs_editline::init_bindings();
  vs_menu::init_bindings();
  vs_menubar::init_bindings();
  vs_pager::init_bindings();
  vs_statuschoice::init_bindings();
  vs_table::init_bindings();
  vs_text_layout::init_bindings();
  vs_tree::init_bindings();

  set_style("Error",
	    style_fg(COLOR_WHITE)+style_bg(COLOR_RED)+style_attrs_on(A_BOLD));

  // The 'base' style for the display.
  set_style("Default",
	    style_fg(COLOR_WHITE)+style_bg(COLOR_BLACK));

  set_style("Header",
	    style_fg(COLOR_WHITE)+style_bg(COLOR_BLUE)+style_attrs_on(A_BOLD));
  set_style("Status",
	    style_fg(COLOR_WHITE)+style_bg(COLOR_BLUE)+style_attrs_on(A_BOLD));

  set_style("MenuEntry", style_fg(COLOR_WHITE)+style_bg(COLOR_BLUE));
  set_style("MenuBorder", style_fg(COLOR_WHITE)+style_bg(COLOR_BLUE)+style_attrs_on(A_BOLD));
  set_style("HighlightedMenuEntry", style_bg(COLOR_BLUE)+style_fg(COLOR_WHITE)+style_attrs_on(A_BOLD|A_REVERSE));
  set_style("DisabledMenuEntry",
	    style_fg(COLOR_BLACK)+style_bg(COLOR_BLUE)+style_attrs_on(A_DIM));
  set_style("MenuBar", style_fg(COLOR_WHITE)+style_bg(COLOR_BLUE)+style_attrs_on(A_BOLD));
  set_style("HighlightedMenuBar", style_fg(COLOR_BLUE)+style_bg(COLOR_WHITE));

  set_style("MultiplexTab", style_fg(COLOR_WHITE)+style_bg(COLOR_BLUE));
  set_style("MultiplexTabHighlighted", style_fg(COLOR_BLUE)+style_bg(COLOR_WHITE));

  // Edit lines will *always* appear white-on-black.
  set_style("EditLine", style_fg(COLOR_WHITE)+style_bg(COLOR_BLACK)+style_attrs_off(A_REVERSE));

  set_style("TreeBackground", style());

  if(toplevel.valid())
    vscreen_settoplevel(toplevel);


  vscreen_install_sighandlers();


  // Block WINCH so the signal_thread can pick it up.
  sigset_t signals;
  sigemptyset(&signals);
  sigaddset(&signals, SIGWINCH);
  sigprocmask(SIG_BLOCK, &signals, NULL);



  input_thread::start();
  signal_thread::start();
  timeout_thread::start();
}

void vscreen_install_sighandlers()
{
  signal(SIGTERM, sigkilled);
  signal(SIGINT, sigkilled);
  signal(SIGSEGV, sigkilled);
  signal(SIGQUIT, sigkilled);
  signal(SIGABRT, sigkilled);
}

void vscreen_handleresize()
{
  toplevel->set_owner_window(NULL, 0, 0, 0, 0);
  resize();
  toplevel->set_owner_window(rootwin, 0, 0, rootwin.getmaxx(), rootwin.getmaxy());
  vscreen_redraw();
}

class try_update_event : public vscreen_event
{
public:
  void dispatch()
  {
    vscreen_tryupdate();
  }
};

void vscreen_updatecursor()
{
  threads::mutex::lock l(pending_updates_mutex);

  pending_updates.cursorupdate=true;
}

void vscreen_updatecursornow()
{
  if(toplevel->get_cursorvisible())
    {
      point p=toplevel->get_cursorloc();
      toplevel->win.leaveok(false);
      toplevel->win.move(p.y, p.x);
      toplevel->win.noutrefresh();
    }
  else
    toplevel->win.leaveok(true);
}

void vscreen_update()
{
  threads::mutex::lock l(pending_updates_mutex);

  pending_updates.update=true;
  pending_updates.cursorupdate=true;

  vscreen_post_event(new try_update_event);
}

void vscreen_updatenow()
{
  if(toplevel.valid())
    {
      toplevel->display(get_style("Default"));
      toplevel->sync();
    }
}

void vscreen_queuelayout()
{
  threads::mutex::lock l(pending_updates_mutex);

  pending_updates.layout = true;
  pending_updates.update = true;
  pending_updates.cursorupdate = true;

  vscreen_post_event(new try_update_event);
}

void vscreen_layoutnow()
{
  toplevel->do_layout();
}

void vscreen_tryupdate()
{
  threads::mutex::lock l(pending_updates_mutex);

  update_state needs = pending_updates;

  if(needs.layout)
    vscreen_layoutnow();

  if(needs.update)
    vscreen_updatenow();

  if(needs.update || needs.cursorupdate)
    vscreen_updatecursornow();

  doupdate();

  // \todo This appears to just paper over sloppiness -- screen update
  // routines shouldn't be queuing more updates!
  pending_updates = update_state();
}

bool vscreen_poll()
{
  bool rval=false;

  vscreen_event *ev = NULL;

  while(eventq.try_get(ev))
    {
      rval = true;
      ev->dispatch();
      delete ev;
    }

  main_hook();

  return rval;
}

void vscreen_mainloop(int synch)
{
  static int main_level=0;

  main_level++;

  threads::mutex::lock l(vscreen_get_mutex());

  while(!should_exit && toplevel.valid())
    {
      l.release();

      vscreen_event *ev = eventq.get();

      l.acquire();

      ev->dispatch();
      delete ev;

      while(eventq.try_get(ev))
	{
	  ev->dispatch();
	  delete ev;
	}

      main_hook();
    }

  should_exit=false;

  main_level--;
}

void vscreen_exitmain()
{
  should_exit=1;
}

void vscreen_suspend_without_signals()
{
  input_thread::stop();
  signal_thread::stop();
  timeout_thread::stop();

  if(toplevel.valid())
    toplevel->set_owner_window(NULL, 0, 0, 0, 0);

  rootwin.bkgdset(' ');
  rootwin.clear();
  rootwin.refresh();
  endwin();
  curses_avail=false;
}

void vscreen_suspend()
{
  suspended_with_signals = true;

  struct sigaction act;
  act.sa_handler = SIG_IGN;
  act.sa_sigaction = 0;
  sigemptyset(&act.sa_mask);
  act.sa_flags = 0;
  act.sa_restorer = 0;

  sigaction(SIGCONT, &act, &oldsigcont);
  sigaction(SIGTSTP, &act, &oldsigtstp);

  vscreen_suspend_without_signals();
}

void vscreen_shutdown()
{
  toplevel->destroy();
  toplevel = NULL;

  vscreen_suspend();

  // Discard all remaining events.
  vscreen_event *ev = NULL;
  while(eventq.try_get(ev))
    delete ev;
}

void vscreen_resume()
{
  if(suspended_with_signals)
    {
      sigaction(SIGCONT, &oldsigcont, NULL);
      sigaction(SIGTSTP, &oldsigtstp, NULL);
      suspended_with_signals = false;
    }

  curses_avail=true;
  if(toplevel.valid())
    {
      toplevel->set_owner_window(rootwin, 0, 0, rootwin.getmaxx(), rootwin.getmaxy());
      toplevel->display(get_style("Default"));
      toplevel->sync();
      doupdate();
    }
  else
    refresh();

  input_thread::start();
  signal_thread::start();
  timeout_thread::start();
}

void vscreen_redraw()
{
  threads::mutex::lock l(pending_updates_mutex);

  if(toplevel.valid())
    {
      toplevel->focussed();
      toplevel->get_win().touch();
      toplevel->get_win().clearok(true);
      toplevel->do_layout();
      toplevel->display(get_style("Default"));
      vscreen_updatecursornow();
      toplevel->sync();
      doupdate();
      toplevel->get_win().clearok(false);
    }

  // For reasons that aren't entirely clear, running a tryupdate()
  // right after a resize causes stuff to break -- but since resizing
  // triggers a redraw, the tryupdate() shouldn't be necessary anyway.
  // Suppress any pending updates after redrawing:
  pending_updates = update_state();
}

int vscreen_addtimeout(vscreen_event *ev, int msecs)
{
  if(msecs < 0)
    return -1;

  return timeout_thread::get_instance().add_timeout(ev, msecs);
}

void vscreen_deltimeout(int num)
{
  timeout_thread::get_instance().del_timeout(num);
}
