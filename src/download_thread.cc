// download_thread.cc
//
//   Copyright (C) 2005 Daniel Burrows
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

#include "download_thread.h"

#include <generic/apt/download_manager.h>
#include <generic/apt/download_signal_log.h>

#include <vscreen/vscreen.h>

#include <sigc++/bind.h>
#include <sigc++/slot.h>
#include <sigc++/functors/mem_fun.h>

template<typename RVal>
class background_execute : public vscreen_event
{
  sigc::slot0<RVal> slot;

  threads::box<RVal> &return_box;
public:
  background_execute(const sigc::slot0<RVal> &_slot,
		     threads::box<RVal> &_return_box)
    :slot(_slot), return_box(_return_box)
  {
  }

  void dispatch()
  {
    return_box.put(slot());
  }
};

template<>
class background_execute<void> : public vscreen_event
{
  sigc::slot0<void> slot;
  threads::box<void> &return_box;
public:
  background_execute(const sigc::slot0<void> &_slot,
		     threads::box<void> &_return_box)
    :slot(_slot), return_box(_return_box)
  {
  }

  void dispatch()
  {
    slot();
    return_box.put();
  }
};

/** Run the given method call in the foreground and return its value. */
template<typename C, typename RVal>
static
RVal do_foreground_execute(C *inst,
			   RVal (C::* fun) ())
{
  threads::box<RVal> return_box;

  vscreen_post_event(new background_execute<RVal>(sigc::mem_fun(inst, fun),
						  return_box));

  return return_box.take();
}

/** Run the given method call in the foreground and return its value. */
template<typename C, typename RVal, typename Arg0>
static
RVal do_foreground_execute(C *inst,
			   Arg0 arg0,
			   RVal (C::* fun) (Arg0))
{
  threads::box<RVal> return_box;

  vscreen_post_event(new background_execute<RVal>(bind(sigc::mem_fun(inst, fun), arg0),
						  return_box));

  return return_box.take();
}

/** Run the given method call in the foreground and return its value. */
template<typename C, typename RVal, typename Arg0, typename Arg1>
static
RVal do_foreground_execute(C *inst,
			   Arg0 arg0,
			   Arg1 arg1,
			   RVal (C::* fun) (Arg0, Arg1))
{
  threads::box<RVal> return_box;

  vscreen_post_event(new background_execute<RVal>(bind(sigc::mem_fun(inst, fun), arg0, arg1),
						  return_box));

  return return_box.take();
}

/** Run the given function call in the foreground and return its value. */
template<typename C, typename RVal, typename Arg0, typename Arg1, typename Arg2>
static
RVal do_foreground_execute(C *inst,
			   Arg0 arg0,
			   Arg1 arg1,
			   Arg2 arg2,
			   RVal (C::* fun) (Arg0, Arg1, Arg2))
{
  threads::box<RVal> return_box;

  vscreen_post_event(new background_execute<RVal>(bind(sigc::mem_fun(inst, fun), arg0, arg1, arg2),
						  return_box));

  return return_box.take();
}

void background_status::Fetched(unsigned long Size,
				unsigned long ResumePoint)
{
  do_foreground_execute(real_status, Size, ResumePoint,
			&download_signal_log::Fetched);
}

bool background_status::MediaChange(std::string Media, std::string Drive)
{
  threads::box<bool> return_box;

  do_foreground_execute<download_signal_log,
    void, const std::string &, const std::string &,
    const sigc::slot1<void, bool> &>  (real_status, Media, Drive,
				       sigc::mem_fun(return_box,
						     &threads::box<bool>::put),
				       &download_signal_log::MediaChange);

  return return_box.take();
}

void background_status::IMSHit(pkgAcquire::ItemDesc &item)
{
  do_foreground_execute<download_signal_log, void, pkgAcquire::ItemDesc &>(real_status, item, &download_signal_log::IMSHit);
}

void background_status::Fetch(pkgAcquire::ItemDesc &item)
{
  do_foreground_execute<download_signal_log, void, pkgAcquire::ItemDesc &>(real_status, item, &download_signal_log::Fetch);
}

void background_status::Done(pkgAcquire::ItemDesc &item)
{
  do_foreground_execute<download_signal_log, void, pkgAcquire::ItemDesc &>(real_status, item, &download_signal_log::Done);
}

void background_status::Fail(pkgAcquire::ItemDesc &item)
{
  do_foreground_execute<download_signal_log, void, pkgAcquire::ItemDesc &>(real_status, item, &download_signal_log::Fail);
}

bool background_status::Pulse(pkgAcquire *Owner)
{
  threads::box<bool> return_box;

  do_foreground_execute<download_signal_log, void,
    pkgAcquire *,
    const sigc::slot1<void, bool> &>(real_status, Owner,
				     sigc::mem_fun(&return_box,
						   &threads::box<bool>::put),
				     &download_signal_log::Pulse);

  return return_box.take();
}

void background_status::Start()
{
  do_foreground_execute(real_status, &download_signal_log::Start);
}

void background_status::Stop()
{
  threads::box<void> return_box;

  do_foreground_execute<download_signal_log,
    void,
    const sigc::slot0<void> &>(real_status,
			       sigc::mem_fun(&return_box,
					     &threads::box<void>::put),
			       &download_signal_log::Stop);

  return_box.take();
}

class download_thread_complete_event : public vscreen_event
{
  download_thread *t;
  pkgAcquire::RunResult res;

  sigc::slot2<void, download_thread *, pkgAcquire::RunResult> continuation;
public:
  download_thread_complete_event(download_thread *_t,
				 pkgAcquire::RunResult _res,
				 sigc::slot2<void, download_thread *, pkgAcquire::RunResult> &_continuation)
    :t(_t), res(_res), continuation(_continuation)
  {
  }

  void dispatch()
  {
    t->join();
    continuation(t, res);
  }
};

void download_thread::operator()()
{
  vscreen_post_event(new download_thread_complete_event(this, m->do_download(),
							continuation));
}
