// channel.h                                              -*-c++-*-
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

#ifndef EVENT_QUEUE_H
#define EVENT_QUEUE_H

#include "threads.h"

#include <deque>

namespace threads
{
  /** A simple unbounded communications channel suitable for use as,
   *  eg, an event queue.  Writers never block (except as necessary to
   *  maintain consistency), but readers block while the queue is
   *  empty.  If there are multiple readers, they will receive results
   *  in an arbitrary order.
   *
   *  This implementation is safe and flexible, but not terribly
   *  efficient.  For instance, readers and writers block each other
   *  out (other approaches can avoid this unless the queue is empty).
   *  In aptitude it's used for the global event queue, which doesn't
   *  get all that many deliveries, so it should be good enough.  Just
   *  don't use it to stream bits off a network connection and then
   *  complain I didn't warn you!
   */
  template<typename T>
  class event_queue
  {
    std::deque<T> q;

    condition c;
    mutable mutex m;

    struct not_empty
    {
      const std::deque<T> &q;
    public:
      not_empty(const std::deque<T> &_q)
	:q(_q)
      {
      }

      bool operator()() const
      {
	return !q.empty();
      }
    };

    event_queue(const event_queue &other);
    event_queue &operator=(const event_queue &other);
  public:
    /** Create an empty queue. */
    event_queue()
    {
    }

    ~event_queue()
    {
    }

    /** Push the given value onto the event queue. */
    void put(const T &t)
    {
      mutex::lock l(m);

      q.push_back(t);
      c.wake_one();
    }

    /** Retrieve a single value from the event queue. */
    T get()
    {
      mutex::lock l(m);

      c.wait(l, not_empty(q));
      T rval = q.front();
      q.pop_front();

      return rval;
    }

    /** Retrieve a single value from the event queue if the queue is
     *  non-empty.
     *
     *  \param out the location in which to store the retrieved value
     *  \return \b true iff a value was retrieved.
     */
    bool try_get(T &out)
    {
      mutex::lock l(m);

      if(q.empty())
	return false;
      else
	{
	  out = q.front();
	  q.pop_front();
	  return true;
	}
    }

    /** Retrieve a single value from the event queue, or fail if the
     *  time "until" is reached.
     */
    bool timed_get(T &out, const timespec &until)
    {
      mutex::lock l(m);

      if(c.timed_wait(l, until, not_empty(q)))
	{
	  out = q.front();
	  q.pop_front();
	  return true;
	}
      else
	return false;
    }

    /** Return \b true if the event queue is currently empty. */
    bool empty() const
    {
      // Not sure the lock is required here, but it makes things a bit
      // safer in case the STL is thread-unsafe in weird ways.
      mutex::lock l(m);
      bool rval = q.empty();
      return rval;
    }
  };
}

#endif
