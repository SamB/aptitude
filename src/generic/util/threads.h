// threads.h                                              -*-c++-*-
//
//   Copyright (C) 2005-2006 Daniel Burrows
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
// A simple thread wrapper library.  I'm not using the existing ones
// in order to keep aptitude's dependency count low (as long as I
// don't need too much out of it, this should be fairly
// simple..right?).  The API was inspired by that of boost::threads.

#ifndef THREADS_H
#define THREADS_H

#include <errno.h>
#include "exception.h"

namespace threads
{
  /** Base exception class for thread errors. */
  class ThreadException : public Exception
  {
  };

  /** Creation failure; according to pthread_create(3), this only occurs
   *  if there aren't enough system resources to create a thread.
   */
  class ThreadCreateException : public ThreadException
  {
  public:
    std::string errmsg() const;
  };

  /** Join failure. */
  class ThreadJoinException : public ThreadException
  {
    std::string reason;
  public:
    ThreadJoinException(const int error);

    std::string errmsg() const;
  };

  /** Thrown when code tries to wait on a condition and passes a
   *  released lock in.
   */
  class ConditionNotLockedException : public ThreadException
  {
  public:
    std::string errmsg() const;
  };

  /** Thrown when a mutex is locked twice. */
  class DoubleLockException : public ThreadException
  {
  public:
    std::string errmsg() const;
  };

  /** A thread class based on the Boost thread class.  Like the Boost
   *  class, it is non-copyable.
   */
  class thread
  {
    pthread_t tid;
    bool joined;

    thread(const thread &other);
    thread &operator=(const thread &other);



    template<typename F>
    static void *bootstrap(void *p)
    {
      F thunk(*((F *) p));

      delete ((F *) p);

      thunk();

      return 0;
    }

  public:
    class attr
    {
      pthread_attr_t attrs;

      friend class thread;
    public:
      attr()
      {
	pthread_attr_init(&attrs);
      }

      // All attributes except detach state can be manipulated (detach
      // state is left at PTHREAD_CREATE_JOINABLE).

      void set_inherit_sched(int i)
      {
	pthread_attr_setinheritsched(&attrs, i);
      }

      int get_inherit_sched() const
      {
	int rval;
	pthread_attr_getinheritsched(&attrs, &rval);
	return rval;
      }

      void set_sched_param(const sched_param &sp)
      {
	pthread_attr_setschedparam(&attrs, &sp);
      }

      sched_param get_sched_param() const
      {
	sched_param rval;
	pthread_attr_getschedparam(&attrs, &rval);
	return rval;
      }

      void set_sched_policy(int p)
      {
	pthread_attr_setschedpolicy(&attrs, p);
      }

      int get_sched_policy() const
      {
	int rval;
	pthread_attr_getschedpolicy(&attrs, &rval);
	return rval;
      }

      void set_scope(int p)
      {
	pthread_attr_setscope(&attrs, p);
      }

      int get_scope() const
      {
	int rval;
	pthread_attr_getscope(&attrs, &rval);
	return rval;
      }

      ~attr()
      {
	pthread_attr_destroy(&attrs);
      }
    };

    /** Create a new thread.
     *
     *  \param thunk a function object of no parameters that should
     *  be invoked to start this thread.  Must be copyable.
     *
     *  \param a the attributes with which to create the new thread.
     */
    template<typename F>
    thread(const F &thunk, const attr &a = attr())
      :joined(false)
    {
      // Create a thunk on the heap to pass to the new thread.
      F *tmp = new F(thunk);

      if(pthread_create(&tid, &a.attrs, &thread::bootstrap<F>, tmp) != 0)
	{
	  delete tmp;

	  throw ThreadCreateException();
	}
    }

    ~thread()
    {
      if(!joined)
	pthread_detach(tid);
    }

    /** Wait for this thread to finish. */
    void join()
    {
      int rval = pthread_join(tid, NULL);

      if(rval != 0)
	throw ThreadJoinException(rval);
      else
	joined = true;
    }

    /** Cancel this thread. */
    void cancel()
    {
      pthread_cancel(tid);
    }
  };

  // If you have a necessarily noncopyable object to bootstrap a
  // thread with, you can pass it through the following structure; be
  // warned, though, that this won't delete it when the operation
  // completes.
  template<typename F>
  struct noncopy_bootstrap
  {
    F &f;
  public:
    noncopy_bootstrap(F &_f)
      :f(_f)
    {
    }

    void operator()()
    {
      f();
    }
  };

  class condition;

  // The mutex abstraction
  class mutex
  {
  public:
    class lock;
    class try_lock;

  private:
    pthread_mutex_t m;

    friend class lock;
    friend class try_lock;

    // Conditions need to look inside mutexes and locks to find the
    // real mutex object so the underlying thread library can do an
    // atomic unlock-and-wait.
    friend class condition;

    mutex(const mutex &other);
    mutex &operator=(const mutex &other);
  public:
    /** A mutex attributes object. */
    class attr
    {
      pthread_mutexattr_t attrs;

      friend class mutex;

    public:
      attr()
      {
	pthread_mutexattr_init(&attrs);
      }

      attr(int kind)
      {
	pthread_mutexattr_init(&attrs);
	pthread_mutexattr_settype(&attrs, kind);
      }

      ~attr()
      {
	pthread_mutexattr_destroy(&attrs);
      }

      int settype(int kind)
      {
	return pthread_mutexattr_settype(&attrs, kind);
      }

      int gettype()
      {
	int rval;
	pthread_mutexattr_gettype(&attrs, &rval);
	return rval;
      }
    };

    /** Represents a lock on a mutex.  Can be released and re-asserted
     *  as desired; when the lock goes out of scope, it will
     *  automatically be released if necessary.
     */
    class lock
    {
      mutex &parent;

      bool locked;

      friend class condition;

      lock(const lock &other);
      lock &operator=(const lock &other);
    public:
      lock(mutex &_parent)
	:parent(_parent), locked(false)
      {
	acquire();
      }

      /** Lock the associated mutex. */
      void acquire()
      {
	if(locked)
	  throw DoubleLockException();

	pthread_mutex_lock(&parent.m);
	locked = true;
      }

      /** Unlock the associated mutex. */
      void release()
      {
	pthread_mutex_unlock(&parent.m);
	locked = false;
      }

      bool get_locked() const
      {
	return locked;
      }

      ~lock()
      {
	if(locked)
	  pthread_mutex_unlock(&parent.m);
      }
    };

    /** Represents a non-blocking lock on a mutex. */
    class try_lock
    {
      mutex &parent;

      bool locked;

      friend class condition;

      try_lock(const try_lock &other);
      try_lock &operator=(const try_lock &other);
    public:
      try_lock(mutex &_parent)
	:parent(_parent)
      {
	acquire();
      }

      ~try_lock()
      {
	if(locked)
	  pthread_mutex_unlock(&parent.m);
      }

      void acquire()
      {
	if(locked)
	  throw DoubleLockException();

	locked = pthread_mutex_trylock(&parent.m);
      }

      void release()
      {
	pthread_mutex_unlock(&parent.m);
	locked = false;
      }

      bool get_locked() const
      {
	return locked;
      }
    };

    mutex()
    {
      pthread_mutex_init(&m, NULL);
    }

    mutex(const attr &a)
    {
      pthread_mutex_init(&m, &a.attrs);
    }

    ~mutex()
    {
      pthread_mutex_destroy(&m);
    }
  };

  /** A mutex that is initialized to be recursive.  Used because
   *  apparently C++ global constructors can't take arguments.
   */
  class recursive_mutex : public mutex
  {
  public:
    recursive_mutex()
      :mutex(attr(PTHREAD_MUTEX_RECURSIVE))
    {
    }
  };

  /** A abstraction over conditions.  When a condition variable is
   *  destroyed, any threads that are still blocked on it are woken
   *  up.
   */
  class condition
  {
    pthread_cond_t cond;
  public:
    condition()
    {
      pthread_cond_init(&cond, NULL);
    }

    ~condition()
    {
      // Wakey wakey
      pthread_cond_broadcast(&cond);
      pthread_cond_destroy(&cond);
    }

    void wake_one()
    {
      pthread_cond_signal(&cond);
    }

    void wake_all()
    {
      pthread_cond_broadcast(&cond);
    }

    /** Wait with the given guard (should be a lock type that is a
     *  friend of this condition object).
     */
    template<typename Lock>
    void wait(const Lock &l)
    {
      if(!l.get_locked())
	throw ConditionNotLockedException();

      pthread_cond_wait(&cond, &l.parent.m);
    }

    /** Wait until the given predicate returns \b true. */
    template<typename Lock, typename Pred>
    void wait(const Lock &l, Pred p)
    {
      if(!l.get_locked())
	throw ConditionNotLockedException();

      while(!p())
	wait(l);
    }

    /** Wait until either the condition is signalled or until the
     *  given time.
     *
     *  \param l the guard of the condition
     *  \param until the time at which the wait should terminate
     *
     *  \return \b true if the condition occurred or \b false if time
     *                  ran out.
     */
    template<typename Lock>
    bool timed_wait(const Lock &l, const timespec &until)
    {
      if(!l.get_locked())
	throw ConditionNotLockedException();

      int rval;

      // Ignore EINTR for the time being.
      while((rval = pthread_cond_timedwait(&cond, &l.parent.m, &until)) == EINTR)
	;

      return rval != ETIMEDOUT;
    }

    /** Wait either until the condition is signalled while the given
     *  predicate is \b true or until the given time.
     */
    template<typename Lock, typename Pred>
    bool timed_wait(const Lock &l, const timespec &until, const Pred &p)
    {
      if(!l.get_locked())
	throw ConditionNotLockedException();

      while(!p())
	{
	  if(!timed_wait(l, until))
	    return false;
	}

      return true;
    }
  };

  /** A higher-level abstraction borrowed from Concurrent Haskell,
   *  which borrowed it from another language I forget.  This
   *  represents a "box" that can either hold a value or be empty.
   *  Any thread can take the current value of the box or place a new
   *  value inside it; the attempt will block until a value is
   *  available or the box is empty, respectively.  It's sort of a
   *  single-element bounded communications channel.
   *
   *  The value in the box is stored with copying semantics.  Like the
   *  other threading primitives, boxes are not copyable.
   */
  template<typename T>
  class box
  {
    T val;
    bool filled;

    condition cond;
    mutex m;

    box(const box &other);
    box &operator=(const box &other);
  public:
    /** Create an empty box. */
    box()
      :filled(false)
    {
    }

    /** Create a box containing the given value. */
    box(const T &_val)
      :val(_val), filled(true)
    {
    }

    /** Retrieve the current value of this box.  If the box is empty,
     *  block until it is full.
     */
    T take();

    /** Fill this box with a value.  If the box is full, block until
     *  it is empty.
     */
    void put(const T &t);

    /** If there is a value in the box, retrieve it immediately;
     *  otherwise do nothing.
     *
     *  \param out the location in which the value should be stored
     *  \return \b true iff a value was found in the box
     */
    bool try_take(T &out);

    /** If the box is empty, place a value in it; otherwise, do
     *  nothing.
     *
     *  \param t the value to place in the box
     *
     *  \return \b true iff the box was empty (and hence was filled
     *  with t)
     */
    bool try_put(const T &t);

    /** As try_take(), but wait for the given amount of time before
     *  giving up.
     */
    bool timed_take(T &out, const timespec &until);

    /** As try_put(), but wait for the given amount of time before
     *  giving up.
     */
    bool timed_put(const T &t, const timespec &until);

    /** Atomically modify the contents of the box; if an exception is
     *  thrown by the given function object, no action will be
     *  performed.
     */
    template<typename Mutator>
    void update(const Mutator &m);
  };

  /** A box specialized for 'void'; may make it easier to write
   *  other templated classes.  Could maybe just be a mutex, but I
   *  don't think you can quite mimic the box API that way.
   */
  template<>
  class box<void>
  {
    bool filled;
    mutex m;
    condition cond;
  public:
    box()
      :filled(false)
    {
    }

    box(bool _filled)
      :filled(_filled)
    {
    }

    void take();

    void put();

    bool try_take();
    bool try_put();

    bool timed_take(const timespec &until);
    bool timed_put(const timespec &until);

    template<typename Mutator>
    void update(const Mutator &m)
    {
      take();
      try
	{
	  m();
	}
      catch(...)
	{
	  put();
	  throw;
	}

      put();
    }
  };

  /** Internal helper struct. */
  struct bool_ref_pred
  {
    const bool &b;
  public:
    bool_ref_pred(const bool &_b)
      :b(_b)
    {
    }

    bool operator()() const
    {
      return b;
    }
  };

  /** Internal helper struct. */
  struct not_bool_ref_pred
  {
    const bool &b;
  public:
    not_bool_ref_pred(const bool &_b)
      :b(_b)
    {
    }

    bool operator()() const
    {
      return !b;
    }
  };

  template<typename T>
  inline
  T box<T>::take()
  {
    mutex::lock l(m);

    cond.wait(l, bool_ref_pred(filled));

    filled = false;

    // Interesting question: does l get released before or after the
    // copy?  To be safe, I explicitly copy before I return.
    T rval = val;
    return rval;
  }

  inline
  void box<void>::take()
  {
    mutex::lock l(m);
    cond.wait(l, bool_ref_pred(filled));
    filled = false;
  }

  template<typename T>
  inline
  bool box<T>::try_take(T &out)
  {
    mutex::lock l(m);

    if(filled)
      {
	filled = false;
	out = val;
	return true;
      }
    else
      return false;
  }

  inline
  bool box<void>::try_take()
  {
    mutex::lock l(m);

    if(filled)
      {
	filled = false;
	return true;
      }
    else
      return false;
  }

  template<typename T>
  inline
  bool box<T>::timed_take(T &out, const timespec &until)
  {
    mutex::lock l(m);

    if(cond.timed_wait(l, until, bool_ref_pred(filled)))
      {
	filled = false;
	out = val;
	return true;
      }
    else
      return false;
  }

  inline
  bool box<void>::timed_take(const timespec &until)
  {
    mutex::lock l(m);

    if(cond.timed_wait(l, until, bool_ref_pred(filled)))
      {
	filled = false;
	return true;
      }
    else
      return false;
  }

  template<typename T>
  inline
  void box<T>::put(const T &new_val)
  {
    mutex::lock l(m);

    cond.wait(l, not_bool_ref_pred(filled));

    filled = true;
    val = new_val;
    cond.wake_one();
  }

  inline
  void box<void>::put()
  {
    mutex::lock l(m);

    cond.wait(l, not_bool_ref_pred(filled));

    filled = true;
    cond.wake_one();
  }

  template<typename T>
  inline
  bool box<T>::try_put(const T &new_val)
  {
    mutex::lock l(m);

    if(!filled)
      {
	filled = true;
	val = new_val;
	cond.wake_one();
	return true;
      }
    else
      return false;
  }

  inline
  bool box<void>::try_put()
  {
    mutex::lock l(m);

    if(!filled)
      {
	filled = true;
	cond.wake_one();
	return true;
      }
    else
      return false;
  }

  template<typename T>
  inline
  bool box<T>::timed_put(const T &new_val, const timespec &until)
  {
    mutex::lock l(m);

    if(cond.timed_wait(l, until, not_bool_ref_pred(filled)))
      {
	filled = true;
	val = new_val;
	cond.wake_one();
	return true;
      }
    else
      return false;
  }

  inline
  bool box<void>::timed_put(const timespec &until)
  {
    mutex::lock l(m);

    if(cond.timed_wait(l, until, not_bool_ref_pred(filled)))
      {
	filled = true;
	cond.wake_one();
	return true;
      }
    else
      return false;
  }

  template<typename T>
  template<typename Mutator>
  inline
  void box<T>::update(const Mutator &m)
  {
    mutex::lock l(m);

    cond.wait(l, bool_ref_pred(filled));

    T new_val = m(val);

    val = new_val;
    cond.wake_one();
  }

  // A ptr_box is like a box, but it wraps a pointer to its internal
  // object.  When a filled ptr_box is destroyed, it deletes the
  // pointer that it contains.
  template<typename T>
  class ptr_box
  {
    box<T *> b;
  public:
    ptr_box()
    {
    }

    ptr_box(const T *val)
      :b(val)
    {
    }

    ~ptr_box()
    {
      T *x;

      if(b.try_get(x))
	delete x;
    }

    T *take()
    {
      return b.take();
    }

    bool try_take(const T * &out)
    {
      return b.try_take(out);
    }

    bool timed_take(const T * &out, const timespec &until)
    {
      return b.timed_take(out);
    }

    void put(const T *in)
    {
      b.put(in);
    }

    bool try_put(const T *in)
    {
      return b.try_put(in);
    }

    bool timed_put(const T *in, const timespec &until)
    {
      return b.timed_put(in, until);
    }
  };
}

#endif // THREADS_H

