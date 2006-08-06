// test_threads.cc
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

#include <cppunit/extensions/HelperMacros.h>

#include <generic/util/event_queue.h>
#include <generic/util/threads.h>

#include <iostream>

#include <sys/time.h>
#include <time.h>

class TestThreads : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(TestThreads);

  CPPUNIT_TEST(testBox);
  CPPUNIT_TEST(testTimedTake);
  CPPUNIT_TEST(testEventQueue);
  CPPUNIT_TEST(testAutoDetach);

  CPPUNIT_TEST_SUITE_END();

public:
  // Ye Olde Addition Thread
  struct add_thread
  {
    int n;
    threads::box<int> &count;
  public:
    add_thread(int _n, threads::box<int> &_count)
      :n(_n), count(_count)
    {
    }

    void operator()() const
    {
      for(int i = 0; i < n; ++i)
	count.put(count.take()+1);
    }
  };

  void do_testBox()
  {
    const int thread_count = 50;
    const int thread_limit = 1000;

    threads::box<int> b(100);

    CPPUNIT_ASSERT_EQUAL(100, b.take());

    std::auto_ptr<threads::thread> writers[thread_count];

    for(int i = 0; i<thread_count; ++i)
      writers[i] = std::auto_ptr<threads::thread>(new threads::thread(add_thread(thread_limit, b)));

    int foo;
    CPPUNIT_ASSERT(!b.try_take(foo));

    CPPUNIT_ASSERT(b.try_put(-1));

    for(int i = 0; i<thread_count; ++i)
      writers[i]->join();

    CPPUNIT_ASSERT_EQUAL(thread_count*thread_limit-1, b.take());
  }

  void testBox()
  {
    try
      {
	do_testBox();
      }
    catch(const Exception &e)
      {
	std::cerr << "Caught exception in testBox: " << e.errmsg() << std::endl;
	throw;
      }
  }

  void testTimedTake()
  {
    timeval now;

    threads::box<int> b;

    gettimeofday(&now, NULL);
    timespec timeout;
    timeout.tv_sec = now.tv_sec + 2;
    timeout.tv_nsec = now.tv_usec * 1000;

    int dummy;

    // Test that timing out trying to take values from an empty box works:

    CPPUNIT_ASSERT(!b.timed_take(dummy, timeout));

    timeval now2;
    gettimeofday(&now2, NULL);

    CPPUNIT_ASSERT(now2.tv_sec >= now.tv_sec + 2);
    CPPUNIT_ASSERT(now2.tv_sec > now.tv_sec + 2 || now2.tv_usec >= now.tv_usec);

    // Test that we can retrieve a value from a full box:

    timeout.tv_sec = now2.tv_sec + 2;
    timeout.tv_nsec = now2.tv_usec * 1000;
    b.put(5);
    CPPUNIT_ASSERT(b.timed_take(dummy, timeout));
    CPPUNIT_ASSERT_EQUAL(5, dummy);
  }

  class event_queue_write_thread
  {
    threads::event_queue<std::pair<int, int> > &eq;

    int id, n;
  public:
    event_queue_write_thread(threads::event_queue<std::pair<int, int> > &_eq,
			     int _id, int _n)
      :eq(_eq), id(_id), n(_n)
    {
    }

    void operator()() const
    {
      for(int i = 0; i < n; ++i)
	eq.put(std::pair<int, int>(id, i));
    }
  };

  void testEventQueue()
  {
    const int thread_count = 100;
    const int thread_limit = 1000;

    threads::event_queue<std::pair<int, int> > eq;

    std::auto_ptr<threads::thread> writers[thread_count];
    int last_thread_msg[thread_count];

    for(int i = 0; i < thread_count; ++i)
      last_thread_msg[i] = -1;

    for(int i = 0; i < thread_count; ++i)
      writers[i] = std::auto_ptr<threads::thread>(new threads::thread(event_queue_write_thread(eq, i, thread_limit)));

    for(int i = 0; i < thread_count * thread_limit; ++i)
      {
	std::pair<int, int> next = eq.get();

	CPPUNIT_ASSERT_EQUAL(next.second-1, last_thread_msg[next.first]);
	last_thread_msg[next.first] = next.second;
      }

    for(int i = 0; i < thread_count; ++i)
      writers[i]->join();

    CPPUNIT_ASSERT(eq.empty());
  }

  struct do_nothing
  {
  public:
    void operator()() const
    {
    }
  };

  void testAutoDetach()
  {
    threads::thread t(do_nothing());
  }
};

CPPUNIT_TEST_SUITE_REGISTRATION(TestThreads);
