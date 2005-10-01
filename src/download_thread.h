// download_thread                              -*-c++-*-
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

#ifndef DOWNLOAD_THREAD_H
#define DOWNLOAD_THREAD_H

#include <apt-pkg/acquire.h>
#include <apt-pkg/error.h>

#include <generic/util/threads.h>

#include <sigc++/slot.h>

class download_manager;
class download_signal_log;

/** A proxy status object that posts messages to the main thread.
 *  Each message "blocks" the download until the user deals with it,
 *  just as if it was running in the main thread (I'm not sure
 *  anything else is safe).
 *
 *  This object does NOT own the encapsulated status object and will
 *  NOT delete it.
 */
class background_status : public pkgAcquireStatus
{
  download_signal_log *real_status;
public:
  void Fetched(unsigned long Size, unsigned long ResumePoint);
  bool MediaChange(std::string Media, std::string Drive);
  void IMSHit(pkgAcquire::ItemDesc &);
  void Fetch(pkgAcquire::ItemDesc &);
  void Done(pkgAcquire::ItemDesc &);
  void Fail(pkgAcquire::ItemDesc &);
  bool Pulse(pkgAcquire *Owner);
  void Start();
  void Stop();
  void Complete();

  background_status(download_signal_log *_real_status)
    :real_status(_real_status)
  {
  }
};

/** The thread that performs the download. */
class download_thread
{
  threads::box<bool> cancelled;

  /** The bundled download_manager object.  It should have been
   *  initialized using a background_status wrapper as above, and you
   *  should join() this thread before deleting it.  (it is also OK
   *  to join() the thread first thing in the object's destructor)
   */
  download_manager *m;

  /** The continuation of this download, invoked in the main thread
   *  with this thread and the result of the run as parameters.  The
   *  thread will be automatically join()ed before the continuation is
   *  deleted.
   */
  sigc::slot2<void, download_thread *, pkgAcquire::RunResult> continuation;

  threads::thread *t;

  download_thread(const download_thread &other);
  download_thread &operator=(const download_thread &other);
public:
  download_thread(download_manager *manager,
		  const sigc::slot2<void, download_thread *, pkgAcquire::RunResult> &_continuation)
    : cancelled(false), m(manager), continuation(_continuation), t(NULL)
  {
  }

  ~download_thread()
  {
    delete t;
  }

  void operator()();

  void start()
  {
    if(t != NULL)
      _error->Error("Attempt to start a download thread twice!");
    else
      t = new threads::thread(threads::noncopy_bootstrap<download_thread>(*this));
  }

  void join()
  {
    t->join();
  }
};

#endif // DOWNLOAD_THREAD_H
