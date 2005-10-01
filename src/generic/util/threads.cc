// threads.cc
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

#include "threads.h"

#include "util.h"

#include <aptitude.h>

#include <errno.h>

namespace threads
{
  std::string ThreadCreateException::errmsg() const
  {
    return _("Not enough resources to create thread");
  }

  ThreadJoinException::ThreadJoinException(int error)
  {
    std::string msg;

    // Deliberately untranslated as these refer to errors that are
    // internal to the program; the user can't do anything about them.
    switch(error)
      {
      case ESRCH:
	msg = "Invalid thread ID.";
	break;
      case EINVAL:
	msg = "Thread previously detached or joined";
	break;
      case EDEADLK:
	msg = "Deadlock (attempt to self-join)";
	break;
      }

    reason = ssprintf("Unable to join thread: %s", msg.c_str());
  }

  std::string ThreadJoinException::errmsg() const
  {
    return reason;
  }

  std::string ConditionNotLockedException::errmsg() const
  {
    return "Attempt to wait on a condition with a non-locked mutex";
  }

  std::string DoubleLockException::errmsg() const
  {
    return "Mutex double-locked";
  }
}

