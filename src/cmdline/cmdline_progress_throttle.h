/** \file cmdline_progress_throttle.h */   // -*-c++-*-

// Copyright (C) 2010 Daniel Burrows
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; see the file COPYING.  If not, write to
// the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

#ifndef CMDLINE_PROGRESS_THROTTLE_H
#define CMDLINE_PROGRESS_THROTTLE_H

// System includes:
#include <boost/shared_ptr.hpp>

namespace aptitude
{
  namespace cmdline
  {
    /** \brief Used to check whether enough time has passed that a
     *  progress display should be updated.
     */
    class progress_throttle
    {
    public:
      virtual ~progress_throttle() = 0;

      /** \return \b true if the progress display should be updated. */
      virtual bool update_required() = 0;

      /** \brief Reset the timer that controls when the display is
       *  updated.
       */
      virtual void reset_timer() = 0;
    };

    /** \brief Create a progress_throttle object. */
    boost::shared_ptr<progress_throttle> create_progress_throttle();
  }
}

#endif // CMDLINE_PROGRESS_THROTTLE_H
