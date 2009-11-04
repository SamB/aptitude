/** \file screenshot.h */   // -*-c++-*-

// Copyright (C) 2009 Daniel Burrows
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; see the file COPYING.  If not, write to
// the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

#ifndef SCREENSHOT_H
#define SCREENSHOT_H

#include <boost/shared_ptr.hpp>

#include <generic/util/safe_slot.h>

#include "post_thunk.h"

namespace aptitude
{
  class download_callbacks;
  class download_request;

  /** \brief The type of screenshot to download. */
  enum screenshot_type
    {
      /** \brief Download a small thumbnail image. */
      screenshot_thumbnail,
      /** \brief Download a full-size screenshot. */
      screenshot_full
    };

  /** \brief  Add a download to the global queue that will fetch a
   *          screenshot for the given package.
   *
   *  \param package       The name of the package whose screenshot should
   *                       be downloaded.
   *
   *  \param callbacks     A callback object invoked for events relating
   *                       to the download.
   *
   *  \param post_thunk    A function used to invoke callbacks in the
   *                       main thread.
   *
   *  \param type          Which screenshot to download.
   *
   *  This function will search the global download cache, if one is
   *  initialized, for an existing screenshot.  It can be invoked from
   *  a background thread, as long as the download cache isn't
   *  destroyed while it's running (i.e., whatever calls it should
   *  hook into cache_closing and wait for get_screenshot() to
   *  return before it allows cache_closing to return).
   *
   *  \return a handle that can be used to cancel the download.
   */
  boost::shared_ptr<download_request>
  get_screenshot(const std::string &name,
		 const boost::shared_ptr<download_callbacks> &callbacks,
		 post_thunk_f post_thunk,
		 screenshot_type type);
}

#endif // SCREENSHOT_H
