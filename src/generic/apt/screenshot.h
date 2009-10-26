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

#include <generic/util/safe_slot.h>

namespace aptitude
{
  class download_manager;

  /** \brief The type of screenshot to download. */
  enum screenshot_type
    {
      /** \brief Download a small thumbnail image. */
      screenshot_thumbnail,
      /** \brief Download a full-size screenshot. */
      screenshot_full
    };

  /** \brief Handle to a screenshot download request; can be used to
   *  cancel the request after it has been enqueued.
   */
  class download_request
  {
  public:
    virtual ~download_request();

    /** \brief Cancel this download request.
     *
     *  This is safe to call from any thread.  There is no guarantee
     *  that the download won't complete anyway, but if it hasn't
     *  completed by the next call to Pulse() (once a second or so),
     *  it will be canceled.  Even if it does complete, the callbacks
     *  won't be invoked.
     */
    virtual void cancel() = 0;
  };

  /** \brief  Create a download manager that will fetch a screenshot
   *          for the given package.
   *
   *  \param package       The name of the package whose screenshot should
   *                       be downloaded.
   *
   *  \param success_slot  A callback to invoke when the screenshot is
   *                       downloaded.  Invoked from the thread that
   *                       calls run().
   *
   *  \param failure_slot  A callback to invoke when the screenshot
   *                       fails to be downloaded.  Invoked from the
   *                       thread that calls run().
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
		 const safe_slot1<void, temp::name> &success_slot,
		 const safe_slot1<void, std::string> &failure_slot,
		 screenshot_type type);
}
