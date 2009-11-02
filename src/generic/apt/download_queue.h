/** \file download_queue.h */     // -*-c++-*-


// Copyright (C) 2009 Daniel Burrows
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

#ifndef DOWNLOAD_QUEUE_H
#define DOWNLOAD_QUEUE_H

#include <boost/shared_ptr.hpp>

#include <generic/util/temp.h>

#include <sigc++/slot.h>

#include "post_thunk.h"

namespace aptitude
{
  /** \brief The callbacks that should be invoked in response to
   *  events that occur during the download of a particular item.
   */
  class download_callbacks
  {
  public:
    download_callbacks()
    {
    }

    virtual ~download_callbacks()
    {
    }

    /** \brief The callback invoked when the download completes
     *  successfully.
     *
     *  \param filename The file into which the item was downloaded.
     */
    virtual void success(const temp::name &filename) = 0;

    /** \brief The callback invoked when the download fails.
     *
     *  \param msg  A message describing what happened.
     *
     *  The default implementation does nothing.
     */
    virtual void failure(const std::string &msg)
    {
    }

    /** \brief Invoked when the item is partly downloaded.
     *
     *  \param filename  The file name that the download is
     *                   being written to.
     *  \param currentSize  The current size of the file.
     *  \param totalSize    The total size of the file.
     *
     *  The default implementation does nothing.
     */
    virtual void partial_download(const temp::name &filename,
				  unsigned long currentSize,
				  unsigned long totalSize)
    {
    }
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

  /** \brief Add a new download to the background thread's queue.
   *
   *  \param uri          The URI to download.
   *
   *  \param callbacks    A callback object used to respond to events
   *                      on the download.
   *
   *  \param post_thunk   A function used to pass download events to
   *                      the main thread.
   *
   *  \return a handle that can be used to cancel the download.
   */
  boost::shared_ptr<download_request>
  queue_download(const std::string &uri,
		 const boost::shared_ptr<download_callbacks> &callbacks,
		 post_thunk_f post_thunk);
}

#endif // DOWNLOAD_QUEUE_H
