/** \file cmdline_download_progress_display.h */    // -*-c++-*-

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

#ifndef CMDLINE_DOWNLOAD_PROGRESS_DISPLAY
#define CMDLINE_DOWNLOAD_PROGRESS_DISPLAY

// System includes:
#include <boost/shared_ptr.hpp>

namespace aptitude
{
  namespace views
  {
    class download_progress;
  }

  namespace cmdline
  {
    class terminal_locale;
    class terminal_metrics;
    class transient_message;

    /**
     * Create a new command-line download progress display.
     *
     *  \param message    The transient-message object used to display the
     *                    download status.
     *  \param terminal   The terminal on which the display will appear.
     *                    Used to get the screen width for formatting purposes.
     *  \param terminal_locale  The locale of the given terminal.
     */
    boost::shared_ptr<views::download_progress>
    create_download_progress_display(const boost::shared_ptr<transient_message> &message,
                                     const boost::shared_ptr<terminal_locale> &term_locale,
                                     const boost::shared_ptr<terminal_metrics> &term_metrics);
  }
}

#endif // CMDLINE_DOWNLOAD_PROGRESS_DISPLAY
