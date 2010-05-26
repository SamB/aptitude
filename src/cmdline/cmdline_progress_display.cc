/** \file cmdline_progress_display.cc */

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

// Local includes:
#include "cmdline_progress_display.h"

#include "transient_message.h"

#include <generic/util/progress_info.h>


// System includes:
#include <boost/format.hpp>
#include <boost/make_shared.hpp>

#include <cwidget/generic/util/transcode.h>

#include <sys/time.h>

using aptitude::util::progress_info;
using aptitude::util::progress_type_bar;
using aptitude::util::progress_type_none;
using aptitude::util::progress_type_pulse;
using boost::make_shared;
using boost::shared_ptr;
using boost::wformat;
using cwidget::util::transcode;

namespace aptitude
{
  namespace cmdline
  {
    namespace
    {
      // \todo This should be configurable.
      const int progress_update_interval = 0.25;

      class progress_display_impl : public progress_display
      {
        // The last time that we updated; initially set to the
        // beginning of the epoch.
        struct timeval last_update_time;

        // The last progress that was displayed; we always update the
        // display if the mode or the message changed.
        progress_info last_progress;

        shared_ptr<transient_message> message;

        // Using the current time and the progress information to
        // display, determine if an update is required.
        bool should_update(const progress_info &progress) const;

      public:
        progress_display_impl(const shared_ptr<transient_message> &_message);

        void set_progress(const progress_info &progress,
                          bool force_update);
      };

      progress_display_impl::progress_display_impl(const shared_ptr<transient_message> &_message)
        : last_progress(progress_info::none()),
          message(_message)
      {
      }

      bool progress_display_impl::should_update(const progress_info &progress) const
      {
        if(progress.get_type() != last_progress.get_type())
          return true;

        if(progress.get_progress_status() != last_progress.get_progress_status())
          return true;


        // Time checking code shamelessly stolen from apt, since we
        // know theirs works.
        struct timeval now;
        gettimeofday(&now, 0);
        double diff =
          now.tv_sec - last_update_time.tv_sec +
          (now.tv_usec - last_update_time.tv_usec)/1000000.0;

        return diff < progress_update_interval;
      }

      void progress_display_impl::set_progress(const progress_info &progress,
                                               bool force_update)
      {
        if(force_update || should_update(progress))
          {
            switch(progress.get_type())
              {
              case progress_type_none:
                message->set_text(L"");
                break;

              case progress_type_pulse:
                message->set_text( (wformat(L"      %s")
                                    % transcode(progress.get_progress_status())).str() );
                break;

              case progress_type_bar:
                message->set_text( (wformat(L"[%3d%%] %s")
                                    % progress.get_progress_percent_int()
                                    % transcode(progress.get_progress_status())).str() );
                break;

              default:
                message->set_text(L"INTERNAL ERROR");
                break;
              }

            last_progress = progress;
            gettimeofday(&last_update_time, 0);
          }
      }
    }

    progress_display::~progress_display()
    {
    }

    shared_ptr<progress_display>
    create_progress_display(const shared_ptr<transient_message> &message)
    {
      return make_shared<progress_display_impl>(message);
    }

    shared_ptr<progress_display>
    create_progress_display(const shared_ptr<terminal> &term,
                            const shared_ptr<terminal_locale> &term_locale)
    {
      const shared_ptr<transient_message> message =
        create_transient_message(term, term_locale);

      return create_progress_display(message);
    }
  }
}
