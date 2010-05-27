/** \file cmdline_search_progress.cc */


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


#include "cmdline_search_progress.h"

#include "cmdline_progress_display.h"
#include "cmdline_progress_throttle.h"

#include <generic/util/progress_info.h>


#include <boost/format.hpp>

#include <cwidget/generic/util/ref_ptr.h>

using aptitude::util::progress_info;
using aptitude::util::progress_type_bar;
using aptitude::util::progress_type_none;
using aptitude::util::progress_type_pulse;
using boost::format;
using boost::shared_ptr;
using cwidget::util::ref_ptr;

namespace aptitude
{
  namespace cmdline
  {
    void search_progress(const progress_info &info,
                         const shared_ptr<progress_display> &progress_msg,
                         const shared_ptr<progress_throttle> &throttle,
                         const std::string &pattern)
    {
      if(!throttle->update_required())
        return;

      // We interpret the progress_info to add a prefix to its message
      // if it has one.
      switch(info.get_type())
        {
        case progress_type_none:
          progress_msg->set_progress(info, false);
          break;

        case progress_type_pulse:
          progress_msg->set_progress(progress_info::pulse( (format("%s: %s")
                                                            % pattern
                                                            % info.get_progress_status())
                                                           .str()),
                                     false);
          break;

        case progress_type_bar:
          progress_msg->set_progress(progress_info::bar(info.get_progress_fraction(),
                                                        (format("%s: %s")
                                                         % pattern
                                                         % info.get_progress_status())
                                                        .str()),
                                     false);
          break;
        }

      throttle->reset_timer();
    }
  }
}
