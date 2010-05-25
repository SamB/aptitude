/** \file text_progress.cc */


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

#include "text_progress.h"

#include "apt.h"
#include "config_signal.h"

#include <aptitude.h>


#include <apt-pkg/error.h>

#include <boost/format.hpp>
#include <boost/make_shared.hpp>

#include <cwidget/generic/util/transcode.h>

#include <iostream>

namespace cw = cwidget;

using boost::format;
using boost::make_shared;
using boost::shared_ptr;
using cw::util::transcode;

namespace aptitude
{
  namespace apt
  {
    namespace
    {
      class text_progress : public OpProgress
      {
        // If true, this code will assume it has a TTY as its output
        // and use terminal-based trickery.
        bool use_tty_decorations;

        // The length of the last line we displayed.
        std::size_t last_line_len;

        // The last operation we were displaying; used to output an
        // indicator when the operation finishes.
        std::string last_op;

        /** \brief Clear the currently displayed progress indicator. */
        void Clear();

        /** \brief Display the given line without a newline and remember its length
         *  so it can be cleared with Clear().
         */
        void Display(const std::string &line);

      public:
        text_progress(bool _use_tty_decorations)
          : use_tty_decorations(_use_tty_decorations),
            last_line_len(0)
        {
        }

        void Done();
        void Update();
      };

      void text_progress::Clear()
      {
        std::cout << '\r';
        for(std::size_t i = 0; i < last_line_len; ++i)
          std::cout << ' ';
        std::cout << '\r' << std::flush;

        last_line_len = 0;
      }

      void text_progress::Display(const std::string &line)
      {
        std::cout << line << std::flush;

        std::wstring line_w = transcode(line);
        last_line_len = wcslen(line_w.c_str());
      }

      void text_progress::Done()
      {
        // If we displayed a progress indicator, finish it off.
        if(use_tty_decorations)
          {
            if(!last_op.empty())
              {
                Clear();

                if(_error->PendingError() == true)
                  {
                    std::cout << (format(_("%s... Error!")) % last_op) << std::endl;
                    last_line_len = 0;
                  }
              }
          }
        else if(!last_op.empty())
          {
            std::cout << std::endl;
            last_op.clear();
          }
      }

      void text_progress::Update()
      {
        if(CheckChange(0.7))
          {
            if(!use_tty_decorations)
              {
                if(MajorChange)
                  {
                    if(!last_op.empty())
                      std::cout << std::endl;

                    std::cout << Op << "...";
                    last_op = Op;
                  }
              }
            else
              {
                Clear();
                Display((format("%s... %d%%") % Op % Percent).str());
                last_op = Op;
              }
          }
      }
    }

    shared_ptr<OpProgress>
    make_text_progress(bool require_tty_decorations)
    {
      bool hide_tty_decorations = false;
      bool hidden = false;

      if(!isatty(1))
        hide_tty_decorations = true;

      if(aptcfg->FindI("Quiet", 0))
        hide_tty_decorations = true;

      if(require_tty_decorations && hide_tty_decorations)
        hidden = true;

      if(hidden)
        return make_shared<OpProgress>();
      else
        return make_shared<text_progress>(!hide_tty_decorations);
    }
  }
}
