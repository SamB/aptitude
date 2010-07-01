/** \file cmdline_download_progress_display.cc */

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
#include "cmdline_download_progress_display.h"

#include <generic/views/download_progress.h>

// System includes:
#include <boost/make_shared.hpp>

using boost::make_shared;
using boost::shared_ptr;

namespace aptitude
{
  namespace cmdline
  {
    namespace
    {
      class download_progress : public views::download_progress
      {
        shared_ptr<transient_message> message;
        shared_ptr<terminal> term;
        shared_ptr<terminal_locale> term_locale;

        download_progress(const shared_ptr<transient_message> &_message,
                          const shared_ptr<terminal> &_term,
                          const shared_ptr<terminal_locale> &_term_locale);
        friend shared_ptr<download_progress>
        make_shared<download_progress>(const shared_ptr<transient_message> &,
                                       const shared_ptr<terminal> &,
                                       const shared_ptr<terminal_locale> &);

      public:
        bool update_progress(const status &current_status);

        void file_started(const std::string &description,
                          const boost::optional<unsigned long> &id,
                          const boost::optional<unsigned long> &file_size,
                          const status &current_status);

        void error(bool ignored,
                   const std::string &error,
                   const std::string &description,
                   const boost::optional<unsigned long> &id,
                   const status &current_status);

        void file_finished(const std::string &description,
                           const boost::optional<unsigned long> &id,
                           const status &current_status);

        void done();

        void media_change(const std::string &media,
                          const std::string &drive,
                          const sigc::slot1<void, bool> &k);
      };

      download_progress::download_progress(const shared_ptr<transient_message> &_message,
                                           const shared_ptr<terminal> &_term,
                                           const shared_ptr<terminal_locale> &_term_locale)
        : message(_message),
          term(_term),
          term_locale(_term_locale)
      {
      }


      bool download_progress::update_progress(const status &current_status)
      {
        return true;
      }

      void download_progress::file_started(const std::string &description,
                                           const boost::optional<unsigned long> &id,
                                           const boost::optional<unsigned long> &file_size,
                                           const status &current_status)
      {
      }

      void download_progress::error(bool ignored,
                                    const std::string &error,
                                    const std::string &description,
                                    const boost::optional<unsigned long> &id,
                                    const status &current_status)
      {
      }

      void download_progress::file_finished(const std::string &description,
                                            const boost::optional<unsigned long> &id,
                                            const status &current_status)
      {
      }

      void download_progress::done()
      {
      }

      void download_progress::media_change(const std::string &media,
                                           const std::string &drive,
                                           const sigc::slot1<void, bool> &k)
      {
      }
    }

    shared_ptr<views::download_progress>
    create_download_progress_display(const boost::shared_ptr<transient_message> &message,
                                     const boost::shared_ptr<terminal> &term,
                                     const boost::shared_ptr<terminal_locale> &term_locale)
    {
      return make_shared<download_progress>(message, term, term_locale);
    }
  }
}
