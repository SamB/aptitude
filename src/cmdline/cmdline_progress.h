// cmdline_progress.h                  -*-c++-*-
//
//  Copyright 2004, 2010 Daniel Burrows

#ifndef CMDLINE_PROGRESS_H
#define CMDLINE_PROGRESS_H

/** \file cmdline_progress.h
 */

#include <boost/shared_ptr.hpp>

class download_signal_log;

namespace aptitude
{
  namespace cmdline
  {
    class terminal;
  }
}

download_signal_log *
gen_cmdline_download_progress(const boost::shared_ptr<aptitude::cmdline::terminal> &term);

#endif // CMDLINE_PROGRESS_H
