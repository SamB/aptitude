/** \file loggers.h    -*-c++-*- */

//   Copyright (C) 2009 Daniel Burrows
//
//   This program is free software; you can redistribute it and/or
//   modify it under the terms of the GNU General Public License as
//   published by the Free Software Foundation; either version 2 of
//   the License, or (at your option) any later version.
//
//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//   General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with this program; see the file COPYING.  If not, write to
//   the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//   Boston, MA 02111-1307, USA.


#ifndef LOGGERS_H
#define LOGGERS_H

#include <log4cxx/logger.h>

// Safe logging macros -- unlike the log4cxx macros, these expand to
// statements needing a semicolon, so you can write them like function
// calls with no surprises.  They also save a few keystrokes.
#define LOG_TRACE(logger, args) do { LOG4CXX_TRACE(logger, args); } while(0)
#define LOG_DEBUG(logger, args) do { LOG4CXX_DEBUG(logger, args); } while(0)
#define LOG_INFO(logger, args) do { LOG4CXX_INFO(logger, args); } while(0)
#define LOG_WARN(logger, args) do { LOG4CXX_WARN(logger, args); } while(0)
#define LOG_ERROR(logger, args) do { LOG4CXX_ERROR(logger, args); } while(0)
#define LOG_FATAL(logger, args) do { LOG4CXX_FATAL(logger, args); } while(0)

namespace aptitude
{
  /** \brief A global repository for the loggers used in aptitude.
   *  The sole purpose of this file is to keep that information in one
   *  place.
   *
   *  It would be nicer if we could enforce that somehow, but that's
   *  not compatible with the design of log4cxx.
   */
  class Loggers
  {
    // Could define static variables here, but that would have no real purpose.
  public:
    /** \brief The logger for the dependency resolver.
     *
     *  Name: aptitude.resolver
     */
    static log4cxx::LoggerPtr getAptitudeResolver();
  };
}

#endif // LOGGERS_H
