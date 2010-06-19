/** \file cmdline_progress_throttle.cc */   // -*-c++-*-

#include "cmdline_progress_throttle.h"

// Local includes:
#include <loggers.h>

// System includes:
#include <boost/make_shared.hpp>
#include <boost/optional.hpp>

#include <generic/util/util.h>

#include <errno.h>
#include <sys/time.h>

using aptitude::Loggers;
using boost::make_shared;
using boost::optional;
using boost::shared_ptr;
using logging::LoggerPtr;

namespace aptitude
{
  namespace cmdline
  {
    namespace
    {
      class progress_throttle_impl : public progress_throttle
      {
        boost::optional<struct timeval> last_update;

        logging::LoggerPtr logger;

        // Used to ensure that we only warn once about gettimeofday()
        // failing.
        bool wrote_time_error;

        static const double update_interval = 0.7;

        void write_time_error(int errnum);

      public:
        progress_throttle_impl();

        /** \return \b true if the progress display should be updated. */
        bool update_required();

        /** \brief Reset the timer that controls when the display is
         *  updated.
         */
        void reset_timer();
      };

      const double progress_throttle_impl::update_interval;

      void progress_throttle_impl::write_time_error(int errnum)
      {
        if(!wrote_time_error)
          {
            LOG_ERROR(logger,
                      "gettimeofday() failed: " <<
                      sstrerror(errnum));
            wrote_time_error = true;
          }
      }

      progress_throttle_impl::progress_throttle_impl()
        : logger(Loggers::getAptitudeCmdlineThrottle()),
          wrote_time_error(false)
      {
      }

      bool progress_throttle_impl::update_required()
      {
        if(!last_update)
          return true;
        else
          {
            // Time checking code shamelessly stolen from apt, since
            // we know theirs works.
            struct timeval now;
            if(gettimeofday(&now, 0) != 0)
              {
                write_time_error(errno);
                return false;
              }
            else
              {
                const struct timeval &last_update_time = *last_update;
                double diff =
                  now.tv_sec - last_update_time.tv_sec +
                  (now.tv_usec - last_update_time.tv_usec)/1000000.0;

                bool rval = diff >= update_interval;

                return rval;
              }
          }
      }

      void progress_throttle_impl::reset_timer()
      {
        LOG_TRACE(logger, "Resetting the update timer.");

        struct timeval now;
        if(gettimeofday(&now, 0) != 0)
          write_time_error(errno);
        else
          last_update = now;
      }
    }

    progress_throttle::~progress_throttle()
    {
    }

    shared_ptr<progress_throttle> create_progress_throttle()
    {
      return make_shared<progress_throttle_impl>();
    }
  }
}
