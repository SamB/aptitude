#ifndef BOOST_TEST_NO_MAIN
#define BOOST_TEST_NO_MAIN
#endif

#include <boost/test/unit_test.hpp>

#include <gmock/gmock.h>

#include <log4cxx/basicconfigurator.h>
#include <log4cxx/level.h>
#include <log4cxx/logger.h>

#include <loggers.h>

// One dummy test so that this can be dropped in before the actual
// test suite is written.
BOOST_AUTO_TEST_CASE(dummy)
{
}

bool init_unit_test()
{
  return true;
}

char *argv0 = NULL;

int main(int argc, char **argv)
{
  argv0 = argv[0];

  // TODO: setting throw_on_failure is a hack; instead, we should
  // configure the mock framework to emit Boost.Test errors.
  // Unfortunately, I can't access the documentation (for the Google
  // Test event listener API) that would tell me how to do this
  // without a network connection.  I hate libraries without offline
  // documentation.
  ::testing::GTEST_FLAG(throw_on_failure) = true;
  ::testing::InitGoogleMock(&argc, argv);

  bool debug = false;
  for(int i = 1; i < argc; ++i)
    {
      if(!strcmp(argv[i], "--debug"))
	debug = true;
    }

  if(debug)
    logging::Logger::getRootLogger()->setLevel(logging::Level::getTrace());
  else
    logging::Logger::getRootLogger()->setLevel(logging::Level::getWarn());
  logging::BasicConfigurator::configure();

  return boost::unit_test::unit_test_main(init_unit_test, argc, argv);
}
