#define BOOST_TEST_NO_MAIN

#include <boost/test/unit_test.hpp>

#include <log4cxx/basicconfigurator.h>
#include <log4cxx/level.h>
#include <log4cxx/logger.h>

// One dummy test so that this can be dropped in before the actual
// test suite is written.
BOOST_AUTO_TEST_CASE(dummy)
{
}

bool init_unit_test()
{
  return true;
}

int main(int argc, char **argv)
{
  bool debug = false;
  for(int i = 1; i < argc; ++i)
    {
      if(!strcmp(argv[i], "--debug"))
	debug = true;
    }

  if(debug)
    log4cxx::Logger::getRootLogger()->setLevel(log4cxx::Level::getTrace());
  else
    log4cxx::Logger::getRootLogger()->setLevel(log4cxx::Level::getWarn());
  log4cxx::BasicConfigurator::configure();

  boost::unit_test::unit_test_main(init_unit_test, argc, argv);
}
