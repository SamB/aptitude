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

// The Google mock library emits Google Test failures; we need to be
// able to convert those to Boost.Test failures.
class gtest_to_boost_adaptor : public testing::EmptyTestEventListener
{
  void OnTestPartResult(const testing::TestPartResult &test_part_result)
  {
    if(test_part_result.nonfatally_failed())
      BOOST_ERROR(test_part_result.file_name()
                  << ":"
                  << test_part_result.line_number()
                  << ": "
                  << test_part_result.summary());
    else if(test_part_result.fatally_failed())
      BOOST_FAIL(test_part_result.file_name()
                 << ":"
                 << test_part_result.line_number()
                 << ": "
                 << test_part_result.summary());
  }
};

bool init_unit_test()
{
  return true;
}

char *argv0 = NULL;

int main(int argc, char **argv)
{
  argv0 = argv[0];

  ::testing::InitGoogleMock(&argc, argv);
  {
    ::testing::TestEventListeners &listeners =
      ::testing::UnitTest::GetInstance()->listeners();
    listeners.Append(new gtest_to_boost_adaptor);
  }

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
