#include <boost/test/unit_test.hpp>

#include <generic/util/file_cache.h>
#include <generic/util/temp.h>

#include <sys/stat.h>

using aptitude::util::file_cache;

bool exists(const std::string &s)
{
  // Slightly lame way to test that a file exists.  Should be enough
  // for our purposes.
  struct stat buf;

  return stat(s.c_str(), &buf) == 0;
}

BOOST_AUTO_TEST_CASE(createFileCache)
{
  {
    temp::dir td("testFileCache");
    temp::name tn(td, "testFileCache");

    BOOST_CHECK(!exists(tn.get_name()));

    {
      boost::shared_ptr<file_cache> cache(file_cache::create(tn.get_name(), 10000, 10000));
      BOOST_CHECK(exists(tn.get_name()));
    }
  }

  {
    temp::dir td("testFileCache");
    temp::name tn(td, "testFileCache");

    {
      boost::shared_ptr<file_cache> cache(file_cache::create(tn.get_name(), 0, 10000));
      BOOST_CHECK(exists(tn.get_name()));
    }
  }
  {
    temp::dir td("testFileCache");
    temp::name tn(td, "testFileCache");

    {
      boost::shared_ptr<file_cache> cache(file_cache::create(tn.get_name(), 10000, 0));
      BOOST_CHECK(!exists(tn.get_name()));
    }
  }
}
