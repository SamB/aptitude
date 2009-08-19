#include <boost/function.hpp>
#include <boost/lambda/bind.hpp>
#include <boost/test/unit_test.hpp>

#include <generic/util/file_cache.h>
#include <generic/util/temp.h>

#include <sys/stat.h>

#include <apt-pkg/fileutl.h>

#include <fstream>

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

class raw_istream_iterator
{
  std::istream *in;
  char c;
  bool finished;

public:
  raw_istream_iterator(std::istream &_in)
    : in(&_in),
      c(0),
      finished(!in->good())
  {
    if(!finished)
      {
	in->get(c);
	finished = in->eof();
      }
  }

  raw_istream_iterator()
    : in(0), c(0), finished(true)
  {
  }

  bool operator==(const raw_istream_iterator &other) const
  {
    // Only end iterators are equivalent.
    return finished && other.finished;
  }

  bool operator!=(const raw_istream_iterator &other) const
  {
    return !(*this == other);
  }

  const char &operator*() const
  {
    BOOST_REQUIRE(!finished);

    return c;
  }

  raw_istream_iterator &operator++()
  {
    if(in->good())
      {
	in->get(c);
	finished = in->eof();
      }
    else
      finished = true;

    return *this;
  }
};

#define CHECK_FILE_CONTENTS(filename, collection)			\
  do {									\
    std::ifstream ___tmp_stream(filename.c_str());			\
    BOOST_CHECK_EQUAL_COLLECTIONS(raw_istream_iterator(___tmp_stream), raw_istream_iterator(), \
				  collection.begin(), collection.end()); \
  } while(0)

#define CHECK_CACHED_VALUE(cache, key, collection)			\
  do {									\
    temp::name ___cached_name(cache->getItem(key));			\
    BOOST_CHECK_MESSAGE(___cached_name.valid(), "The key " << key << " does not exist in the cache."); \
    if(___cached_name.valid())						\
      CHECK_FILE_CONTENTS(___cached_name.get_name(), collection);	\
  } while(0)


struct fileCacheTestInfo
{
  temp::name infilename1;
  temp::name infilename2;
  temp::name infilename3;

  std::vector<char> infileData1;
  std::vector<char> infileData2;
  std::vector<char> infileData3;

  std::string key1;
  std::string key2;
  std::string key3;
};

// Creates a file cache containing three files whose sizes add up to 1,000 bytes.
void setupFileCacheTest(const temp::dir &td,
			const boost::shared_ptr<file_cache> cache,
			fileCacheTestInfo &testInfo)
{
  // Store three files whose sizes add up to 1,000 bytes.
  testInfo.infilename1 = temp::name(td, "testInFile");
  testInfo.infilename2 = temp::name(td, "testInFile");
  testInfo.infilename3 = temp::name(td, "testInFile");

  std::ofstream
    infile1(testInfo.infilename1.get_name().c_str()),
    infile2(testInfo.infilename2.get_name().c_str()),
    infile3(testInfo.infilename3.get_name().c_str());

  for(int i = 0; i < 333; ++i)
    testInfo.infileData1.push_back((char)i);

  for(int i = 333; i < 666; ++i)
    testInfo.infileData2.push_back((char)i);

  for(int i = 666; i < 1000; ++i)
    testInfo.infileData3.push_back((char)i);

  infile1.write(&testInfo.infileData1.front(), testInfo.infileData1.size());
  infile2.write(&testInfo.infileData2.front(), testInfo.infileData2.size());
  infile3.write(&testInfo.infileData3.front(), testInfo.infileData3.size());

  infile1.close();
  infile2.close();
  infile3.close();


  {
    // Sanity-check the raw iterator class.
    std::ifstream
      testFile1(testInfo.infilename1.get_name().c_str()),
      testFile2(testInfo.infilename2.get_name().c_str()),
      testFile3(testInfo.infilename3.get_name().c_str());

    BOOST_REQUIRE_EQUAL_COLLECTIONS(raw_istream_iterator(testFile1), raw_istream_iterator(),
				    testInfo.infileData1.begin(), testInfo.infileData1.end());
    BOOST_REQUIRE_EQUAL_COLLECTIONS(raw_istream_iterator(testFile2), raw_istream_iterator(),
				    testInfo.infileData2.begin(), testInfo.infileData2.end());
    BOOST_REQUIRE_EQUAL_COLLECTIONS(raw_istream_iterator(testFile3), raw_istream_iterator(),
				    testInfo.infileData3.begin(), testInfo.infileData3.end());
  }


  testInfo.key1 = "key1";
  testInfo.key2 = "key2";
  testInfo.key3 = "key3";

  BOOST_CHECK(!cache->getItem(testInfo.key1).valid());
  BOOST_CHECK(!cache->getItem(testInfo.key2).valid());
  BOOST_CHECK(!cache->getItem(testInfo.key3).valid());


  cache->putItem(testInfo.key1, testInfo.infilename1.get_name());

  CHECK_CACHED_VALUE(cache, testInfo.key1, testInfo.infileData1);
  BOOST_CHECK(!cache->getItem(testInfo.key2).valid());
  BOOST_CHECK(!cache->getItem(testInfo.key3).valid());


  cache->putItem(testInfo.key2, testInfo.infilename2.get_name());

  CHECK_CACHED_VALUE(cache, testInfo.key1, testInfo.infileData1);
  CHECK_CACHED_VALUE(cache, testInfo.key2, testInfo.infileData2);
  BOOST_CHECK(!cache->getItem(testInfo.key3).valid());


  cache->putItem(testInfo.key3, testInfo.infilename3.get_name());

  CHECK_CACHED_VALUE(cache, testInfo.key1, testInfo.infileData1);
  CHECK_CACHED_VALUE(cache, testInfo.key2, testInfo.infileData2);
  CHECK_CACHED_VALUE(cache, testInfo.key3, testInfo.infileData3);
}

BOOST_AUTO_TEST_CASE(fileCacheStoreDisk)
{
  temp::dir td("testFileCache");
  temp::name tn(td, "testFileCache");

  boost::shared_ptr<file_cache> cache(file_cache::create(tn.get_name(), 0, 1000));
  BOOST_CHECK(exists(tn.get_name()));

  fileCacheTestInfo testInfo;
  setupFileCacheTest(td, cache, testInfo);
}

BOOST_AUTO_TEST_CASE(fileCacheStoreMemory)
{
  temp::dir td("testFileCache");
  temp::name tn(td, "testFileCache");

  boost::shared_ptr<file_cache> cache(file_cache::create(tn.get_name(), 1000, 0));

  fileCacheTestInfo testInfo;
  setupFileCacheTest(td, cache, testInfo);
}

BOOST_AUTO_TEST_CASE(fileCacheStoreDiskAndMemory)
{
  temp::dir td("testFileCache");
  temp::name tn(td, "testFileCache");

  boost::shared_ptr<file_cache> cache(file_cache::create(tn.get_name(), 333, 667));
  BOOST_CHECK(exists(tn.get_name()));

  fileCacheTestInfo testInfo;
  setupFileCacheTest(td, cache, testInfo);
}


void runDropLeastRecentlyUsedTest(const temp::dir &td,
				  const boost::function<boost::shared_ptr<file_cache> (std::string)> &cache_k)
{
  // Check that we can control which of the three entries is dropped
  // when we add a fourth entry.


  // Drop key3.
  {
    temp::name tn(td, "testFileCache");
    boost::shared_ptr<file_cache> cache(cache_k(tn.get_name()));

    fileCacheTestInfo testInfo;
    setupFileCacheTest(td, cache, testInfo);

    CHECK_CACHED_VALUE(cache, testInfo.key3, testInfo.infileData3);
    CHECK_CACHED_VALUE(cache, testInfo.key1, testInfo.infileData1);
    CHECK_CACHED_VALUE(cache, testInfo.key2, testInfo.infileData2);

    cache->putItem("key4", testInfo.infilename1.get_name());


    CHECK_CACHED_VALUE(cache, testInfo.key1, testInfo.infileData1);
    CHECK_CACHED_VALUE(cache, testInfo.key2, testInfo.infileData2);
    CHECK_CACHED_VALUE(cache, "key4", testInfo.infileData1);
    BOOST_CHECK(!cache->getItem(testInfo.key3).valid());
  }


  // Drop key2.
  {
    temp::name tn(td, "testFileCache");
    boost::shared_ptr<file_cache> cache(cache_k(tn.get_name()));

    fileCacheTestInfo testInfo;
    setupFileCacheTest(td, cache, testInfo);

    CHECK_CACHED_VALUE(cache, testInfo.key2, testInfo.infileData2);
    CHECK_CACHED_VALUE(cache, testInfo.key1, testInfo.infileData1);
    CHECK_CACHED_VALUE(cache, testInfo.key3, testInfo.infileData3);

    cache->putItem("key4", testInfo.infilename1.get_name());


    CHECK_CACHED_VALUE(cache, testInfo.key1, testInfo.infileData1);
    CHECK_CACHED_VALUE(cache, testInfo.key3, testInfo.infileData3);
    CHECK_CACHED_VALUE(cache, "key4", testInfo.infileData1);
    BOOST_CHECK(!cache->getItem(testInfo.key2).valid());
  }


  // Drop key1.
  {
    temp::name tn(td, "testFileCache");
    boost::shared_ptr<file_cache> cache(cache_k(tn.get_name()));

    fileCacheTestInfo testInfo;
    setupFileCacheTest(td, cache, testInfo);

    CHECK_CACHED_VALUE(cache, testInfo.key1, testInfo.infileData1);
    CHECK_CACHED_VALUE(cache, testInfo.key3, testInfo.infileData3);
    CHECK_CACHED_VALUE(cache, testInfo.key2, testInfo.infileData2);

    cache->putItem("key4", testInfo.infilename1.get_name());


    CHECK_CACHED_VALUE(cache, testInfo.key2, testInfo.infileData2);
    CHECK_CACHED_VALUE(cache, testInfo.key3, testInfo.infileData3);
    CHECK_CACHED_VALUE(cache, "key4", testInfo.infileData1);
    BOOST_CHECK(!cache->getItem(testInfo.key1).valid());
  }
}

BOOST_AUTO_TEST_CASE(fileCacheDropLeastRecentlyUsedDisk)
{
  temp::dir td("testFileCache");
  temp::name tn(td, "testFileCache");

  runDropLeastRecentlyUsedTest(td, boost::lambda::bind(&file_cache::create, boost::lambda::_1, 0, 1000));
}
