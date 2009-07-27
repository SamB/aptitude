#include <boost/test/unit_test.hpp>

#include <generic/util/sqlite.h>

using namespace aptitude::sqlite;

// Allocates a database in memory for testing purposes.
struct memory_db_fixture
{
  boost::shared_ptr<db> tmpdb;

  memory_db_fixture()
    : tmpdb(db::create(":memory:"))
  {
  }
};

// Allocates a database in memory for testing purposes and populates
// it with some test data.
//
// Creates a table "test" with columns A (primary key), B, C, and
// inserts some values:
//
// A       B            C
// 50      "aardvark"   -5
// 51      "balderdash" -5
// 52      "contusion"  x'5412'
struct test_db_fixture : public memory_db_fixture
{
  test_db_fixture()
  {
    statement::prepare(*tmpdb, "create table test(A integer primary key, B text, C integer)")->exec();
    statement::prepare(*tmpdb, "insert into test (A, B, C) values (50, 'aardvark', -5)")->exec();
    statement::prepare(*tmpdb, "insert into test (A, B, C) values (51, 'balderdash', -5)")->exec();
    statement::prepare(*tmpdb, "insert into test (A, B, C) values (52, 'contusion', X'5412')")->exec();
  }
};

BOOST_AUTO_TEST_CASE(cantOpenDb)
{
  // Test that a failed open throws an exception (don't know how to
  // test that it doesn't leak, which is the other thing we want).
  //
  // Note: This test will fail if the ridiculous name it uses exists;
  // a nice enhancement would be to generate names on the fly with a
  // RNG.

  BOOST_REQUIRE_THROW(db::create("ridiculous-and-not-existing-database-name-foo-12983474yf4yrt1839y4vcf8913bh4fiuv",
				 SQLITE_OPEN_READWRITE),
		      exception);
}

BOOST_FIXTURE_TEST_CASE(prepareStatement, memory_db_fixture)
{
  statement::prepare(*tmpdb, "create table foo(bar int)");
  statement::prepare(*tmpdb, std::string("create table foo(bar int)"));
}

BOOST_FIXTURE_TEST_CASE(prepareStatementFail, memory_db_fixture)
{
  BOOST_REQUIRE_THROW(statement::prepare(*tmpdb, "select * from bar"),
		      exception);
}

// Test that we can create the test DB and do nothing else.
BOOST_FIXTURE_TEST_CASE(testSetupDb, test_db_fixture)
{
}

BOOST_FIXTURE_TEST_CASE(testGetBlob, test_db_fixture)
{
  boost::shared_ptr<statement> stmt =
    statement::prepare(*tmpdb, "select A, B, C from test where A = 52");

  BOOST_REQUIRE(stmt->step());

  int len = -1;
  const void *val;

  val = stmt->get_blob(0, len);
  const char * const fiftytwo = "52";
  BOOST_CHECK_EQUAL(len, strlen(fiftytwo));
  BOOST_CHECK_EQUAL_COLLECTIONS(fiftytwo, fiftytwo + strlen(fiftytwo),
				reinterpret_cast<const char *>(val),
				reinterpret_cast<const char *>(val) + len);

  val = stmt->get_blob(1, len);
  const char * const contusion = "contusion";
  BOOST_CHECK_EQUAL(len, strlen(contusion));
  BOOST_CHECK_EQUAL_COLLECTIONS(contusion, contusion + strlen(contusion),
				reinterpret_cast<const char *>(val),
				reinterpret_cast<const char *>(val) + len);

  val = stmt->get_blob(2, len);
  const char arr[2] = { 0x54, 0x12 };
  BOOST_CHECK_EQUAL(len, sizeof(arr));
  BOOST_CHECK_EQUAL_COLLECTIONS(arr, arr + sizeof(arr),
				reinterpret_cast<const char *>(val),
				reinterpret_cast<const char *>(val) + len);


  BOOST_CHECK(!stmt->step());
  BOOST_CHECK_THROW(stmt->get_blob(2, len),
		    exception);
}

BOOST_FIXTURE_TEST_CASE(testGetDouble, test_db_fixture)
{
  boost::shared_ptr<statement> stmt =
    statement::prepare(*tmpdb, "select C from test where A = 51");

  BOOST_REQUIRE(stmt->step());

  BOOST_CHECK_EQUAL(stmt->get_double(0), -5);

  BOOST_CHECK(!stmt->step());
  BOOST_CHECK_THROW(stmt->get_double(0),
		    exception);
}

BOOST_FIXTURE_TEST_CASE(testGetInt, test_db_fixture)
{
  boost::shared_ptr<statement> stmt =
    statement::prepare(*tmpdb, "select A from test where A <> 51 order by A");

  BOOST_REQUIRE(stmt->step());

  BOOST_CHECK_EQUAL(stmt->get_int(0), 50);

  BOOST_REQUIRE(stmt->step());

  BOOST_CHECK_EQUAL(stmt->get_int(0), 52);

  BOOST_CHECK(!stmt->step());
  BOOST_CHECK_THROW(stmt->get_int(0),
		    exception);
}

BOOST_FIXTURE_TEST_CASE(testGetInt64, test_db_fixture)
{
  boost::shared_ptr<statement> stmt =
    statement::prepare(*tmpdb, "select A from test where A <> 51 order by A");

  BOOST_REQUIRE(stmt->step());
  BOOST_CHECK_EQUAL(stmt->get_int64(0), 50);

  BOOST_REQUIRE(stmt->step());
  BOOST_CHECK_EQUAL(stmt->get_int64(0), 52);

  BOOST_CHECK(!stmt->step());
  BOOST_CHECK_THROW(stmt->get_int64(0),
		    exception);
}

BOOST_FIXTURE_TEST_CASE(testGetString, test_db_fixture)
{
  boost::shared_ptr<statement> stmt =
    statement::prepare(*tmpdb, "select B from test where C = -5 order by A");

  BOOST_REQUIRE(stmt->step());
  BOOST_CHECK_EQUAL(stmt->get_string(0), "aardvark");

  BOOST_REQUIRE(stmt->step());
  BOOST_CHECK_EQUAL(stmt->get_string(0), "balderdash");

  BOOST_CHECK(!stmt->step());
  BOOST_CHECK_THROW(stmt->get_string(0),
		    exception);
}
