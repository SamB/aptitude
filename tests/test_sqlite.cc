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
