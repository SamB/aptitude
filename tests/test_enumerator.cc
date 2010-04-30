#include <generic/util/enumerator.h>

#include <boost/array.hpp>
#include <boost/test/unit_test.hpp>
#include <boost/weak_ptr.hpp>

using aptitude::util::enumerator;
using aptitude::util::iterator_enumerator;
using aptitude::util::iterator_enumerator_with_keepalive;
using boost::weak_ptr;

struct fourNumbers
{
  std::vector<int> numbers;
  boost::shared_ptr<std::vector<int> > numbersShared;

  typedef iterator_enumerator<std::vector<int>::const_iterator> int_enum;
  typedef iterator_enumerator_with_keepalive<std::vector<int>::const_iterator,
                                             std::vector<int> > int_keepalive_enum;

  fourNumbers()
  {
    for(int i = 1; i <= 4; ++i)
      {
        numbers.push_back(i);
        numbersShared->push_back(i);
      }
  }
};

BOOST_FIXTURE_TEST_CASE(iteratorEnumeratorCurrent, fourNumbers)
{
  int_enum e1(numbers.begin(), numbers.end());
  int_enum e2(numbers.begin() + 1, numbers.end());
  int_enum e3(numbers.begin() + 2, numbers.end());
  int_enum e4(numbers.begin() + 3, numbers.end());

  BOOST_CHECK_EQUAL(e1.get_current(), 1);
  BOOST_CHECK_EQUAL(e2.get_current(), 2);
  BOOST_CHECK_EQUAL(e3.get_current(), 3);
  BOOST_CHECK_EQUAL(e4.get_current(), 4);
}

BOOST_FIXTURE_TEST_CASE(iteratorEnumeratorAdvance, fourNumbers)
{
  int_enum e1(numbers.begin(), numbers.end());
  int_enum e2(numbers.begin() + 1, numbers.end());
  int_enum e3(numbers.begin() + 2, numbers.end());
  int_enum e4(numbers.begin() + 3, numbers.end());

  BOOST_REQUIRE(e1.advance());
  BOOST_CHECK_EQUAL(e1.get_current(), 2);

  BOOST_REQUIRE(e2.advance());
  BOOST_CHECK_EQUAL(e2.get_current(), 3);

  BOOST_REQUIRE(e2.advance());
  BOOST_CHECK_EQUAL(e3.get_current(), 4);

  BOOST_CHECK(!e3.advance());
}

BOOST_FIXTURE_TEST_CASE(iteratorEnumeratorKeepaliveCurrent, fourNumbers)
{
  int_keepalive_enum e1(numbersShared->begin(), numbersShared->end(),
                        numbersShared);
  int_keepalive_enum e2(numbersShared->begin() + 1, numbersShared->end(),
                        numbersShared);
  int_keepalive_enum e3(numbersShared->begin() + 2, numbersShared->end(),
                        numbersShared);
  int_keepalive_enum e4(numbersShared->begin() + 3, numbersShared->end(),
                        numbersShared);

  BOOST_CHECK_EQUAL(e1.get_current(), 1);
  BOOST_CHECK_EQUAL(e2.get_current(), 2);
  BOOST_CHECK_EQUAL(e3.get_current(), 3);
  BOOST_CHECK_EQUAL(e4.get_current(), 4);
}

BOOST_FIXTURE_TEST_CASE(iteratorEnumeratorKeepaliveAdvance, fourNumbers)
{
  int_keepalive_enum e1(numbersShared->begin(), numbersShared->end(),
                        numbersShared);
  int_keepalive_enum e2(numbersShared->begin() + 1, numbersShared->end(),
                        numbersShared);
  int_keepalive_enum e3(numbersShared->begin() + 2, numbersShared->end(),
                        numbersShared);
  int_keepalive_enum e4(numbersShared->begin() + 3, numbersShared->end(),
                        numbersShared);

  BOOST_REQUIRE(e1.advance());
  BOOST_CHECK_EQUAL(e1.get_current(), 2);

  BOOST_REQUIRE(e2.advance());
  BOOST_CHECK_EQUAL(e2.get_current(), 3);

  BOOST_REQUIRE(e2.advance());
  BOOST_CHECK_EQUAL(e3.get_current(), 4);

  BOOST_CHECK(!e3.advance());
}

BOOST_FIXTURE_TEST_CASE(iteratorEnumeratorKeepalive, fourNumbers)
{
  weak_ptr<std::vector<int> > numbersWeak(numbersShared);

  {
    int_keepalive_enum e(numbersShared->begin(),
                         numbersShared->end(),
                         numbersShared);

    BOOST_CHECK(!numbersWeak.expired());

    numbersShared.reset();

    BOOST_CHECK(!numbersWeak.expired());
  }

  BOOST_CHECK(numbersWeak.expired());
}
