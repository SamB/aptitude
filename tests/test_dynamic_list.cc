#include <generic/util/dynamic_list.h>
#include <generic/util/dynamic_list_collection.h>
#include <generic/util/dynamic_list_impl.h>

#include <boost/test/unit_test.hpp>
#include <boost/variant.hpp>

using aptitude::util::dynamic_list;
using aptitude::util::dynamic_list_collection;
using aptitude::util::dynamic_list_impl;
using aptitude::util::enumerator;
using aptitude::util::writable_dynamic_list;

namespace
{
  template<typename T>
  class appended_call
  {
    T value;

  public:
    appended_call(const T &_value)
      : value(_value)
    {
    }

    const T get_value() const { return value; }
    bool operator==(const appended_call &other) const
    {
      return value == other.value;
    }

    bool operator!=(const appended_call &other) const
    {
      return value != other.value;
    }
  };

  template<typename T>
  std::ostream &operator<<(std::ostream &out,
                           const appended_call<T> &call)
  {
    return out << "appended(" << call.get_value() << ")";
  }

  template<typename T>
  class removed_call
  {
    T value;
    std::size_t idx;

  public:
    removed_call(const T &_value, std::size_t _idx)
      : value(_value), idx(_idx)
    {
    }

    const T get_value() const { return value; }
    std::size_t get_idx() const { return idx; }
    bool operator==(const removed_call &other) const
    {
      return value == other.value && idx == other.idx;
    }
    bool operator!=(const removed_call &other) const
    {
      return value != other.value && idx != other.idx;
    }
  };

  template<typename T>
  std::ostream &operator<<(std::ostream &out,
                           const removed_call<T> &call)
  {
    return out << "removed(" << call.get_value() << ", "
               << call.get_idx() << ")";
  }

  template<typename T>
  class list_signal
  {
  public:
    typedef boost::variant<appended_call<T>, removed_call<T> > value_type;

  private:
    value_type value;

  public:
    list_signal(const appended_call<T> &_value)
      : value(_value)
    {
    }

    list_signal(const removed_call<T> &_value)
      : value(_value)
    {
    }

    list_signal(const value_type &_value)
      : value(_value)
    {
    }

    const value_type &get_value() const { return value; }

    bool operator==(const list_signal &other) const
    {
      return value == other.value;
    }

    bool operator!=(const list_signal &other) const
    {
      return !(value == other.value);
    }
  };

  template<typename T>
  std::ostream &operator<<(std::ostream &out, const list_signal<T> &signal)
  {
    return out << signal.get_value();
  }

  template<typename T>
  class dynamic_list_signals
  {
    std::vector<list_signal<T> > calls;

    void appended(const T &value)
    {
      calls.push_back(appended_call<T>(value));
    }

    void removed(const T &value, int idx)
    {
      calls.push_back(removed_call<T>(value, idx));
    }

    dynamic_list_signals(const dynamic_list_signals &);

  public:
    dynamic_list_signals()
    {
    }

    ~dynamic_list_signals()
    {
    }

    void clear()
    {
      calls.clear();
    }

    void attach(dynamic_list<T> &list)
    {
      list.signal_appended.connect(sigc::mem_fun(*this, &dynamic_list_signals::appended));
      list.signal_removed.connect(sigc::mem_fun(*this, &dynamic_list_signals::removed));

      BOOST_REQUIRE(list.signal_appended.size() > 0);
    }

    /** \brief To be used only by test code. */
    void push_back(const list_signal<T> &signal)
    {
      calls.push_back(signal);
    }

    typedef typename std::vector<list_signal<T> >::const_iterator const_iterator;
    const_iterator begin() const { return calls.begin(); }
    const_iterator end() const { return calls.end(); }

    bool operator==(const dynamic_list_signals &other) const
    {
      return calls == other.calls;
    }

    bool operator!=(const dynamic_list_signals &other) const
    {
      return calls != other.calls;
    }
  };

  template<typename T>
  std::ostream &operator<<(std::ostream &out,
                           const dynamic_list_signals<T> &signals)
  {
    out << "[";
    for(typename dynamic_list_signals<T>::const_iterator
          it = signals.begin(); it != signals.end(); ++it)
      {
        if(it != signals.begin())
          out << ", ";
        out << *it;
      }

    out << "]";

    return out;
  }

  struct list_test
  {
    boost::shared_ptr<dynamic_list_impl<int> > valuesPtr;
    dynamic_list_impl<int> &values;
    dynamic_list_signals<int> signals;

    std::vector<int> expected;

    list_test()
      : valuesPtr(dynamic_list_impl<int>::create()),
        values(*valuesPtr),
        signals()
    {
      values.append(1);
      values.append(2);
      values.append(3);
      signals.attach(values);

      expected.push_back(1);
      expected.push_back(2);
      expected.push_back(3);
    }

    std::vector<int> as_vector() const
    {
      std::vector<int> rval;
      boost::shared_ptr<enumerator<int> > e = values.enumerate();
      while(e->advance())
        rval.push_back(e->get_current());

      return rval;
    }
  };
}

BOOST_AUTO_TEST_CASE(listSignals)
{
  dynamic_list_signals<int> signals1, signals2, signals3;

  typedef appended_call<int> app;
  typedef removed_call<int> rem;

  signals1.push_back(app(1));
  signals1.push_back(app(2));
  signals1.push_back(app(3));

  signals2.push_back(app(1));
  signals2.push_back(rem(2, 5));
  signals2.push_back(app(3));

  signals3.push_back(app(1));
  signals3.push_back(app(2));
  signals3.push_back(app(3));

  BOOST_CHECK_EQUAL(signals1, signals3);
  BOOST_CHECK_EQUAL(signals3, signals1);
  BOOST_CHECK_EQUAL_COLLECTIONS(signals1.begin(), signals1.end(),
                                signals3.begin(), signals3.end());
  BOOST_CHECK_EQUAL_COLLECTIONS(signals3.begin(), signals3.end(),
                                signals1.begin(), signals1.end());

  BOOST_CHECK_NE(signals1, signals2);
  BOOST_CHECK_NE(signals2, signals1);
  BOOST_CHECK_NE(signals2, signals3);
  BOOST_CHECK_NE(signals3, signals2);
}

BOOST_AUTO_TEST_CASE(listSignalsAttach)
{
  dynamic_list_signals<int> signals, expected;
  dynamic_list_impl<int> list;
  signals.attach(list);

  typedef appended_call<int> app;
  typedef removed_call<int> rem;

  expected.push_back(app(5));
  expected.push_back(app(2));
  expected.push_back(rem(5, 0));

  // Note: call the signals directly so we test the attachment and not
  // the dynamic list.
  list.signal_appended(5);
  list.signal_appended(2);
  list.signal_removed(5, 0);

  BOOST_CHECK_EQUAL_COLLECTIONS(expected.begin(), expected.end(),
                                signals.begin(), signals.end());
}

BOOST_FIXTURE_TEST_CASE(dynamicListEnumerate, list_test)
{
  // Enumerate over the entries of the list and ensure that they're
  // all accounted for.

  std::vector<int> values_vector = as_vector();
  BOOST_CHECK_EQUAL_COLLECTIONS(expected.begin(), expected.end(),
                                values_vector.begin(), values_vector.end());
}

BOOST_FIXTURE_TEST_CASE(dynamicListAppend, list_test)
{
  // Append some values, check that the proper signals were emitted,
  // and check that the values are visible in the list.
  values.append(4);
  values.append(5);
  values.append(6);

  std::vector<int> values_vector = as_vector();
  dynamic_list_signals<int> expected_calls;
  for(int i = 4; i <= 6; ++i)
    {
      expected.push_back(i);
      expected_calls.push_back(appended_call<int>(i));
    }

  BOOST_CHECK_EQUAL_COLLECTIONS(expected.begin(), expected.end(),
                                values_vector.begin(), values_vector.end());

  BOOST_CHECK_EQUAL_COLLECTIONS(expected_calls.begin(), expected_calls.end(),
                                signals.begin(), signals.end());
}

BOOST_FIXTURE_TEST_CASE(dynamicListRemove, list_test)
{
  values.remove(2); // New list: [1, 3], removed (2, 1)
  values.remove(0); // New list: [1, 3], no change reported
  values.remove(1); // New list: [3], removed (1, 0)

  std::vector<int> values_vector = as_vector();
  dynamic_list_signals<int> expected_calls;

  expected.clear();
  expected.push_back(3);

  expected_calls.push_back(removed_call<int>(2, 1));
  expected_calls.push_back(removed_call<int>(1, 0));


  BOOST_CHECK_EQUAL_COLLECTIONS(expected.begin(), expected.end(),
                                values_vector.begin(), values_vector.end());

  BOOST_CHECK_EQUAL_COLLECTIONS(expected_calls.begin(), expected_calls.end(),
                                signals.begin(), signals.end());
}

struct list_collection_test
{
  boost::shared_ptr<writable_dynamic_list<int> > list1, list2, list3;
  boost::shared_ptr<dynamic_list_collection<int> > collection;

  dynamic_list_signals<int> signals, expected;

  typedef appended_call<int> app;
  typedef removed_call<int> rem;

  list_collection_test()
    : list1(dynamic_list_impl<int>::create()),
      list2(dynamic_list_impl<int>::create()),
      list3(dynamic_list_impl<int>::create()),
      collection(dynamic_list_collection<int>::create())
  {
    list1->append(1);
    list1->append(2);
    list1->append(3);

    list2->append(5);

    signals.attach(*collection);
  }

  static std::vector<int> as_vector(dynamic_list<int> &list)
  {
    std::vector<int> rval;

    for(boost::shared_ptr<enumerator<int> > e = list.enumerate();
        e->advance(); )
      rval.push_back(e->get_current());

    return rval;
  }
};

BOOST_FIXTURE_TEST_CASE(dynamicListCollectionAppendList, list_collection_test)
{
  collection->add_list(list2);
  collection->add_list(list3);
  collection->add_list(list1);

  std::vector<int> expected_values;
  expected_values.push_back(5);
  expected_values.push_back(1);
  expected_values.push_back(2);
  expected_values.push_back(3);

  expected.push_back(app(5));
  expected.push_back(app(1));
  expected.push_back(app(2));
  expected.push_back(app(3));

  std::vector<int> collection_vector = as_vector(*collection);
  BOOST_CHECK_EQUAL_COLLECTIONS(expected_values.begin(), expected_values.end(),
                                collection_vector.begin(), collection_vector.end());

  BOOST_CHECK_EQUAL_COLLECTIONS(expected.begin(), expected.end(),
                                signals.begin(), signals.end());
}

BOOST_FIXTURE_TEST_CASE(dynamicListCollectionRemoveList, list_collection_test)
{
  collection->add_list(list1);
  collection->add_list(list2);
  collection->add_list(list3);

  // Clear the current list of signals, since for the purposes of this
  // test, we don't care about the ones emitted by add_list() above.
  signals.clear();

  collection->remove_list(list3);
  collection->remove_list(list2);
  collection->remove_list(list1);

  expected.push_back(rem(5, 3));
  expected.push_back(rem(1, 0));
  expected.push_back(rem(2, 0));
  expected.push_back(rem(3, 0));

  std::vector<int> expected_values;
  std::vector<int> collection_vector = as_vector(*collection);
  BOOST_CHECK_EQUAL_COLLECTIONS(expected_values.begin(), expected_values.end(),
                                collection_vector.begin(), collection_vector.end());

  BOOST_CHECK_EQUAL_COLLECTIONS(expected.begin(), expected.end(),
                                signals.begin(), signals.end());
}
