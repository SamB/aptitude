/** \file test_dynamic_set.cc */   // -*-c++-*-


// Copyright (C) 2010 Daniel Burrows
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; see the file COPYING.  If not, write to
// the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.


#include <generic/util/dynamic_set.h>
#include <generic/util/dynamic_set_impl.h>
#include <generic/util/dynamic_set_union.h>

#include <boost/test/unit_test.hpp>
#include <boost/variant.hpp>

#include <vector>

using aptitude::util::dynamic_set;
using aptitude::util::dynamic_set_impl;
using aptitude::util::dynamic_set_union;
using aptitude::util::enumerator;
using aptitude::util::writable_dynamic_set;

namespace
{
  template<typename T>
  class inserted_call
  {
    T value;

  public:
    inserted_call(const T &_value)
      : value(_value)
    {
    }

    T get_value() const { return value; }

    bool operator==(const inserted_call &other) const
    {
      return value == other.value;
    }

    bool operator!=(const inserted_call &other) const
    {
      return !(*this == other);
    }
  };

  template<typename T>
  std::ostream &operator<<(std::ostream &out,
                           const inserted_call<T> &call)
  {
    return out << "inserted(value = " << call.get_value() << ")";
  }

  template<typename T>
  class removed_call
  {
    T value;

  public:
    removed_call(const T &_value)
      : value(_value)
    {
    }

    T get_value() const { return value; }

    bool operator==(const removed_call &other) const
    {
      return value == other.value;
    }

    bool operator!=(const removed_call &other) const
    {
      return !(*this == other);
    }
  };

  template<typename T>
  std::ostream &operator<<(std::ostream &out,
                           const removed_call<T> &call)
  {
    return out << "removed(value = " << call.get_value() << ")";
  }

  template<typename T>
  class set_signal
  {
  public:
    typedef boost::variant<inserted_call<T>,
                           removed_call<T> > value_type;

  private:
    value_type value;

  public:
    set_signal(const inserted_call<T> &_value)
      : value(_value)
    {
    }

    set_signal(const removed_call<T> &_value)
      : value(_value)
    {
    }

    set_signal(const value_type &_value)
      : value(_value)
    {
    }

    value_type get_value() const
    {
      return value;
    }

    bool operator==(const set_signal &other) const
    {
      return value == other.value;
    }

    bool operator!=(const set_signal &other) const
    {
      return !(*this == other);
    }
  };

  template<typename T>
  std::ostream &operator<<(std::ostream &out, const set_signal<T> &signal)
  {
    return out << signal.get_value();
  }

  template<typename T>
  class dynamic_set_signals
  {
    std::vector<set_signal<T> > calls;

    void inserted(const T &value)
    {
      calls.push_back(inserted_call<T>(value));
    }

    void removed(const T &value)
    {
      calls.push_back(removed_call<T>(value));
    }

    dynamic_set_signals(const dynamic_set_signals &);

  public:
    dynamic_set_signals()
    {
    }

    ~dynamic_set_signals()
    {
    }

    void clear()
    {
      calls.clear();
    }

    void attach(dynamic_set<T> &list)
    {
      list.connect_inserted(sigc::mem_fun(*this, &dynamic_set_signals::inserted));
      list.connect_removed(sigc::mem_fun(*this, &dynamic_set_signals::removed));
    }

    void push_back(const set_signal<T> &signal)
    {
      calls.push_back(signal);
    }

    typedef typename std::vector<set_signal<T> >::const_iterator const_iterator;
    const_iterator begin() const { return calls.begin(); }
    const_iterator end() const { return calls.end(); }

    bool operator==(const dynamic_set_signals &other) const
    {
      return calls == other.calls;
    }

    bool operator!=(const dynamic_set_signals &other) const
    {
      return !(*this == other);
    }
  };

  template<typename T>
  std::ostream &operator<<(std::ostream &out,
                           const dynamic_set_signals<T> &signals)
  {
    out << "[";
    for(typename dynamic_set_signals<T>::const_iterator
          it = signals.begin(); it != signals.end(); ++it)
      {
        if(it != signals.begin())
          out << ", ";
        out << *it;
      }
    out << "]";

    return out;
  }

  struct set_test
  {
    boost::shared_ptr<writable_dynamic_set<int> > valuesPtr;
    writable_dynamic_set<int> &values;
    dynamic_set_signals<int> signals;

    std::vector<int> expected;

    typedef inserted_call<int> ins;
    typedef removed_call<int> rem;

    set_test()
      : valuesPtr(dynamic_set_impl<int>::create()),
        values(*valuesPtr)
    {
      values.insert(1);
      values.insert(2);
      values.insert(3);
    }

    std::vector<int> as_vector() const
    {
      std::vector<int> rval;

      for(boost::shared_ptr<enumerator<int> > e = values.enumerate();
          e->advance(); )
        {
          rval.push_back(e->get_current());
        }

      return rval;
    }
  };
}

BOOST_AUTO_TEST_CASE(dynamicSetSignals)
{
  dynamic_set_signals<int> signals1, signals2, signals3;

  typedef inserted_call<int> ins;
  typedef removed_call<int> rem;

  signals1.push_back(ins(4));
  signals1.push_back(rem(2));
  signals1.push_back(ins(9));

  signals2.push_back(ins(4));
  signals2.push_back(ins(2));
  signals2.push_back(ins(9));

  signals3.push_back(ins(4));
  signals3.push_back(rem(2));
  signals3.push_back(ins(9));


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
