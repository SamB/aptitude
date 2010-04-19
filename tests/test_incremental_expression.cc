// test_incremental_expression.cc
//
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

#include <generic/problemresolver/incremental_expression.h>

#include <boost/variant.hpp>

#include <cppunit/extensions/HelperMacros.h>

namespace cw = cwidget;

namespace
{
  // Make it possible to "show" vectors.
  template<typename T, typename Alloc>
  std::ostream &operator<<(std::ostream &out, const std::vector<T, Alloc> &v)
  {
    out << "[";

    for(typename std::vector<T, Alloc>::const_iterator it =
          v.begin(); it != v.end(); ++it)
      {
        if(it != v.begin())
          out << ", ";

        out << *it;
      }

    out << "]";

    return out;
  }

  // Helper class for the code below that records a single call to
  // child_modified().
  template<typename T>
  class child_modified_call
  {
    cw::util::ref_ptr<expression<T> > child;
    T old_value, new_value;

  public:
    child_modified_call(const cw::util::ref_ptr<expression<T> > &_child,
                        const T &_old_value, const T &_new_value)
      : child(_child), old_value(_old_value), new_value(_new_value)
    {
    }

    const cw::util::ref_ptr<expression<T> > &get_child() const { return child; }
    const T &get_old_value() const { return old_value; }
    const T &get_new_value() const { return new_value; }

    bool operator==(const child_modified_call &other) const
    {
      return
        child == other.child &&
        old_value == other.old_value &&
        new_value == other.new_value;
    }
  };

  template<typename T>
  std::ostream &operator<<(std::ostream &out, const child_modified_call<T> &t)
  {
    return out << "child_modified(old_value = " << t.get_old_value()
               << ", new_value = " << t.get_new_value() << ")";
  }

  template<typename T>
  class get_value_call
  {
    T return_value;

  public:
    get_value_call(const T &_return_value)
      : return_value(_return_value)
    {
    }

    const T &get_return_value() const { return return_value; }

    bool operator==(const get_value_call &other) const
    {
      return return_value == other.return_value;
    }
  };

  template<typename T>
  std::ostream &operator<<(std::ostream &out, const get_value_call<T> &c)
  {
    return out << "get_value() => " << c.get_return_value();
  }

  // A class that implements the expression container interface,
  // passing all interesting calls to a single sub-object and
  // recording all the calls to child_modified.
  template<typename T>
  class fake_container : public expression_container<T>
  {
    cw::util::ref_ptr<expression<T> > real_object;
    std::vector<child_modified_call<T> > calls;

    fake_container(const cw::util::ref_ptr<expression<T> > &_real_object)
      : real_object(_real_object)
    {
      real_object->add_parent(this);
    }

  public:
    ~fake_container()
    {
      real_object->remove_parent(this);
    }

    static cw::util::ref_ptr<fake_container<T> >
    create(const cw::util::ref_ptr<expression<T> > &child)
    {
      return new fake_container<T>(child);
    }

    const std::vector<child_modified_call<T> > &get_calls() const { return calls; }

    void child_modified(const cw::util::ref_ptr<expression<T> > &child,
                        T old_value,
                        T new_value)
    {
      calls.push_back(child_modified_call<T>(child, old_value, new_value));
    }

    T get_value()
    {
      return real_object->get_value();
    }

    void dump(std::ostream &out)
    {
      real_object->dump(out);
    }
  };

  // Records calls to expression_box::changed.
  template<typename T>
  class changed_call
  {
    T new_value;

  public:
    changed_call(const T &_new_value)
      : new_value(_new_value)
    {
    }

    const T &get_new_value() const { return new_value; }
  };

  template<typename T>
  std::ostream &operator<<(std::ostream &out, const changed_call<T> &c)
  {
    return out << "changed(" << c.get_new_value() << ")";
  }

  // A helper class used to test that expression_wrapper behaves as
  // advertised.
  //
  // Wraps its subexpression and records all the calls to
  // child_modified() and changed().
  template<typename T>
  class fake_wrapper : public expression_wrapper<T>
  {
    std::vector<boost::variant<child_modified_call<T>, changed_call<T> > > calls;

    fake_wrapper() : expression_wrapper<T>() { }
    fake_wrapper(const cw::util::ref_ptr<expression<T> > &child)
      : expression_wrapper<T>(child)
    {
    }

  public:
    static cw::util::ref_ptr<fake_wrapper> create()
    {
      return new fake_wrapper;
    }

    static cw::util::ref_ptr<fake_wrapper>
    create(const cw::util::ref_ptr<expression<T> > &child)
    {
      return new fake_wrapper(child);
    }

    void child_modified(const cw::util::ref_ptr<expression<T> > &child,
                        T old_value,
                        T new_value)
    {
      calls.push_back(child_modified_call<T>(child, old_value, new_value));

      expression_wrapper<T>::child_modified(child, old_value, new_value);
    }

    void changed(T new_value)
    {
      calls.push_back(changed_call<T>(new_value));

      expression_wrapper<T>::changed(this->get_child());
    }
  };
}

class TestIncrementalExpression : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(TestIncrementalExpression);

  CPPUNIT_TEST(testIncrementalExpressionGetVarValue);
  CPPUNIT_TEST(testIncrementalExpressionSetVarValue);
  CPPUNIT_TEST(testIncrementalExpressionVarSignalChange);

  CPPUNIT_TEST_SUITE_END();

public:
  void testIncrementalExpressionGetVarValue()
  {
    cw::util::ref_ptr<var_e<int> >
      v0 = var_e<int>::create(0),
      v5 = var_e<int>::create(5),
      v9 = var_e<int>::create(9);

    CPPUNIT_ASSERT_EQUAL(0, v0->get_value());
    CPPUNIT_ASSERT_EQUAL(5, v5->get_value());
    CPPUNIT_ASSERT_EQUAL(9, v9->get_value());
  }

  void testIncrementalExpressionSetVarValue()
  {
    cw::util::ref_ptr<var_e<int> > v = var_e<int>::create(123456);

    v->set_value(987654);
    CPPUNIT_ASSERT_EQUAL(987654, v->get_value());
  }

  void testIncrementalExpressionVarSignalChange()
  {
    cw::util::ref_ptr<var_e<int> > v = var_e<int>::create(55555);

    cw::util::ref_ptr<fake_container<int> > c = fake_container<int>::create(v);

    std::vector<child_modified_call<int> > expected_calls;
    expected_calls.push_back(child_modified_call<int>(v, 55555, 42));
    expected_calls.push_back(child_modified_call<int>(v, 42, 10));

    v->set_value(42);
    // Test that setting to the same value doesn't emit a signal.
    v->set_value(42);
    v->set_value(10);

    CPPUNIT_ASSERT_EQUAL(expected_calls, c->get_calls());
  }
};
