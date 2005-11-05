// ref_ptr.h                                       -*-c++-*-
//
// A reference-counting pointer.  The object behind the pointer should
// implement incref() and decref(); the pointer arranges for these
// methods to be called at the appropriate times.

#ifndef REF_PTR_H
#define REF_PTR_H

#include <sigc++/reference_wrapper.h>

#include <generic/util/eassert.h>

template<class T>
class ref_ptr
{
  T *ref;

public:
  ref_ptr(T *_ref)
    :ref(_ref)
  {
    if(ref != 0)
      ref->incref();
  }

  ref_ptr(const ref_ptr &other)
    :ref(other.ref)
  {
    if(ref != 0)
      ref->incref();
  }

  template<class S>
  ref_ptr(const ref_ptr<S> &other)
    :ref(other.unsafe_get_ref())
  {
    if(ref != 0)
      ref->incref();
  }

  ref_ptr()
    :ref(0)
  {
  }

  ~ref_ptr()
  {
    if(ref != 0)
      ref->decref();
  }

  ref_ptr &operator=(const ref_ptr &other)
  {
    if(other.ref != 0)
      other.ref->incref();

    if(ref != 0)
      ref->decref();

    ref = other.ref;

    return *this;
  }

  const sigc::reference_wrapper<T> weak_ref() const
  {
    eassert(ref != 0);

    return sigc::ref(*ref);
  }

  // If S is assignment-compatible with T and both have
  // reference-counting methods, perform this assignment.
  //
  // Read: upcasting pointers.
  template<class S>
  ref_ptr<T> &operator=(const ref_ptr<S> &other)
  {
    S * const other_ref = other.unsafe_get_ref();

    if(other_ref != 0)
      other_ref->incref();

    if(ref != 0)
      ref->decref();

    ref = other_ref;

    return *this;
  }

  template<class S>
  bool operator==(const ref_ptr<S> &other) const
  {
    return ref == other.unsafe_get_ref();
  }

  template<class S>
  bool operator!=(const ref_ptr<S> &other) const
  {
    return ref != other.unsafe_get_ref();
  }

  template<class S>
  bool operator<(const ref_ptr<S> &other) const
  {
    return ref < other.unsafe_get_ref();
  }

  template<class S>
  bool operator>(const ref_ptr<S> &other) const
  {
    return ref > other.unsafe_get_ref();
  }

  template<class S>
  bool operator<=(const ref_ptr<S> &other) const
  {
    return ref <= other.unsafe_get_ref();
  }

  template<class S>
  bool operator>=(const ref_ptr<S> &other) const
  {
    return ref >= other.unsafe_get_ref();
  }

  // Safe downcasting.
  template<class S>
  ref_ptr<S> dyn_downcast() const
  {
    return ref_ptr<S>(dynamic_cast<S*>(ref));
  }

  bool valid() const
  {
    return ref != 0;
  }

  T *operator->() const
  {
    return ref;
  }

  /** Extract the pointer.  Should generally be used with care (but is
   *  used in the implementation to cast/compare between differently
   *  templated instances).
   */
  T *unsafe_get_ref() const
  {
    return ref;
  }
};

#endif
