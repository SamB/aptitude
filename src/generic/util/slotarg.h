// slotarg.h			-*-c++-*-
//
//  Copyright 2000 Daniel Burrows
//
//  Provides a mechanism for nicely passing in optional slots to a function.
// (you can pass either a reference to one or a pointer (which can be NULL))
//
//  Eg: some_slot_function(slotarg, slotarg, slotarg) can be called as:
// some_slot_function(arg(slota), NULL, arg(slotb)) to omit the second slot.

#ifndef SLOTARG_H
#define SLOTARG_H

#include <sigc++/functors/slot.h>

template<typename T>
class slotarg
{
  bool hasslot;
  T theslot;
public:
  slotarg(const T *slot)
  {
    if(slot)
      {
	theslot=*slot;
	hasslot=true;
      }
    else
      hasslot=false;
  }
  slotarg(const T &slot)
    :hasslot(true), theslot(slot)
  {
  }

  template <typename S>
  operator slotarg<S>() const
  {
    if(hasslot)
      return slotarg<S>(theslot);
    else
      return slotarg<S>(NULL);
  }

  operator bool() const {return hasslot;}
  const T & operator*() const {return theslot;}
  T & operator*() {return theslot;}
};

typedef slotarg<sigc::slot0<void> > slot0arg;

template<typename T>
slotarg<T> arg(const T &slot)
{
  return slotarg<T>(slot);
}

#endif
