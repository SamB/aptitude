/** \file dynamic_list.h */   // -*-c++-*-

#ifndef TOPLEVEL_DYNAMIC_LIST_H
#define TOPLEVEL_DYNAMIC_LIST_H

#include "enumerator.h"

#include <boost/shared_ptr.hpp>

#include <sigc++/signal.h>

namespace aptitude
{
  namespace util
  {
    /** \brief An abstract description of a dynamic collection of
     *  objects that only allows appending at the end.
     *
     *  This interface is read-only because it supports both actual
     *  lists, and synthetic views into lists for which modification
     *  is poorly defined.  If clients should be able to modify a list
     *  themselves, use writable_dynamic_list.
     *
     *  In addition to the usual requirements of containers, T must
     *  provide equality comparison and a hash function.
     *
     *  The advantages relative to TreeModel are that this is a simpler
     *  and narrower interface, and that it doesn't require GTK+.
     */
    template<typename T>
    class dynamic_list : public sigc::trackable
    {
    public:
      virtual ~dynamic_list();

      /** \brief Enumerate the values in this list.
       *
       *  To get a consistent picture of the values, the caller should
       *  enumerate them before any other process adds or removes a
       *  value.  Typically this means enumerating them in a tight
       *  loop.
       *
       *  The returned enumerator will be valid indefinitely.  In
       *  order to provide this guarantee, it may hold a strong
       *  pointer to the dynamic_list that created it.  Do not store
       *  it in a structure that is likely to form a cycle with the
       *  dynamic_list.
       */
      virtual boost::shared_ptr<enumerator<T> > enumerate() = 0;




      /** \brief Signals */
      // @{

      /** \brief Emitted after a value is added to the list. */
      sigc::signal<void, T> signal_appended;

      /** \brief Emitted after a value is removed from the list.
       *
       *  The arguments are the item, and its position within the
       *  list.
       */
      sigc::signal<void, T, std::size_t> signal_removed;

      // @}
    };

    /** \brief An abstract description of a dynamic collection of
     *  objects that allows client code to append and remove elements.
     */
    template<typename T>
    class writable_dynamic_list : public dynamic_list<T>
    {
    public:
      /** \brief Append a value to this list. */
      virtual void append(const T &value) = 0;

      /** \brief Remove a value from this list.
       *
       *  An arbitrary matching element is removed if there are
       *  duplicates.
       */
      virtual void remove(const T &value) = 0;
    };

    template<typename T>
    dynamic_list<T>::~dynamic_list()
    {
    }
  }
}

#endif // TOPLEVEL_DYNAMIC_LIST_H
