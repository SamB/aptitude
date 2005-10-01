// fragment_contents.h                     -*-c++-*-
//
//   Copyright 2004 Daniel Burrows
//
// A nice way of storing the contents of a fragment.

#ifndef FRAGMENT_CONTENTS_H
#define FRAGMENT_CONTENTS_H

#include "curses++.h"

#include <vector>

/** The type used to represent a line of a fragment; it might be
 *  worthwhile to change this to a rope<chtype> if this class is
 *  used to format larger pieces of text.
 */
typedef wchstring fragment_line;

/** This class represents the formatted contents of a fragment.
 *
 *  To minimize silly mistakes, the lines are reference-counted.
 *
 *  \todo give this proper const_iterators and deal with the
 *  mutable-sharing problem (this structure is mutable but has shared
 *  data).
 */
class fragment_contents
{
  class fragment_lines
  {
    std::vector<fragment_line> v;
    mutable int refs;

  public:
    class const_iterator;

    class iterator
    {
      typedef std::vector<fragment_line>::iterator ittype;

      fragment_lines *lines;
      ittype i;

    public:
      iterator(fragment_lines *_lines, const ittype &_i)
	:lines(_lines), i(_i)
      { lines->incref(); }

      iterator(const iterator &other)
	:lines(other.lines), i(other.i)
      { lines->incref(); }

      ~iterator() { lines->decref(); }

      iterator &operator++() { ++i; return *this; }
      iterator &operator--() { --i; return *this; }

      fragment_line &operator*() { return *i; }
      fragment_line *operator->() { return &*i; }

      iterator &operator=(const iterator &other) {
	other.lines->incref();
	lines->decref();
	lines=other.lines;
	i=other.i;

	return *this;
      }

      bool operator==(const iterator &other) const {return i==other.i;}
      bool operator!=(const iterator &other) const {return i!=other.i;}

      friend class const_iterator;
    };

    class const_iterator
    {
      typedef std::vector<fragment_line>::const_iterator ittype;

      const fragment_lines *lines;
      ittype i;

    public:
      const_iterator(const fragment_lines *_lines, const ittype &_i)
	:lines(_lines), i(_i)
      { lines->incref(); }

      const_iterator(const iterator &other)
	:lines(other.lines), i(other.i)
      {	lines->incref(); }

      const_iterator(const const_iterator &other)
	:lines(other.lines), i(other.i)
      { lines->incref(); }

      ~const_iterator() { lines->decref(); }

      const_iterator &operator++() { ++i; return *this; }
      const_iterator &operator--() { --i; return *this; }

      const fragment_line &operator*() { return *i; }
      const fragment_line *operator->() { return &*i; }

      const_iterator &operator=(const const_iterator &other) {
	other.lines->incref();
	lines->decref();
	lines=other.lines;
	i=other.i;

	return *this;
      }

      const_iterator &operator=(const iterator &other) {
	other.lines->incref();
	lines->decref();
	lines=other.lines;
	i=other.i;

	return *this;
      }

      bool operator==(const iterator &other) const {return i==other.i;}
      bool operator==(const const_iterator &other) const {return i==other.i;}

      bool operator!=(const iterator &other) const {return i!=other.i;}
      bool operator!=(const const_iterator &other) const {return i!=other.i;}
    };

    fragment_lines():refs(0) {}

    void incref() const {++refs;}
    void decref() const {--refs; if(refs==0) delete this;}

    void push_back(const fragment_line &l) {v.push_back(l);}

    iterator begin() {return iterator(this, v.begin());}
    const_iterator begin() const {return const_iterator(this, v.begin());}

    iterator end() {return iterator(this, v.end());}
    const_iterator end() const {return const_iterator(this, v.end());}

    fragment_line &front() {return v.front();}
    const fragment_line &front() const {return v.front();}

    fragment_line &back() {return v.back();}
    const fragment_line &back() const {return v.back();}

    fragment_line &operator[](int i) { return v[i]; }
    const fragment_line &operator[](int i) const { return v[i]; }

    size_t size() const {return v.size();}
  };
public:
  typedef fragment_lines::iterator iterator;
  typedef fragment_lines::const_iterator const_iterator;

  /** Generate empty contents for a fragment. */
  fragment_contents():lines(new fragment_lines), final_nl(false)
  {lines->incref();}

  /** Copy constructor. */
  fragment_contents(const fragment_contents &other)
    :lines(other.lines), final_nl(other.final_nl)
  {
    lines->incref();
  }

  /** When this is destroyed, decrement the lines' reference count. */
  ~fragment_contents() {lines->decref();}

  void push_back(const fragment_line &l) {lines->push_back(l);}

  iterator begin() {return lines->begin();}
  const_iterator begin() const {return lines->begin();}

  iterator end() {return lines->end();}
  iterator end() const {return lines->end();}

  fragment_line &front() {return lines->front();}
  const fragment_line &front() const {return lines->front();}

  fragment_line &back() {return lines->back();}
  const fragment_line &back() const {return lines->back();}

  size_t size() const {return lines->size();}

  void set_final_nl(bool final_nl_new) {final_nl=final_nl_new;}

  bool get_final_nl() {return final_nl;}

  fragment_line &operator[](int i) { return (*lines)[i]; }
  const fragment_line &operator[](int i) const { return (*lines)[i]; }

  fragment_contents &operator=(const fragment_contents &other)
  {
    other.lines->incref();
    lines->decref();

    lines=other.lines;
    final_nl=other.final_nl;

    return *this;
  }

private:
  /** The vector of lines that this contents object is associated with. */
  fragment_lines *lines;

  /** If \b true, indicates that the last line of this fragment
   *  should be followed by a newline.
   */
  bool final_nl;
};

#endif
