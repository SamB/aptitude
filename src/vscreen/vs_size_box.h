// vs_size_box.h                       -*-c++-*-
//
// A container to ensure that its child has a particular minimum size
// (at least).
//
// Copyright 2004 Daniel Burrows

#ifndef VS_SIZE_BOX_H
#define VS_SIZE_BOX_H

#include "vs_bin.h"

/** A vs_size_box ensures that the requested size of its child is a
 *  given size or larger.
 */
class vs_size_box:public vs_bin
{
  size min_size;

  /** Internal: actually allocates the child's size. */
  void layout_me();
protected:
  vs_size_box(size s, const vs_widget_ref &w);

public:
  /** Create a vs_size_box.
   *
   *  \param s the minimum size of this box
   *  \param w the widget initially contained in this box (\b NULL to
   *           create an initially empty box)
   */
  static ref_ptr<vs_size_box> create(size s, const vs_widget_ref &w=NULL)
  {
    ref_ptr<vs_size_box> rval(new vs_size_box(s, w));
    rval->decref();
    return rval;
  }

  /** \return the least upper bound of the minimum size passed to the
   *  constructor and the true size request of the child.
   */
  int width_request();

  /**  \param w the width for which a height should be calculated.
   *
   *   \return the least upper bound of the minimum size passed to the
   *   constructor and the true size request of the child.
   */
  int height_request(int w);
};

typedef ref_ptr<vs_size_box> vs_size_box_ref;

#endif
