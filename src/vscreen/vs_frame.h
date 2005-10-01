// vs_frame.h        -*-c++-*-
//
// A container that draws a frame around the widget it contains.
// (needs a lot more work to gracefully handle layout issues :) )

#ifndef VS_FRAME_H
#define VS_FRAME_H

#include "vs_bin.h"

class vs_frame:public vs_bin
{
  void layout_me();

protected:
  vs_frame(const vs_widget_ref &w);

public:
  static ref_ptr<vs_frame> create(const vs_widget_ref &w)
  {
    ref_ptr<vs_frame> rval(new vs_frame(w));
    rval->decref();
    return rval;
  }

  /** \return the desired width of the frame.  A frame is 2 larger
   *   than its contents in every direction.
   */
  int width_request();

  /** Calculate the desired height of the frame.  A frame is 2 larger
   *  than its contents in every direction.
   *
   *  \param width the width of the frame
   *  \return the desired height
   */
  int height_request(int width);

  virtual void paint(const style &st);
};

typedef ref_ptr<vs_frame> vs_frame_ref;

#endif
