// vs_center.h        -*-c++-*-
//
//  A simple container/layout widget which centers its child in itself.

#ifndef VS_CENTER_H
#define VS_CENTER_H

#include "vs_bin.h"

class vs_center:public vs_bin
{
  void layout_me();

protected:
  vs_center(const vs_widget_ref &w = NULL);

public:
  static ref_ptr<vs_center> create(const vs_widget_ref &w = NULL)
  {
    ref_ptr<vs_center> rval(new vs_center(w));
    rval->decref();
    return rval;
  }

  int width_request();
  int height_request(int width);
};

typedef ref_ptr<vs_center> vs_center_ref;

#endif
