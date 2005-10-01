// vs_progress.h   -*-c++-*-
//
//  Copyright 2000, 2004-2005 Daniel Burrows
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; see the file COPYING.  If not, write to
//  the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//  Boston, MA 02111-1307, USA.
//
//  A vscreen_widget that also acts as a progress bar.

#ifndef VS_PROGRESS_H
#define VS_PROGRESS_H

#include "vscreen/vs_tree.h"
#include "vscreen/vscreen_widget.h"

#include <apt-pkg/progress.h>

class vs_progress:public vscreen_widget, public OpProgress
{
protected:
  vs_progress();
public:
  static ref_ptr<vs_progress> create()
  {
    return new vs_progress;
  }

  virtual void paint(const style &st);
  virtual void Update();
  virtual void Done();

  int width_request();
  int height_request(int w);

  bool get_cursorvisible();
  point get_cursorloc();
};

typedef ref_ptr<vs_progress> vs_progress_ref;

#endif
