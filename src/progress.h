// progress.h   -*-c++-*-
//
//  Copyright 2000, 2004-2005, 2007 Daniel Burrows
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

#ifndef VS_PROGRESS_H
#define VS_PROGRESS_H

#include <cwidget/widgets/tree.h>
#include <cwidget/widgets/widget.h>

#include <apt-pkg/progress.h>

/** \brief A cwidget::widgets::widget that also acts as a progress bar.
 * 
 *  \file progress.h
 */

class progress : public cwidget::widgets::widget, public OpProgress
{
protected:
  progress();
public:
  static cwidget::util::ref_ptr<progress> create()
  {
    return new progress;
  }

  virtual void paint(const cwidget::style &st);
  virtual void Update();
  virtual void Done();

  int width_request();
  int height_request(int w);

  bool get_cursorvisible();
  cwidget::widgets::point get_cursorloc();
};

typedef cwidget::util::ref_ptr<progress> progress_ref;

#endif
