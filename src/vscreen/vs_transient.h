// vs_transient.h  -*-c++-*-
//
//   Copyright 2005 Daniel Burrows

#ifndef VS_TRANSIENT_H
#define VS_TRANSIENT_H

#include "vs_bin.h"

/** This class is a visually transparent wrapper around another
 *  widget.  It captures all keystrokes (preventing the subwidget from
 *  recieving them), and destroys itself upon receiving one.
 */
class vs_transient:public vs_bin
{
private:
  /** Handle layout: the subwidget is assigned the entire area of this
   *  widget.
   */
  void layout_me();

protected:
  vs_transient(const vs_widget_ref &w);
public:
  /** Create a new vs_transient.
   *
   *  \param w the widget to place inside the transient wrapper.
   */
  static ref_ptr<vs_transient>
  create(const vs_widget_ref &w = NULL)
  {
    ref_ptr<vs_transient> rval(new vs_transient(w));
    rval->decref();
    return rval;
  }

  /** \return the desired width of the subwidget. */
  int width_request();

  /** Calculate the desired height of the subwidget.
   *
   *  \param width the width of this widget
   *  \return the desired height
   */
  int height_request(int width);

  /** \return \b true: vs_transients can always be focussed. */
  bool focus_me();

  /** Destroy the transient.
   *
   *  \return \b true.
   */
  bool handle_char(chtype ch);
};

typedef ref_ptr<vs_transient> vs_transient_ref;

#endif // VS_TRANSIENT_H
