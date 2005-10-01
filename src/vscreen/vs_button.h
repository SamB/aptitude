// vs_button.h      -*-c++-*-
//
//  A button is just a widget which accepts keyboard focus and can be
// "pressed".  I'm going to make a stab at sharing code between
// normal buttons, radio buttons, and checkbuttons..this may not be
// worth it..

#ifndef VS_BUTTON_H
#define VS_BUTTON_H

#include "vscreen_widget.h"

#include <string>

class fragment;
class fragment_cache;

/** This class represents a push-button. */
class vs_button:public vscreen_widget
{
  fragment_cache *label;

  void accept_focus();
  void lose_focus();

protected:
  bool handle_key(const key &k);
  fragment_cache *get_label() const {return label;}

  /** Instantiate a vs_button.
   *
   *  \param _label the new label of this button; it will be placed
   *  inside a simple text_fragment.
   */
  vs_button(const std::wstring &_label);
  vs_button(fragment *_label);
  vs_button(const std::string &_label);
public:

  ~vs_button();

  static ref_ptr<vs_button>
  create(const std::wstring &label)
  {
    ref_ptr<vs_button> rval(new vs_button(label));
    // Remove the initial construction reference.
    rval->decref();
    return rval;
  }

  /** Instantiate a vs_button.
   *
   *  \param _label the new label of this button; the button is
   *  responsible for deleting it.
   */
  static ref_ptr<vs_button> create(fragment *label)
  {
    ref_ptr<vs_button> rval(new vs_button(label));
    rval->decref();
    return rval;
  }

  /** Instantiate a vs_button.
   *
   *  \param _label the new label of this button; it will be placed
   *  inside a simple text_fragment.
   */
  static ref_ptr<vs_button> create(const std::string &label)
  {
    ref_ptr<vs_button> rval(new vs_button(label));
    rval->decref();
    return rval;
  }

  void paint(const style &st);

  bool get_cursorvisible();
  point get_cursorloc();
  bool focus_me();

  int width_request();
  int height_request(int width);
  void dispatch_mouse(short id, int x, int y, int z, mmask_t bmask);

  void set_label(const fragment *_label);

  // Signals:

  // The button has been "pressed" (activated)
  sigc::signal0<void> pressed;
};

typedef ref_ptr<vs_button> vs_button_ref;

#endif
