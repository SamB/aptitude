// vs_label.h          -*-c++-*-

#ifndef VS_LABEL_H
#define VS_LABEL_H

#include "vscreen_widget.h"

class fragment;
class fragment_cache;

/** vs_label widgets display some (possibly formatted) text
 *  statically.  The text cannot be scrolled or selected in any way;
 *  if there isn't room for it, it just gets clipped.
 *
 *  Passing a "background" style into the constructor modifies the
 *  background style of the widget (as set_bg_style would); this
 *  differs from wrapping the text in a style_fragment in that it
 *  even affects parts of the widget which aren't covered by text.
 */
class vs_label:public vscreen_widget
{
  fragment_cache *txt;
protected:
  vs_label(fragment *f);
  vs_label(const std::string &_txt, const style &st);
  vs_label(const std::string &_txt);
  vs_label(const std::wstring &_txt, const style &st);
  vs_label(const std::wstring &_txt);

public:
  static ref_ptr<vs_label> create(fragment *f)
  {
    ref_ptr<vs_label> rval(new vs_label(f));
    rval->decref();
    return rval;
  }

  /** Create a vs_label with the given text and background. */
  static ref_ptr<vs_label> create(const std::string &txt, const style &st);

  /** Create a vs_label with the given text. */
  static ref_ptr<vs_label> create(const std::string &txt);

  /** Create a vs_label with the given text and background. */
  static ref_ptr<vs_label> create(const std::wstring &txt, const style &st);

  /** CReate a vs_label with the given text. */
  static ref_ptr<vs_label> create(const std::wstring &txt); 


 ~vs_label();

  bool get_cursorvisible();
  point get_cursorloc();

  /** \return the maximum width of any line in the label. */
  int width_request();

  /** \return the number of lines in the label. */
  int height_request(int width);

  void paint(const style &st);
  void set_text(const std::string &_txt, const style &st);
  void set_text(const std::string &_txt);
  void set_text(const std::wstring &_txt, const style &st);
  void set_text(const std::wstring &_txt);
  void set_text(fragment *f);
};

class vs_transientlabel:public vs_label
// Displays a transient message -- grabs the input focus and vanishes when a
// key is pressed.
{
protected:
  virtual bool handle_char(chtype ch);

  vs_transientlabel(const std::string &msg, const style &st)
    :vs_label(msg, st)
  {
  }
public:
  static
  ref_ptr<vs_transientlabel> create(const std::string &msg,
				    const style &st)
  {
    return new vs_transientlabel(msg, st);
  }

  bool focus_me() {return true;}
};

typedef ref_ptr<vs_label> vs_label_ref;

typedef ref_ptr<vs_transientlabel> vs_transientlabel_ref;

#endif
