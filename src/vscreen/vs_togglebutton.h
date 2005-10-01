// vs_togglebutton.h			-*-c++-*-
//
//  I like having a "togglable" button which doesn't force a particular
// policy..it makes radio buttons much easier to do right.

#ifndef VS_TOGGLEBUTTON_H
#define VS_TOGGLEBUTTON_H

#include "vs_button.h"

#include <sigc++/functors/mem_fun.h>

class vs_togglebutton:public vs_button
{
  bool checked;
  char bracketl, mark, bracketr;

  void paint_check(int row);

protected:
  void silent_set_checked(bool _checked);
  // to be used mainly to avoid emitting signals (eg, if you're trying to
  // coordinate a togglebutton with an underlying option)

  vs_togglebutton(char _bracketl, char _mark, char _bracketr,
		  fragment *_label, bool _checked);

  vs_togglebutton(char _bracketl, char _mark, char _bracketr,
		  const std::string &_label, bool _checked);

public:
  static ref_ptr<vs_togglebutton>
  create(char bracketl, char mark, char bracketr,
	 fragment *label, bool checked = false)
  {
    ref_ptr<vs_togglebutton>
      rval(new vs_togglebutton(bracketl, mark, bracketr,
			       label, checked));
    rval->decref();
    return rval;
  }

  static ref_ptr<vs_togglebutton>
  create(char bracketl, char mark, char bracketr,
	 const std::string &label, bool checked = false)
  {
    ref_ptr<vs_togglebutton>
      rval(new vs_togglebutton(bracketl, mark, bracketr,
			       label, checked));
    rval->decref();
    return rval;
  }

  point get_cursorloc();

  void paint(const style &st);

  bool get_checked() {return checked;}
  void set_checked(bool _checked)
  {
    if(checked!=_checked)
      do_toggle();
  }

  // named "do_toggle" to avoid typos wrt "toggled"
  void do_toggle();

  sigc::signal0<void> toggled;
};

class vs_checkbutton:public vs_togglebutton
{
protected:
  vs_checkbutton(fragment *_label, bool _checked)
    :vs_togglebutton('[', 'X', ']', _label, _checked)
  {
    pressed.connect(sigc::mem_fun(*this, &vs_togglebutton::do_toggle));
  }

  vs_checkbutton(const std::string &_label, bool _checked)
    :vs_togglebutton('[', 'X', ']', _label, _checked)
  {
    pressed.connect(sigc::mem_fun(*this, &vs_togglebutton::do_toggle));
  }

  vs_checkbutton(char bracketr, char mark, char bracketl,
		 fragment *_label, bool _checked)
    :vs_togglebutton(bracketr, mark, bracketl, _label, _checked)
  {
    pressed.connect(sigc::mem_fun(*this, &vs_togglebutton::do_toggle));
  }

  vs_checkbutton(char bracketr, char mark, char bracketl,
		 const std::string &_label, bool _checked)
    :vs_togglebutton(bracketr, mark, bracketl, _label, _checked)
  {
    pressed.connect(sigc::mem_fun(*this, &vs_togglebutton::do_toggle));
  }

public:
  static ref_ptr<vs_checkbutton>
  create(fragment *label, bool checked = false)
  {
    return new vs_checkbutton(label, checked);
  }

  static ref_ptr<vs_checkbutton>
  create(const std::string &label, bool checked = false)
  {
    return new vs_checkbutton(label, checked);
  }

  static ref_ptr<vs_checkbutton>
  create(char bracketr, char mark, char bracketl,
	 fragment *label, bool checked = false)
  {
    return new vs_checkbutton(bracketr, mark, bracketl,
			      label, checked);
  }

  static ref_ptr<vs_checkbutton>
  create(char bracketr, char mark, char bracketl,
	 const std::string &label, bool checked = false)
  {
    return new vs_checkbutton(bracketr, mark, bracketl,
			      label, checked);
  }
};

class vs_radiobutton:public vs_togglebutton
{
protected:
  vs_radiobutton(fragment *_label, bool _checked)
    :vs_togglebutton('(', '*', ')', _label, _checked)
  {
  }

  vs_radiobutton(const std::string &_label, bool _checked)
    :vs_togglebutton('(', '*', ')', _label, _checked)
  {
  }

public:
  static ref_ptr<vs_radiobutton>
  create(fragment *label, bool checked = false)
  {
    return new vs_radiobutton(label, checked);
  }

  static ref_ptr<vs_radiobutton>
  create(const std::string &label, bool checked = false)
  {
    return new vs_radiobutton(label, checked);
  }
};

typedef ref_ptr<vs_togglebutton> vs_togglebutton_ref;
typedef ref_ptr<vs_checkbutton> vs_checkbutton_ref;
typedef ref_ptr<vs_radiobutton> vs_radiobutton_ref;

#endif
