// vs_label.cc

#include "vs_label.h"

#include "fragment_cache.h"
#include "vscreen.h"

#include <config/colors.h>

#include <algorithm>

using namespace std;

vs_label::vs_label(fragment *f)
  :txt(new fragment_cache(f))
{
}

vs_label::vs_label(const string &_txt, const style &st)
  :txt(new fragment_cache(text_fragment(_txt)))
{
  set_bg_style(st);
}

vs_label::vs_label(const string &_txt)
  :txt(new fragment_cache(text_fragment(_txt)))
{
}

vs_label::vs_label(const wstring &_txt, const style &st)
  :txt(new fragment_cache(text_fragment(_txt)))
{
  set_bg_style(st);
}

vs_label::vs_label(const wstring &_txt)
  :txt(new fragment_cache(text_fragment(_txt)))
{
}

vs_label_ref vs_label::create(const string &txt, const style &st)
{
  vs_label_ref rval(new vs_label(txt, st));
  rval->decref();
  return rval;
}

vs_label_ref vs_label::create(const string &txt)
{
  vs_label_ref rval(new vs_label(txt));
  rval->decref();
  return rval;
}

vs_label_ref vs_label::create(const wstring &txt, const style &st)
{
  vs_label_ref rval(new vs_label(txt, st));
  rval->decref();
  return rval;
}

vs_label_ref vs_label::create(const wstring &txt)
{
  vs_label_ref rval(new vs_label(txt));
  rval->decref();
  return rval;
}

vs_label::~vs_label()
{
  delete txt;
}

bool vs_label::get_cursorvisible()
{
  return false;
}

point vs_label::get_cursorloc()
{
  return point(0,0);
}

void vs_label::set_text(const string &_txt, const style &st)
{
  set_text(text_fragment(_txt));
  set_bg_style(st);
}

void vs_label::set_text(const string &_txt)
{
  set_text(text_fragment(_txt));
}

void vs_label::set_text(const wstring &_txt, const style &st)
{
  set_text(text_fragment(_txt));
  set_bg_style(st);
}

void vs_label::set_text(const wstring &_txt)
{
  set_text(text_fragment(_txt));
}

void vs_label::set_text(fragment *f)
{
  delete txt;
  txt=new fragment_cache(f);
  // Our size might have changed, so re-layout the screen.
  vscreen_queuelayout();
}

void vs_label::paint(const style &st)
{
  fragment_contents lines=txt->layout(getmaxx(), getmaxx(), st);

  for(size_t i=0; i<lines.size() && i<(unsigned) getmaxy(); ++i)
    mvaddnstr(i, 0, lines[i], lines[i].size());
}

int vs_label::width_request()
{
  return txt->max_width(0, 0);
}

int vs_label::height_request(int width)
{
  return txt->layout(width, width, style()).size();
}

bool vs_transientlabel::handle_char(chtype ch)
{
  destroy();
  return true;
}
