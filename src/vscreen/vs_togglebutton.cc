// vs_togglebutton.cc

#include "vscreen.h"
#include "vs_togglebutton.h"

#include "fragment.h"
#include "fragment_cache.h"

#include <algorithm>

using namespace std;

vs_togglebutton::vs_togglebutton(char _bracketl, char _mark, char _bracketr,
				 fragment *_label, bool _checked)
:vs_button(_label), checked(_checked),
 bracketl(_bracketl), mark(_mark), bracketr(_bracketr)
{
}

vs_togglebutton::vs_togglebutton(char _bracketl, char _mark, char _bracketr,
				 const std::string &_label, bool _checked)
:vs_button(_label), checked(_checked),
 bracketl(_bracketl), mark(_mark), bracketr(_bracketr)
{
}

void vs_togglebutton::paint_check(int row)
{
  mvaddch(row, 0, bracketl);

  if(checked)
    addch(mark);
  else
    addch(' ');

  addch(bracketr);
}

point vs_togglebutton::get_cursorloc()
{
  return point(0, getmaxy()/2);
}

void vs_togglebutton::paint(const style &st)
{
  const size_t labelw=getmaxx()>=4?getmaxx()-4:0;
  const fragment_contents lines=get_label()->layout(labelw, labelw, st);
  const size_t checkheight=getmaxy()/2;

  const style button_style=get_isfocussed()?st+style_attrs_flip(A_REVERSE):st;

  for(size_t i=0; i<min<size_t>(lines.size(), getmaxy()); ++i)
    {
      if(i==checkheight)
	{
	  apply_style(button_style);

	  paint_check(i);

	  apply_style(st);
	}

      mvaddnstr(i, 4, lines[i], lines[i].size());
    }
}

void vs_togglebutton::do_toggle()
{
  checked=!checked;
  toggled();
  vscreen_update();
}

void vs_togglebutton::silent_set_checked(bool _checked)
{
  if(checked!=_checked)
    {
      checked=_checked;
      vscreen_update();
    }
}
