// vs_pager.cc
//
//  Copyright 2000 Daniel Burrows

#include "vs_pager.h"

// For _()
#include "../aptitude.h"

#include "transcode.h"
#include "vscreen.h"
#include "vs_minibuf_win.h"
#include "vs_editline.h"
#include "config/keybindings.h"

#include <unistd.h>
#include <errno.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/fcntl.h>

#include <sigc++/functors/mem_fun.h>

using namespace std;

keybindings *vs_pager::bindings=NULL;

vs_pager::vs_pager(const char *text, int len, const char *encoding)
  :vscreen_widget(), first_line(0), first_column(0), text_width(0)
{
  set_text(text, len, encoding);

  do_layout.connect(sigc::mem_fun(*this, &vs_pager::layout_me));
}

vs_pager::vs_pager(const string &s, const char *encoding)
  :vscreen_widget(), first_line(0), first_column(0), text_width(0)
{
  set_text(s, encoding);

  do_layout.connect(sigc::mem_fun(*this, &vs_pager::layout_me));
}

vs_pager::vs_pager(const wstring &s)
  :vscreen_widget(), first_line(0), first_column(0), text_width(0)
{
  set_text(s);

  do_layout.connect(sigc::mem_fun(*this, &vs_pager::layout_me));
}

vs_pager::~vs_pager() {}

void vs_pager::set_text(const string &s, const char *encoding)
{
  set_text(transcode(s, encoding));
}

void vs_pager::set_text(const char *txt, string::size_type len,
			const char *encoding)
{
  // FIXME: get rid of the intermediate copy of the data if possible.
  set_text(transcode(string(txt, len), encoding));
}

void vs_pager::set_text(const wstring &s)
{
  vs_widget_ref tmpref(this);

  wstring::size_type loc=0;

  text_width=0;

  lines.clear();

  while(loc<s.size())
    {
      wstring curline;
      col_count cur_width=0;

      while(loc<s.size() && s[loc]!=L'\n')
	{
	  wchar_t ch=s[loc];
	  bool printable=iswprint(ch);

	  // Strip out tabs, as it's easier to figure out how many
	  // spaces they correspond to here rather than waiting until
	  // we're in the throes of rendering.
	  if(ch==L'\t')
	    {
	      const unsigned int amt
		= 8 - cur_width + 8;

	      cur_width += amt;
	      curline.append(amt, L' ');
	    }
	  else if(printable)
	    {
	      cur_width += wcwidth(ch);
	      curline   += ch;
	    }

	  ++loc;
	}

      if(loc<s.size())
	++loc;

      text_width=max(cur_width, text_width);

      lines.push_back(curline);
    }

  // Bouncing to the start is easiest.
  first_line=0;
  first_column=0;

  do_line_signal();
  vscreen_queuelayout();
  vscreen_redraw();
}

void vs_pager::do_line_signal()
{
  vs_widget_ref tmpref(this);

  int realmax=max<int>(lines.size()-getmaxy(), 0);
  line_changed(first_line, realmax);
}

void vs_pager::do_column_signal()
{
  vs_widget_ref tmpref(this);

  int realmax=max<int>(text_width-getmaxx(), 0);
  column_changed(first_column, realmax);
}

void vs_pager::scroll_up(line_count nlines)
{
  vs_widget_ref tmpref(this);

  if(first_line<nlines)
    first_line=0;
  else
    first_line-=nlines;

  do_line_signal();
  vscreen_update();
}

void vs_pager::scroll_down(line_count nlines)
{
  vs_widget_ref tmpref(this);

  first_line=min(first_line+nlines, lines.size()-getmaxy());

  do_line_signal();
  vscreen_update();
}

void vs_pager::scroll_left(col_count ncols)
{
  vs_widget_ref tmpref(this);

  if(first_column<ncols)
    first_column=0;
  else
    first_column-=ncols;

  do_column_signal();
  vscreen_update();
}

void vs_pager::scroll_right(col_count ncols)
{
  vs_widget_ref tmpref(this);

  first_column=min(first_column+ncols, text_width-getmaxx());

  do_column_signal();
  vscreen_update();
}

void vs_pager::scroll_top()
{
  vs_widget_ref tmpref(this);

  first_line=0;

  do_line_signal();
  vscreen_update();
}

void vs_pager::scroll_bottom()
{
  vs_widget_ref tmpref(this);

  first_line=lines.size()-getmaxy();

  do_line_signal();
  vscreen_update();
}

void vs_pager::scroll_page(bool dir)
{
  vs_widget_ref tmpref(this);

  if(dir)
    scroll_up(getmaxy());
  else
    scroll_down(getmaxy());
}

void vs_pager::search_omnidirectional_for(const wstring &s, bool forward)
{
  vs_widget_ref tmpref(this);

  if(s!=L"")
    last_search=s;
  else if(last_search==L"")
    {
      beep();
      return;
    }

  line_count i = forward ? first_line + 1 : first_line - 1;

  while(i > 0 && i < lines.size())
    {
      wstring::size_type loc
	= forward ? lines[i].find(last_search) : lines[i].rfind(last_search);

      if(loc!=wstring::npos)
	{
	  int last_search_width=wcswidth(last_search.c_str(), last_search.size());
	  wstring &line=lines[i];
	  col_count foundcol=0;
	  for(wstring::size_type j=0; j<loc; ++j)
	    foundcol+=wcwidth(line[j]);

	  first_line=i;
	  do_line_signal();

	  if(foundcol<first_column)
	    {
	      first_column=foundcol;
	      do_column_signal();
	    }
	  else if(foundcol+last_search_width>=first_column+getmaxx())
	    {
	      if(last_search_width>(col_count) getmaxx())
		first_column=foundcol;
	      else
		first_column=foundcol+last_search_width-getmaxx();

	      do_column_signal();
	    }

	  vscreen_update();
	  return;
	}

      if(forward)
	++i;
      else
	--i;
    }
  beep();
}

bool vs_pager::handle_key(const key &k)
{
  vs_widget_ref tmpref(this);

  if(bindings->key_matches(k, "Up"))
    scroll_up(1);
  else if(bindings->key_matches(k, "Down"))
    scroll_down(1);
  else if(bindings->key_matches(k, "Left"))
    scroll_left(1);
  else if(bindings->key_matches(k, "Right"))
    scroll_right(1);
  else if(bindings->key_matches(k, "PrevPage"))
    scroll_up(getmaxy());
  else if(bindings->key_matches(k, "NextPage"))
    scroll_down(getmaxy());
  else if(bindings->key_matches(k, "Begin"))
    scroll_top();
  else if(bindings->key_matches(k, "End"))
    scroll_bottom();
  else
    return vscreen_widget::handle_key(k);

  return true;
}

// Could find out what dimensions changed and only update along those?
void vs_pager::layout_me()
{
  vs_widget_ref tmpref(this);

  do_line_signal();
  do_column_signal();
}

void vs_pager::paint(const style &st)
{
  vs_widget_ref tmpref(this);

  int width,height;
  getmaxyx(height, width);

  for(int y=0; y<height && first_line+y<lines.size(); ++y)
    {
      const wstring &s=lines[first_line+y];
      col_count x=0;
      wstring::size_type curr=0;

      while(curr<s.size() && x<first_column+width)
	{
	  wchar_t ch = s[curr];
	  // No nonprintables should appear (set_text screens them
	  // out)
	  eassert(iswprint(ch));

	  if(x >= first_column)
	    {

	      mvadd_wch(y, x-first_column, ch);
	      x += wcwidth(ch);
	    }
	  else
	    x += wcwidth(ch);

	  ++curr;
	}
    }
}

int vs_pager::width_request()
{
  return text_width;
}

int vs_pager::height_request(int w)
{
  return lines.size();
}

void vs_pager::init_bindings()
{
  bindings=new keybindings(&global_bindings);
}

vs_file_pager::vs_file_pager():vs_pager("")
{
}

vs_file_pager::vs_file_pager(const string &filename,
			     const char *encoding):vs_pager("")
{
  load_file(filename, encoding);
}

vs_file_pager::vs_file_pager(const wstring &filename,
			     const char *encoding):vs_pager("")
{
  load_file(filename, encoding);
}

vs_file_pager::vs_file_pager(const char *text, int size,
			     const char *encoding)
  :vs_pager(text, size, encoding)
{
}

void vs_file_pager::load_file(const string &filename,
			      const char *encoding)
{
  vs_widget_ref tmpref(this);

  int fd=open(filename.c_str(), O_RDONLY, 0644);

  if(fd==-1)
    set_text("open: "+filename+": "+strerror(errno));
  else
    {
      struct stat buf;
      if(fstat(fd, &buf)<0)
	{
	  close(fd);
	  fd=-1;
	  set_text("fstat: "+filename+": "+strerror(errno));
	}
      else
	{
	  const char *contents=(const char *) mmap(NULL,
						   buf.st_size,
						   PROT_READ,
						   MAP_SHARED,
						   fd,
						   0);

	  if(contents==MAP_FAILED)
	    {
	      close(fd);
	      fd=-1;
	      set_text("mmap: "+filename+": "+strerror(errno));
	      // FIXME: just display something in the widget itself?
	    }
	  else
	    vs_pager::set_text(contents, buf.st_size, encoding);


	  if(fd!=-1)
	    {
	      munmap((void *) contents, buf.st_size);
	      close(fd);
	    }
	}
    }
}

void vs_file_pager::load_file(const wstring &filename,
			      const char *encoding)
{
  vs_widget_ref tmpref(this);

  string mbfilename;

  if(transcode(filename, mbfilename))
    load_file(mbfilename, encoding);
  else
    {
      wchar_t buf[512];

      swprintf(buf, sizeof(buf)/sizeof(wchar_t),
	       transcode(_("Unable to load filename: the string %ls has no multibyte representation.")).c_str(), filename.c_str());
      set_text(buf);
    }
}

void vs_file_pager::load_file(const wstring &filename)
{
  load_file(filename, NULL);
}
