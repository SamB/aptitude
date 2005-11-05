// curses++.cc
//
//  Copyright 1999-2005 Daniel Burrows
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
//  A few initialization routines and so on.

#include "curses++.h"
#include "config/style.h"

#include <stdarg.h>

#include <string>

//  Note: resize handling is *really* nasty.  REALLY nasty.  I mean it. :)

#include <unistd.h>
#include <fcntl.h>
#include <termios.h>
#include <sys/ioctl.h>

cwindow rootwin=NULL;
cwindow rootwinhack=NULL;

using namespace std;

chstring::chstring(const string &s)
{
  (*this)=s;
}

chstring::chstring(const string &s, const style &st)
{
  (*this)=s;
  apply_style(st);
}

chstring::chstring(const chstring &s, const style &st)
  :super(s)
{
  (*this)=s;
  apply_style(st);
}

void chstring::apply_style(const style &st)
{
  for(iterator i=begin(); i!=end(); ++i)
    *i=st.apply_to(*i);
}

wchstring::wchstring(const wstring &s)
{
  for(wstring::const_iterator i=s.begin(); i!=s.end(); ++i)
    push_back(wchtype(*i, A_NORMAL));
}

wchstring::wchstring(const wstring &s, const style &st)
{
  attr_t attrs=st.get_attrs();

  for(wstring::const_iterator i=s.begin(); i!=s.end(); ++i)
    push_back(wchtype(*i, attrs));
}

wchstring::wchstring(const wchstring &s, const style &st)
  :super(s)
{
  (*this)=s;
  apply_style(st);
}

void wchstring::apply_style(const style &st)
{
  for(iterator i=begin(); i!=end(); ++i)
    *i=st.apply_to(*i);
}

int wchstring::width() const
{
  int rval=0;
  for(const_iterator i=begin(); i!=end(); ++i)
    rval+=wcwidth(i->ch);

  return rval;
}

int char_traits<chtype>::compare(const chtype *s1,
				 const chtype *s2,
				 size_t n)
{
  const chtype *s1end=s1+n;

  while(s1!=s1end)
    {
      chtype c1=*s1;
      chtype c2=*s2;

      if(c1<c2)
	return -1;
      else if(c1>c2)
	return 1;

      ++s1;
      ++s2;
    }

  return 0;
}

size_t char_traits<chtype>::length (const char_type* s)
{
  size_t rval=0;

  while(*s!=eos())
    {
      ++rval;
      ++s;
    }

  return rval;
}

chtype *char_traits<chtype>::assign(char_type *s,
				    size_t n,
				    const char_type &c)
{
  char_type *ends=s+n;

  while(s!=ends)
    {
      *s=c;
      ++s;
    }

  return s;
}

int char_traits<wchtype>::compare(const wchtype *s1,
				  const wchtype *s2,
				  size_t n)
{
  const wchtype *s1end=s1+n;

  while(s1!=s1end)
    {
      wchtype c1=*s1;
      wchtype c2=*s2;

      if(c1<c2)
	return -1;
      else if(c1>c2)
	return 1;

      ++s1;
      ++s2;
    }

  return 0;
}

size_t char_traits<wchtype>::length (const char_type* s)
{
  size_t rval=0;

  while(*s!=eos())
    {
      ++rval;
      ++s;
    }

  return rval;
}

wchtype *char_traits<wchtype>::assign(char_type *s,
				      size_t n,
				      const char_type &c)
{
  char_type *ends=s+n;

  while(s!=ends)
    {
      *s=c;
      ++s;
    }

  return s;
}

chstring &chstring::operator=(const std::string &s)
{
  erase();

  for(std::string::const_iterator i=s.begin();
      i!=s.end(); ++i)
    push_back((*i)|A_NORMAL);

  return *this;
}

void init_curses()
{
  rootwin=initscr();
  rootwinhack=rootwin;

  cbreak();
  noecho();
  nonl();
  intrflush(stdscr,FALSE);
  keypad(stdscr,TRUE);

  start_color();
  init_colors();
}

void resize()
{
  int fd;

  if( (fd=open("/dev/tty",O_RDONLY)!=-1))
    {
      struct winsize w;
      if(ioctl(fd, TIOCGWINSZ, &w)!=-1)
	{
	  resize_term(w.ws_row,w.ws_col);
	  rootwin=newwin(w.ws_row, w.ws_col, 0, 0);
	  eassert(rootwin);
	  //eassert(rootwin.getmaxy()==w.ws_row);
	  //eassert(rootwin.getmaxx()==w.ws_col);
	  return;
	}
      else
	{
	  beep();
	  perror("ioctl");
	}
      close(fd);
    }
  else
    {
      beep();
      perror("open");
    }
}

int cwindow::printw(char *str, ...)
{
  va_list args;
  int amt;

  va_start(args, str);
  amt=vwprintw(win, str, args);
  va_end(args);

  return amt;
}

void cwindow::show_string_as_progbar(int x, int y, const wstring &s,
				     int attr1, int attr2, int size1,
				     int totalsize)
{
  int width,height;

  getmaxyx(height,width);

  attrset(attr1);
  move(y, x);

  size_t loc=0;
  while(x<width)
    {
      if(x>=size1)
	attrset(attr2);

      wchar_t ch=L' ';
      if(loc<s.size())
	{
	  ch=s[loc];
	  ++loc;
	}

      add_wch(ch);
      x+=wcwidth(ch);
    }
}

void cwindow::display_header(wstring s, const attr_t attr)
{
  attrset(attr);

  int width,height;
  getmaxyx(height,width);

  move(0, 0);
  int x=0;

  wstring::size_type sloc=0;

  while(x<width)
    {
      if(sloc<s.size())
	{
	  const wchar_t wch=s[sloc];

	  add_wch(wch);
	  x+=wcwidth(wch);
	  ++sloc;
	}
      else
	{
	  add_wch(L' ');
	  x+=wcwidth(L' ');
	}
    }
}

void cwindow::display_status(wstring s, attr_t attr)
{
  attrset(attr);

  int width,height;
  getmaxyx(height,width);

  move(height-1, 0);
  int x=0;

  wstring::size_type sloc=0;

  while(x<width)
    {
      if(sloc<s.size())
	{
	  const wchar_t wch=s[sloc];

	  add_wch(wch);
	  x+=wcwidth(wch);
	  ++sloc;
	}
      else
	{
	  add_wch(L' ');
	  x+=wcwidth(L' ');
	}
    }
}

int cwindow::addstr(const wchstring &str)
{
  return addnstr(str, str.size());
}

int cwindow::addnstr(const wchstring &str, size_t n)
{
  int rval=OK;

  for(string::size_type i=0; i<n && i<str.size(); ++i)
    {
      // Construct a cchar_t from the single character at the head of
      // the string.  The weird intermediate single-character string
      // exists to work around the vagueness of the setcchar
      // semantics.


      cchar_t wch;
      wchar_t dummy[2];

      dummy[0]=str[i].ch;
      dummy[1]=L'\0';

      // How can I notify the user of errors?
      if(setcchar(&wch, dummy, str[i].attrs,
		  PAIR_NUMBER(str[i].attrs), 0) == ERR)
	{
	  rval=ERR;
	  attr_t a=get_style("Error").get_attrs();
	  if(setcchar(&wch, L"?", a, PAIR_NUMBER(a), 0) == ERR)
	    continue;
	}

      if(wadd_wch(win, &wch) == ERR)
	rval=ERR;
    }

  return rval;
}

int cwindow::mvaddstr(int y, int x, const wchstring &str)
{
  return mvaddnstr(y, x, str, str.size());
}

int cwindow::mvaddnstr(int y, int x, const wchstring &str, size_t n)
{
  if(move(y, x) == ERR)
    return ERR;
  else
    return addnstr(str, n);
}
