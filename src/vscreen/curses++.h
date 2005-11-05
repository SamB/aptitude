// curses++.h  (this is -*-c++-*-)
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
//  Simple class wrappers around various CURSES functions.

#ifndef CURSES_PLUSPLUS_H
#define CURSES_PLUSPLUS_H

#include <string>
#include <ncursesw/curses.h>

#include <generic/util/eassert.h>

// For isspace
#include <ctype.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

/** A structure that amalgamates a wchar_t together with attributes.
 *  This is similar to cchar_t, but has the advantage of having
 *  well-defined properties and behavior; the routines to manipulate
 *  cchar_t's are vaguely documented and a lot of their behavior has
 *  to be guessed at or inferred from source code.  I don't trust
 *  interfaces where I have to guess at their behavior, and I think
 *  that I won't lose too much efficiency by doing things this way.
 */
struct wchtype
{
  /** The character value associated with this string.  This code
   *  presently assumes that wchar_t is large enough to hold any
   *  single Unicode (UCS-4) character, which is ok for GNU but might
   *  not port everywhere.
   */
  wchar_t ch;

  /** The text attributes (including color) associated with this
   *  character.
   */
  attr_t attrs;

  wchtype()
  {
  }

  wchtype(const wchar_t &_ch, const attr_t &_attrs)
    :ch(_ch), attrs(_attrs)
  {
  }

  bool operator==(const wchtype &other) const
  {
    return ch==other.ch && attrs==other.attrs;
  }

  bool operator!=(const wchtype &other) const
  {
    return ch!=other.ch || attrs!=other.attrs;
  }

  bool operator<(const wchtype &other) const
  {
    return ch<other.ch || ch==other.ch && attrs<other.attrs;
  }

  bool operator<=(const wchtype &other) const
  {
    return (*this) == other || (*this) < other;
  }

  bool operator>(const wchtype &other) const
  {
    return !((*this)<=other);
  }

  bool operator>=(const wchtype &other) const
  {
    return !((*this)<other);
  }
};

/** Based on libstdc++-3's instantiation of this for characters.
 *
 *  This could do something clever, such as changing comparisons to
 *  only apply to the text itself, but it doesn't; I'm not sure what
 *  the effect of changes like that would be.
 *
 *  The int_type stuff is not implemented; it could perhaps be
 *  implemented later if it's needed.
 */
namespace std {
template <>
struct TRAITS_CLASS<chtype> {
  typedef chtype char_type;

  static void assign (char_type& c1, const char_type& c2)
    { c1 = c2; }
  static bool eq (const char_type & c1, const char_type& c2)
    { return (c1 == c2); }
  static bool ne (const char_type& c1, const char_type& c2)
    { return (c1 != c2); }
  static bool lt (const char_type& c1, const char_type& c2)
    { return (c1 < c2); }
  static char_type eos () { return 0; }
  static bool is_del(char_type a) { return isspace(a|A_CHARTEXT); }

  static int compare (const char_type* s1, const char_type* s2, size_t n);
  static size_t length (const char_type* s);
  static char_type* copy (char_type* s1, const char_type* s2, size_t n)
    { return (char_type*) memcpy (s1, s2, n*sizeof(char_type)); }
  static char_type* move (char_type* s1, const char_type* s2, size_t n)
    { return (char_type*) memmove (s1, s2, n*sizeof(char_type)); }
  static char_type* assign (char_type* s1, size_t n, const char_type& c);
};

  template <>
  struct TRAITS_CLASS<wchtype> {
    typedef wchtype char_type;

    static void assign (char_type& c1, const char_type& c2)
    { c1 = c2; }
    static bool eq (const char_type & c1, const char_type& c2)
    { return (c1 == c2); }
    static bool ne (const char_type& c1, const char_type& c2)
    { return (c1 != c2); }
    static bool lt (const char_type& c1, const char_type& c2)
    { return (c1 < c2); }
    static char_type eos () { return wchtype(0,0); }
    static bool is_del(char_type a) { return isspace(a.ch); }

    static int compare (const char_type* s1, const char_type* s2, size_t n);
    static size_t length (const char_type* s);
    static char_type* copy (char_type* s1, const char_type* s2, size_t n)
    { return (char_type*) memcpy (s1, s2, n*sizeof(char_type)); }
    static char_type* move (char_type* s1, const char_type* s2, size_t n)
    { return (char_type*) memmove (s1, s2, n*sizeof(char_type)); }
    static char_type* assign (char_type* s1, size_t n, const char_type& c);
  };
}

class style;

/** A string class which stores attributes along with characters.
 *
 *  This has to derive from basic_string in order to implement operator=
 *  (which can't be a global function -- it has to be a nonstatic member)
 */
class chstring:public std::basic_string<chtype>
{
  typedef std::basic_string<chtype> super;
public:
  chstring(const std::basic_string<chtype> &s)
    :std::basic_string<chtype>(s) {}

  chstring(const std::string &s);
  chstring(const std::string &s, const style &st);

  chstring(const chstring &s):super(s) {}
  /** Apply the given style to the given chstring, and set ourselves
   *  to the result.
   */
  chstring(const chstring &s, const style &st);

  chstring(const chstring &s, size_t loc, size_t n=npos)
    :super(s, loc, n) {}

  chstring(size_t n, chtype c)
    :super(n, c) {}

  /** Assign the characters of s to this, setting all attributes to A_NORMAL. */
  chstring &operator=(const std::string &s);

  /** Change the attributes of this string by using the given style. */
  void apply_style(const style &st);
};

class wchstring:public std::basic_string<wchtype>
{
  typedef std::basic_string<wchtype> super;
public:
  wchstring(const std::basic_string<wchtype> &s)
    :std::basic_string<wchtype>(s) {}

  /** Create a new wchstring with empty attribute information. */
  wchstring(const std::wstring &s);

  /** Create a new wchstring from the given wide string with the given
   *  attributes.
   */
  wchstring(const std::wstring &s, const style &st);

  wchstring(const wchstring &s):super(s) {}
  /** Apply the given style to the given chstring, and set ourselves
   *  to the result.
   */
  wchstring(const wchstring &s, const style &st);

  wchstring(const wchstring &s, size_t loc, size_t n=npos)
    :super(s, loc, n) {}

  wchstring(size_t n, wchtype c)
    :super(n, c) {}

  wchstring(size_t n, wchar_t c, attr_t a)
    :super(n, wchtype(c, a)) {}

  /** Change the attributes of this string by using the given style. */
  void apply_style(const style &st);

  /** Return the number of columns occupied by this string. */
  int width() const;
};

inline chtype _getbkgd(WINDOW *win)
{
  return getbkgd(win);
}

inline int _box(WINDOW *win, chtype verch, chtype horch)
{
  return box(win, verch, horch);
}

inline void _getyx(WINDOW *win, int &y, int &x)
{
  getyx(win, y, x);
}

inline void _getparyx(WINDOW *win, int &y, int &x)
{
  getparyx(win, y, x);
}

inline void _getbegyx(WINDOW *win, int &y, int &x)
{
  getbegyx(win, y, x);
}

inline void _getmaxyx(WINDOW *win, int &y, int &x)
{
  getmaxyx(win, y, x);
}

inline int _getmaxy(WINDOW *win)
{
  return getmaxy(win);
}

inline int _getmaxx(WINDOW *win)
{
  return getmaxx(win);
}

inline int _touchwin(WINDOW *win)
{
  return touchwin(win);
}

inline int _untouchwin(WINDOW *win)
{
  return untouchwin(win);
}

#undef getbkgd
#undef box

#undef getyx
#undef getparyx
#undef getbegyx
#undef getmaxyx
#undef getmaxy
#undef getmaxx

#undef addch
#undef addchnstr
#undef addchstr
#undef add_wch
#undef addnstr
#undef addstr
#undef attroff
#undef attron
#undef attrset
#undef attr_set
#undef bkgd
#undef bkgdset
#undef clear
#undef clrtobot
#undef clrtoeol
#undef delch
#undef deleteln
#undef echochar
#undef erase
#undef getch
#undef get_wch
#undef getstr
#undef inch
#undef inchnstr
#undef innstr
#undef insch
#undef insdelln
#undef insertln
#undef insnstr
#undef insstr
#undef instr
#undef move
#undef refresh
#undef scrl
#undef scroll
#undef setscrreg
#undef standend
#undef standout
#undef timeout

#undef mvaddch
#undef mvadd_wch
#undef mvaddchnstr
#undef mvaddchstr
#undef mvaddnstr
#undef mvaddstr
#undef mvdelch
#undef mvgetch
#undef mvget_wch
#undef mvgetnstr
#undef mvgetstr
#undef mvhline
#undef mvinch
#undef mvinchnstr
#undef mvinchstr
#undef mvinnstr
#undef mvinsch
#undef mvinsnstr
#undef mvinsstr
#undef mvinstr
#undef mvvline

#undef border
#undef hline
#undef vline

#undef touchline

//  The following class encapsulates a CURSES window, mostly with inlined
// versions of w* and mvw*.  subwin and newwin are encapsulated with
// constructors; casting to WINDOW * is also supported.
//
//  er, these will be inlined.  Right?
class cwindow
{
  // Blech.  Used to memory-manage WINDOW *s
  class cwindow_master
  {
    WINDOW *win;
    int refs;
    cwindow_master *parent;

    friend class cwindow;

    ~cwindow_master()
    {
      eassert(refs==0);

      if(win)
	delwin(win);
      if(parent)
	parent->deref();
    }
  public:
    cwindow_master(WINDOW *_win, cwindow_master *_parent)
      :win(_win), refs(0), parent(_parent)
    {
      if(parent)
	parent->ref();
    }

    inline void ref()
    {
      refs++;
    }

    // ??????
    inline void deref()
    {
      refs--;

      if(refs==0)
	delete this;
    }
  };

  WINDOW *win;
  // The actual curses window

  cwindow_master *master;
  // Keeps track of where we got this from (so we can deref() it later)

  cwindow(WINDOW *_win, cwindow_master *_master)
    :win(_win), master(_master)
  {
    master->ref();
  }
public:
  cwindow(WINDOW *_win):win(_win), master(new cwindow_master(_win, NULL))
  {
    master->ref();
  }
  cwindow(const cwindow &a):win(a.win), master(a.master)
  {
    master->ref();
  }

  ~cwindow()
  {
    master->deref();
  }

  cwindow derwin(int h, int w, int y, int x)
  {
    WINDOW *new_win=::derwin(win, h, w, y, x);
    return cwindow(new_win, new cwindow_master(new_win, master));
  }

  int mvwin(int y, int x) {return ::mvwin(win, y, x);}

  void syncup() {wsyncup(win);}
  int syncok(bool bf) {return ::syncok(win, bf);}
  void cursyncup() {wcursyncup(win);}
  void syncdown() {wsyncdown(win);}

  int scroll(int n=1) {return wscrl(win, n);}
  // Does both scroll() and wscsrl()

  int addch(chtype ch) {return waddch(win, ch);}
  int mvaddch(int y, int x, chtype ch) {return mvwaddch(win, y, x, ch);}

  int add_wch(wchar_t wch)
  {
    wchar_t tmp[2];
    tmp[0]=wch;
    tmp[1]=0;

    cchar_t cch;
    if(setcchar(&cch, tmp, 0, 0, 0)==ERR)
      return ERR;
    else
      return wadd_wch(win, &cch);
  }

  int mvadd_wch(int y, int x, wchar_t wch)
  {
    move(y, x);
    return add_wch(wch);
  }

  int add_wch(const cchar_t *cch)
  {
    return wadd_wch(win, cch);
  }

  int mvadd_wch(int y, int x, const cchar_t *cch)
  {
    return mvwadd_wch(win, y, x, cch);
  }

  int addstr(const std::wstring &str) {return addstr(str.c_str());}
  int addnstr(const std::wstring &str, int n) {return addnstr(str.c_str(), n);}
  int mvaddstr(int y, int x, const std::wstring &str) {return mvaddstr(y, x, str.c_str());}
  int mvaddnstr(int y, int x, const std::wstring &str, int n) {return mvaddnstr(y, x, str.c_str(), n);}

  int addstr(const wchar_t *str) {return waddwstr(win, str);}
  int addnstr(const wchar_t *str, int n) {return waddnwstr(win, str, n);}
  int mvaddstr(int y, int x, const wchar_t *str) {return mvwaddwstr(win, y, x, str);}
  int mvaddnstr(int y, int x, const wchar_t *str, int n) {return mvwaddnwstr(win, y, x, str, n);}

  int addstr(const char *str) {return waddstr(win, str);}
  int addnstr(const char *str, int n) {return waddnstr(win, str, n);}
  int mvaddstr(int y, int x, const char *str) {return mvwaddstr(win, y, x, str);}
  int mvaddnstr(int y, int x, const char *str, int n) {return mvwaddnstr(win, y, x, str, n);}

  // The following are implemented hackily due to the weirdness of
  // curses.  NB: they don't work with characters of negative width.
  int addstr(const wchstring &str);
  int addnstr(const wchstring &str, size_t n);
  int mvaddstr(int y, int x, const wchstring &str);
  int mvaddnstr(int y, int x, const wchstring &str, size_t n);

  int addstr(const chstring &str) {return waddchstr(win, str.c_str());}
  int addnstr(const chstring &str, int n) {return waddchnstr(win, str.c_str(), n);}
  int mvaddstr(int y, int x, const chstring &str) {return mvwaddchstr(win, y, x, str.c_str());}
  int mvaddnstr(int y, int x, const chstring &str, int n) {return mvwaddchnstr(win, y, x, str.c_str(), n);}

  int attroff(int attrs) {return wattroff(win, attrs);}
  int attron(int attrs) {return wattron(win, attrs);}
  int attrset(int attrs) {return wattrset(win, attrs);}
  //  int attr_set(int attrs, void *opts) {return wattr_set(win, attrs, opts);}

  void bkgdset(const chtype ch) {wbkgdset(win, ch);}
  int bkgd(const chtype ch) {return wbkgd(win, ch);}
  chtype getbkgd() {return _getbkgd(win);}

  int border(chtype ls, chtype rs, chtype ts, chtype bs, chtype tl, chtype tr, chtype bl, chtype br)
  {return wborder(win, ls, rs, ts, bs, tl, tr, bl, br);}

  int box(chtype verch, chtype horch) {return _box(win, verch, horch);}
  int hline(chtype ch, int n) {return whline(win, ch, n);}
  int vline(chtype ch, int n) {return wvline(win, ch, n);}
  int mvhline(int y, int x, chtype ch, int n) {return mvwhline(win, y, x, ch, n);}
  int mvvline(int y, int x, chtype ch, int n) {return mvwvline(win, y, x, ch, n);}

  int delch() {return wdelch(win);}
  int mvdelch(int y, int x) {return mvwdelch(win, y, x);}

  int deleteln() {return wdeleteln(win);}
  int insdelln(int n) {return winsdelln(win,n);}
  int insertln() {return winsertln(win);}

  int echochar(chtype ch) {return wechochar(win, ch);}

  int getch() {return wgetch(win);}
  int mvgetch(int y, int x) {return mvwgetch(win, y, x);}

  int get_wch(wint_t *wch) {return wget_wch(win, wch);}
  int mvget_wch(int y, int x, wint_t *wch) {return mvwget_wch(win, y, x, wch);}

  int move(int y, int x) {return wmove(win, y, x);}
  void getyx(int &y, int &x) {_getyx(win, y, x);}
  void getparyx(int &y, int &x) {_getparyx(win, y, x);}
  void getbegyx(int &y, int &x) {_getbegyx(win, y, x);}
  void getmaxyx(int &y, int &x) {_getmaxyx(win, y, x);}
  int getmaxy() {return _getmaxy(win);}
  int getmaxx() {return _getmaxx(win);}

  void show_string_as_progbar(int x, int y, const std::wstring &s,
			      int attr1, int attr2, int size1,
			      int totalsize);
  // Glitz bit :) Displays the given string with a progress bar behind it.

  void display_header(std::wstring s, const attr_t attr);
  void display_status(std::wstring s, const attr_t attr);
  // Make it easier to write interfaces that have a header and status line..
  // they do what they say :)

  int overlay(cwindow &dstwin) {return ::overlay(win, dstwin.win);}
  int overwrite(cwindow &dstwin) {return ::overwrite(win, dstwin.win);}
  int copywin(cwindow &dstwin, int sminrow, int smincol, int dminrow, int dmincol, int dmaxrow, int dmaxcol, int overlay)
  {return ::copywin(win, dstwin.win, sminrow, smincol, dminrow, dmincol, dmaxrow, dmaxcol, overlay);}

  int refresh() {return wrefresh(win);}
  int noutrefresh() {return wnoutrefresh(win);}

  int touch() {return _touchwin(win);}
  int untouch() {return _untouchwin(win);}
  int touchln(int y, int n, int changed) {return ::wtouchln(win, y, n, changed);}
  int touchline(int start, int count) {return touchln(start, count, 1);}
  int untouchline(int start, int count) {return touchln(start, count, 0);}

  int erase() {return werase(win);}
  int clear() {return wclear(win);}
  int clrtobot() {return wclrtobot(win);}
  int clrtoeol() {return wclrtoeol(win);}

  int keypad(bool bf) {return ::keypad(win,bf);}
  int meta(bool bf) {return ::meta(win,bf);}
  int nodelay(bool bf) {return ::nodelay(win, bf);}
  int notimeout(bool bf) {return ::notimeout(win, bf);}
  void timeout(int delay) {wtimeout(win, delay);}

  int clearok(bool bf) {return ::clearok(win, bf);}
  int idlok(bool bf) {return ::idlok(win, bf);}
  void idcok(bool bf) {::idcok(win, bf);}
  void immedok(bool bf) {::immedok(win, bf);}
#if defined(NCURSES_VERSION_MAJOR) && NCURSES_VERSION_MAJOR>=5
  int leaveok(bool bf) {int rval=::leaveok(win, bf); curs_set(bf?0:1); return rval;}
#else
  int leaveok(bool bf) {return ::leaveok(win, bf);}
#endif
  int setscrreg(int top, int bot) {return wsetscrreg(win, top, bot);}
  int scrollok(bool bf) {return ::scrollok(win,bf);}

  int printw(char *str, ...);
  /* You guessed it.. :) */

  bool enclose(int y, int x) {return wenclose(win, y, x);}

  WINDOW *getwin() {return win;}
  operator bool () {return win!=NULL;}
  cwindow &operator =(const cwindow &a)
  {
    cwindow_master *newmaster=a.master;
    newmaster->ref();

    master->deref();
    master=newmaster;
    win=a.win;
    return *this;
  }
  bool operator ==(cwindow &other) {return win==other.win;}
  bool operator !=(cwindow &other) {return win!=other.win;}

  static void remove_cruft();
};

extern cwindow rootwin;
// This is stdscr, but calling it 'stdscr' would confuse the compiler, so I'm
// confusing the programmer instead :)

void init_curses();
// Initializes curses and sets rootwin to the correct value

void resize();
// Called when a terminal resize is detected.
#endif
