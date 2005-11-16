// keybindings.h, -*-c++-*-
//
//  Copyright 1999-2001, 2004-2005 Daniel Burrows
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
//  A global repository for keybindings.

#include "keybindings.h"

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

// For _()
#include <aptitude.h>

#ifdef HAVE_LIBAPT_PKG
#include <apt-pkg/error.h>
#endif

#include <ctype.h>

#include <algorithm>
#include <map>

using namespace std;
using namespace __gnu_cxx;

keybindings global_bindings;

map<wstring, key> keynames;
map<wstring, key> s_keynames;
// For simplicity, we use the convention that the names are stored in
// lowercase; however, the routines to parse keys take this into account and
// convert the input to lowercase before checking it.
// FIXME: Function keys (F0-Fx) really ought to be handled specially
map<key, wstring> rev_keynames;

bool key_tables_initialized=false;

void init_key_tables()
{
  if(key_tables_initialized)
    return;

  key_tables_initialized=true;

  keynames[L"tab"]=key(L'\t', false);
  rev_keynames[key(L'\t', false)]=L"tab";
  keynames[L"space"]=key(L' ', false);
  rev_keynames[key(L' ', false)]=L"space";

  keynames[L"comma"]=key(L',', false);
  rev_keynames[key(L',', false)]=L"comma";

  keynames[L"break"]=key(KEY_BREAK, true);
  rev_keynames[key(KEY_BREAK, true)]=L"break";
  keynames[L"down"]=key(KEY_DOWN, true);
  rev_keynames[key(KEY_DOWN, true)]=L"down";
  keynames[L"up"]=key(KEY_UP, true);
  rev_keynames[key(KEY_UP, true)]=L"up";
  keynames[L"left"]=key(KEY_LEFT, true);
  rev_keynames[key(KEY_LEFT, true)]=L"left";
  keynames[L"right"]=key(KEY_RIGHT, true);
  rev_keynames[key(KEY_RIGHT, true)]=L"right";
  keynames[L"home"]=key(KEY_HOME, true);
  rev_keynames[key(KEY_HOME, true)]=L"home";
  keynames[L"backspace"]=key(KEY_BACKSPACE, true);
  rev_keynames[key(KEY_BACKSPACE, true)]=L"backspace";
  keynames[L"f0"]=key(KEY_F(0), true);
  rev_keynames[key(KEY_F(0), true)]=L"f0";
  keynames[L"f1"]=key(KEY_F(1), true);
  rev_keynames[key(KEY_F(1), true)]=L"f1";
  keynames[L"f2"]=key(KEY_F(2), true);
  rev_keynames[key(KEY_F(2), true)]=L"f2";
  keynames[L"f3"]=key(KEY_F(3), true);
  rev_keynames[key(KEY_F(3), true)]=L"f3";
  keynames[L"f4"]=key(KEY_F(4), true);
  rev_keynames[key(KEY_F(4), true)]=L"f4";
  keynames[L"f5"]=key(KEY_F(5), true);
  rev_keynames[key(KEY_F(5), true)]=L"f5";
  keynames[L"f6"]=key(KEY_F(6), true);
  rev_keynames[key(KEY_F(6), true)]=L"f6";
  keynames[L"f7"]=key(KEY_F(7), true);
  rev_keynames[key(KEY_F(7), true)]=L"f7";
  keynames[L"f8"]=key(KEY_F(8), true);
  rev_keynames[key(KEY_F(8), true)]=L"f8";
  keynames[L"f9"]=key(KEY_F(9), true);
  rev_keynames[key(KEY_F(9), true)]=L"f9";
  keynames[L"f10"]=key(KEY_F(10), true);
  rev_keynames[key(KEY_F(10), true)]=L"f10";
  keynames[L"delete_line"]=key(KEY_DL, true);
  rev_keynames[key(KEY_DL, true)]=L"delete_line";
  keynames[L"insert_line"]=key(KEY_IL, true);
  rev_keynames[key(KEY_IL, true)]=L"insert_line";
  keynames[L"delete"]=key(KEY_DC, true);
  rev_keynames[key(KEY_DC, true)]=L"delete";
  keynames[L"insert"]=key(KEY_IC, true);
  rev_keynames[key(KEY_IC, true)]=L"insert";
  keynames[L"insert_exit"]=key(KEY_EIC, true);
  rev_keynames[key(KEY_EIC, true)]=L"insert_exit";
  keynames[L"clear"]=key(KEY_CLEAR, true);
  rev_keynames[key(KEY_CLEAR, true)]=L"clear";
  keynames[L"clear_eos"]=key(KEY_EOS, true);
  rev_keynames[key(KEY_EOS, true)]=L"clear_eos";
  keynames[L"clear_eol"]=key(KEY_EOL, true);
  rev_keynames[key(KEY_EOL, true)]=L"clear_eol";
  keynames[L"scrollf"]=key(KEY_SF, true);
  rev_keynames[key(KEY_SF, true)]=L"scrollf";
  keynames[L"scrollr"]=key(KEY_SR, true);
  rev_keynames[key(KEY_SR, true)]=L"scrollr";
  keynames[L"pagedown"]=key(KEY_NPAGE, true);
  rev_keynames[key(KEY_NPAGE, true)]=L"pagedown";
  keynames[L"pageup"]=key(KEY_PPAGE, true);
  rev_keynames[key(KEY_PPAGE, true)]=L"pageup";
  keynames[L"enter"]=key(KEY_ENTER, true);
  rev_keynames[key(KEY_ENTER, true)]=L"enter";
  keynames[L"return"]=key(KEY_ENTER, true);
  keynames[L"print"]=key(KEY_PRINT, true);
  rev_keynames[key(KEY_PRINT, true)]=L"print";
  keynames[L"a1"]=key(KEY_A1, true);
  rev_keynames[key(KEY_A1, true)]=L"a1";
  keynames[L"a3"]=key(KEY_A3, true);
  rev_keynames[key(KEY_A3, true)]=L"a3";
  keynames[L"b2"]=key(KEY_B2, true);
  rev_keynames[key(KEY_B2, true)]=L"b2";
  keynames[L"c1"]=key(KEY_C1, true);
  rev_keynames[key(KEY_C1, true)]=L"c1";
  keynames[L"c3"]=key(KEY_C3, true);
  rev_keynames[key(KEY_C3, true)]=L"c3";
  keynames[L"backtab"]=key(KEY_BTAB, true);
  rev_keynames[key(KEY_BTAB, true)]=L"backtab";
  keynames[L"begin"]=key(KEY_BEG, true);
  rev_keynames[key(KEY_BEG, true)]=L"begin";
  keynames[L"cancel"]=key(KEY_CANCEL, true);
  rev_keynames[key(KEY_CANCEL, true)]=L"cancel";
  keynames[L"close"]=key(KEY_CLOSE, true);
  rev_keynames[key(KEY_CLOSE, true)]=L"close";
  keynames[L"command"]=key(KEY_COMMAND, true);
  rev_keynames[key(KEY_COMMAND, true)]=L"command";
  keynames[L"copy"]=key(KEY_COPY, true);
  rev_keynames[key(KEY_COPY, true)]=L"copy";
  keynames[L"create"]=key(KEY_CREATE, true);
  rev_keynames[key(KEY_CREATE, true)]=L"create";
  keynames[L"end"]=key(KEY_END, true);
  rev_keynames[key(KEY_END, true)]=L"end";
  keynames[L"exit"]=key(KEY_EXIT, true);
  rev_keynames[key(KEY_EXIT, true)]=L"exit";
  keynames[L"find"]=key(KEY_FIND, true);
  rev_keynames[key(KEY_FIND, true)]=L"find";
  keynames[L"help"]=key(KEY_HELP, true);
  rev_keynames[key(KEY_HELP, true)]=L"help";
  keynames[L"mark"]=key(KEY_MARK, true);
  rev_keynames[key(KEY_MARK, true)]=L"mark";
  keynames[L"message"]=key(KEY_MESSAGE, true);
  rev_keynames[key(KEY_MESSAGE, true)]=L"message";
  keynames[L"move"]=key(KEY_MOVE, true);
  rev_keynames[key(KEY_MOVE, true)]=L"move";
  keynames[L"next"]=key(KEY_NEXT, true);
  rev_keynames[key(KEY_NEXT, true)]=L"next";
  keynames[L"open"]=key(KEY_OPEN, true);
  rev_keynames[key(KEY_OPEN, true)]=L"open";
  keynames[L"options"]=key(KEY_OPTIONS, true);
  rev_keynames[key(KEY_OPTIONS, true)]=L"options";
  keynames[L"previous"]=key(KEY_PREVIOUS, true);
  rev_keynames[key(KEY_PREVIOUS, true)]=L"previous";
  keynames[L"redo"]=key(KEY_REDO, true);
  rev_keynames[key(KEY_REDO, true)]=L"redo";
  keynames[L"reference"]=key(KEY_REFERENCE, true);
  rev_keynames[key(KEY_REFERENCE, true)]=L"reference";
  keynames[L"refresh"]=key(KEY_REFRESH, true);
  rev_keynames[key(KEY_REFRESH, true)]=L"refresh";
  keynames[L"replace"]=key(KEY_REPLACE, true);
  rev_keynames[key(KEY_REPLACE, true)]=L"replace";
  keynames[L"restart"]=key(KEY_RESTART, true);
  rev_keynames[key(KEY_RESTART, true)]=L"restart";
  keynames[L"resume"]=key(KEY_RESUME, true);
  rev_keynames[key(KEY_RESUME, true)]=L"resume";
  keynames[L"save"]=key(KEY_SAVE, true);
  rev_keynames[key(KEY_SAVE, true)]=L"save";
  keynames[L"select"]=key(KEY_SELECT, true);
  rev_keynames[key(KEY_SELECT, true)]=L"select";
  keynames[L"suspend"]=key(KEY_SUSPEND, true);
  rev_keynames[key(KEY_SUSPEND, true)]=L"suspend";
  keynames[L"undo"]=key(KEY_UNDO, true);
  rev_keynames[key(KEY_UNDO, true)]=L"undo";

  s_keynames[L"begin"]=key(KEY_SBEG, true);
  rev_keynames[key(KEY_SBEG, true)]=L"begin";
  s_keynames[L"cancel"]=key(KEY_SCANCEL, true);
  rev_keynames[key(KEY_SCANCEL, true)]=L"cancel";
  s_keynames[L"command"]=key(KEY_SCOMMAND, true);
  rev_keynames[key(KEY_SCOMMAND, true)]=L"command";
  s_keynames[L"copy"]=key(KEY_SCOPY, true);
  rev_keynames[key(KEY_SCOPY, true)]=L"copy";
  s_keynames[L"create"]=key(KEY_SCREATE, true);
  rev_keynames[key(KEY_SCREATE, true)]=L"create";
  s_keynames[L"delete"]=key(KEY_SDC, true);
  rev_keynames[key(KEY_SDC, true)]=L"delete";
  s_keynames[L"delete_line"]=key(KEY_SDL, true);
  rev_keynames[key(KEY_SDL, true)]=L"delete_line";
  s_keynames[L"end"]=key(KEY_SEND, true);
  rev_keynames[key(KEY_SEND, true)]=L"end";
  s_keynames[L"clear_eol"]=key(KEY_SEOL, true);
  rev_keynames[key(KEY_SEOL, true)]=L"clear_eol";
  s_keynames[L"exit"]=key(KEY_SEXIT, true);
  rev_keynames[key(KEY_SEXIT, true)]=L"exit";
  s_keynames[L"find"]=key(KEY_SFIND, true);
  rev_keynames[key(KEY_SFIND, true)]=L"find";
  s_keynames[L"help"]=key(KEY_SHELP, true);
  rev_keynames[key(KEY_SHELP, true)]=L"help";
  s_keynames[L"home"]=key(KEY_SHOME, true);
  rev_keynames[key(KEY_SHOME, true)]=L"home";
  s_keynames[L"insert"]=key(KEY_SIC, true);
  rev_keynames[key(KEY_SIC, true)]=L"insert";
  s_keynames[L"left"]=key(KEY_SLEFT, true);
  rev_keynames[key(KEY_SLEFT, true)]=L"left";
  s_keynames[L"message"]=key(KEY_SMESSAGE, true);
  rev_keynames[key(KEY_SMESSAGE, true)]=L"message";
  s_keynames[L"move"]=key(KEY_SMOVE, true);
  rev_keynames[key(KEY_SMOVE, true)]=L"move";
  s_keynames[L"next"]=key(KEY_SNEXT, true);
  rev_keynames[key(KEY_SNEXT, true)]=L"next";
  s_keynames[L"options"]=key(KEY_SOPTIONS, true);
  rev_keynames[key(KEY_SOPTIONS, true)]=L"options";
  s_keynames[L"previous"]=key(KEY_SPREVIOUS, true);
  rev_keynames[key(KEY_SPREVIOUS, true)]=L"previous";
  s_keynames[L"print"]=key(KEY_SPRINT, true);
  rev_keynames[key(KEY_SPRINT, true)]=L"print";
  s_keynames[L"redo"]=key(KEY_SREDO, true);
  rev_keynames[key(KEY_SREDO, true)]=L"redo";
  s_keynames[L"replace"]=key(KEY_SREPLACE, true);
  rev_keynames[key(KEY_SREPLACE, true)]=L"replace";
  s_keynames[L"right"]=key(KEY_SRIGHT, true);
  rev_keynames[key(KEY_SRIGHT, true)]=L"right";
  s_keynames[L"resume"]=key(KEY_SRSUME, true);
  rev_keynames[key(KEY_SRSUME, true)]=L"resume";
  s_keynames[L"save"]=key(KEY_SSAVE, true);
  rev_keynames[key(KEY_SSAVE, true)]=L"save";
  s_keynames[L"suspend"]=key(KEY_SSUSPEND, true);
  rev_keynames[key(KEY_SSUSPEND, true)]=L"suspend";
  s_keynames[L"undo"]=key(KEY_SUNDO, true);
  rev_keynames[key(KEY_SUNDO, true)]=L"undo";
}

// Needed because C++ doesn't understand that &toupper
// can be used with the signature (char -> char).
struct toupper_struct
{
public:
  char operator()(char c) const
  {
    return toupper(c);
  }
};

void keybindings::set(string tag, keybinding strokes)
{
  transform(tag.begin(), tag.end(),
	    tag.begin(), toupper_struct());

  keymap[tag]=strokes;
}

bool keybindings::key_matches(const key &k, string tag)
{
  transform(tag.begin(), tag.end(),
	    tag.begin(), toupper_struct());

  hash_map<string, keybinding>::iterator found=keymap.find(tag);
  if(found==keymap.end())
    return parent?parent->key_matches(k, tag):false;
  else
    {
      for(keybinding::iterator i=found->second.begin(); i!=found->second.end(); i++)
	{
	  if(*i==key(KEY_ENTER, true))
	    {
	      if(k==key(KEY_ENTER, true) ||
		 k==key(L'\r', false) || k==key(L'\n', false))
		return true;
	    }
	  else if(k==*i)
	    return true;
	}
      return false;
    }
}

key parse_key(wstring keystr)
{
  bool sfound=false,cfound=false,afound=false;
  wstring tmpstr=keystr;
  key rval((wint_t) ERR, true);

  init_key_tables();

  while(tmpstr.size()>2 && tmpstr[1]==L'-')
    {
      switch(tmpstr[0])
	{
	case L's':
	case L'S':
	  sfound=true;
	  break;
	case L'a':
	case L'A':
	case L'm':
	case L'M':
	  afound=true;
	  break;
	case L'c':
	case L'C':
	  cfound=true;
	  break;
	default:
#ifdef HAVE_LIBAPT_PKG
	  _error->Error(_("Cannot parse key description: %ls"), keystr.c_str());
#endif
	  return key((wint_t) ERR, true);
	}
      tmpstr=wstring(tmpstr,2);
    }

  if(tmpstr.size()==0)
    {
#ifdef HAVE_LIBAPT_PKG
      _error->Error(_("Invalid null keybinding"));
#endif
      return key((wint_t) ERR, true);
    }

  if(cfound && tmpstr.size()>1)
    {
#ifdef HAVE_LIBAPT_PKG
      _error->Error(_("Sorry, control modifiers may not be used with unprintable characters"));
#endif
      return key((wint_t) ERR, true);
    }

  if(tmpstr.size()==1)
    {
      wint_t ch=sfound?towupper(tmpstr[0]):tmpstr[0];
      // How do control-keys interact with wide characters?
      if(cfound)
	return KEY_CTRL(ch);
      if(afound)
	return KEY_ALT(ch);

      return key(ch, false);
    }
  else
    {
      for(unsigned int i=0; i<tmpstr.size(); i++)
	tmpstr[i]=towlower(tmpstr[i]);
      map<wstring, key>::iterator found=(sfound?s_keynames:keynames).find(tmpstr);
      if(found==(sfound?s_keynames:keynames).end())
	return key((wint_t) ERR, true);
      else
	{
	  rval=found->second;
	  if(afound)
	    rval=KEY_ALT(rval.ch);

	  return rval;
	}
    }
}

wstring keyname(const key &k)
{
  init_key_tables();

  // This is a nasty special-case..all of this stuff is nasty special-cases..
  // someday I need to learn the underlying logic, if there is any..
  if(k.ch==31 && k.function_key)
    return L"C-_";

  // Control characters are characters whose 64-place is 0.  (what
  // about others? hm)
  if(k.ch < 32 && !k.function_key)
    return L"C-"+keyname(key(k.ch|64, false));
  // and 200 seems to be the ALT-character?
  else if(k.ch&0x200)
    return L"A-"+keyname(key(k.ch&~0x200, false));
  else
    {
      map<key, wstring>::iterator found=rev_keynames.find(k);

      if(found!=rev_keynames.end())
	return found->second;
      else
	{
	  wchar_t tmp[2];
	  tmp[0]=k.ch;
	  tmp[1]=0;
	  return wstring(tmp);
	}
    }
}

wstring readable_keyname(const key &k)
{
  if(k == key(L',', false))
    return L",";
  else
    return keyname(k);
}

// Doesn't return all available bindings as that gets way too long.
wstring keybindings::keyname(const string &tag)
{
  string realtag(tag);
  transform(realtag.begin(), realtag.end(),
	    realtag.begin(), toupper_struct());

  hash_map<string, keybinding>::iterator found=keymap.find(realtag);

  if(found!=keymap.end())
    return ::keyname(found->second.front());
  else
    return L"";
}

wstring keybindings::readable_keyname(const string &tag)
{
  string realtag(tag);
  transform(realtag.begin(), realtag.end(),
	    realtag.begin(), toupper_struct());

  hash_map<string, keybinding>::iterator found=keymap.find(realtag);

  if(found != keymap.end())
    return ::readable_keyname(found->second.front());
  else
    return L"";
}
