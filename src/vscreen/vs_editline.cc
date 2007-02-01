// vs_editline.cc
//
//  Copyright 2000 Daniel Burrows

#include "vs_editline.h"

#include "config/colors.h"
#include "config/keybindings.h"
#include "transcode.h"
#include "vscreen.h"

#include <sigc++/functors/mem_fun.h>

using namespace std;

keybindings *vs_editline::bindings=NULL;

vs_editline::vs_editline(const string &_prompt, const string &_text,
			 history_list *_history)
  :vscreen_widget(), curloc(_text.size()),
   startloc(0), desired_size(-1), history(_history),
   history_loc(0), using_history(false), clear_on_first_edit(false)
{
  // Just spew a partial/null string if errors happen for now.
  transcode(_prompt.c_str(), prompt);
  transcode(_text.c_str(), text);

  set_bg_style(get_style("EditLine"));

  // This ensures that the cursor is set to the right location when the
  // widget is displayed or resized:
  do_layout.connect(sigc::mem_fun(*this, &vs_editline::normalize_cursor));
}

vs_editline::vs_editline(const wstring &_prompt, const wstring &_text,
			 history_list *_history)
  :vscreen_widget(), prompt(_prompt), text(_text), curloc(_text.size()),
   startloc(0), desired_size(-1), history(_history),
   history_loc(0), using_history(false), clear_on_first_edit(false)
{
  set_bg_style(get_style("EditLine"));

  // This ensures that the cursor is set to the right location when the
  // widget is displayed or resized:
  do_layout.connect(sigc::mem_fun(*this, &vs_editline::normalize_cursor));
}

vs_editline::vs_editline(int maxlength, const string &_prompt,
			 const string &_text, history_list *_history)
  :vscreen_widget(), curloc(0),
   startloc(0), desired_size(maxlength), history(_history), history_loc(0),
   using_history(false), clear_on_first_edit(false)
{
  // As above, ignore errors.
  transcode(_prompt, prompt);
  transcode(_text, text);

  set_bg_style(get_style("EditLine"));
  do_layout.connect(sigc::mem_fun(*this, &vs_editline::normalize_cursor));
}

vs_editline::vs_editline(int maxlength, const wstring &_prompt,
			 const wstring &_text, history_list *_history)
  :vscreen_widget(), prompt(_prompt), text(_text), curloc(0),
   startloc(0), desired_size(maxlength), history(_history), history_loc(0),
   using_history(false), clear_on_first_edit(false)
{
  set_bg_style(get_style("EditLine"));
  do_layout.connect(sigc::mem_fun(*this, &vs_editline::normalize_cursor));
}

wchar_t vs_editline::get_char(size_t loc)
{
  vs_widget_ref tmpref(this);

  if(loc>=prompt.size())
    return text[loc-prompt.size()];
  else
    return prompt[loc];
}

void vs_editline::normalize_cursor()
{
  vs_widget_ref tmpref(this);

  if(get_width() <= 0)
    return;

  int w=get_width();

  int promptwidth=wcswidth(prompt.c_str(), prompt.size());
  int textwidth=wcswidth(text.c_str(), text.size());

  int cursorx=0;
  if(curloc+prompt.size()>startloc)
    for(size_t i=startloc; i<curloc+prompt.size(); ++i)
      cursorx+=wcwidth(get_char(i));
  else
    for(size_t i=curloc+prompt.size(); i<startloc; ++i)
      cursorx-=wcwidth(get_char(i));

  if(promptwidth+textwidth+1<w)
    startloc=0;
  else if(w>2)
    {
      // Need to move the screen start to this far behind the cursor
      // loc.
      int decamt=0;
      bool needs_move=false;

      if(cursorx>=w-2)
	{
	  decamt=w-2;
	  needs_move=true;
	}
      else if(cursorx<2)
	{
	  decamt=2;
	  needs_move=true;
	}

      if(needs_move) // equivalent to but more readable than decamt!=0
	{
	  // Do it by moving back this many chars
	  size_t chars=0;

	  while(decamt>0 && chars<curloc+prompt.size())
	    {
	      ++chars;
	      decamt-=wcwidth(get_char(prompt.size()+curloc-chars));
	    }

	  if(decamt<0 && chars>1)
	    --chars;

	  startloc=curloc+prompt.size()-chars;
	}
    }
  else
    {
      // if width=1, use a primitive approach (we're screwed anyway in
      // this case)
      if(cursorx>=w)
	startloc=prompt.size()+curloc-w+1;

      if(cursorx<0)
	startloc=prompt.size()+curloc;
    }

  vscreen_updatecursor();
}

bool vs_editline::get_cursorvisible()
{
  return true;
}

point vs_editline::get_cursorloc()
{
  vs_widget_ref tmpref(this);

  if(getmaxx()>0)
    {
      int x=0;

      for(size_t loc=startloc; loc<curloc+prompt.size(); ++loc)
	x+=wcwidth(get_char(loc));

      return point(x, 0);
    }
  else
    return point(0,0);
}

bool vs_editline::focus_me()
{
  return true;
}

bool vs_editline::handle_key(const key &k)
{
  vs_widget_ref tmpref(this);

  bool clear_on_this_edit = clear_on_first_edit;
  clear_on_first_edit = false;

  if(bindings->key_matches(k, "DelBack"))
    {
      if(curloc>0)
	{
	  text.erase(--curloc, 1);
	  normalize_cursor();
	  text_changed(wstring(text));
	  vscreen_update();
	}
      else
	{
	  beep();
	  //	  refresh();
	}
      return true;
    }
  else if(bindings->key_matches(k, "DelForward"))
    {
      if(curloc<text.size())
	{
	  text.erase(curloc, 1);
	  normalize_cursor();
	  text_changed(wstring(text));
	  vscreen_update();
	}
      else
	{
	  beep();
	  //	  refresh();
	}
      return true;
    }
  else if(bindings->key_matches(k, "Confirm"))
    {
      // I create a new string here because otherwise modifications to
      // "text" are seen by the widgets! (grr, sigc++)
      entered(wstring(text));
      return true;
    }
  else if(bindings->key_matches(k, "Left"))
    {
      if(curloc>0)
	{
	  curloc--;
	  normalize_cursor();
	  vscreen_update();
	}
      else
	{
	  beep();
	  //	  refresh();
	}
      return true;
    }
  else if(bindings->key_matches(k, "Right"))
    {
      if(curloc<text.size())
	{
	  curloc++;
	  normalize_cursor();
	  vscreen_update();
	}
      else
	{
	  beep();
	  //	  refresh();
	}
      return true;
    }
  else if(bindings->key_matches(k, "Begin"))
    {
      curloc=0;
      startloc=0;
      normalize_cursor();
      vscreen_update();
      return true;
    }
  else if(bindings->key_matches(k, "End"))
    {
      curloc=text.size();
      normalize_cursor();
      vscreen_update();
      return true;
    }
  else if(bindings->key_matches(k, "DelEOL"))
    {
      text.erase(curloc);
      normalize_cursor();
      text_changed(wstring(text));
      vscreen_update();
      return true;
    }
  else if(bindings->key_matches(k, "DelBOL"))
    {
      text.erase(0, curloc);
      curloc=0;
      normalize_cursor();
      text_changed(wstring(text));
      vscreen_update();
      return true;
    }
  else if(history && bindings->key_matches(k, "HistoryPrev"))
    {
      if(history->size()==0)
	return true;

      if(!using_history)
	{
	  using_history=true;
	  history_loc=history->size()-1;
	  pre_history_text=text;
	}
      else if(history_loc>0)
	--history_loc;
      else
	// Break out
	return true;

      text=(*history)[history_loc];
      curloc=text.size();
      startloc=0;
      normalize_cursor();
      text_changed(wstring(text));
      vscreen_update();

      return true;
    }
  else if(history && bindings->key_matches(k, "HistoryNext"))
    {
      if(history->size()==0 || !using_history)
	return true;

      if(history_loc>=history->size()-1)
	{
	  using_history=false;
	  history_loc=0;
	  text=pre_history_text;
	  pre_history_text=L"";
	  curloc=text.size();
	  startloc=0;
	  normalize_cursor();
	  text_changed(wstring(text));
	  vscreen_update();

	  // FIXME: store the pre-history edit and restore that.
	  return true;
	}

      if(history_loc>=0)
	{
	  ++history_loc;
	  text=(*history)[history_loc];
	  curloc=text.size();
	  startloc=0;
	  normalize_cursor();
	  text_changed(wstring(text));
	  vscreen_update();

	  return true;
	}
      else
	return true;
    }
  else if(k.function_key)
    return vscreen_widget::handle_key(k);
  else if(k.ch=='\t') // HACK
    return false;
  else
    {
      if(clear_on_this_edit)
	text.clear();

      text.insert(curloc++, 1, k.ch);
      normalize_cursor();
      text_changed(wstring(text));
      vscreen_update();
      return true;
    }
}

void vs_editline::paint(const style &st)
{
  vs_widget_ref tmpref(this);

  int width=getmaxx();

  int used=0;
  size_t chars=0;

  while(used<width && startloc+chars<prompt.size()+text.size())
    {
      wchar_t ch=get_char(startloc+chars);
      used+=wcwidth(ch);
      ++chars;
    }

  if(used>width && chars>1)
    --chars;

  wstring todisp=prompt+text;
  mvaddstr(0, 0, wstring(todisp, startloc, chars));
}

void vs_editline::dispatch_mouse(short id, int x, int y, int z, mmask_t bstate)
{
  vs_widget_ref tmpref(this);

  size_t mouseloc=startloc; // The character at which the mouse press occured

  clear_on_first_edit = false;

  while(mouseloc<prompt.size()+text.size() && x>0)
    {
      int curwidth=wcwidth(get_char(mouseloc));

      if(curwidth>x)
	break;
      else
	{
	  ++mouseloc;
	  x-=curwidth;
	}
    }

  if(mouseloc>=prompt.size())
    {
      mouseloc-=prompt.size();

      if(mouseloc<=text.size())
	curloc=mouseloc;
      else
	curloc=text.size();
    }
  else
    return; // Break out

  vscreen_update();
}

void vs_editline::add_to_history(std::wstring s,
				 history_list *lst)
{
  eassert(lst);

  if(lst->empty() || lst->back()!=s)
    lst->push_back(s);
}

void vs_editline::add_to_history(std::wstring s)
{
  vs_widget_ref tmpref(this);

  if(history)
    add_to_history(s, history);
}

void vs_editline::reset_history()
{
  vs_widget_ref tmpref(this);

  pre_history_text=L"";
  using_history=false;
  history_loc=0;
}

void vs_editline::set_text(wstring _text)
{
  vs_widget_ref tmpref(this);

  text=_text;
  if(curloc>text.size())
    curloc=text.size();
  text_changed(wstring(text));
  vscreen_update();
}

void vs_editline::set_text(string _text)
{
  vs_widget_ref tmpref(this);

  wstring wtext;
  if(transcode(_text, wtext))
    set_text(wtext);
}

void vs_editline::init_bindings()
{
  bindings=new keybindings(&global_bindings);

  bindings->set("Left", key(KEY_LEFT, true));
  bindings->set("Right", key(KEY_RIGHT, true));
  // Override these for the case where left and right have multiple bindings
  // in the global keymap
}

int vs_editline::width_request()
{
  vs_widget_ref tmpref(this);

  if(desired_size == -1)
    return wcswidth(prompt.c_str(), prompt.size())+wcswidth(text.c_str(), text.size());
  else
    return desired_size;
}

int vs_editline::height_request(int width)
{
  return 1;
}
