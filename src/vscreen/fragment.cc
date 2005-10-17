// fragment.cc
//
//
//   Copyright (C) 2004-2005 Daniel Burrows
//
//   This program is free software; you can redistribute it and/or
//   modify it under the terms of the GNU General Public License as
//   published by the Free Software Foundation; either version 2 of
//   the License, or (at your option) any later version.
//
//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//   General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with this program; see the file COPYING.  If not, write to
//   the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//   Boston, MA 02111-1307, USA.

#include "fragment.h"
#include "transcode.h"

#include "config/colors.h"

#include <stdarg.h>

#include <algorithm>

#include <wctype.h>

using namespace std;

fragment::~fragment()
{
}

/** A fragment of text, possibly with attached attributes.  It's
 *  assumed that the text contains no literal newlines.
 */
class _text_fragment:public fragment
{
public:
  _text_fragment(const wstring &_s):s(_s) {}

  fragment_contents layout(size_t firstw, size_t restw,
			   const style &st)
  {
    fragment_contents rval;
    rval.push_back(fragment_line(s, st));
    return rval;
  }

  size_t max_width(size_t first_indent,
		   size_t rest_indent) const
  {
    return first_indent+wcswidth(s.c_str(), s.size());
  }

  size_t trailing_width(size_t first_indent,
			size_t rest_indent) const
  {
    return first_indent+wcswidth(s.c_str(), s.size());
  }

  bool final_newline() const
  {
    return false;
  }
private:
  wstring s;
};

fragment *text_fragment(const wstring &s)
{
  std::vector<fragment *> fragments;

  wstring tmp;

  for(size_t i = 0; i < s.size(); ++i)
    {
      if(s[i] == L'\t')
	tmp.append(8, L' ');
      else if(s[i] == L'\n' || !iswprint(s[i]))
	{
	  fragments.push_back(new _text_fragment(tmp));

	  if(s[i] == L'\n')
	    fragments.push_back(newline_fragment());
	  else
	    fragments.push_back(style_fragment(new _text_fragment(L"?"),
					       get_style("ERROR")));

	  tmp.clear();
	}
      else
	tmp += s[i];
    }

  if(fragments.size() == 0)
    return new _text_fragment(tmp);
  else
    {
      if(!tmp.empty())
	fragments.push_back(new _text_fragment(tmp));

      return sequence_fragment(fragments);
    }
}



fragment *text_fragment(const wstring &s,
			const style &st)
{
  return style_fragment(text_fragment(s),
			st);
}

fragment *text_fragment(const string &s,
			const char *encoding)
{
  wstring decoded;

  // The error code is not translated, because that would require an
  // additional decoding step and could result in infinite loops and
  // other bad stuff.  Besides, something is very wrong if it appears;
  // better to just output a diagnostic message that can be cut+paste.
  if(!transcode(s.c_str(), decoded, encoding))
    return sequence_fragment(text_fragment(decoded),
			     text_fragment(L"Error decoding multibyte string",
					   get_style("Error")),
			     NULL);
  else
    return text_fragment(decoded);
}

fragment *text_fragment(const string &s,
			const style &st,
			const char *encoding)
{
  return style_fragment(text_fragment(s, encoding),
			st);
}

/** This fragment just expands to a newline. */
class _newline_fragment:public fragment
{
public:
  _newline_fragment() {}

  fragment_contents layout(size_t firstw, size_t restw, const style &st)
  {
    fragment_contents rval;
    rval.set_final_nl(true);
    return rval;
  }

  size_t max_width(size_t first_indent, size_t rest_indent) const
  {
    return first_indent;
  }

  size_t trailing_width(size_t first_indent, size_t rest_indent) const
  {
    return rest_indent;
  }

  bool final_newline() const
  {
    return true;
  }
};

fragment *newline_fragment()
{
  return new _newline_fragment;
}

class _style_fragment:public fragment
{
public:
  _style_fragment(fragment *_contents, const style &_st)
    :contents(_contents), st(_st)
  {
  }
  ~_style_fragment() {delete contents;}

  fragment_contents layout(size_t firstw, size_t restw,
			   const style &st2)
  {
    return contents->layout(firstw, restw, st2+st);
  }

  size_t max_width(size_t first_indent, size_t rest_indent) const
  {
    return contents->max_width(first_indent, rest_indent);
  }

  size_t trailing_width(size_t first_indent, size_t rest_indent) const
  {
    return contents->trailing_width(first_indent, rest_indent);
  }

  bool final_newline() const
  {
    return contents->final_newline();
  }
private:
  fragment *contents;
  style st;
};

fragment *style_fragment(fragment *f,
			 const style &st)
{
  return new _style_fragment(f, st);
}

/** A base class for fragment containers that supports caching
 *  information about the children.  (yum, premature optimization)
 */
class fragment_container:public fragment
{
  void update_width(size_t first_indent,
		    size_t rest_indent) const
  {
    if(width_stale ||
       first_indent != stale_first_indent ||
       rest_indent != stale_rest_indent)
      {
	max_width_cache=calc_max_width(first_indent, rest_indent);
	trailing_width_cache=calc_trailing_width(first_indent,
						 rest_indent);
	width_stale=false;
	stale_first_indent=first_indent;
	stale_rest_indent=rest_indent;
      }
  }
public:
  fragment_container()
    :width_stale(true), final_nl_stale(true)
  {
  }

  /** Actually calculate the maximum width. */
  virtual size_t calc_max_width(size_t first_indent,
				size_t rest_indent) const=0;

  /** Actually calculate the trailing width. */
  virtual size_t calc_trailing_width(size_t first_indent,
				     size_t rest_indent) const=0;

  /** Actually calculate the final-nl status. */
  virtual bool calc_final_newline() const=0;

  size_t max_width(size_t first_indent,
		   size_t rest_indent) const
  {
    update_width(first_indent, rest_indent);
    return max_width_cache;
  }

  size_t trailing_width(size_t first_indent,
			size_t rest_indent) const
  {
    update_width(first_indent, rest_indent);
    return trailing_width_cache;
  }

  bool final_newline() const
  {
    if(final_nl_stale)
      {
	final_nl_cache=calc_final_newline();
	final_nl_stale=false;
      }

    return final_nl_cache;
  }

private:
  mutable size_t max_width_cache, trailing_width_cache;
  mutable size_t stale_first_indent, stale_rest_indent;
  mutable bool final_nl_cache:1;
  mutable bool width_stale:1, final_nl_stale:1;
};

/** A fragment generated by composing a sequence of other fragments. */
class _sequence_fragment:public fragment_container
{
public:
  _sequence_fragment(const vector<fragment*> &_contents):contents(_contents)

  {
  }

  fragment_contents layout(size_t firstw, const size_t restw,
			   const style &st)
  {
    fragment_contents rval;

    rval.push_back(fragment_line(L""));

    for(vector<fragment*>::const_iterator i=contents.begin();
	i!=contents.end(); ++i)
      {
	fragment_contents lines=(*i)->layout(firstw, restw, st);

	// Update firstw appropriately.
	if(lines.get_final_nl())
	  firstw=restw;
	else if(lines.size()>0)
	  {
	    int deduct_from;

	    if(lines.size()==1)
	      deduct_from=firstw;
	    else
	      deduct_from=restw;

	    if(deduct_from>=lines.back().width())
	      firstw=deduct_from-lines.back().width();
	    else
	      firstw=0;
	  }

	// Make sure that implicit newlines are handled correctly.
	if(lines.size()==0)
	  {
	    if(rval.get_final_nl() && lines.get_final_nl())
	      rval.push_back(fragment_line(L""));

	    rval.set_final_nl(rval.get_final_nl() || lines.get_final_nl());
	  }

	for(fragment_contents::const_iterator j=lines.begin();
	    j!=lines.end(); ++j)
	  {
	    if(!rval.get_final_nl())
	      {
		rval.back()+=*j;
		rval.set_final_nl(true);
	      }
	    else
	      rval.push_back(*j);
	  }

	rval.set_final_nl(lines.get_final_nl());
      }

    return rval;
  }

  ~_sequence_fragment()
  {
    for(vector<fragment*>::const_iterator i=contents.begin();
	i!=contents.end(); ++i)
      delete *i;
  }

  size_t calc_max_width(size_t first_indent,
			size_t rest_indent) const
  {
    size_t rval=0;
    size_t partial=first_indent;
    for(vector<fragment*>::const_iterator i=contents.begin();
	i!=contents.end(); ++i)
      {
	rval=max(rval, (*i)->max_width(partial, rest_indent));

	if((*i)->final_newline())
	  rval=max(partial, rval);

	partial=(*i)->trailing_width(partial, rest_indent);
      }

    rval=max(partial, rval);

    return rval;
  }

  size_t calc_trailing_width(size_t first_indent,
			     size_t rest_indent) const
  {
    size_t rval=first_indent;

    for(vector<fragment*>::const_iterator i=contents.begin();
	i!=contents.end(); ++i)
      rval=(*i)->trailing_width(rval, rest_indent);

    return rval;
  }

  bool calc_final_newline() const
  {
    return !contents.empty() && contents.back()->final_newline();
  }

private:
  const vector<fragment*> contents;
};

fragment *sequence_fragment(const vector<fragment*> &contents)
{
  return new _sequence_fragment(contents);
}

fragment *sequence_fragment(fragment *f, ...)
{
  vector<fragment*> fragments;

  if(f==NULL)
    return new _sequence_fragment(fragments);

  fragments.push_back(f);

  va_list lst;

  va_start(lst, f);
  do
    {
      f=va_arg(lst, fragment*);
      if(f!=NULL)
	fragments.push_back(f);
    } while(f!=NULL);

  va_end(lst);

  return new _sequence_fragment(fragments);
}

// Fall back on sequence_fragment [for now?]
fragment *join_fragments(const vector<fragment *> &fragments,
			 const wstring &between)
{
  vector<fragment*> rval;

  for(vector<fragment *>::const_iterator i=fragments.begin();
      i!=fragments.end();
      ++i)
    {
      if(i!=fragments.begin())
	rval.push_back(text_fragment(between));

      rval.push_back(*i);
    }

  return sequence_fragment(rval);
}

class _flowbox:public fragment_container
{
public:
  _flowbox(fragment *_contents):contents(_contents) {}

  fragment_contents layout(size_t firstw, const size_t restw,
			   const style &st)
  {
    if(restw==0)
      return fragment_contents();

    fragment_contents rval,lines=contents->layout(firstw, restw,
						  st);

    for(fragment_contents::const_iterator i=lines.begin();
	i!=lines.end(); ++i)
      {
	fragment_line s=*i;

	// Make sure we at least advance the cursor for every line read.
	//
	// Is there a less gross way to express this?
	bool output_something=false;

	size_t first=0;

	// Flow the current line:
	while(first<s.size())
	  {
	    // Strip leading whitespace.
	    while(first<s.size() && iswspace(s[first].ch))
	      ++first;

	    if(first==s.size())
	      break;

	    // If there are few enough characters to fit on a single
	    // line, do that.
	    fragment_line maybe_curr(s, first, s.size()-first);
	    if(maybe_curr.width()<=(signed) firstw)
	      {
		rval.push_back(maybe_curr);
		firstw=restw;
		first=s.size();
		output_something=true;
	      }
	    else
	      {
		int chunkw=0;
		size_t chars=0;
		while(chunkw<(signed) firstw && chars+first<s.size())
		  {
		    chunkw+=wcwidth(s[first+chars].ch);
		    ++chars;
		  }

		// Save this for later (see below).  Note that if we
		// actually overshot the width goal, we need to
		// possibly strip the last character off (it's
		// guaranteed to be only one since otherwise we'd have
		// stopped sooner...)
		const size_t high_water_mark=chars;
		const int high_water_width=chunkw;

		// We pushed the line as far as possible; back up
		// until we are no longer in the middle of a word AND
		// the string is short enough.  (we know it's not at
		// the end of the whole string because of the earlier
		// test)
		while(chars>0 && (chunkw>(signed) firstw ||
				  !iswspace(s[first+chars].ch)))
		  {
		    --chars;
		    chunkw-=wcwidth(s[first+chars].ch);
		  }

		if(chars==0)
		  {
		    // Oops, there's a word that's longer than the
		    // current line.  Push as much as fits onto the
		    // current line.

		    // First, try to exclude any characters that
		    // overlap the right margin.
		    chars=high_water_mark;
		    chunkw=high_water_width;
		    while(chars>0 && chunkw>(signed) firstw)
		      {
			--chars;
			chunkw-=wcwidth(s[first+chars].ch);
		      }

		    // If even that's impossible, go ahead and push a
		    // single character onto the end.  Note that this
		    // means we're probably in such a tiny space that
		    // the result will suck no matter what..
		    if(chars==0)
		      chars=1;

		    rval.push_back(fragment_line(s, first, chars));
		    first+=chars;
		    firstw=restw;
		    output_something=true;
		  }
		else
		  {
		    // Strip trailing whitespace, then `output' the
		    // line.
		    while(chars>0 &&
			  iswspace(s[first+chars-1].ch))
		      --chars;

		    rval.push_back(fragment_line(s, first, chars));
		    firstw=restw;
		    first+=chars;
		    output_something=true;
		  }
	      }
	  }

	if(!output_something)
	  {
	    rval.push_back(fragment_line(L""));
	    firstw=restw;
	  }

	// Ok, go to the next line now.
      }

    // flowboxes always have a final newline.
    rval.set_final_nl(true);

    return rval;
  }

  size_t calc_max_width(size_t first_indent,
			size_t rest_indent) const
  {
    return contents->max_width(first_indent, rest_indent);
  }

  size_t calc_trailing_width(size_t first_indent,
			     size_t rest_indent) const
  {
    return rest_indent;
  }

  bool calc_final_newline() const
  {
    return true;
  }

  ~_flowbox() { delete contents; }

private:
  fragment * const contents;
};

fragment *flowbox(fragment *contents) {return new _flowbox(contents);}

class _fillbox:public fragment_container
{
public:
  _fillbox(fragment *_contents):contents(_contents) {}

  fragment_contents layout(size_t firstw, size_t restw,
			   const style &st)
  {
    // As far as I know, this is valid everywhere...but it would be
    // rather tricky to write this algorithm without making this
    // assumption.
    assert(wcwidth(L' ')==1);

    if(restw==0)
      return fragment_contents();

    fragment_contents rval,lines=contents->layout(firstw, restw,
						  st);

    for(fragment_contents::const_iterator i=lines.begin();
	i!=lines.end(); ++i)
      {
	fragment_line s=*i;

	size_t first=0;

	// Build a list of words on the current line.
	vector<fragment_line> words;

	bool output_something=false;

	while(first<s.size())
	  {
	    // Strip leading whitespace.
	    while(first<s.size() && iswspace(s[first].ch))
	      ++first;

	    size_t amt=0;
	    while(first+amt<s.size() && !iswspace(s[first+amt].ch))
	      ++amt;

	    if(amt>0)
	      words.push_back(fragment_line(s, first, amt));

	    first+=amt;
	  }

	// Now place them onto output lines.

	size_t word_start=0;

	while(word_start<words.size())
	  {
	    size_t curwidth=0;
	    size_t nwords=0;

	    // As long as adding the *next* word doesn't put us
	    // past the right edge, add it.
	    while(word_start+nwords < words.size() &&
		  curwidth+words[word_start+nwords].width()+nwords <= firstw)
	      {
		curwidth+=words[word_start+nwords].width();
		++nwords;
	      }

	    if(nwords==0)
	      {
		// Split a single word: just chop the beginning off.
		size_t chars=0;
		fragment_line &word=words[word_start];
		while(chars<word.size() && curwidth<firstw)
		  {
		    curwidth+=wcwidth(word[chars].ch);
		    ++chars;
		  }

		while(chars>0 && curwidth>firstw)
		  {
		    --chars;
		    curwidth-=wcwidth(word[chars].ch);
		  }

		if(chars==0)
		  chars=1;

		rval.push_back(fragment_line(words[word_start], 0, chars));
		words[word_start]=fragment_line(words[word_start], chars);
		firstw=restw;
		output_something=true;
	      }
	    else
	      {
		size_t diff;

		if(word_start+nwords<words.size())
		  diff=firstw-(curwidth+nwords-1);
		else
		  // Cheat to disable filling on the last line of the
		  // paragraph.
		  diff=0;

		// Now spit the words into an output string, filled
		// left and right.
		fragment_line final(L"");

		// This is similar to the famous algorithm for drawing
		// a line.  The idea is to add diff/(words-1) spaces
		// (in addition to one space per word); since
		// fractional spaces aren't allowed, I approximate by
		// adding a number of spaces equal to the integral
		// part, then keeping the remainder for the next word.
		size_t extra_spaces=0;

		for(size_t word=0; word<nwords; ++word)
		  {
		    if(word>0)
		      // Insert spaces between words:
		      {
			extra_spaces+=diff;

			size_t nspaces=1+extra_spaces/(nwords-1);
			extra_spaces%=nwords-1;

			final+=fragment_line(nspaces, L' ', st.get_attrs());
		      }

		    final+=words[word+word_start];
		  }

		output_something=true;
		rval.push_back(final);
		firstw=restw;

		word_start+=nwords;
	      }
	  }

	if(!output_something)
	  {
	    rval.push_back(fragment_line(L""));
	    firstw=restw;
	  }
      }

    // fillboxes always have a final newline.
    rval.set_final_nl(true);

    return rval;
  }

  size_t calc_max_width(size_t first_indent,
			size_t rest_indent) const
  {
    return contents->max_width(first_indent, rest_indent);
  }

  size_t calc_trailing_width(size_t first_indent,
			     size_t rest_indent) const
  {
    return rest_indent;
  }

  bool calc_final_newline() const
  {
    return true;
  }

  ~_fillbox() { delete contents; }

private:
  fragment * const contents;
};

fragment *fillbox(fragment *contents) {return new _fillbox(contents);}

class _hardwrapbox:public fragment_container
{
public:
  _hardwrapbox(fragment *_contents):contents(_contents) {}

  ~_hardwrapbox() {delete contents;}

  fragment_contents layout(size_t firstw, const size_t restw,
			   const style &st)
  {
    if(restw==0)
      return fragment_contents();

    fragment_contents rval, lines=contents->layout(firstw, restw,
						   st);

    for(fragment_contents::const_iterator i=lines.begin();
	i!=lines.end(); ++i)
      {
	if(i->empty())
	  {
	    rval.push_back(fragment_line(L""));
	    firstw=restw;
	  }
	else
	  {
	    fragment_line s=*i;
	    fragment_line::size_type start=0;

	    while(start<s.size())
	      {
		size_t chars=0;
		int width=0;

		while(width<(signed) firstw && start+chars<s.size())
		  {
		    width+=wcwidth(s[start+chars].ch);
		    ++chars;
		  }

		// If we spilled over, it's the last character that's
		// responsible.
		if(width>(signed) firstw && chars>1)
		  --chars;

		rval.push_back(fragment_line(s, start, chars));
		start+=chars;
		firstw=restw;
	      }
	  }
      }

    rval.set_final_nl(true);

    return rval;
  }

  size_t calc_max_width(size_t first_indent, size_t rest_indent) const
  {
    return contents->max_width(first_indent, rest_indent);
  }

  size_t calc_trailing_width(size_t first_indent, size_t rest_indent) const
  {
    return rest_indent;
  }

  bool calc_final_newline() const
  {
    return true;
  }

private:
  fragment * const contents;
};

fragment *hardwrapbox(fragment *contents)
{
  return new _hardwrapbox(contents);
}

class _clipbox:public fragment_container
{
public:
  _clipbox(fragment *_contents):contents(_contents) {}

  fragment_contents layout(size_t firstw, const size_t restw,
			   const style &st)
  {
    fragment_contents rval, lines=contents->layout(firstw, restw,
						   st);

    for(fragment_contents::const_iterator i=lines.begin(); i!=lines.end(); ++i)
      {
	size_t chars=0;
	int width=0;

	while(width<(signed) firstw && chars<i->size())
	  {
	    width+=wcwidth((*i)[chars].ch);
	    ++chars;
	  }

	if(width>(signed) firstw && chars>1)
	  --chars;

	rval.push_back(fragment_line(*i, 0, chars));
	firstw=restw;
      }

    // Clipboxes are always followed by a final newline.
    rval.set_final_nl(true);

    return rval;
  }

  size_t calc_max_width(size_t first_indent,
			size_t rest_indent) const
  {
    return contents->max_width(first_indent, rest_indent);
  }

  size_t calc_trailing_width(size_t first_indent,
			     size_t rest_indent) const
  {
    return rest_indent;
  }

  bool calc_final_newline() const
  {
    return true;
  }

  ~_clipbox() { delete contents; }

private:
  fragment *contents;
};

fragment *clipbox(fragment *contents) {return new _clipbox(contents);}

class _indentbox:public fragment_container
{
public:
  _indentbox(size_t _firstindent, size_t _restindent, fragment *_contents)
    :contents(_contents), firstindent(_firstindent),
     restindent(_restindent) {}

  fragment_contents layout(size_t firstw, size_t restw,
			   const style &st)
  {
    if(restw<=restindent)
      return fragment_contents();

    fragment_line firstprepend(firstindent, L' ', st.get_attrs());
    fragment_line restprepend(restindent, L' ', st.get_attrs());

    firstprepend.apply_style(st);
    restprepend.apply_style(st);

    size_t child_firstw=firstw>=firstindent?firstw-firstindent:0;
    size_t child_restw=restw>=restindent?restw-restindent:0;

    fragment_contents rval, lines=contents->layout(child_firstw,
						   child_restw,
						   st);

    for(fragment_contents::const_iterator i=lines.begin(); i!=lines.end(); ++i)
      {
	fragment_line l=((i==lines.begin())?firstprepend:restprepend)+*i;

	rval.push_back(l);
      }

    // Indentboxes are always followed by a final newline.
    rval.set_final_nl(true);

    return rval;
  }

  size_t calc_max_width(size_t my_first_indent,
			size_t my_rest_indent) const
  {
    return contents->max_width(my_first_indent+firstindent,
			       my_rest_indent+restindent);
  }

  size_t calc_trailing_width(size_t my_first_indent,
			     size_t my_rest_indent) const
  {
    return my_rest_indent+restindent;
  }

  bool calc_final_newline() const
  {
    return true;
  }

  ~_indentbox() { delete contents; }
private:
  fragment *contents;

  size_t firstindent, restindent;
};

fragment *indentbox(size_t firstindent,
		    size_t restindent,
		    fragment *contents)
{
  return new _indentbox(firstindent, restindent, contents);
}

class _fragment_columns : public fragment_container
{
  vector<fragment_column_entry> columns;

  void update_widths(vector<size_t> &widths,
		     size_t w) const
  {
    size_t total = 0, denominator = 0;

    for(size_t i = 0; i < columns.size(); ++i)
      {
	if(columns[i].proportional)
	  {
	    widths[i]    = 0;
	    denominator += columns[i].width;
	  }
	else
	  {
	    widths[i] = columns[i].width;
	    total    += widths[i];
	  }
      }

    if(total < w && denominator > 0)
      {
	size_t remainder = w - total;

	for(size_t i = 0; i < columns.size(); ++i)
	  if(columns[i].proportional)
	    {
	      widths[i]   += (remainder * columns[i].width) / denominator;
	      denominator -= columns[i].width;
	      remainder   -= widths[i];
	    }
      }

    // Clip.
    for(size_t i = 0; i < columns.size(); ++i)
      {
	widths[i] = min<int>(w, widths[i]);
	w        -= widths[i];
      }
  }
public:
  _fragment_columns(const vector<fragment_column_entry> &_columns)
    :columns(_columns)
  {
  }

  ~_fragment_columns()
  {
    for(vector<fragment_column_entry>::const_iterator
	  i = columns.begin(); i != columns.end(); ++i)
      delete i->f;
  }

  fragment_contents layout(size_t firstw, size_t restw, const style &st)
  {
    assert(firstw == restw);

    vector<size_t> widths(columns.size());
    update_widths(widths, restw);

    vector<fragment_contents> child_layouts(columns.size());

    for(size_t i = 0; i < columns.size(); ++i)
      if(columns[i].f != NULL)
	child_layouts[i]    = columns[i].f->layout(widths[i], widths[i], st);

    size_t height = 0;
    for(size_t i = 0; i < columns.size(); ++i)
      if(child_layouts[i].size() > height)
	height = child_layouts[i].size();

    vector<size_t> starting_lines(columns.size());

    for(size_t i = 0; i < columns.size(); ++i)
      {
	switch(columns[i].vert_align)
	  {
	  case fragment_column_entry::top:
	    starting_lines[i] = 0;
	    break;

	  case fragment_column_entry::center:
	    starting_lines[i] = (height - child_layouts[i].size()) / 2;
	    break;

	  case fragment_column_entry::bottom:
	    starting_lines[i] = height - child_layouts[i].size();
	    break;
	  }
      }

    fragment_contents rval;
    for(size_t y = 0; y < height; ++y)
      {
	fragment_line tmp(L"");

	for(size_t i = 0; i < columns.size(); ++i)
	  if(columns[i].f != NULL &&
	     y >= starting_lines[i] &&
	     y < starting_lines[i] + child_layouts[i].size())
	    {
	      fragment_line &s = child_layouts[i][y - starting_lines[i]];
	      tmp += s;

	      if(widths[i] > s.size())
		tmp += fragment_line(widths[i] - s.size(), L' ',
				     st.get_attrs());
	    }
	  else
	    tmp += fragment_line(widths[i], L' ', st.get_attrs());

	rval.push_back(tmp);
      }

    rval.set_final_nl(true);

    return rval;
  }

  size_t calc_max_width(size_t first_indent, size_t rest_indent) const
  {
    assert(first_indent == rest_indent);

    size_t rval = 0;

    for(vector<fragment_column_entry>::const_iterator
	  i = columns.begin(); i != columns.end(); ++i)
      {
	size_t thisw =
	  i->f == NULL ? 0 : i->f->max_width(first_indent, rest_indent);

	if(!i->proportional && thisw < i->width)
	  thisw = i->width;

	rval += thisw;

	if(first_indent < thisw)
	  first_indent = 0;
	else
	  first_indent -= thisw;

	if(rest_indent < thisw)
	  rest_indent = 0;
	else
	  rest_indent -= thisw;
      }

    return rval;
  }

  size_t calc_trailing_width(size_t first_indent, size_t rest_indent) const
  {
    assert(first_indent == rest_indent);

    return rest_indent;
  }

  bool calc_final_newline() const
  {
    return true;
  }
};

fragment *fragment_columns(const vector<fragment_column_entry> &columns)
{
  return new _fragment_columns(columns);
}

struct argument
{
  argument():format(0) {}

  char format;
  /** If \b true, the 'l' modifier was attached to this argument. */
  bool islong:1;
  union
  {
    fragment *F;
    const char *s;
    const wchar_t *ls;
    int attr;
  };
};

// hack
string char_to_str(char code)
{
  string s;

  if(isprint(code))
    s+=code;
  else
    {
      char buf[64];

      snprintf(buf, 64, "\\%d", code);
      s+=buf;
    }

  return s;
}

fragment *fragf(const char *format, ...)
{
  int argcount=0;
  int posargcount=0;

  const char *start=format;
  // find all the arguments.
  char *nextpercent=strchr(start, '%');

  // loop 1: count the arguments.
  while(nextpercent!=NULL)
    {
      if(*(nextpercent+1)=='l')
	++nextpercent;

      switch(*(nextpercent+1))
	{
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
	  {
	    char *endptr;

	    int pos=strtol(nextpercent+1, &endptr, 10);

	    if(*endptr!='$' || (*(endptr+1)!='F' && *(endptr+1)!='s'))
	      {
		string s="Internal error: bad character in positional argument: '"+char_to_str(*endptr)+"'";

		return text_fragment(s, get_style("Error"));
	      }

	    posargcount=max(posargcount, pos);

	    start=endptr+2;
	  }
	  break;

	case 'B':
	case 'b':
	case 'R':
	case 'r':
	case 'D':
	case 'd':
	case 'n':
	case 'N':
	case '%':
	  start=nextpercent+2;
	  break;
	case 's':
	case 'F':
	case 'A':
	case 'C':
	case 'S':
	  ++argcount;
	  start=nextpercent+2;
	  break;
	default:
	  return text_fragment("Internal error: bad format string code '"+char_to_str(*(nextpercent+1))+"'",
			       get_style("Error"));
	}

      nextpercent=strchr(start, '%');
    }

  int nargs=max(argcount, posargcount);

  argument arguments[nargs];

  argcount=0;

  // loop 2: read the list of arguments and parse their type.
  start=format;
  nextpercent=strchr(start, '%');
  while(nextpercent!=NULL)
    {
      bool islong=false;
      if(*(nextpercent+1)=='l')
	{
	  islong=true;
	  ++nextpercent;
	}
      
      switch(*(nextpercent+1))
	{
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
	  {
	    char *endptr;

	    int pos=strtol(nextpercent+1, &endptr, 10)-1;

	    // if we saw it before it had better be the same type.
	    if(arguments[pos].format!=0 &&
	       (arguments[pos].format!=*(endptr+1) ||
		arguments[pos].islong!=islong))
	      return text_fragment("Bad argument string to fragf: inconsistent positional parameter types!",
				   get_style("Error"));

	    arguments[pos].format=*(endptr+1);

	    start=endptr+2;
	  }
	  break;

	case 'B':
	case 'b':
	case 'R':
	case 'r':
	case 'D':
	case 'd':
	case 'n':
	case 'N':
	case '%':
	  start=nextpercent+2;
	  break;
	case 's':
	case 'F':
	case 'S':
	  if(arguments[argcount].format!=0 && arguments[argcount].format!=*(nextpercent+1))
	    return text_fragment("Bad argument string to fragf: inconsistent parameter types!",
				 get_style("Error"));

	  arguments[argcount].format=*(nextpercent+1);

	  ++argcount;
	  start=nextpercent+2;
	  break;
	default:
	  return text_fragment("Internal error: bad format string code '"+char_to_str(*(nextpercent+1))+"'",
			       get_style("Error"));
	}

      nextpercent=strchr(start, '%');
    }

  style st;

  va_list arglst;
  va_start(arglst, format);

  for(int i=0; i<nargs; ++i)
    {
      switch(arguments[i].format)
	{
	case 0:
	  break; // I suppose unused arguments are ok
	case 'F':
	  arguments[i].F=va_arg(arglst, fragment *);
	  break;
	case 's':
	  arguments[i].s=va_arg(arglst, const char *);
	  break;
	case 'S':
	  arguments[i].s=va_arg(arglst, const char *);
	  break;
	default:
	  return text_fragment("Internal error: bad argument format '"+char_to_str(arguments[i].format)+"'",
			       get_style("Error"));
	}
    }

  va_end(arglst);

  // Now parse this and generate a result.
  vector<fragment *> rval;

  start=format;
  nextpercent=strchr(start, '%');

  // Current argument for non-positional arguments.
  argcount=0;

  // Optimization: don't create lots of unnecessary text fragments
  // when one will do.
  string curstr("");

  // Loop 3: execute the program

  while(nextpercent!=NULL)
    {
      bool islong=false;

      // First, make a fragment for everything until the percent:
      curstr+=string(start, nextpercent-start);

      if(*(nextpercent+1)=='l')
	{
	  islong=true;
	  ++nextpercent;
	}

      // This is almost always what we want; in the cases when it's not,
      // I override it explicitly.
      start=nextpercent+2;

      // Now, find what's at the percent.
      switch(*(nextpercent+1))
	{
	case '%':
	  curstr+="%";
	  break;
	case 'B':
	case 'b':
	case 'R':
	case 'r':
	case 'D':
	case 'd':
	  if(!curstr.empty())
	    {
	      rval.push_back(text_fragment(curstr, st));
	      curstr="";
	    }
	  switch(*(nextpercent+1))
	    {
	    case 'B':
	    case 'b':
	      st.attrs_flip(A_BOLD);
	      break;
	    case 'R':
	    case 'r':
	      st.attrs_flip(A_REVERSE);
	      break;
	    case 'D':
	    case 'd':
	      st.attrs_flip(A_DIM);
	      break;
	    }
	  break;
	case 'n':
	  if(!curstr.empty())
	    {
	      rval.push_back(text_fragment(curstr, st));
	      curstr="";
	    }

	  rval.push_back(newline_fragment());
	  break;
	case 'N':
	  if(!curstr.empty())
	    {
	      rval.push_back(text_fragment(curstr, st));
	      curstr="";
	    }
	  st=style();
	  break;
	case 'F':
	  // should have been verified above.
	  assert(arguments[argcount].format=='F');

	  if(!curstr.empty())
	    {
	      rval.push_back(text_fragment(curstr, st));
	      curstr="";
	    }
	  rval.push_back(arguments[argcount].F);
	  ++argcount;
	  break;
	case 's':
	  // should have been verified above.
	  assert(arguments[argcount].format=='s');

	  if(islong)
	    curstr+=transcode(arguments[argcount].ls);
	  else
	    curstr+=arguments[argcount].s;
	  ++argcount;
	  break;
	case 'S':
	  assert(arguments[argcount].format=='S');

	  if(!curstr.empty())
	    {
	      rval.push_back(text_fragment(curstr, st));
	      curstr="";
	    }

	  st+=get_style(arguments[argcount].s);
	  ++argcount;
	  break;
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
	  {
	    char *endptr;

	    int pos=strtol(nextpercent+1, &endptr, 10)-1;

	    assert(arguments[pos].format==*(endptr+1));

	    switch(*(endptr+1))
	      {
	      case 'F':
		if(!curstr.empty())
		  {
		    rval.push_back(text_fragment(curstr, st));
		    curstr="";
		  }

		rval.push_back(arguments[pos].F);
		break;
	      case 's':
		curstr+=arguments[pos].s;
		break;
	      case 'S':
		if(!curstr.empty())
		  {
		    rval.push_back(text_fragment(curstr, st));
		    curstr="";
		  }

		st+=get_style(arguments[pos].s);
		break;
	      }

	    start=endptr+2;
	    break;
	  }
	}

      nextpercent=strchr(start, '%');
    }

  // Get any trailing bit o' string:
  if(*start!=0)
    curstr+=start;

  if(!curstr.empty())
    rval.push_back(text_fragment(curstr, st));

  return sequence_fragment(rval);
}
