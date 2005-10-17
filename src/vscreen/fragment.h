// fragment.h             -*-c++-*-
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
//
// Fragments are pieces of text that live in a vs_text_layout widget.
// See vs_text_layout.h for details.

#ifndef FRAGMENT_H
#define FRAGMENT_H

#include "fragment_contents.h"

#include "config/style.h"

#include <string>
#include <vector>

/** A fragment represents a logical unit of text.
 */
class fragment
{
public:
  /** Return all the lines of this fragment, given the "shape" of the
   *  fragment.  Note that some fragments ignore the given widths, so
   *  the caller is expected to either put everything in a formatting
   *  box (one that forces its contents to stay "in bounds") or
   *  manually clip the return value.
   *
   *  \param firstw the width to which the first line of the fragment
   *  should be formatted.
   *
   *  \param w the width to which subsequent lines of the fragment
   *  should be formatted.
   *
   *  \param s the enclosing style of this fragment.  The fragment's
   *  size is guaranteed to be independent of s.
   *
   *  \return the lines of this fragment; the caller is responsible
   *  for deleting it.
   */
  virtual fragment_contents layout(size_t firstw,
				   size_t w,
				   const style &st)=0;

  /** \param first_indent the indentation of the first line, relative
   *  to a baseline (which may be outside this fragment).
   *
   *  \param rest_indent the indentation of any other lines.
   *
   *  \return the maximum length of any line in this fragment.  Any
   *  call to layout() with a width greater than this maximum length
   *  will produce the same result.
   */
  virtual size_t max_width(size_t first_indent,
			   size_t rest_indent) const=0;

  /** \param first_indent the indentation of the first line.
   *
   *  \param rest_indent the indentation of any other lines.
   *
   * \return the length of any "trailing" line in the fragment,
   * including indentation.
   */
  virtual size_t trailing_width(size_t first_indent,
				size_t rest_indent) const=0;

  /** \return \b true if this fragment ends in a newline. */
  virtual bool final_newline() const=0;

  /** Nothing to do in the base class */
  virtual ~fragment();
};

// Factory methods to avoid cluttering the .h file:

/** Create a fragment from a string of text.  The text will simply be
 *  formatted as is, with line breaks at newlines and tabs replaced by
 *  eight spaces.
 *
 *  \param s the text to use
 *
 *  \return the new fragment
 */
fragment *text_fragment(const std::wstring &s);

/** Create a fragment from a string of text.  The text will simply be
 *  formatted as a single line without clipping.
 *
 *  \param s the text to use
 *  \param style the base style for the new fragment; an implicit
 *  style_fragment for this style is wrapped around the new text_fragment.
 *
 *  \return the new fragment
 */
fragment *text_fragment(const std::wstring &s,
			const style &st);

/** Create a fragment from a string of multibyte-encoded text.
 *
 *  \param s the text to use
 *  \param encoding the text's encoding; if this is \b null or
 *   unspecified, LC_CTYPE will be used.
 *
 *  \return the new fragment
 */
fragment *text_fragment(const std::string &s,
			const char *encoding=NULL);

/** Create a fragment from a string of multibyte-encoded text,
 *  wrapping an implicit style_fragment around it.
 */
fragment *text_fragment(const std::string &s,
			const style &st,
			const char *encoding=NULL);

/** Create a fragment from a string of text.  The text will simply be
 *  formatted as a single line without clipping.
 *
 *  \param s the text to use
 *  \param attr attributes to assign to it
 *
 *  \return the new fragment
 */
inline fragment *text_fragment(const char *s,
			       const style &st=style())
{
  return text_fragment(std::string(s), st);
}

/** Create a fragment which simply produces a newline wherever it occurs. */
fragment *newline_fragment();

/** Create a fragment which alters the style of its contents.
 *
 *  \param f the child of this fragment
 *  \param attr the text attribute which should be assigned to f
 *
 *  \return the new fragment
 */
fragment *style_fragment(fragment *f,
			 const style &st);

/** Create a fragment from a sequence of other fragments.
 *
 *  The fragment will simply "shove" the two sequences together,
 *  respecting the value of final_nl on each.
 *
 *  \todo can this be made more efficient?  It should be possible to
 *  just "plug" the sequences together if they're lists -- but that only
 *  works if they aren't cached elsewhere.
 *
 *  \param fragments the fragments in the sequence
 *
 *  \return the new fragment
 */
fragment *sequence_fragment(const std::vector<fragment *> &fragments);

/** Create a fragment from a sequence of other fragments.
 *
 *  The fragment will simply "shove" the two sequences together,
 *  respecting the value of final_nl on each.
 *
 *  \param f the first fragment in the sequence; the sequence should
 *           be terminated with a NULL pointer.
 *
 *  \return the new fragment
 */
fragment *sequence_fragment(fragment *f, ...);

/** Join fragments into a single fragment, placing text between them.
 *
 *  This is useful for creating lists, for instance.  The new fragment
 *  takes ownership of all pointers in fragments.
 *
 *  \param fragments the list of fragments to join
 *  \param between a string to place between adjacent items in the input list
 */
fragment *join_fragments(const std::vector<fragment *> &fragments,
			 const std::wstring &between);

/** Create a flowbox.
 *
 *  Each line of the fragment placed inside the flowbox will be
 *  reflowed (word-wrapped) to the current width, possibly to
 *  several lines.
 *
 *  The contents of a flowbox always have final_nl=\b true. (ie, a
 *  flowbox is always followed by a line break)
 *
 *  \param contents the contents of the flowbox
 *
 *  \return the new flowbox
 */
fragment *flowbox(fragment *contents);

/** Create a fillbox.
 *
 *  Each line of the fragment placed inside the fillbox will be
 *  reflowed (word-wrapped) and expanded to the current width,
 *  possibly to several lines.
 *
 *  The contents of a fillbox always have final_nl=\b true.
 *
 *  \param contents the contents of the fillbox
 *
 *  \return the new fillbox
 */
fragment *fillbox(fragment *contents);

/** Create a hardwrapbox.
 *
 *  Each line of the fragment inside the box will be hard-wrapped
 *  to the current width.
 *
 *  The contents of a wrapbox always have final_nl=\b true.
 *
 *  \param contents the contents of the hardwrapbox
 *
 *  \return the new hardwrapbox
 */
fragment *hardwrapbox(fragment *contents);

/** Create a clipbox.
 *
 *  Each line of the fragment placed inside the clipbox will be
 *  clipped to the current width.  The whole layout widget
 *  implicitly uses one of these, but there may be other uses for
 *  clipboxes as well.
 *
 *  \param contents the contents of the clipbox
 *
 *  \return the new clipbox
 */
fragment *clipbox(fragment *contents);

/** Create an indentbox.
 *
 *  Each line of the indentbox will be indented by the specified
 *  amount.  (this effectively decreases the width of each line) If
 *  desired, the first line can be indented a different amount
 *  (typically less) than the remaining lines, although it is
 *  formatted to the same width; this supports things like bulletted
 *  lists.
 *
 *  \param firstindent the number of spaces of indentation to use for the first line
 *  \param restindent the number of spaces of indentation to use for later lines
 *  \param contents the contents of the indentbox
 *  \return the new indentbox
 */
fragment *indentbox(size_t firstindent, size_t restindent, fragment *contents);

/** Stores information on a single column of fragments. */
struct fragment_column_entry
{
  /** If \b true, this column is allocated space proportionally;
   *  otherwise, its width is exactly what is specified.
   */
  bool proportional;

  /** If proportional is \b true, this is a number giving the relative
   *  size of this column compared to other proportional columns;
   *  otherwise, this is the width of the column in character cells.
   */
  size_t width;

  enum align {top, center, bottom};

  /** The vertical alignment of the column.  If top, the top of this
   *  column is placed at the top of the fragment.  If center, the
   *  center of this column is aligned with the center of the
   *  fragment.  And if bottom, the bottom of this column is aligned
   *  with the bottom of the fragment.
   */
  align vert_align;

  /** The fragment to display, or \b NULL for blank space. */
  fragment *f;

  fragment_column_entry(bool _proportional, size_t _width, align _vert_align,
			fragment *_f)
    :proportional(_proportional),
     width(_width),
     vert_align(_vert_align),
     f(_f)
  {
  }

  fragment_column_entry()
    :proportional(false), width(0), vert_align(top)
  {
  }
};

/** A fragment that formats its contents into columns.  If the
 *  fixed-width columns overflow the available space, they will be
 *  clipped hard.
 *
 *  This fragment may NOT be placed inside an indent box or any other
 *  box that alters the shape of its contents.  Doing so will cause
 *  the program to abort.
 *
 *  \param columns a list of column entry information ordered from
 *  left to right.
 */
fragment *fragment_columns(const std::vector<fragment_column_entry> &columns);

/** A printf-alike for fragments.
 *
 *  Formatting codes:
 *
 *  - %F: substitutes a fragment into the sequence being built
 *  - %s: substitutes a const char * into the sequence being built
 *        with transcoding according to LC_CTYPE
 *  - %n: substitutes a newline fragment into the sequence being built
 *  - %%: inserts a literal %
 *  - %B/%b: toggle the bold character attribute
 *  - %R/%r: toggle the reverse video character attribute
 *  - %D/%d: toggle the dim character attribute
 *  - %S: Apply the style corresponding to a
 *        string (looked up via get_style) to the attributes.
 *  - %N: Reset the text style to the empty ("null") style.
 *
 *  For instance,
 *
 *   fragf("%S%BWARNING%b: something bad happened in routine %s,"
 *         "expect a segfault.", "Error", some_routine);
 *
 *  Note: if you use a parameter index multiple times, you are virtually
 *  GUARANTEED to segfault!
 *
 *  Note 2: as usual, format should not contain literal newlines.
 *
 *  \param format the format string
 *  \return the formatted fragment, or NULL if there is an error in the format.
 */
fragment *fragf(const char *format, ...);

#endif
