// vs_pager.h       -*-c++-*-
//
//  Copyright 2000-2004 Daniel Burrows
//
//  A vscreen which acts as a text pager.  Recently rewritten to use
//  an internal vector of lines (makes its behavior much nicer from
//  the user's point of view, even if it's slightly less efficient).

#ifndef VS_PAGER_H
#define VS_PAGER_H

#include "vscreen_widget.h"

#include <string>
#include <vector>

class keybindings;

class vs_pager:public vscreen_widget
{
public:
  typedef std::vector<std::wstring>::size_type line_count;
  typedef int col_count;
private:
  /** The lines of text being displayed: */
  std::vector<std::wstring> lines;

  /** The first visible line. */
  line_count first_line;

  /** How much the widget is scrolled to the right. */
  col_count first_column;

  /** The bounds of the text contained in the pager. */
  col_count text_width;

  /** The last string the user searched for (so we can repeat searches) */
  std::wstring last_search;

  /** Handles resizing the widget. */
  void layout_me();

  /** The workhorse search routine. */
  void search_omnidirectional_for(const std::wstring &s, bool forward);

protected:
  vs_pager(const char *text, int len, const char *encoding = NULL);
  vs_pager(const std::string &s, const char *encoding = NULL);
  vs_pager(const std::wstring &s);

public:
  /** Create a vs_pager from the given memory region.
   *
   *  \param text the text to display
   *  \param len the length of the buffer
   *  \param encoding the encoding of text, or \b NULL to use LC_CTYPE
   */
  static ref_ptr<vs_pager>
  create(const char *text, int len, const char *encoding = NULL)
  {
    ref_ptr<vs_pager> rval(new vs_pager(text, len, encoding));
    rval->decref();
    return rval;
  }

  /** Create a vs_pager from a string.
   *
   *  \param s the text to display
   *  \param encoding the encoding of s, or \b NULL to use LC_CTYPE
   */
  static ref_ptr<vs_pager>
  create(const std::string &s, const char *encoding = NULL)
  {
    ref_ptr<vs_pager> rval(new vs_pager(s, encoding));
    rval->decref();
    return rval;
  }

  /** Create a vs_pager from a wide character string.
   *
   *  \param s the text to display
   */
  static ref_ptr<vs_pager>
  create (const std::wstring &s)
  {
    ref_ptr<vs_pager> rval(new vs_pager(s));

    rval->decref();

    return rval;
  }

  /** Destroy this vs_pager. */
  virtual ~vs_pager();

  /** Set the text to the given memory region.
   *
   *  \param text the text to display
   *  \param len the length of the buffer
   *  \param encoding the encoding of text, or \b NULL to use LC_CTYPE
   */
  virtual void set_text(const char *text,
			std::string::size_type len,
			const char *encoding=NULL);

  /** Change the displayed text.
   *
   *  \param s the text to display
   *  \param encoding the encoding of s, or \b NULL to use LC_CTYPE
   */
  virtual void set_text(const std::string &s, const char *encoding=NULL);

  /** Change the displayed text.
   *
   *  \param s the text to display
   */
  virtual void set_text(const std::wstring &s);

  /** Scroll the screen up by the given number of lines. */
  void scroll_up(line_count nlines);

  /** Scroll the screen down by the given number of lines. */
  void scroll_down(line_count nlines);

  /** Scroll the screen right by the given number of columns. */
  void scroll_right(col_count ncols);

  /** Scroll the screen left by the given number of columns. */
  void scroll_left(col_count ncols);

  /** Scroll to the top of the screen. */
  void scroll_top();

  /** Scroll to the bottom of the screen. */
  void scroll_bottom();

  /** Scroll by a page in the given direction.
   *
   *  \param dir if \b true, scroll a page up; otherwise, scroll a
   *  page down.
   */
  void scroll_page(bool dir);

  /** Find the next line containing the given string.
   *
   *  \param s the string to search for
   */
  void search_for(const std::wstring &s)
  {
    search_omnidirectional_for(s, true);
  }

  /** Find the previous line containing the given string.
   *
   *  \param s the string to search for
   */
  void search_back_for(const std::wstring &s)
  {
    search_omnidirectional_for(s, false);
  }

  /** Return the last string which the user searched for. */
  std::wstring get_last_search() {return last_search;}

  line_count get_first_line() {return first_line;}
  line_count get_num_lines() {return lines.size();}
  col_count get_first_column() {return first_column;}
  col_count get_num_columns() {return text_width;}

  /** Emits a signal describing the verical location of the display
   *  within the text.
   */
  void do_line_signal();

  /** Emits a signal describing the horizontal location of the display
   *  within the text.
   */
  void do_column_signal();

  // vscreen overrides:

  virtual bool handle_key(const key &k);
  virtual bool focus_me() {return true;}
  virtual void paint(const style &st);

  int width_request();
  int height_request(int w);
  bool get_cursorvisible() {return true;}
  point get_cursorloc() {return point(0,0);}

  /** Announces that the user has scrolled vertically. */
  sigc::signal2<void, int, int> line_changed;

  /** Announces that the user has scrolled horizontally. */
  sigc::signal2<void, int, int> column_changed;

  static keybindings *bindings;
  static void init_bindings();
};

/** Load a file from disk; it's assumed to be ASCII for now. */
class vs_file_pager:public vs_pager
{
protected:
  vs_file_pager();
  vs_file_pager(const std::string &filename, const char *encoding = NULL);
  vs_file_pager(const std::wstring &filename, const char *encoding = NULL);

  vs_file_pager(const char *text, int len, const char *encoding = NULL);
public:
  static ref_ptr<vs_file_pager> create()
  {
    return new vs_file_pager;
  }

  static ref_ptr<vs_file_pager> create(const std::string &filename, const char *encoding=NULL)
  {
    return new vs_file_pager(filename, encoding);
  }

  /** Attempts to convert the string to a multibyte representation and
   *  then load it; a nonconvertible string is treated as any other
   *  load failure would be.
   */
  static ref_ptr<vs_file_pager>
  create(const std::wstring &filename, const char *encoding=NULL)
  {
    return new vs_file_pager(filename, encoding);
  }

  static ref_ptr<vs_file_pager>
  create(const char *text, int len, const char *encoding=NULL)
  {
    return new vs_file_pager(text, len, encoding);
  }

  /** Loads the given file into the pager.
   *
   *  \param filename the name of the file to load
   *  \param encoding the encoding of the file's contents; if \b NULL, LC_CTYPE
   *                  is used.
   */
  void load_file(const std::string &filename, const char *encoding=NULL);

  /** Attempts to convert the string to a multibyte representation and
   *  then load it; a nonconvertible string is treated as any other
   *  load failure would be.
   *
   *  \param filename the name of the file to load
   *  \param encoding the encoding of the file's contents; if \b NULL, LC_CTYPE is used.
   */
  void load_file(const std::wstring &filename, const char *encoding);

  /** Attempts to convert the string to a multibyte representation and
   *  then load it; a nonconvertible string is treated as any other
   *  load failure would be.  The file is assumed to contain text in
   *  the encoding specified by LC_CTYPE.
   *
   *  \param filename the name of the file to load
   */
  void load_file(const std::wstring &filename);
};

typedef ref_ptr<vs_pager> vs_pager_ref;
typedef ref_ptr<vs_file_pager> vs_file_pager_ref;

#endif
