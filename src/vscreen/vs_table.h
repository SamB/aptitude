// vs_table.h   -*-c++-*-
//
//  Your usual table-layout stuff.

#ifndef VS_TABLE_H
#define VS_TABLE_H

#include "vs_passthrough.h"
#include <list>
#include <vector>

#include <sigc++/connection.h>

class keybindings;

class vs_table:public vs_passthrough
{
public:
  // Options for laying out the widget..
  static const int EXPAND=0x1, SHRINK=0x2, FILL=0x4;
  static const int ALIGN_LEFT=0x8, ALIGN_RIGHT=0x10;
  static const int ALIGN_CENTER=ALIGN_LEFT|ALIGN_RIGHT;
  static const int IGNORE_SIZE_REQUEST=0x20;
private:
  struct child_info
  {
    // The widget itself
    vs_widget_ref w;

    // The upper-left corner of this widget
    int row_start, col_start;

    // How big is it?
    int row_span, col_span;

    /** The amount of space (perhaps provisionally) allocated to this
     *  widget.  A scratchpad for internal algorithms.
     */
    int alloc_w, alloc_h;

    /** The amount of space that the widget requested.  A scratchpad
     *  for internal algorithms.
     */
    int request_w, request_h;

    sigc::connection shown_conn, hidden_conn;

    /** If \b true, expand the widget in the given direction. */
    bool expand_x:1, expand_y:1;

    /** If \b true, the widget will expand to fill its whole cell even
     *  if expand_* are false.
     */
    bool fill_x:1, fill_y:1;

    /** If \b true, shrink the widget in the given direction. */
    bool shrink_x:1, shrink_y:1;

    /** Should the widget be aligned left/right on the given axis?  If both
     *  flags are set, it will be centered in its cell.
     */
    bool align_left_x:1, align_left_y:1, align_right_x:1, align_right_y:1;

    /** If set, ignore the widget's requisition in the given dimension
     *  and pretend that it requested (0,0).
     */
    bool ignore_size_x:1, ignore_size_y:1;

    child_info(const vs_widget_ref &_w, int _row_start, int _col_start,
	       int _row_span, int _col_span, int xopts, int yopts,
	       sigc::connection &_shown_conn, sigc::connection &_hidden_conn);
  };

  bool lies_on_axis(const child_info &base,
		    bool horizontal,
		    const child_info &c);
  class better_fit;
  class nrow_lt;
  class ncol_lt;

  typedef std::list<child_info> childlist;

  // Tables have an automatic behavior similar to dialogs in other widget
  // sets -- they can give the focus to any widget that can handle it.
  //
  // Widgets are given focus in the order in which they are added to the
  // table (cyclically)
  childlist children;
  childlist::iterator focus;

  // Separation between rows/columns; initially 0.
  int rowsep, colsep;

  /** Recalculate the dimensions of the table. */
  void calc_dimensions();

  /** The number of rows in the table. */
  int num_rows;

  /** The number of columns in the table. */
  int num_cols;

  void layout_me();

  // Focus-handling stuff
  vs_widget_ref get_focus();
  void hide_widget(const vs_widget_ref &w);
  void hide_widget_bare(vscreen_widget &w);
  void show_widget(const vs_widget_ref &w);
  void show_widget_bare(vscreen_widget &w);

  /** Populates the given vector with lists of the widgets in each
   *  row.
   *
   *  \param row_contents a vector of length num_rows.
   */
  void get_row_contents(std::vector<std::vector<child_info *> > row_contents);

  /** Populates the given vector with lists of the widgets in each
   *  column.
   *
   *  \param col_contents a vector of length num_cols.
   */
  void get_col_contents(std::vector<std::vector<child_info *> > col_contents);

  void alloc_ideal_widths(std::vector<int> &col_sizes);
  void expand_widths(std::vector<int> &col_sizes, int target_w);
  void shrink_widths(std::vector<int> &col_sizes, int target_w);
  void alloc_ideal_heights(std::vector<int> &row_sizes,
				     const std::vector<int> &col_sizes);
  void expand_heights(std::vector<int> &row_sizes, int target_h);
  void shrink_heights(std::vector<int> &row_sizes, int target_h);
  void alloc_child_sizes(const std::vector<int> &col_sizes,
			 const std::vector<int> &row_sizes);



  void got_focus();
  void lost_focus();

  // Moves the focus in the given direction
  childlist::iterator find_best_focus(childlist::iterator start,
				      int dx,
				      int dy);

protected:
  bool handle_key(const key &k);
  vs_table();

public:
  static ref_ptr<vs_table> create()
  {
    ref_ptr<vs_table> rval(new vs_table);
    rval->decref();
    return rval;
  }

  ~vs_table();

  void destroy();

  void add_widget_opts(const vs_widget_ref &w, int row_start, int col_start, int row_span, int col_span, int xopts, int yopts);
  void add_widget_opts_bare(vscreen_widget &w, int row_start, int col_start, int row_span, int col_span, int xopts, int yopts);

  void add_widget(const vs_widget_ref &w, int row_start, int col_start, int row_span=1, int col_span=1, bool expand=true, bool shrink=true);
  void add_widget_bare(vscreen_widget &w, int row_start, int col_start, int row_span=1, int col_span=1, bool expand=true, bool shrink=true);

  void add_widget(const vs_widget_ref &w);

  void rem_widget(const vs_widget_ref &w);

  void focus_widget(const vs_widget_ref &w);
  void focus_widget_bare(vscreen_widget &w);

  /** Set the separation between adjacent rows to the given number of
   *  characters.
   */
  void set_rowsep(int n);

  /** Set the separation between adjacent rows to the given number of
   *  characters.
   */
  void set_colsep(int n);

  void show_all();

  /** Calculates the requested width of the entire table.
   *
   *  \return the requested width
   */
  int width_request();

  /** Calculates the requested height of the entire table.  At the
   *  moment, this is a bit wasteful, since it goes ahead and
   *  provisionally allocates widths that will be re-allocated anyway.
   *
   *  \param w the width of the table
   *  \return the requested height
   */
  int height_request(int w);

  void paint(const style &st);
  void dispatch_mouse(short id, int x, int y, int z, mmask_t bstate);

  static keybindings *bindings;
  static void init_bindings();
};

typedef ref_ptr<vs_table> vs_table_ref;

#endif
