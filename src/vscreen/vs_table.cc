// vs_table.cc        -*-c++-*-
//
//  Implementation of the vs_table class

#include "vs_table.h"

#include "vscreen.h"

#include "config/keybindings.h"

#include <sigc++/adaptors/bind.h>
#include <sigc++/functors/mem_fun.h>

#include <numeric>

using namespace std;

keybindings *vs_table::bindings=NULL;

// #define DEBUG_TABLES

#ifdef DEBUG_TABLES
FILE *debug=fopen("/tmp/vs.log", "w");
#endif

vs_table::child_info::child_info(const vs_widget_ref &_w, int _row_start, int _col_start,
				 int _row_span, int _col_span, int xopts, int yopts,
				 sigc::connection &_shown_conn, sigc::connection &_hidden_conn)
  :w(_w), row_start(_row_start), col_start(_col_start),
   row_span(_row_span), col_span(_col_span),
   shown_conn(_shown_conn), hidden_conn(_hidden_conn)
{
  expand_x=((xopts&EXPAND)!=0);
  expand_y=((yopts&EXPAND)!=0);

  shrink_x=((xopts&SHRINK)!=0);
  shrink_y=((yopts&SHRINK)!=0);

  fill_x=((xopts&FILL)!=0);
  fill_y=((yopts&FILL)!=0);

  align_left_x=((xopts&ALIGN_LEFT)!=0);
  align_right_x=((xopts&ALIGN_RIGHT)!=0);

  align_left_y=((yopts&ALIGN_LEFT)!=0);
  align_right_y=((yopts&ALIGN_RIGHT)!=0);

  ignore_size_x=((xopts&IGNORE_SIZE_REQUEST)!=0);
  ignore_size_y=((yopts&IGNORE_SIZE_REQUEST)!=0);
}

vs_table::vs_table()
  :rowsep(0), colsep(0), num_rows(0), num_cols(0)
{
  do_layout.connect(sigc::mem_fun(*this, &vs_table::layout_me));
  focus=children.end();

  focussed.connect(sigc::mem_fun(*this, &vs_table::got_focus));
  unfocussed.connect(sigc::mem_fun(*this, &vs_table::lost_focus));
}

vs_table::~vs_table()
{
  eassert(children.empty());
}

void vs_table::destroy()
{
  vs_widget_ref tmpref(this);

  while(!children.empty())
    children.front().w->destroy();

  vs_passthrough::destroy();
}

void vs_table::set_rowsep(int n)
{
  vs_widget_ref tmpref(this);

  if(n!=rowsep)
    {
      rowsep=n;
      if(get_visible())
	vscreen_update();
    }
}

void vs_table::set_colsep(int n)
{
  vs_widget_ref tmpref(this);

  if(n!=colsep)
    {
      colsep=n;
      if(get_visible())
	vscreen_update();
    }
}

// We need to call get_focus() here to update the "focus" pointer.
void vs_table::got_focus()
{
  vs_widget_ref w=get_focus();

  if(w.valid())
    w->focussed();
}

void vs_table::lost_focus()
{
  vs_widget_ref tmpref(this);

  vs_widget_ref w=get_focus();

  if(w.valid())
    w->unfocussed();
}

void vs_table::add_widget_bare(vscreen_widget &w, int row_start, int col_start, int row_span, int col_span, bool expand, bool shrink)
{
  vs_widget_ref tmpref(this);

  add_widget(vs_widget_ref(&w), row_start, col_start,
	     row_span, col_span, expand, shrink);
}

void vs_table::add_widget(const vs_widget_ref &w, int row_start, int col_start, int row_span, int col_span, bool expand, bool shrink)
{
  vs_widget_ref tmpref(this);

  int opts=ALIGN_CENTER;
  if(expand)
    opts|=EXPAND|FILL;
  if(shrink)
    opts|=SHRINK;

  add_widget_opts(w, row_start, col_start, row_span, col_span, opts, opts);
}

void vs_table::add_widget_opts_bare(vscreen_widget &w, int row_start, int col_start, int row_span, int col_span, int xopts, int yopts)
{
  vs_widget_ref tmpref(this);

  add_widget_opts(vs_widget_ref(&w), row_start, col_start,
		  row_span, col_span, xopts, yopts);
}

void vs_table::add_widget_opts(const vs_widget_ref &w, int row_start, int col_start, int row_span, int col_span, int xopts, int yopts)
{
  vs_widget_ref tmpref(this);

  // sanity check
  for(childlist::iterator i=children.begin(); i!=children.end(); ++i)
    if(i->w==w)
      // FIXME: throw something/print a nasty error message?
      abort();

  sigc::connection shown_conn=w->shown_sig.connect(sigc::bind(sigc::mem_fun(*this, &vs_table::show_widget_bare), w.weak_ref()));
  sigc::connection hidden_conn=w->hidden_sig.connect(sigc::bind(sigc::mem_fun(*this, &vs_table::hide_widget_bare), w.weak_ref()));

  children.push_back(child_info(w, row_start, col_start, row_span, col_span, xopts, yopts, shown_conn, hidden_conn));

  num_rows=max(num_rows, row_start+row_span);
  num_cols=max(num_cols, col_start+col_span);

  w->set_owner(this);

  if(focus==children.end() && w->focus_me() && w->get_visible())
    {
      focus=children.end();
      focus--;

      if(focus!=children.end() && get_isfocussed())
	focus->w->focussed();
    }

  vscreen_queuelayout();
}

void vs_table::hide_widget_bare(vscreen_widget &w)
{
  vs_widget_ref tmpref(this);

  hide_widget(vs_widget_ref(&w));
}

void vs_table::hide_widget(const vs_widget_ref &w)
{
  vs_widget_ref tmpref(this);

  if(focus!=children.end() && w==focus->w)
    {
      if(get_isfocussed())
	focus->w->unfocussed();

      focus++;

      while(focus != children.end() &&
	    (!focus->w->get_visible() || !focus->w->focus_me()))
	focus++;

      if(focus==children.end())
	{
	  focus=children.begin();

	  while(focus != children.end() &&
		(!focus->w->get_visible() || !focus->w->focus_me()))
	    focus++;
	}

      if(focus!=children.end() && get_isfocussed())
	focus->w->focussed();
    }

  vscreen_queuelayout();
}

void vs_table::show_widget_bare(vscreen_widget &w)
{
  vs_widget_ref tmpref(this);

  show_widget(vs_widget_ref(&w));
}

void vs_table::show_widget(const vs_widget_ref &w)
{
  vs_widget_ref tmpref(this);

  if(w->focus_me() && focus==children.end())
    {
      for(childlist::iterator i=children.begin(); i!=children.end(); ++i)
	if(i->w==w)
	  {
	    focus=i;
	    if(get_isfocussed())
	      focus->w->focussed();
	    break;
	  }
    }

  vscreen_queuelayout();
}

void vs_table::add_widget(const vs_widget_ref &w)
{
  vs_widget_ref tmpref(this);

  add_widget(w, num_rows, 0, 1);
}

void vs_table::calc_dimensions()
{
  vs_widget_ref tmpref(this);

  num_rows=0;
  num_cols=0;

  for(childlist::iterator i=children.begin(); i!=children.end(); ++i)
    {
      num_rows=max(num_rows, i->row_start+i->row_span);
      num_cols=max(num_cols, i->col_start+i->col_span);
    }

  num_rows=num_rows;
  num_cols=num_cols;
}

void vs_table::rem_widget(const vs_widget_ref &wBare)
{
  vs_widget_ref tmpref(this);

  vs_widget_ref w = wBare;

  for(childlist::iterator i = children.begin(); i != children.end(); ++i)
    if(i->w == w)
      {
	if(i == focus)
	  {
	    if(get_isfocussed())
	      focus->w->unfocussed();

	    focus++;

	    while(focus != children.end() && (focus == i ||
					      !focus->w->get_visible() ||
					      !focus->w->focus_me()))
	      focus++;

	    if(focus == children.end())
	      {
		focus = children.begin();

		while(focus != children.end() && (focus == i ||
						  !focus->w->get_visible() ||
						  focus->w->focus_me()))
		  focus++;
	      }

	    if(focus != children.end())
	      {
		eassert(focus != i);

		if(get_isfocussed())
		  focus->w->focussed();
	      }
	  }

	i->shown_conn.disconnect();
	i->hidden_conn.disconnect();

	children.erase(i);


	vscreen_queuelayout();
	w->set_owner(NULL);

	// No better way to do this..
	calc_dimensions();

	return;
      }
}

void vs_table::focus_widget(const vs_widget_ref &w)
{
  vs_widget_ref tmpref(this);

  for(childlist::iterator i=children.begin(); i!=children.end(); ++i)
    if(i->w==w)
      {
	if(i!=focus)
	  {
	    eassert(i->w->get_visible() && i->w->focus_me());

	    if(focus!=children.end() && get_isfocussed())
	      focus->w->unfocussed();

	    focus=i;

	    if(get_isfocussed())
	      focus->w->focussed();

	    vscreen_update();
	  }
	return;
      }
}

void vs_table::focus_widget_bare(vscreen_widget &w)
{
  focus_widget(vs_widget_ref(&w));
}

vs_widget_ref vs_table::get_focus()
{
  vs_widget_ref tmpref(this);

  if(focus!=children.end() && focus->w->get_visible() &&
     focus->w->focus_me())
    return focus->w;
  else
    {
      if(focus!=children.end() && get_isfocussed())
	focus->w->unfocussed();

      for(focus=children.begin(); focus!=children.end(); focus++)
	{
	  if(focus->w->get_visible() && focus->w->focus_me())
	    {
	      focus->w->focussed();
	      return focus->w;
	    }
	}

      return NULL;
    }
}

void vs_table::show_all()
{
  vs_widget_ref tmpref(this);

  for(childlist::iterator i=children.begin(); i!=children.end(); ++i)
    i->w->show_all();

  show();
}

class vs_table::better_fit
{
  const child_info &base;

  int dx;
  int dy;

  // the table dimensions
  int width;
  int height;
public:
  better_fit(const child_info &c,
	     int _dx, int _dy, int _width, int _height)
    :base(c), dx(_dx), dy(_dy), width(_width), height(_height)
  {
  }

  // Basically operate by shifting the world so that the base object lies in
  // the lower-right corner, then working from there.
  inline bool operator()(const childlist::iterator &a,
			 const childlist::iterator &b)
  {
    int aminx=a->col_start-base.col_start-base.col_span;
    if(aminx<0)
      aminx+=width;

    int aminy=a->row_start-base.row_start-base.row_span;
    if(aminy<0)
      aminy+=height;

    int bminx=b->col_start-base.col_start-base.col_span;
    if(bminx<0)
      bminx+=width;

    int bminy=b->row_start-base.row_start-base.row_span;
    if(bminy<0)
      bminy+=height;

    int amaxy=a->row_start+a->row_span-base.row_start-base.row_span-1;
    if(amaxy<0)
      amaxy+=height;

    int bmaxy=b->row_start+b->row_span-base.row_start-base.row_span-1;
    if(bmaxy<0)
      bmaxy+=height;

    int amaxx=a->col_start+a->col_span-base.col_start-base.col_span-1;
    if(amaxx<0)
      amaxx+=width;

    int bmaxx=b->col_start+b->col_span-base.col_start-base.col_span-1;
    if(bmaxx<0)
      bmaxx+=width;

    if(dy==0)
      {
	if(dx>0)
	  {
	    if(aminx<bminx)
	      return true;
	    else if(aminx>bminx)
	      return false;
	  }
	else
	  {
	    if(bmaxx<amaxx)
	      return true;
	    else if(bmaxx>amaxx)
	      return false;
	  }

	int besty=(height-base.row_span)/2;

	int adiff=abs((aminy+amaxy)/2-besty);
	int bdiff=abs((bminy+bmaxy)/2-besty);

	if(adiff<bdiff)
	  return true;
	else if(adiff>bdiff)
	  return false;

	// ERRRR, they're exactly the same?

	return false;
      }
    else
      {
	if(dy>0)
	  {
	    if(aminy<bminy)
	      return true;
	    else if(aminy>bminy)
	      return false;
	  }
	else
	  {
	    if(bmaxy<amaxy)
	      return true;
	    else if(bmaxy>amaxy)
	      return false;
	  }

	int bestx=(width-base.col_span)/2;

	int adiff=abs((aminx+amaxx)/2-bestx);
	int bdiff=abs((bminx+bmaxx)/2-bestx);

	if(adiff<bdiff)
	  return true;
	else if(adiff>bdiff)
	  return false;

	return false;
      }

  }
};

// a simple predicate--separated from the function below to keep
// find_best_focus's high-level logic as clean as possible.
// Checks whether the given child lies within the "shadow" of the given
// base widget in the given direction.

// There are two cases to worry about: either the "beginning" of the child is
// within the base, or it isn't.
// If it is, we can determine that the child does overlap the base in that
// dimension.
// If it is not, and it lies before the "beginning" of the base, we have to
// check its ending.  If its ending lies after the "beginning" of the base,
// there is (again) clearly overlap.  Otherwise, there is not.
// If it is not, and it lies after the "ending" of the base, there is no
// overlap.

inline bool vs_table::lies_on_axis(const child_info &base,
				   bool horizontal,
				   const child_info &c)
{
  if(horizontal)
    return (c.row_start<=base.row_start && c.row_start+c.row_span-1>=base.row_start) ||
      (c.row_start>=base.row_start && c.row_start<=base.row_start+base.row_span-1);
  else
    return (c.col_start<=base.col_start && c.col_start+c.col_span-1>=base.col_start) ||
      (c.col_start>=base.col_start && c.col_start<=base.col_start+base.col_span-1);
}

// FIXME: either dx or dy must be 0; only their signs are checked..
vs_table::childlist::iterator vs_table::find_best_focus(childlist::iterator start,
							int dx,
							int dy)
{
  eassert(start!=children.end());
  eassert(dx==0 || dy==0);
  eassert(!(dx==dy));

  list<childlist::iterator> sorted_children;

  for(childlist::iterator i=children.begin();
      i!=children.end();
      ++i)
    if(i!=start && i->w->get_visible() &&
       i->w->focus_me() && lies_on_axis(*start, (dy==0), *i))
       sorted_children.push_back(i);

  if(sorted_children.size()==0)
    return start;

  sorted_children.sort(better_fit(*start, dx, dy, num_cols, num_rows));

  return sorted_children.front();
}

bool vs_table::handle_key(const key &k)
{
  vs_widget_ref tmpref(this);

  if(focus!=children.end())
    {
      vs_widget_ref w = focus->w;

      if(w->dispatch_key(k))
	return true;
      else if(bindings->key_matches(k, "Cycle"))
	{
	  childlist::iterator oldfocus=focus;

	  focus++;

	  while(focus!=children.end() &&
		!(focus->w->get_visible() && focus->w->focus_me()))
	    focus++;

	  if(focus==children.end())
	    {
	      focus=children.begin();
	      while(focus!=children.end() &&
		    !(focus->w->get_visible() && focus->w->focus_me()))
		focus++;
	    }

	  if(focus!=children.end() && focus!=oldfocus)
	    {
	      if(get_isfocussed())
		{
		  oldfocus->w->unfocussed();
		  focus->w->focussed();
		}
	      vscreen_updatecursor();
	    }

	  return focus!=oldfocus;
	}
      else if(bindings->key_matches(k, "Left"))
	{
	  childlist::iterator oldfocus=focus;

	  focus=find_best_focus(focus, -1, 0);

	  if(focus!=children.end() && focus!=oldfocus)
	    {
	      if(get_isfocussed())
		{
		  oldfocus->w->unfocussed();
		  focus->w->focussed();
		}
	      vscreen_updatecursor();
	    }

	  return focus!=oldfocus;
	}
      else if(bindings->key_matches(k, "Right"))
	{
	  childlist::iterator oldfocus=focus;

	  focus=find_best_focus(focus, 1, 0);

	  if(focus!=children.end() && focus!=oldfocus)
	    {
	      if(get_isfocussed())
		{
		  oldfocus->w->unfocussed();
		  focus->w->focussed();
		}
	      vscreen_updatecursor();
	    }

	  return focus!=oldfocus;
	}
      else if(bindings->key_matches(k, "Up"))
	{
	  childlist::iterator oldfocus=focus;

	  focus=find_best_focus(focus, 0, -1);

	  if(focus!=children.end() && focus!=oldfocus)
	    {
	      if(get_isfocussed())
		{
		  oldfocus->w->unfocussed();
		  focus->w->focussed();
		}
	      vscreen_updatecursor();
	    }

	  return focus!=oldfocus;
	}
      else if(bindings->key_matches(k, "Down"))
	{
	  childlist::iterator oldfocus=focus;

	  focus=find_best_focus(focus, 0, 1);

	  if(focus!=children.end() && focus!=oldfocus)
	    {
	      if(get_isfocussed())
		{
		  oldfocus->w->unfocussed();
		  focus->w->focussed();
		}
	      vscreen_updatecursor();
	    }

	  return focus!=oldfocus;
	}
      else
	return vs_passthrough::handle_key(k);
    }
  else
    return vs_passthrough::handle_key(k);
}

class vs_table::nrow_lt
{
public:
  inline bool operator()(const child_info *a,
			 const child_info *b)
  {
    return a->row_span<b->row_span;
  }
};

class vs_table::ncol_lt
{
public:
  inline bool operator()(const child_info *a,
			 const child_info *b)
  {
    return a->col_span<b->col_span;
  }
};

/** Allocate "ideal" widths to all widgets: make every widget as large
 *  as it wants and expand other widgets to accomodate.  This routine
 *  also calculates the width_request member of the child.
 *
 *  \param col_sizes a vector of size num_cols; the size of each
 *  column will be stored here.
 */
void vs_table::alloc_ideal_widths(vector<int> &col_sizes)
{
  vs_widget_ref tmpref(this);

  vector<bool> col_expandable(num_cols, false);
  vector<child_info *> sorted_children;

#ifdef DEBUG_TABLES
  fprintf(debug, "---------- Begin ideal width allocation for 0x%x (w=%d,h=%d) ----------\n", this, getmaxx(), getmaxy());
#endif

  for(vector<int>::iterator i=col_sizes.begin(); i!=col_sizes.end(); ++i)
    *i=0;

  for(childlist::iterator i=children.begin(); i!=children.end(); ++i)
    if(i->w->get_visible())
      sorted_children.push_back(&*i);

  // Sort the children according to how many columns they span
  sort(sorted_children.begin(), sorted_children.end(), ncol_lt());

  // Decide which columns to expand: first mark smaller widgets for
  // expansion; then, if a larger widget doesn't overlap any smaller
  // widget that's to be expanded, mark all of its cols for expansion.
  for(vector<child_info *>::const_iterator i=sorted_children.begin();
      i!=sorted_children.end(); ++i)
    if((*i)->expand_x)
      {
	bool expanded=false;

	for(int j=0; j<(*i)->col_span; ++j)
	  if(col_expandable[j+(*i)->col_start])
	    {
	      expanded=true;
	      break;
	    }

	if(!expanded)
	  for(int j=0; j<(*i)->col_span; ++j)
	    col_expandable[j+(*i)->col_start]=true;
      }

  // Now try to expand columns.
  for(vector<child_info *>::const_iterator i=sorted_children.begin();
      i!=sorted_children.end(); ++i)
    {
      // If this widget doesn't have enough space, we need to expand
      // some of the columns it spans.  Otherwise, figure out which
      // columns are expandable and try to expand them.  If none of
      // the columns are expandable, just expand each column that it
      // spans equally.

      int current_width=0;
      int n_expandable=0;

      for(int j=(*i)->col_start; j<(*i)->col_start+(*i)->col_span; ++j)
	{
	  current_width+=col_sizes[j];
	  if(col_expandable[j])
	    ++n_expandable;
	}

      if(n_expandable==0)
	n_expandable=(*i)->col_span;

      if(!(*i)->ignore_size_x)
	(*i)->request_w=(*i)->w->width_request();
      else
	(*i)->request_w=0;
      int shortfall=(*i)->request_w-current_width;
      if(shortfall>0)
	for(int j=(*i)->col_start; n_expandable>0; --n_expandable,++j)
	  {
	    int amt=shortfall/n_expandable;

	    col_sizes[j]+=amt;
	    shortfall-=amt;
	  }

#ifdef DEBUG_TABLES
      fprintf(debug, "Child at (%d,%d) requested %d columns\n",
	      (*i)->col_start, (*i)->row_start, (*i)->request_w, current_width);
#endif
    }

#ifdef DEBUG_TABLES
  fprintf(debug, "Column sizes after:");
  for(int i=0; i<num_rows; ++i)
    fprintf(debug, " %d", col_sizes[i]);
  fprintf(debug, "\n");

  fprintf(debug, "---------------- end ideal allocation for 0x%x -----------------\n\n", this);
#endif
}

/** Expand the widths of all columns.  The resulting widths will give
 *  each widget the amount of width it requested, and ensure that the
 *  widget occupies at least w columns.  If the widget occupies less
 *  than w columns prior to invoking this method, it will occupy at
 *  most w columns when the method terminates.
 *
 *  If no widget is expandable, then nothing will be expanded by this
 *  algorithm.
 *
 *  \param target_w the minimum width to occupy.
 *
 *  \param col_sizes a vector of length num_cols containing the
 *  current size of each column; will be modified to contain the new
 *  sizes after this algorithm runs.
 */
void vs_table::expand_widths(vector<int> &col_sizes, int target_w)
{
  vs_widget_ref tmpref(this);

  int current_width=accumulate(col_sizes.begin(), col_sizes.end(), 0);

  int shortfall=target_w-current_width;
  // If we're already "too big", don't bother doing anything.
  if(shortfall<=0)
    return;

#ifdef DEBUG_TABLES
  fprintf(debug, "**************** Expanding 0x%x (w=%d, h=%d) to %d columns ******************\n", this, getmaxx(), getmaxy(), target_w);
#endif

  // Note that this is redundant with the previous algorithm.  Merge?
  vector<child_info *> sorted_children;

  for(childlist::iterator i=children.begin(); i!=children.end(); ++i)
    if(i->w->get_visible() && i->expand_x)
      sorted_children.push_back(&*i);

  // Sort the children according to how many columns they span
  sort(sorted_children.begin(), sorted_children.end(), ncol_lt());


  // This is also redundant; merge?
  vector<bool> col_expandable(num_cols, false);

  int n_expandable=0;
  for(vector<child_info *>::const_iterator i=sorted_children.begin();
      i!=sorted_children.end(); ++i)
    {
      bool expanded=false;

      for(int j=0; j<(*i)->col_span; ++j)
	if(col_expandable[j+(*i)->col_start])
	  {
	    expanded=true;
	    break;
	  }

      if(!expanded)
	for(int j=0; j<(*i)->col_span; ++j)
	  col_expandable[j+(*i)->col_start]=true;
    }

  for(int i=0; i<num_cols; ++i)
    if(col_expandable[i])
      ++n_expandable;

#ifdef DEBUG_TABLES
  fprintf(debug, "Column sizes before:");
  for(int i=0; i<num_cols; ++i)
    fprintf(debug, " %d", col_sizes[i]);
  fprintf(debug, "\n");
#endif

  // Now expand columns from left to right.
  for(int i=0; i<num_cols && n_expandable>0; ++i)
    if(col_expandable[i])
      {
	int amt=shortfall/n_expandable;
	col_sizes[i]+=amt;
	shortfall-=amt;
	--n_expandable;
      }

#ifdef DEBUG_TABLES
  fprintf(debug, "Column sizes after:");
  for(int i=0; i<num_cols; ++i)
    fprintf(debug, " %d", col_sizes[i]);
  fprintf(debug, "\n");

  fprintf(debug, "********************* end expanding columns of 0x%x ********************\n\n", this);
#endif
}

/** Shrink the width of each column of the table to meet the given
 *  target width.  If the table is already small enough, no action
 *  will be taken.  In the worst case, some widgets may be shrunk to
 *  invisibility.
 *
 *  \param col_sizes a vector of (proposed) sizes of the table; will
 *  be modified to hold the newly shrunk sizes by this method.
 *  \param target_w how small the table should be after shrinkage.
 */
void vs_table::shrink_widths(vector<int> &col_sizes, int target_w)
{
  vs_widget_ref tmpref(this);

  vector<bool> col_shrinkable(num_cols, false);
  int n_shrinkable=0;
  int current_width=accumulate(col_sizes.begin(), col_sizes.end(), 0);
  int overflow=current_width-target_w;

  if(overflow<=0)
    return;

#ifdef DEBUG_TABLES
  fprintf(debug, "++++++++++ Shrinking columns of 0x%x (w=%d, h=%d) to %d ++++++++\n", this, getmaxx(), getmaxy(), target_w);
#endif

  for(int i=0; i<num_cols; ++i)
    col_shrinkable[i]=(col_sizes[i]>1);

  for(childlist::const_iterator i=children.begin(); i!=children.end(); ++i)
    {
      if(i->w->get_visible() && !i->shrink_x)
	{
	  for(int j=0; j<i->col_span; ++j)
	    col_shrinkable[j+i->col_start]=false;
	}
    }

  for(int i=0; i<num_cols; ++i)
    if(col_shrinkable[i])
      ++n_shrinkable;

#ifdef DEBUG_TABLES
  fprintf(debug, "Column sizes before:");
  for(int i=0; i<num_cols; ++i)
    fprintf(debug, " %d", col_sizes[i]);
  fprintf(debug, "\n");
#endif

  while(n_shrinkable>0 && overflow>0)
    {
      int toshrink=n_shrinkable;

      // Actually try to shrink stuff.
      for(int i=0; i<num_cols && toshrink>0; ++i)
	if(col_shrinkable[i])
	  {
	    int amt=min(overflow/toshrink, col_sizes[i]-1);

	    col_sizes[i]-=amt;
	    overflow-=amt;
	    --toshrink;

	    if(col_sizes[i]<=1)
	      {
		col_shrinkable[i]=false;
		--n_shrinkable;
	      }
	  }
    }

  // It was impossible to shrink enough widgets; just clip the end of
  // the table off.
  if(overflow>0)
    {
      for(int i=num_cols-1; i>=0 && overflow>0; --i)
	{
	  int amt=min(overflow, col_sizes[i]);

	  col_sizes[i]-=amt;
	  overflow-=amt;
	}

      eassert(overflow==0);
    }

#ifdef DEBUG_TABLES
  fprintf(debug, "Column sizes after:");
  for(int i=0; i<num_cols; ++i)
    fprintf(debug, " %d", col_sizes[i]);
  fprintf(debug, "\n");

  fprintf(debug, "++++++++++++++++++++++++ end shrinking 0x%x +++++++++++++++++++++\n\n", this);
#endif
}

/** Allocate "ideal" heights to all widgets: make every widget as
 *  large as it wants and expand other widgets to accomodate.  This
 *  routine also calculates the height_request member of the child.
 *
 *  \param row_sizes a vector of length num_rows; the size of each row
 *  will be stored here.
 *
 *  \param col_sizes a vector of length num_cols containing the size
 *  of each column.
 */
void vs_table::alloc_ideal_heights(vector<int> &row_sizes,
				   const vector<int> &col_sizes)
{
  vs_widget_ref tmpref(this);

  vector<bool> row_expandable(num_rows, false);
  vector<child_info *> sorted_children;

#ifdef DEBUG_TABLES
  fprintf(debug, "---------- Begin ideal height allocation for 0x%x (w=%d,h=%d) ----------\n", this, getmaxx(), getmaxy());
#endif

  for(vector<int>::iterator i=row_sizes.begin(); i!=row_sizes.end(); ++i)
    *i=0;

  for(childlist::iterator i=children.begin(); i!=children.end(); ++i)
    if(i->w->get_visible())
      sorted_children.push_back(&*i);

  // Sort the children according to how many rows they span
  sort(sorted_children.begin(), sorted_children.end(), nrow_lt());

  // Decide which rows to expand: first mark smaller widgets for
  // expansion; then, if a larger widget doesn't overlap any smaller
  // widget that's to be expanded, mark all of its rows for expansion.
  for(vector<child_info *>::const_iterator i=sorted_children.begin();
      i!=sorted_children.end(); ++i)
    if((*i)->expand_y)
      {
	bool expanded=false;

	for(int j=0; j<(*i)->row_span; ++j)
	  if(row_expandable[j+(*i)->row_start])
	    {
	      expanded=true;
	      break;
	    }

	if(!expanded)
	  for(int j=0; j<(*i)->row_span; ++j)
	    row_expandable[j+(*i)->row_start]=true;
      }

  // Now try to expand rows.
  for(vector<child_info *>::const_iterator i=sorted_children.begin();
      i!=sorted_children.end(); ++i)
    {
      // If this widget doesn't have enough space, we need to expand
      // some of the rows it spans.  Otherwise, figure out which
      // rows are expandable and try to expand them.  If none of
      // the rows are expandable, just expand each row that it
      // spans equally.

      int current_width=0;
      for(int j=(*i)->col_start; j<(*i)->col_start+(*i)->col_span; ++j)
	current_width+=col_sizes[j];

      int current_height=0;
      int n_expandable=0;

      for(int j=(*i)->row_start; j<(*i)->row_start+(*i)->row_span; ++j)
	{
	  current_height+=row_sizes[j];
	  if(row_expandable[j])
	    ++n_expandable;
	}

      if(n_expandable==0)
	n_expandable=(*i)->row_span;

      if(!(*i)->ignore_size_y)
	(*i)->request_h=(*i)->w->height_request(current_width);
      else
	(*i)->request_h=0;
      int shortfall=(*i)->request_h-current_height;

      if(shortfall>0)
	for(int j=(*i)->row_start; n_expandable>0; --n_expandable,++j)
	  {
	    int amt=shortfall/n_expandable;

	    row_sizes[j]+=amt;
	    shortfall-=amt;
	  }

#ifdef DEBUG_TABLES
      fprintf(debug, "Child at (%d,%d) requested %d rows for %d columns\n",
	      (*i)->col_start, (*i)->row_start, (*i)->request_h, current_width);
#endif
    }

#ifdef DEBUG_TABLES
  fprintf(debug, "Row sizes after:");
  for(int i=0; i<num_rows; ++i)
    fprintf(debug, " %d", row_sizes[i]);
  fprintf(debug, "\n");

  fprintf(debug, "---------------- end ideal allocation for 0x%x -----------------\n\n", this);
#endif
}

/** Expand the heights of all rows.  The resulting heights will give
 *  each widget the amount of height it requested, and try to ensure
 *  that the widget occupies at least w rows.  If the widget occupies
 *  less than w rows prior to invoking this method, it will occupy at
 *  most w rows when the method terminates.
 *
 *  If no widget is expandable, then nothing will be expanded by this
 *  algorithm.
 *
 *  \param target_h the minimum height to occupy.
 *
 *  \param row_sizes a vector of length num_rows containing the
 *  current size of each row; will be modified to contain the new
 *  sizes after this algorithm runs.
 */
void vs_table::expand_heights(vector<int> &row_sizes, int target_h)
{
  vs_widget_ref tmpref(this);

  int current_height=accumulate(row_sizes.begin(), row_sizes.end(), 0);

  int shortfall=target_h-current_height;
  // If we're already "too big", don't bother doing anything.
  if(shortfall<=0)
    return;

#ifdef DEBUG_TABLES
  fprintf(debug, "**************** Expanding 0x%x (w=%d, h=%d) to %d rows ******************\n", this, getmaxx(), getmaxy(), target_h);
#endif

  // Note that this is redundant with the previous algorithm.  Merge?
  vector<child_info *> sorted_children;

  for(childlist::iterator i=children.begin(); i!=children.end(); ++i)
    if(i->w->get_visible() && i->expand_y)
      sorted_children.push_back(&*i);

  // Sort the children according to how many rows they span
  sort(sorted_children.begin(), sorted_children.end(), nrow_lt());


  // This is also redundant; merge?
  vector<bool> row_expandable(num_rows, false);

  int n_expandable=0;
  for(vector<child_info *>::const_iterator i=sorted_children.begin();
      i!=sorted_children.end(); ++i)
    {
      bool expanded=false;

      for(int j=0; j<(*i)->row_span; ++j)
	if(row_expandable[j+(*i)->row_start])
	  {
	    expanded=true;
	    break;
	  }

      if(!expanded)
	for(int j=0; j<(*i)->row_span; ++j)
	  row_expandable[j+(*i)->row_start]=true;
    }

  for(int i=0; i<num_rows; ++i)
    if(row_expandable[i])
      ++n_expandable;

#ifdef DEBUG_TABLES
  fprintf(debug, "Row sizes before:");
  for(int i=0; i<num_rows; ++i)
    fprintf(debug, " %d", row_sizes[i]);
  fprintf(debug, "\n");
#endif

  // Now expand rows from left to right.
  for(int i=0; i<num_rows && n_expandable>0; ++i)
    if(row_expandable[i])
      {
	int amt=shortfall/n_expandable;
	row_sizes[i]+=amt;
	shortfall-=amt;
	--n_expandable;
      }

#ifdef DEBUG_TABLES
  fprintf(debug, "Row sizes after:");
  for(int i=0; i<num_rows; ++i)
    fprintf(debug, " %d", row_sizes[i]);
  fprintf(debug, "\n");

  fprintf(debug, "********************* end expanding rows of 0x%x ********************\n\n", this);
#endif
}

/** Shrink the height of each row of the table to meet the given
 *  target height.  If the table is already small enough, no action
 *  will be taken.  In the worst case, some widgets may be shrunk to
 *  invisibility.
 *
 *  \param row_sizes a vector of (proposed) sizes of the table; will
 *  be modified to hold the newly shrunk sizes by this method.
 *  \param target_h how small the table should be after shrinkage.
 */
void vs_table::shrink_heights(vector<int> &row_sizes, int target_h)
{
  vs_widget_ref tmpref(this);

  vector<bool> row_shrinkable(num_rows, false);
  int n_shrinkable=0;
  int current_height=accumulate(row_sizes.begin(), row_sizes.end(), 0);
  int overflow=current_height-target_h;

  if(overflow<=0)
    return;

#ifdef DEBUG_TABLES
  fprintf(debug, "++++++++++ Shrinking rows of 0x%x (w=%d, h=%d) to %d ++++++++\n", this, getmaxx(), getmaxy(), target_h);
#endif

  for(int i=0; i<num_rows; ++i)
    row_shrinkable[i]=(row_sizes[i]>1);

  for(childlist::const_iterator i=children.begin(); i!=children.end(); ++i)
    {
      if(i->w->get_visible() && !i->shrink_y)
	{
	  for(int j=0; j<i->row_span; ++j)
	    row_shrinkable[j+i->row_start]=false;
	}
    }

  for(int i=0; i<num_rows; ++i)
    if(row_shrinkable[i])
      ++n_shrinkable;

#ifdef DEBUG_TABLES
  fprintf(debug, "Row sizes before:");
  for(int i=0; i<num_rows; ++i)
    fprintf(debug, " %d", row_sizes[i]);
  fprintf(debug, "\n");
#endif

  while(n_shrinkable>0 && overflow>0)
    {
      int toshrink=n_shrinkable;

      // Actually try to shrink stuff.
      for(int i=0; i<num_rows && toshrink>0; ++i)
	if(row_shrinkable[i])
	  {
	    int amt=min(overflow/toshrink, row_sizes[i]-1);

	    row_sizes[i]-=amt;
	    overflow-=amt;
	    --toshrink;

	    if(row_sizes[i]<=1)
	      {
		row_shrinkable[i]=false;
		--n_shrinkable;
	      }
	  }
    }

  // It was impossible to shrink enough widgets; just clip the end of
  // the table off.
  if(overflow>0)
    {
      for(int i=num_rows-1; i>=0 && overflow>0; --i)
	{
	  int amt=min(overflow, row_sizes[i]);

	  row_sizes[i]-=amt;
	  overflow-=amt;
	}

      eassert(overflow==0);
    }

#ifdef DEBUG_TABLES
  fprintf(debug, "Row sizes after:");
  for(int i=0; i<num_rows; ++i)
    fprintf(debug, " %d", row_sizes[i]);
  fprintf(debug, "\n");

  fprintf(debug, "++++++++++++++++++++++++ end shrinking 0x%x +++++++++++++++++++++\n\n", this);
#endif
}

/** Uses the given column and row sizes to allocate space to all child
 *  widgets.  Assumes that request_* entries are filled in
 *  appropriately, to avoid re-executing size request methods.
 *
 *  \param col_sizes a vector of length num_cols representing the
 *  width of each column.
 *
 *  \param row_sizes a vector of length num_rows representing the
 *  height of each row.
 */
void vs_table::alloc_child_sizes(const vector<int> &col_sizes,
				 const vector<int> &row_sizes)
{
  vs_widget_ref tmpref(this);

  vector<int> col_starts(num_cols), row_starts(num_rows);

  // partial_sum almost works...but not quite.
  int startx=0;
  for(int i=0; i<num_cols; ++i)
    {
      col_starts[i]=startx;
      startx+=col_sizes[i];
    }

  int starty=0;
  for(int i=0; i<num_rows; ++i)
    {
      row_starts[i]=starty;
      starty+=row_sizes[i];
    }

  for(childlist::iterator i=children.begin(); i!=children.end(); ++i)
    if(i->w->get_visible())
      {
	int x=col_starts[i->col_start];
	int y=row_starts[i->row_start];
	int width=0, height=0;

	for(int j=0; j<i->col_span; ++j)
	  width+=col_sizes[j+i->col_start];

	for(int j=0; j<i->row_span; ++j)
	  height+=row_sizes[j+i->row_start];

	eassert(x+width<=getmaxx());
	eassert(y+height<=getmaxy());

	// If the widget can't be filled and it was allocated too much
	// space, make sure it's aligned in the space:

	if(width>i->request_w && !i->fill_x)
	  {
	    if(i->align_left_x && i->align_right_x)
	      x+=(width-i->request_w)/2;
	    else if(i->align_right_x)
	      x+=(width-i->request_w);

	    width=i->request_w;
	  }

	if(height>i->request_h && !i->fill_y)
	  {
	    if(i->align_left_y && i->align_right_y)
	      y+=(height-i->request_h)/2;
	    else if(i->align_right_y)
	      y+=(height-i->request_h);

	    height=i->request_h;
	  }

	i->w->alloc_size(x, y, width, height);
      }
}

// Returns the sum of the ideal column widths; doesn't save anything.
int vs_table::width_request()
{
  vs_widget_ref tmpref(this);

  vector<int> col_sizes(num_cols);

  alloc_ideal_widths(col_sizes);

  return accumulate(col_sizes.begin(), col_sizes.end(), 0);
}

// Allocates provisional widths for all widgets; doesn't save
// anything.
int vs_table::height_request(int w)
{
  vs_widget_ref tmpref(this);

  vector<int> col_sizes(num_cols), row_sizes(num_rows);

  alloc_ideal_widths(col_sizes);
  expand_widths(col_sizes, w);
  shrink_widths(col_sizes, w);

  alloc_ideal_heights(row_sizes, col_sizes);

  return accumulate(row_sizes.begin(), row_sizes.end(), 0);
}

void vs_table::layout_me()
{
  vs_widget_ref tmpref(this);

  get_focus();

  if(get_win())
    {
      int w=getmaxx(), h=getmaxy();

      vector<int> col_sizes(num_cols), row_sizes(num_rows);

      alloc_ideal_widths(col_sizes);
      expand_widths(col_sizes, w);
      shrink_widths(col_sizes, w);

      alloc_ideal_heights(row_sizes, col_sizes);
      expand_heights(row_sizes, h);
      shrink_heights(row_sizes, h);

      alloc_child_sizes(col_sizes, row_sizes);
    }
  else
    for(childlist::iterator i=children.begin(); i!=children.end(); ++i)
      i->w->alloc_size(0, 0, 0, 0);
}

void vs_table::paint(const style &st)
{
  vs_widget_ref tmpref(this);

  for(childlist::iterator i=children.begin(); i!=children.end(); ++i)
    if(i->w->get_visible())
      i->w->display(st);
}

void vs_table::dispatch_mouse(short id, int x, int y, int z, mmask_t bstate)
{
  vs_widget_ref tmpref(this);

  for(childlist::iterator i=children.begin(); i!=children.end(); ++i)
    {
      vs_widget_ref w = i->w;

      if(w->get_visible() && w->enclose(y, x))
	{
	  if(w->focus_me())
	    focus_widget(w);

	  w->dispatch_mouse(id, x-w->get_startx(), y-w->get_starty(),
			    z, bstate);
	  return;
	}
    }
}

void vs_table::init_bindings()
{
  bindings=new keybindings(&global_bindings);
}
