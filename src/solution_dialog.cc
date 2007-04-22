// solution_dialog.cc
//
//   Copyright (C) 2005, 2007 Daniel Burrows
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

#include "solution_dialog.h"
#include "solution_fragment.h"
#include "ui.h"

#include <aptitude.h>

#include <vscreen/config/colors.h>
#include <vscreen/config/keybindings.h>
#include <vscreen/fragment.h>
#include <vscreen/vs_button.h>
#include <vscreen/vs_center.h>
#include <vscreen/vs_frame.h>
#include <vscreen/vs_scrollbar.h>
#include <vscreen/vs_table.h>
#include <vscreen/vs_text_layout.h>
#include <vscreen/vscreen.h>

#include <generic/apt/apt.h>
#include <generic/apt/aptitude_resolver_universe.h>
#include <generic/apt/resolver_manager.h>

#include <generic/problemresolver/exceptions.h>
#include <generic/problemresolver/solution.h>

#include <sigc++/bind.h>

typedef generic_solution<aptitude_universe> aptitude_solution;

class solution_dialog:public vs_text_layout
{
  aptitude_solution last_sol;

  void handle_cache_reload()
  {
    if(apt_cache_file)
      resman->state_changed.connect(sigc::mem_fun(*this, &solution_dialog::post_update));

    update();
  }

  void post_update()
  {
    vscreen_post_event(new slot_event(sigc::mem_fun(this, &solution_dialog::update)));
  }

protected:
  solution_dialog()
  {
    if(apt_cache_file)
      resman->state_changed.connect(sigc::mem_fun(*this, &solution_dialog::post_update));

    cache_closed.connect(sigc::mem_fun(*this, &solution_dialog::update));
    cache_reloaded.connect(sigc::mem_fun(*this, &solution_dialog::handle_cache_reload));

    update();
  }

public:
  static ref_ptr<solution_dialog> create()
  {
    ref_ptr<solution_dialog> rval(new solution_dialog);
    rval->decref();
    return rval;
  }

  void update()
  {
    vs_widget_ref tmpref(this);

    if(!apt_cache_file)
      {
	set_fragment(fragf("%s", _("The package cache is not available.")));
	last_sol.nullify();
	return;
      }

    if(!resman->resolver_exists())
      {
	// This makes ASS-U-MPTIONS about how resolver_exists works.
	set_fragment(fragf("%s", _("No packages are broken.")));
	last_sol.nullify();
	return;
      }

    resolver_manager::state state = resman->state_snapshot();

    if(state.solutions_exhausted && state.generated_solutions == 0)
      {
	set_fragment(fragf("%s", _("No resolution found.")));
	return;
      }

    if(state.selected_solution >= state.generated_solutions)
      {
	if(state.background_thread_aborted)
	  {
	    set_fragment(fragf("%s", state.background_thread_abort_msg.c_str()));
	    return;
	  }
	else
	  {
	    set_fragment(fragf("%s", _("Resolving dependencies...")));
	    return;
	  }
      }

    aptitude_solution sol = resman->get_solution(state.selected_solution, 0);

    if(sol == last_sol)
      return;

    last_sol=sol;

    if(sol.get_actions().empty())
      set_fragment(fragf("%s", _("Internal error: unexpected null solution.")));
    else
      set_fragment(solution_fragment(sol));
  }
};

typedef ref_ptr<solution_dialog> solution_dialog_ref;

static void do_apply(vscreen_widget &wBare)
{
  vs_widget_ref w(&wBare);

  do_apply_solution();
  w->destroy();
}

vs_widget_ref make_solution_dialog()
{
  vs_table_ref t=vs_table::create();
  vs_widget_ref rval=vs_center::create(vs_frame::create(t));

  vs_text_layout_ref display=solution_dialog::create();
  vs_scrollbar_ref scrl=vs_scrollbar::create(vs_scrollbar::VERTICAL);

  display->location_changed.connect(sigc::mem_fun(scrl.unsafe_get_ref(), &vs_scrollbar::set_slider));
  scrl->scrollbar_interaction.connect(sigc::mem_fun(display.unsafe_get_ref(), &vs_text_layout::scroll));

  t->add_widget_opts(display,
		     0, 0, 1, 1,
		     vs_table::EXPAND | vs_table::FILL | vs_table::SHRINK,
		     vs_table::EXPAND | vs_table::FILL | vs_table::SHRINK);

  t->add_widget_opts(scrl,
		     0, 1, 1, 1,
		     vs_table::ALIGN_RIGHT,
		     vs_table::ALIGN_CENTER | vs_table::FILL | vs_table::SHRINK);

  vs_table_ref bt=vs_table::create();

  //t->set_bg(get_color("DefaultWidgetBackground")|A_REVERSE);

  // TODO: for dialogs like this, I really should have support for
  // "wrapping" lines of buttons if they get too long, like fragments.
  vs_button_ref bprev  = vs_button::create(_("Previous"));
  vs_button_ref bnext  = vs_button::create(_("Next"));
  vs_button_ref bapply = vs_button::create(_("Apply"));
  vs_button_ref bclose = vs_button::create(_("Close"));

  bprev->pressed.connect(sigc::ptr_fun(do_previous_solution));
  bnext->pressed.connect(sigc::ptr_fun(do_next_solution));
  bapply->pressed.connect(sigc::bind(sigc::ptr_fun(do_apply),
				     rval.weak_ref()));
  bclose->pressed.connect(sigc::mem_fun(rval.unsafe_get_ref(),
					&vscreen_widget::destroy));

  rval->connect_key("ApplySolution", &global_bindings,
		    sigc::bind(sigc::ptr_fun(do_apply),
			       rval.weak_ref()));

  bprev->set_bg_style(style_attrs_flip(A_REVERSE));
  bnext->set_bg_style(style_attrs_flip(A_REVERSE));
  bapply->set_bg_style(style_attrs_flip(A_REVERSE));
  bclose->set_bg_style(style_attrs_flip(A_REVERSE));

  bt->add_widget_opts(bprev,
		     0, 0, 1, 1,
		     vs_table::FILL | vs_table::EXPAND, vs_table::FILL);
  bt->add_widget_opts(bnext,
		     0, 1, 1, 1,
		     vs_table::FILL | vs_table::EXPAND, vs_table::FILL);
  bt->add_widget_opts(bapply,
		     0, 2, 1, 1,
		     vs_table::FILL | vs_table::EXPAND, vs_table::FILL);
  bt->add_widget_opts(bclose,
		     0, 3, 1, 1,
		     vs_table::FILL | vs_table::EXPAND, vs_table::FILL);

  t->add_widget_opts(bt, 1, 0, 1, 2,
		     vs_table::FILL | vs_table::EXPAND | vs_table::SHRINK,
		     vs_table::FILL);

  return rval;
}
