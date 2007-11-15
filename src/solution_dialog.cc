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

#include <cwidget/config/colors.h>
#include <cwidget/config/keybindings.h>
#include <cwidget/fragment.h>
#include <cwidget/widgets/button.h>
#include <cwidget/widgets/center.h>
#include <cwidget/widgets/frame.h>
#include <cwidget/widgets/scrollbar.h>
#include <cwidget/widgets/table.h>
#include <cwidget/widgets/text_layout.h>
#include <cwidget/toplevel.h>

#include <generic/apt/apt.h>
#include <generic/apt/aptitude_resolver_universe.h>
#include <generic/apt/resolver_manager.h>

#include <generic/problemresolver/exceptions.h>
#include <generic/problemresolver/solution.h>

#include <sigc++/bind.h>

typedef generic_solution<aptitude_universe> aptitude_solution;

class solution_dialog:public widgets::text_layout
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
    toplevel::post_event(new slot_event(sigc::mem_fun(this, &solution_dialog::update)));
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
    widgets::widget_ref tmpref(this);

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

static void do_apply(cwidget::widgets::widget &wBare)
{
  widgets::widget_ref w(&wBare);

  do_apply_solution();
  w->destroy();
}

widgets::widget_ref make_solution_dialog()
{
  widgets::table_ref t=widgets::table::create();
  widgets::widget_ref rval=widgets::center::create(widgets::frame::create(t));

  widgets::text_layout_ref display=solution_dialog::create();
  widgets::scrollbar_ref scrl=widgets::scrollbar::create(widgets::scrollbar::VERTICAL);

  display->location_changed.connect(sigc::mem_fun(scrl.unsafe_get_ref(), &widgets::scrollbar::set_slider));
  scrl->scrollbar_interaction.connect(sigc::mem_fun(display.unsafe_get_ref(), &widgets::text_layout::scroll));

  t->add_widget_opts(display,
		     0, 0, 1, 1,
		     widgets::table::EXPAND | widgets::table::FILL | widgets::table::SHRINK,
		     widgets::table::EXPAND | widgets::table::FILL | widgets::table::SHRINK);

  t->add_widget_opts(scrl,
		     0, 1, 1, 1,
		     widgets::table::ALIGN_RIGHT,
		     widgets::table::ALIGN_CENTER | widgets::table::FILL | widgets::table::SHRINK);

  widgets::table_ref bt=widgets::table::create();

  //t->set_bg(get_color("DefaultWidgetBackground")|A_REVERSE);

  // TODO: for dialogs like this, I really should have support for
  // "wrapping" lines of buttons if they get too long, like fragments.
  widgets::button_ref bprev  = widgets::button::create(_("Previous"));
  widgets::button_ref bnext  = widgets::button::create(_("Next"));
  widgets::button_ref bapply = widgets::button::create(_("Apply"));
  widgets::button_ref bclose = widgets::button::create(_("Close"));

  bprev->pressed.connect(sigc::ptr_fun(do_previous_solution));
  bnext->pressed.connect(sigc::ptr_fun(do_next_solution));
  bapply->pressed.connect(sigc::bind(sigc::ptr_fun(do_apply),
				     rval.weak_ref()));
  bclose->pressed.connect(sigc::mem_fun(rval.unsafe_get_ref(),
					&cwidget::widgets::widget::destroy));

  rval->connect_key("ApplySolution", &cw::config::global_bindings,
		    sigc::bind(sigc::ptr_fun(do_apply),
			       rval.weak_ref()));

  bprev->set_bg_style(style_attrs_flip(A_REVERSE));
  bnext->set_bg_style(style_attrs_flip(A_REVERSE));
  bapply->set_bg_style(style_attrs_flip(A_REVERSE));
  bclose->set_bg_style(style_attrs_flip(A_REVERSE));

  bt->add_widget_opts(bprev,
		     0, 0, 1, 1,
		     widgets::table::FILL | widgets::table::EXPAND, widgets::table::FILL);
  bt->add_widget_opts(bnext,
		     0, 1, 1, 1,
		     widgets::table::FILL | widgets::table::EXPAND, widgets::table::FILL);
  bt->add_widget_opts(bapply,
		     0, 2, 1, 1,
		     widgets::table::FILL | widgets::table::EXPAND, widgets::table::FILL);
  bt->add_widget_opts(bclose,
		     0, 3, 1, 1,
		     widgets::table::FILL | widgets::table::EXPAND, widgets::table::FILL);

  t->add_widget_opts(bt, 1, 0, 1, 2,
		     widgets::table::FILL | widgets::table::EXPAND | widgets::table::SHRINK,
		     widgets::table::FILL);

  return rval;
}
