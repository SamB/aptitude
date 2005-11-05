// pkg_view.cc
//
//  Copyright 2000-2005 Daniel Burrows
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

#include "pkg_view.h"

#include "aptitude.h"

#include "desc_parse.h"
#include "edit_pkg_hier.h"
#include "menu_redirect.h"
#include "pkg_columnizer.h"
#include "reason_fragment.h"
#include "trust.h"
#include "ui.h"

#include <vscreen/fragment.h>
#include <vscreen/vs_label.h>
#include <vscreen/vs_multiplex.h>
#include <vscreen/vs_scrollbar.h>
#include <vscreen/vs_table.h>
#include <vscreen/vs_text_layout.h>
#include <vscreen/vscreen_widget.h>
#include <vscreen/config/keybindings.h>
#include <vscreen/transcode.h>

#include <generic/apt/apt.h>
#include <generic/apt/config_signal.h>

#include <apt-pkg/error.h>
#include <apt-pkg/pkgrecords.h>

#include <sigc++/adaptors/bind.h>
#include <sigc++/functors/mem_fun.h>
#include <sigc++/functors/ptr_fun.h>

#include <ctype.h>

#include <string>

using namespace std;

class pkg_handling_label:public vs_label
{
  column_definition_list *columns;

  bool have_pkg;
  pkgCache::PkgIterator pkg;
  pkgCache::VerIterator ver;

  void zap_package()
  {
    have_pkg=false;
  }

protected:
  pkg_handling_label(column_definition_list *_columns)
    :vs_label(" "), columns(_columns), have_pkg(false)
  {
    cache_closed.connect(sigc::mem_fun(*this, &pkg_handling_label::zap_package));
  }

public:
  static ref_ptr<pkg_handling_label>
  create(column_definition_list *columns)
  {
    ref_ptr<pkg_handling_label> rval(new pkg_handling_label(columns));
    rval->decref();
    return rval;
  }

  ~pkg_handling_label() {delete columns;}

  size size_request() {return size(1,1);}

  void set_columns(column_definition_list *_columns)
  {
    delete columns;
    columns=_columns;
    vscreen_update();
  }

  void do_columnify(const pkgCache::PkgIterator &_pkg,
		    const pkgCache::VerIterator &_ver)
  {
    pkg=_pkg;
    ver=_ver;

    have_pkg=!pkg.end();

    vscreen_update();
  }

  void paint(const style &st)
  {
    vs_widget_ref tmpref(this);

    if(apt_cache_file)
      {
	// Needed to initialize translated widths and stuff.
	pkg_item::pkg_columnizer::setup_columns();

	if(!have_pkg)
	  {
	    pkg=pkgCache::PkgIterator();
	    // Reinitialize it all the time to avoid the "magic autochanging
	    // pointers" bug.
	    ver=pkgCache::VerIterator(*apt_cache_file);
	  }

	empty_column_parameters p;
	set_text(pkg_item::pkg_columnizer(pkg, ver, *columns, 0).layout_columns(getmaxx(), p));
      }
    else
      set_text("");

    vs_label::paint(st);
  }
};

typedef ref_ptr<pkg_handling_label> pkg_handling_label_ref;

static void do_set_column_format(string key, string the_default,
				 pkg_handling_label &lBare)
{
  pkg_handling_label_ref l(&lBare);

  string format=aptcfg->Find(key, the_default.c_str());
  wstring wformat;

  column_definition_list *columns=NULL;

  if(!transcode(format.c_str(), wformat))
    _error->Errno("iconv", _("Couldn't transcode column definition"));
  else
    columns=parse_columns(wformat,
			  pkg_item::pkg_columnizer::parse_column_type,
			  pkg_item::pkg_columnizer::defaults);

  if(!columns)
    _error->Error(_("Couldn't parse column definition"));
  else
    l->set_columns(columns);
}

class pkg_description_widget:public vs_text_layout
{
  int start_line;
protected:
  pkg_description_widget()
  {
  }
public:
  static ref_ptr<pkg_description_widget> create()
  {
    ref_ptr<pkg_description_widget> rval(new pkg_description_widget);
    rval->decref();
    return rval;
  }

  void set_package(const pkgCache::PkgIterator &pkg,
		   const pkgCache::VerIterator &ver)
  {
    wstring newdesc;

    if(!pkg.end() && !ver.end())
      {
	if(!transcode(apt_package_records->Lookup(ver.FileList()).LongDesc(), newdesc))
	  {
	    if(!transcode(_("Encoding error in long description."), newdesc))
	      newdesc=L"Encoding error in long description.";
	  }
      }

    fragment *frag=make_desc_fragment(newdesc);

    fragment *tags=make_tags_fragment(pkg);
    if(tags != NULL)
      tags = fragf("%n%n%F", tags);
    else
      tags = fragf("");

    fragment *untrusted_frag;

    if(pkg.end() || ver.end())
      untrusted_frag=NULL;
    else
      untrusted_frag=make_untrusted_warning(ver);

    if(untrusted_frag == NULL)
      set_fragment(fragf("%F%F", frag, tags));
    else
      set_fragment(fragf("%F%n%F%F", untrusted_frag, frag, tags));
  }
};

typedef ref_ptr<pkg_description_widget> pkg_description_widget_ref;

// In order to properly dispatch line-up/down events to the sub-widgets,
// we need a meta-widget that knows about them.
//
// Note: this is not the most efficient way of doing things, but
// (a) it would be very error-prone to reproduce the multiplex's
//     behavior for this special case, and
// (b) only one of these is created per package view, so the overhead
//     of a few redundant pointers is acceptable.  In my opinion.
//
// This is still rather gross, and a better way would be nice.
class info_area_multiplex:public vs_multiplex
{
  vs_hier_editor_ref editor;
  pkg_description_widget_ref description;
  vs_table_ref description_table;
  vs_text_layout_ref reasons;
  vs_table_ref reasons_table;

  pkgCache::PkgIterator lastPkg;
  pkgCache::VerIterator lastVer;
  wstring lastDesc;

  /** True if the package had breakage the last time we checked. */
  bool hadBreakage;
  /** If the view was autoswitched to breakage reasons, this is set
   *  to the widget we switched away from; otherwise, it is \b NULL.
   */
  vs_widget_ref autoswitch;

protected:
  info_area_multiplex(const vs_hier_editor_ref &_editor,
		      const pkg_description_widget_ref &_description,
		      const vs_table_ref &_description_table,
		      const vs_text_layout_ref &_reasons,
		      const vs_table_ref &_reasons_table)
    :vs_multiplex(false),
     editor(_editor),
     description(_description), description_table(_description_table),
     reasons(_reasons), reasons_table(_reasons_table),
     hadBreakage(false), autoswitch(NULL)
  {
    package_states_changed.connect(sigc::mem_fun(*this,
						 &info_area_multiplex::reset_package));
  }

public:
  static ref_ptr<info_area_multiplex>
  create(const vs_hier_editor_ref &editor,
	 const pkg_description_widget_ref &description,
	 const vs_table_ref &description_table,
	 const vs_text_layout_ref &reasons,
	 const vs_table_ref &reasons_table)
  {
    ref_ptr<info_area_multiplex>
      rval(new info_area_multiplex(editor, description, description_table,
				   reasons, reasons_table));
    rval->decref();
    return rval;
  }

  void line_up()
  {
    vs_widget_ref tmpref(this);

    vs_widget_ref w=visible_widget();

    if(w==description_table)
      description->line_up();
    else if(w==reasons_table)
      reasons->line_up();
  }

  void line_down()
  {
    vs_widget_ref tmpref(this);

    vs_widget_ref w=visible_widget();

    if(w==description_table)
      description->line_down();
    else if(w==reasons_table)
      reasons->line_down();
  }

  void set_package(const pkgCache::PkgIterator &pkg,
		   const pkgCache::VerIterator &ver)
  {
    vs_widget_ref tmpref(this);

    bool hasBreakage;

    description->set_package(pkg, ver);
    reasons->set_fragment(reason_fragment(pkg, hasBreakage));
    editor->set_package(pkg, ver);

    // autoswitch if a package is newly broken, or if we have just
    // moved to a broken package.
    if(hasBreakage &&
       (!hadBreakage ||
	!(pkg==lastPkg && ver==lastVer)) &&
       aptcfg->FindB(PACKAGE "::UI::Auto-Show-Reasons", true))
      {
	// Don't clobber the autoswitch bread crumb if we were
	// autoswitched and are still autoswitched.
	if(!autoswitch.valid())
	  autoswitch=visible_widget();
	reasons_table->show();
      }

    // We always set the package anyway in case something changed,
    // but only scroll to the top in this case:
    if(pkg!=lastPkg || ver!=lastVer)
      {
	lastPkg=pkg;
	lastVer=ver;

	description->move_to_top();
	reasons->move_to_top();
      }
    // If we are autoswitched and unbroken, switch back.
    else if(!hasBreakage && autoswitch.valid())
      {
	autoswitch->show();
	autoswitch=NULL;
      }

    hadBreakage=hasBreakage;
  }

  /** Cycles the multiplex, taking autoswitch behavior into account. */
  void cycle()
  {
    vs_widget_ref tmpref(this);

    if(autoswitch.valid() && autoswitch!=visible_widget())
      {
	autoswitch->show();
	autoswitch=NULL;
      }
    else
      {
	autoswitch=NULL;
	cycle_forward();
      }
  }

  /** Re-updates the package views, given that the package state may
   *  have changed.
   */
  void reset_package()
  {
    set_package(lastPkg, lastVer);
  }

  /** Set the description directly, without reference to a package.
   *  Used when no package is selected.
   *
   *  \param s the new description
   */
  void set_description(const std::wstring &s)
  {
    vs_widget_ref tmpref(this);

    if(s!=lastDesc)
      {
	lastDesc=s;

	description->set_fragment(make_desc_fragment(s));
	reasons->set_fragment(sequence_fragment(make_desc_fragment(s),
						newline_fragment(),
						nopackage(),
						NULL));

	description->move_to_top();
	reasons->move_to_top();
      }
  }
};

typedef ref_ptr<info_area_multiplex> info_area_multiplex_ref;

vs_widget_ref make_package_view(list<package_view_item> &format,
				const vs_widget_ref &mainwidget,
				menu_redirect *menu_handler,
				pkg_signal *sig, desc_signal *desc_sig,
				bool show_reason_first)
{
  bool found_mainwidget=false;

  vs_table_ref rval=vs_table::create();

  eassert(mainwidget.valid());

  for(list<package_view_item>::iterator i=format.begin();
      i!=format.end();
      i++)
    {
      switch(i->type)
	{
	case PACKAGE_VIEW_MAINWIDGET:
	  if(found_mainwidget)
	    _error->Error(_("make_package_view: error in arguments -- two main widgets??"));
	  else
	    i->widget=mainwidget;
	  break;
	case PACKAGE_VIEW_STATIC:
	  if(!i->columns)
	    _error->Error(_("make_package_view: error in arguments -- bad column list for static item"));
	  else
	    {
	      pkg_handling_label_ref l=pkg_handling_label::create(i->columns);
	      i->widget=l;

	      if(sig)
		sig->connect(sigc::mem_fun(*l.unsafe_get_ref(), &pkg_handling_label::do_columnify));

	      if(!i->columns_cfg.empty())
		aptcfg->connect(i->columns_cfg,
				sigc::bind(sigc::ptr_fun(do_set_column_format),
					   i->columns_cfg,
					   i->columns_cfg_default,
					   l.weak_ref()));
	    }
	  break;
	case PACKAGE_VIEW_DESCRIPTION:
	  {
	    vs_hier_editor_ref e=vs_hier_editor::create();
	    pkg_description_widget_ref w=pkg_description_widget::create();
	    vs_text_layout_ref l=vs_text_layout::create();

	    vs_table_ref wt=vs_table::create();
	    vs_table_ref lt=vs_table::create();
	    info_area_multiplex_ref m=info_area_multiplex::create(e,
								  w, wt,
								  l, lt);
	    vs_scrollbar_ref ws=vs_scrollbar::create(vs_scrollbar::VERTICAL);
	    vs_scrollbar_ref ls=vs_scrollbar::create(vs_scrollbar::VERTICAL);

	    w->location_changed.connect(sigc::mem_fun(*ws.unsafe_get_ref(), &vs_scrollbar::set_slider));
	    l->location_changed.connect(sigc::mem_fun(*ls.unsafe_get_ref(), &vs_scrollbar::set_slider));

	    wt->add_widget_opts(w, 0, 0, 1, 1, vs_table::EXPAND | vs_table::FILL | vs_table::SHRINK, vs_table::EXPAND | vs_table::FILL | vs_table::SHRINK);
	    wt->add_widget_opts(ws, 0, 1, 1, 1, vs_table::ALIGN_RIGHT, vs_table::ALIGN_CENTER | vs_table::FILL);

	    lt->add_widget_opts(l, 0, 0, 1, 1, vs_table::EXPAND | vs_table::FILL | vs_table::SHRINK, vs_table::EXPAND | vs_table::FILL | vs_table::SHRINK);
	    lt->add_widget_opts(ls, 0, 1, 1, 1, vs_table::ALIGN_RIGHT, vs_table::EXPAND | vs_table::ALIGN_CENTER | vs_table::FILL);

	    // HACK: speaks for itself
	    vs_tree_ref thetree=mainwidget.dyn_downcast<vs_tree>();

	    i->widget=m;

	    // Set up a null reason to start with.  (the package
	    // signal is only called if an actual package is
	    // highlighted)
	    l->set_fragment(nopackage());

	    if(sig)
	      sig->connect(sigc::mem_fun(*m.unsafe_get_ref(), &info_area_multiplex::set_package));

	    if(desc_sig)
	      desc_sig->connect(sigc::mem_fun(*m.unsafe_get_ref(), &info_area_multiplex::set_description));

	    mainwidget->connect_key("DescriptionDown", &global_bindings,
				    sigc::mem_fun(*m.unsafe_get_ref(),
						  &info_area_multiplex::line_down));
	    mainwidget->connect_key("DescriptionUp", &global_bindings,
				    sigc::mem_fun(*m.unsafe_get_ref(),
						  &info_area_multiplex::line_up));
	    mainwidget->connect_key("DescriptionCycle", &global_bindings,
				    sigc::mem_fun(*m.unsafe_get_ref(),
						  &info_area_multiplex::cycle));
	    mainwidget->connect_key("EditHier", &global_bindings,
				    sigc::mem_fun(*e.unsafe_get_ref(),
						  &vscreen_widget::show));
	    mainwidget->connect_key("EditHier", &global_bindings,
				    sigc::mem_fun(*m.unsafe_get_ref(),
						  &vscreen_widget::show));
	    mainwidget->connect_key("EditHier", &global_bindings,
				    sigc::bind(sigc::mem_fun(*rval.unsafe_get_ref(), &vs_table::focus_widget_bare),
					       m.weak_ref()));

	    e->hidden_sig.connect(sigc::bind(sigc::mem_fun(*rval.unsafe_get_ref(), &vs_table::focus_widget_bare),
					     mainwidget.weak_ref()));

	    if(thetree.valid())
	      e->commit_changes.connect(sigc::mem_fun(*thetree.unsafe_get_ref(), &vs_tree::line_down));

	    m->add_visible_widget(e, false);
	    m->add_visible_widget(wt, true);
	    m->add_visible_widget(lt, true);

	    if(show_reason_first)
	      lt->show();
	    else
	      wt->show();

	    // FIXME: this is a grotesque hack.
	    if(mainwidget->get_visible())
	      rval->focus_widget(mainwidget);
	  }
	  break;
	default:
	  _error->Error(_("make_package_view: bad argument!"));
	  break;
	}

      if(i->widget.valid())
	{
	  // If we have a main widget or a description widget, the
	  // size of the widget in question will be ignored.
	  int xopts=i->xopts, yopts=i->yopts;

	  if(i->type == PACKAGE_VIEW_MAINWIDGET ||
	     i->type == PACKAGE_VIEW_DESCRIPTION)
	    {
	      xopts|=vs_table::IGNORE_SIZE_REQUEST;
	      yopts|=vs_table::IGNORE_SIZE_REQUEST;
	    }

	  rval->add_widget_opts(i->widget, i->row, i->col, i->h, i->w,
				xopts, yopts);

	  i->widget->set_bg_style(i->st);

	  if(i->popupdownkey.size()>0)
	    rval->connect_key(i->popupdownkey,
			      &global_bindings,
			      sigc::mem_fun(*i->widget.unsafe_get_ref(),
					    &vscreen_widget::toggle_visible));

	  if(i->visible)
	    i->widget->show();
	}
    }

  // Slow, but the list is (hopefully) << 10 elements or so.
  for(list<package_view_item>::iterator i=format.begin();
      i!=format.end();
      i++)
    if(i->popupdownlinked.size()>0)
      for(list<package_view_item>::iterator j=format.begin();
	  j!=format.end();
	  j++)
	{
	  if(!strcasecmp(j->name.c_str(), i->popupdownlinked.c_str()))
	    {
	      // Having to make two connections is annoying.
	      j->widget->shown_sig.connect(sigc::mem_fun(*i->widget.unsafe_get_ref(), &vscreen_widget::show));
	      j->widget->hidden_sig.connect(sigc::mem_fun(*i->widget.unsafe_get_ref(), &vscreen_widget::hide));
	      break;
	    }
	}

  if(!mainwidget.valid())
    _error->Error(_("make_package_view: no main widget found"));

  if(menu_handler)
    create_menu_bindings(menu_handler, rval);

  return rval;
}
