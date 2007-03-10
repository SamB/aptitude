// view_changelog.cc
//
//   Copyright (C) 2004-2005, 2007 Daniel Burrows
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

#include <vscreen/config/colors.h>
#include <vscreen/config/keybindings.h>
#include <vscreen/transcode.h>
#include <vscreen/vs_pager.h>
#include <vscreen/vs_scrollbar.h>
#include <vscreen/vs_table.h>
#include <vscreen/vs_text_layout.h>

#include "changelog_parse.h"
#include "download_bar.h"
#include "menu_redirect.h"
#include "menu_text_layout.h"
#include "ui.h"
#include "ui_download_manager.h"

#include <generic/apt/apt.h>
#include <generic/apt/pkg_changelog.h>
#include <generic/apt/config_signal.h>
#include <generic/apt/download_signal_log.h>

#include <generic/util/util.h>

#include <sigc++/adaptors/bind.h>
#include <sigc++/functors/mem_fun.h>

using namespace std;

class pkg_changelog_screen : public vs_file_pager, public menu_redirect
{
  bool last_search_forwards;

  void do_search()
  {
    last_search_forwards = true;

    prompt_string(transcode(_("Search for: ")),
		  get_last_search(),
		  arg(sigc::mem_fun(*this, &vs_pager::search_for)),
		  NULL,
		  NULL,
		  NULL);
  }

  void do_search_back()
  {
    last_search_forwards = false;

    prompt_string(transcode(_("Search backwards for: ")),
		  get_last_search(),
		  arg(sigc::mem_fun(*this, &vs_pager::search_back_for)),
		  NULL,
		  NULL,
		  NULL);
  }

  void do_repeat_search()
  {
    if(last_search_forwards)
      search_for(L"");
    else
      search_back_for(L"");
  }

  void do_repeat_search_back()
  {
    if(!last_search_forwards)
      search_for(L"");
    else
      search_back_for(L"");
  }

protected:
  pkg_changelog_screen(const temp::name &filename,
		       int x = 0, int y = 0,
		       int width = 0, int height = 0):
    vs_file_pager(filename.get_name()), last_search_forwards(true)
  {
    connect_key("Search", &global_bindings,
		sigc::mem_fun(*this, &pkg_changelog_screen::do_search));
    connect_key("SearchBack", &global_bindings,
		sigc::mem_fun(*this, &pkg_changelog_screen::do_search_back));
    connect_key("ReSearch", &global_bindings,
		sigc::mem_fun(*this, &pkg_changelog_screen::do_repeat_search));
    connect_key("RepeatSearchBack", &global_bindings,
		sigc::mem_fun(*this, &pkg_changelog_screen::do_repeat_search_back));
  }

public:
  static ref_ptr<pkg_changelog_screen>
  create(const temp::name &filename,
	 int x = 0, int y = 0, int width = 0, int height = 0)
  {
    ref_ptr<pkg_changelog_screen>
      rval(new pkg_changelog_screen(filename, x, y, width, height));
    rval->decref();
    return rval;
  }

  bool find_search_enabled()
  {
    return true;
  }

  bool find_search()
  {
    do_search();
    return true;
  }

  bool find_search_back_enabled()
  {
    return true;
  }

  bool find_search_back()
  {
    do_search_back();
    return true;
  }

  bool find_research_enabled()
  {
    return !get_last_search().empty();
  }

  bool find_research()
  {
    do_repeat_search();
    return true;
  }
};
typedef ref_ptr<pkg_changelog_screen> pkg_changelog_screen_ref;


static void do_view_changelog(temp::name n,
			      string pkgname,
			      string curverstr)
{
  string menulabel =
    ssprintf(_("ChangeLog of %s"), pkgname.c_str());
  string tablabel = ssprintf(_("%s changes"), pkgname.c_str());
  string desclabel = _("View the list of changes made to this Debian package.");

  fragment *f = make_changelog_fragment(n, curverstr);

  vs_table_ref           t = vs_table::create();
  if(f != NULL)
    {
      vs_scrollbar_ref   s = vs_scrollbar::create(vs_scrollbar::VERTICAL);
      menu_text_layout_ref l = menu_text_layout::create();


      l->location_changed.connect(sigc::mem_fun(s.unsafe_get_ref(), &vs_scrollbar::set_slider));
      s->scrollbar_interaction.connect(sigc::mem_fun(l.unsafe_get_ref(), &vs_text_layout::scroll));
      l->set_fragment(f);

      t->add_widget_opts(l, 0, 0, 1, 1,
			 vs_table::EXPAND|vs_table::SHRINK, vs_table::EXPAND);
      t->add_widget_opts(s, 0, 1, 1, 1, 0,
			 vs_table::EXPAND | vs_table::FILL);

      create_menu_bindings(l.unsafe_get_ref(), t);
    }
  else
    {
      pkg_changelog_screen_ref cs = pkg_changelog_screen::create(n);
      vs_scrollbar_ref          s = vs_scrollbar::create(vs_scrollbar::VERTICAL);

      cs->line_changed.connect(sigc::mem_fun(s.unsafe_get_ref(), &vs_scrollbar::set_slider));
      s->scrollbar_interaction.connect(sigc::mem_fun(cs.unsafe_get_ref(), &pkg_changelog_screen::scroll_page));
      cs->scroll_top();

      t->add_widget_opts(cs, 0, 0, 1, 1,
			 vs_table::EXPAND|vs_table::SHRINK, vs_table::EXPAND);
      t->add_widget_opts(s, 0, 1, 1, 1, 0,
			 vs_table::EXPAND | vs_table::FILL);

      create_menu_bindings(cs.unsafe_get_ref(), t);
    }

  t->show_all();

  insert_main_widget(t, menulabel, desclabel, tablabel);
}

void view_changelog(pkgCache::VerIterator ver)
{
  bool in_debian=false;

  string pkgname = ver.ParentPkg().Name();

  pkgCache::VerIterator curver = ver.ParentPkg().CurrentVer();
  string curverstr;
  if(!curver.end() && curver.VerStr() != NULL)
    curverstr = curver.VerStr();

  // TODO: add a configurable association between origins and changelog URLs.
  for(pkgCache::VerFileIterator vf=ver.FileList();
      !vf.end() && !in_debian; ++vf)
    if(!vf.File().end() && vf.File().Origin()!=NULL &&
       strcmp(vf.File().Origin(), "Debian")==0)
      in_debian=true;

  if(!in_debian)
    {
      show_message(_("You can only view changelogs of official Debian packages."),
		   NULL, get_style("Error"));
      return;
    }

  download_manager *manager = get_changelog(ver,
					    sigc::bind(sigc::ptr_fun(&do_view_changelog), pkgname, curverstr));

  if(manager != NULL)
    (new ui_download_manager(manager, true, false, false,
			     _("Downloading Changelog"),
			     "",
			     _("Download Changelog")))->start();
}
