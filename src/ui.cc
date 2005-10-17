// ui.cc
//
//   Copyright 2000-2005 Daniel Burrows <dburrows@debian.org>
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
//  Various UI glue-code and routines.

#include "ui.h"

#include <sigc++/adaptors/bind.h>
#include <sigc++/adaptors/retype_return.h>
#include <sigc++/functors/ptr_fun.h>
#include <sigc++/functors/mem_fun.h>

#include <apt-pkg/acquire.h>
#include <apt-pkg/clean.h>
#include <apt-pkg/configuration.h>
#include <apt-pkg/error.h>
#include <apt-pkg/packagemanager.h>

#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <sys/types.h>

#include <fstream>
#include <utility>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "aptitude.h"

#include "apt_config_widgets.h"
#include "apt_options.h"
#include "broken_indicator.h"
#include "defaults.h"
#include "load_config.h"
#include "load_grouppolicy.h"
#include "load_pkgview.h"
#include "solution_dialog.h"
#include "solution_fragment.h"
#include "solution_screen.h"

#include <vscreen/curses++.h>
#include <vscreen/fragment.h>
#include <vscreen/vscreen.h>
#include <vscreen/vs_button.h>
#include <vscreen/vs_center.h>
#include <vscreen/vs_editline.h>
#include <vscreen/vs_frame.h>
#include <vscreen/vs_label.h>
#include <vscreen/vs_menu.h>
#include <vscreen/vs_menubar.h>
#include <vscreen/vs_multiplex.h>
#include <vscreen/vs_pager.h>
#include <vscreen/vs_scrollbar.h>
#include <vscreen/vs_stacked.h>
#include <vscreen/vs_statuschoice.h>
#include <vscreen/vs_table.h>
#include <vscreen/vs_text_layout.h>
#include <vscreen/vs_transient.h>
#include <vscreen/vs_togglebutton.h>
#include <vscreen/vs_tree.h>
#include <vscreen/vs_util.h>

#include <mine/cmine.h>

#include <generic/apt/apt.h>
#include <generic/apt/apt_undo_group.h>
#include <generic/apt/aptitude_resolver_universe.h>
#include <generic/apt/config_signal.h>
#include <generic/apt/download_install_manager.h>
#include <generic/apt/download_update_manager.h>
#include <generic/apt/download_signal_log.h>
#include <generic/apt/resolver_manager.h>

#include <generic/problemresolver/exceptions.h>
#include <generic/problemresolver/solution.h>

#include <generic/util/temp.h>
#include <generic/util/util.h>

#include "dep_item.h"
#include "download_bar.h"
#include "download_list.h"
#include "download_screen.h"
#include "menu_tree.h"
#include "pkg_columnizer.h"
#include "pkg_grouppolicy.h"
#include "pkg_info_screen.h"
#include "pkg_tree.h"
#include "pkg_ver_item.h"
#include "pkg_view.h"
#include "ui_download_manager.h"
#include "vs_progress.h"

using namespace std;

typedef generic_solution<aptitude_universe> aptitude_solution;

static vs_menubar_ref main_menu;
static vs_menu_ref views_menu;

static vs_stacked_ref main_stacked;

// Hmm, is this table the best idea?..
static vs_table_ref main_table;

static vs_multiplex_ref main_multiplex;
static vs_multiplex_ref main_status_multiplex;

// I think it's better to only have a single preview screen at once.  (note to
// self: data-segment stuff initializes to 0 already..)
static pkg_tree_ref active_preview_tree;
static vs_widget_ref active_preview;

// True if a download or package-list update is proceeding.  This hopefully will
// avoid the nasty possibility of collisions between them.
// FIXME: uses implicit locking -- if multithreading happens, should use a mutex
//       instead.
static bool active_download;

// While a status-widget download progress thingy is active, this will be
// set to it.
vs_widget_ref active_status_download;

sigc::signal0<void> file_quit;

sigc::signal0<bool, accumulate_or> undo_undo;
sigc::signal0<bool, accumulate_or> undo_undo_enabled;

sigc::signal0<void> package_states_changed;

sigc::signal0<bool, accumulate_or> package_menu_enabled;
sigc::signal0<bool, accumulate_or> package_forbid_enabled;
sigc::signal0<bool, accumulate_or> package_information_enabled;
sigc::signal0<bool, accumulate_or> package_changelog_enabled;

sigc::signal0<bool, accumulate_or> find_search_enabled;
sigc::signal0<bool, accumulate_or> find_search_back_enabled;
sigc::signal0<bool, accumulate_or> find_research_enabled;
sigc::signal0<bool, accumulate_or> find_limit_enabled;
sigc::signal0<bool, accumulate_or> find_cancel_limit_enabled;
sigc::signal0<bool, accumulate_or> find_broken_enabled;

sigc::signal0<bool, accumulate_or> package_install;
sigc::signal0<bool, accumulate_or> package_remove;
sigc::signal0<bool, accumulate_or> package_purge;
sigc::signal0<bool, accumulate_or> package_hold;
sigc::signal0<bool, accumulate_or> package_keep;
sigc::signal0<bool, accumulate_or> package_mark_auto;
sigc::signal0<bool, accumulate_or> package_unmark_auto;
sigc::signal0<bool, accumulate_or> package_forbid;
sigc::signal0<bool, accumulate_or> package_information;
sigc::signal0<bool, accumulate_or> package_changelog;

sigc::signal0<bool, accumulate_or> resolver_toggle_rejected;
sigc::signal0<bool, accumulate_or> resolver_toggle_rejected_enabled;
sigc::signal0<bool, accumulate_or> resolver_toggle_approved;
sigc::signal0<bool, accumulate_or> resolver_toggle_approved_enabled;
sigc::signal0<bool, accumulate_or> resolver_view_target;
sigc::signal0<bool, accumulate_or> resolver_view_target_enabled;

sigc::signal0<bool, accumulate_or> find_search;
sigc::signal0<bool, accumulate_or> find_search_back;
sigc::signal0<bool, accumulate_or> find_research;
sigc::signal0<bool, accumulate_or> find_limit;
sigc::signal0<bool, accumulate_or> find_cancel_limit;
sigc::signal0<bool, accumulate_or> find_broken;

const char *default_pkgstatusdisplay="%d";
const char *default_pkgheaderdisplay="%N %n #%B %u %o";
const char *default_grpstr="task,status,section(subdir,passthrough),section(topdir)";

void ui_start_download(bool hide_preview)
{
  active_download = true;

  if(apt_cache_file != NULL)
    (*apt_cache_file)->set_read_only(true);

  if(hide_preview && active_preview.valid())
    active_preview->destroy();
}

void ui_stop_download()
{
  active_download = false;

  if(apt_cache_file != NULL)
    (*apt_cache_file)->set_read_only(false);
}

static fragment *apt_error_fragment()
{
  vector<fragment *> frags;

  if(_error->empty())
    frags.push_back(text_fragment(_("Er, there aren't any errors, this shouldn't have happened..")));
  else while(!_error->empty())
    {
      string currerr, tag;
      bool iserr=_error->PopMessage(currerr);
      if(iserr)
	tag=_("E:");
      else
	tag=_("W:");

      frags.push_back(indentbox(0, 3,
				wrapbox(fragf("%B%s%b %s",
					      tag.c_str(),
					      currerr.c_str()))));
    }

  return sequence_fragment(frags);
}

// Handles "search" dialogs for pagers
static void pager_search(vs_pager &p)
{
  prompt_string(transcode(_("Search for:")),
		p.get_last_search(),
		arg(sigc::mem_fun(p, &vs_pager::search_for)),
		NULL,
		NULL,
		NULL);
}

// similar
static void pager_repeat_search(vs_pager &p)
{
  p.search_for(L"");
}

static vs_widget_ref make_error_dialog(const vs_text_layout_ref &layout)
{
  vs_table_ref t=vs_table::create();

  vs_scrollbar_ref s=vs_scrollbar::create(vs_scrollbar::VERTICAL);

  t->add_widget(layout, 0, 0, 1, 1, true, true);
  t->add_widget_opts(s, 0, 1, 1, 1,
		     vs_table::ALIGN_RIGHT,
		     vs_table::ALIGN_CENTER | vs_table::FILL);

  layout->location_changed.connect(sigc::mem_fun(s.unsafe_get_ref(), &vs_scrollbar::set_slider));
  s->scrollbar_interaction.connect(sigc::mem_fun(layout.unsafe_get_ref(), &vs_text_layout::scroll));

  return vs_dialog_ok(t, NULL, transcode(_("Ok")), get_style("Error"));
}

// blah, I hate C++
static void do_null_ptr(vs_text_layout_ref *p)
{
  *p=NULL;
}

void check_apt_errors()
{
  if(_error->empty())
    return;

  static vs_text_layout_ref error_dialog_layout = NULL;

  if(!error_dialog_layout.valid())
    {
      error_dialog_layout = vs_text_layout::create(apt_error_fragment());
      error_dialog_layout->destroyed.connect(sigc::bind(sigc::ptr_fun(do_null_ptr), &error_dialog_layout));

      main_stacked->add_visible_widget(make_error_dialog(error_dialog_layout),
				       true);
    }
  else
    error_dialog_layout->append_fragment(apt_error_fragment());
}

static void read_only_permissions_table_destroyed(apt_config_widget &w)
{
  w.commit();
  apt_dumpcfg(PACKAGE);

  delete &w;
}

static bool do_read_only_permission()
{
  if(active_download)
    return false;
  else
    {
      (*apt_cache_file)->set_read_only(false);


      if(!aptcfg->FindB(PACKAGE "::Suppress-Read-Only-Warning", false))
	{
	  vs_table_ref t(vs_table::create());

	  fragment *f = wrapbox(text_fragment(_("WARNING: the package cache is opened in read-only mode!  This change and all subsequent changes will not be saved unless you stop all other running apt-based programs and select \"Become root\" from the Actions menu.")));

	  t->add_widget_opts(vs_text_layout::create(f),
			     0, 0, 1, 1, vs_table::EXPAND | vs_table::FILL | vs_table::SHRINK,
			     vs_table::EXPAND | vs_table::FILL);

	  apt_bool_widget *w = new apt_bool_widget(_("Never display this message again."),
						   PACKAGE "::Suppress-Read-Only-Warning", false);

	  // HACK:
	  t->add_widget_opts(vs_label::create(""), 1, 0, 1, 1,
			     vs_table::EXPAND | vs_table::FILL | vs_table::SHRINK,
			     0);

	  t->add_widget_opts(w->cb, 2, 0, 1, 1,
			     vs_table::EXPAND | vs_table::FILL | vs_table::SHRINK,
			     vs_table::EXPAND | vs_table::FILL);

	  // HACK:
	  t->add_widget_opts(vs_label::create(""), 3, 0, 1, 1,
			     vs_table::EXPAND | vs_table::FILL | vs_table::SHRINK,
			     0);

	  vs_button_ref ok(vs_button::create(_("Ok")));

	  t->add_widget_opts(ok, 4, 0, 1, 1,
			     vs_table::EXPAND | vs_table::SHRINK,
			     vs_table::EXPAND);

	  t->show_all();

	  t->focus_widget(ok);

	  vs_frame_ref frame = vs_frame::create(t);
	  frame->set_bg_style(style_attrs_flip(A_REVERSE));

	  vs_center_ref c = vs_center::create(frame);

	  ok->pressed.connect(sigc::mem_fun(c.unsafe_get_ref(), &vscreen_widget::destroy));
	  c->destroyed.connect(sigc::bind(sigc::ptr_fun(&read_only_permissions_table_destroyed), sigc::ref(*w)));

	  popup_widget(c);
	}

      return true;
    }
}

static void do_read_only_fail()
{
  assert(active_download);

  show_message(_("You may not modify the state of any package while a download is underway."));
}

static void do_connect_read_only_callbacks()
{
  if(apt_cache_file != NULL)
    {
      (*apt_cache_file)->read_only_permission.connect(sigc::ptr_fun(&do_read_only_permission));
      (*apt_cache_file)->read_only_fail.connect(sigc::ptr_fun(&do_read_only_fail));
    }
}

// Runs a sub-aptitude with the same selections that the user
// has currently made, but as root.
//
// Because tagfiles don't work on FIFOs, and su closes all open fds,
// this is a lot hairier than it should be.
//
// This writes the current status to a file in a designated temporary
// directory, then loads it in a su'd instance.  A FIFO is used to
// make the reader block until the writer is done writing.  Not
// foolproof but the user would have to go to a lot of pointless
// trouble to screw it up.
//
// Note that the deletion of the temporary files is safe, since this
// routine blocks until the sub-process exits.

static void do_su_to_root(string args)
{
  if(getuid()==0)
    {
      show_message(_("You already are root!"));
      return;
    }

  temp::dir  tempdir("aptitude");
  temp::name statusname(tempdir, "pkgstates");
  temp::name fifoname(tempdir, "control");

  if(mkfifo(fifoname.get_name().c_str(), 0600) != 0)
    {
      _error->Errno("do_su_to_root",
		    "Couldn't create temporary FIFO");
      return;
    }

  int pid=fork();

  if(pid==-1)
    _error->Error("Unable to fork: %s", strerror(errno));
  else if(pid==0) // I'm a child!
    {
      // Read one byte from the FIFO for synchronization
      char tmp;
      int fd = open(fifoname.get_name().c_str(), O_RDONLY);
      read(fd, &tmp, 1); // Will block until the other process writes.
      close(fd);

      // It's ok to use argv0 to generate the command,
      // since the user has to authenticate themselves as root (ie, if
      // they're going to do something evil that way they'd have su'd
      // directly)
      //
      // What happens if tmpdir has spaces in it?  Can I get more
      // control over how the su-to-root function is called?
      char cmdbuf[512];
      snprintf(cmdbuf, 512, "%s -S %s %s",
	       argv0, statusname.get_name().c_str(), args.c_str());
      execl("/bin/su", "/bin/su", "-c", cmdbuf, NULL);
    }
  else
    {
      int status;
      OpProgress foo; // Need a generic non-outputting progress bar

      // Save the selection list.
      (*apt_cache_file)->save_selection_list(foo, statusname.get_name().c_str());

      // Shut curses down.
      vscreen_suspend();

      // Ok, wake the other process up.
      char tmp=0;
      int fd=open(fifoname.get_name().c_str(), O_WRONLY);
      write(fd, &tmp, 1);
      close(fd);

      // Wait for a while so we don't accidentally daemonize ourselves.
      while(waitpid(pid, &status, 0)!=pid)
	;

      if(!WIFEXITED(status) || WEXITSTATUS(status))
	{
	  _error->Error("%s",
			_("Subprocess exited with an error -- did you type your password correctly?"));
	  vscreen_resume();
	  check_apt_errors();
	  // We have to clear these out or the cache won't reload properly (?)

	  vs_progress_ref p = gen_progress_bar();
	  apt_reload_cache(p.unsafe_get_ref(), true, statusname.get_name().c_str());
	  p->destroy();
	}
      else
	{
	  // Clear out our references to these objects so they get
	  // removed.
	  tempdir    = temp::dir();
	  statusname = temp::name();
	  fifoname   = temp::name();

	  exit(0);
	}
    }
}

static bool su_to_root_enabled()
{
  return getuid()!=0;
}

static void update_menubar_autohide()
{
  main_menu->set_always_visible(main_multiplex->num_children()==0 ||
				!aptcfg->FindB(PACKAGE "::UI::Menubar-Autohide",
					       false));
}

vs_widget_ref reload_message;
static void do_show_reload_message()
{
  if(!reload_message.valid())
    {
      vs_widget_ref w = vs_frame::create(vs_label::create(_("Loading cache")));
      reload_message  = vs_center::create(w);
      reload_message->show_all();
      popup_widget(reload_message);

      vscreen_tryupdate();
    }
}

static void do_hide_reload_message()
{
  if(reload_message.valid())
    {
      reload_message->destroy();
      reload_message = NULL;
      vscreen_tryupdate();
    }
}

static void do_quit()
{
  if(aptcfg->FindB(PACKAGE "::UI::Prompt-On-Exit", true))
    prompt_yesno(_("Really quit Aptitude?"), false, arg(file_quit.make_slot()), NULL);
  else
    file_quit();
}

static void do_destroy_visible()
{
  if(aptcfg->FindB(PACKAGE "::UI::Exit-On-Last-Close", true) &&
     main_multiplex->num_children()<=1)
    do_quit();
  else
    {
      vs_widget_ref w=main_multiplex->visible_widget();
      if(w.valid())
	w->destroy();

      // If all the screens are destroyed, we make the menu visible (so the
      // user knows that something is still alive :) )
      update_menubar_autohide();
    }
}

static bool view_next_prev_enabled()
{
  return main_multiplex->num_visible()>1;
}

static bool any_view_visible()
{
  return main_multiplex->visible_widget().valid();
}

// These are necessary because main_multiplex isn't created until after
// the slot in the static initializer..
// (creating the menu info at a later point would solve this problem)
static void do_view_next()
{
  main_multiplex->cycle_forward();
}

static void do_view_prev()
{
  main_multiplex->cycle_backward();
}

static void do_show_ui_options_dlg()
{
  vs_widget_ref w = make_ui_options_dialog();
  main_stacked->add_visible_widget(w, true);
  w->show();
}

static void do_show_misc_options_dlg()
{
  vs_widget_ref w=make_misc_options_dialog();
  main_stacked->add_visible_widget(w, true);
  w->show();
}

static void do_show_dependency_options_dlg()
{
  vs_widget_ref w=make_dependency_options_dialog();
  main_stacked->add_visible_widget(w, true);
  w->show();
}

static void really_do_revert_options()
{
  apt_revertoptions();
  apt_dumpcfg(PACKAGE);
}

static void do_revert_options()
{
  prompt_yesno(_("Really discard your personal settings and reload the defaults?"),
	       false,
	       arg(sigc::ptr_fun(really_do_revert_options)),
	       NULL);
}

static vs_widget_ref make_default_view(const menu_tree_ref &mainwidget,
				       pkg_signal *sig,
				       desc_signal *desc_sig,
				       bool allow_visible_desc=true,
				       bool show_reason_first=false)
{
  if(aptcfg->Exists(PACKAGE "::UI::Default-Package-View"))
    {
      list<package_view_item> *format=load_pkgview(PACKAGE "::UI::Default-Package-View");

      if(format)
	{
	  // The unsafe_get_ref is to convert mainwidget to be a
	  // menu_redirect pointer.
	  vs_widget_ref rval=make_package_view(*format, mainwidget,
					       mainwidget.unsafe_get_ref(), sig,
					       desc_sig, show_reason_first);
	  delete format;

	  if(rval.valid())
	    return rval;
	}
    }

  list<package_view_item> basic_format;

  // FIXME: do the config lookup inside the package-view code?
  basic_format.push_back(package_view_item("static1",
					   parse_columns(transcode(aptcfg->Find(PACKAGE "::UI::Package-Header-Format", default_pkgheaderdisplay)),
							 pkg_item::pkg_columnizer::parse_column_type,
							 pkg_item::pkg_columnizer::defaults),
					   PACKAGE "::UI::Package-Header-Format",
					   0, 0, 1, 1,
					   vs_table::ALIGN_CENTER | vs_table::EXPAND | vs_table::FILL | vs_table::SHRINK,
					   vs_table::ALIGN_CENTER,
					   get_style("Header"),
					   "",
					   "",
					   true));

  basic_format.push_back(package_view_item("main", 1, 0, 1, 1,
					   vs_table::EXPAND | vs_table::FILL | vs_table::SHRINK,
					   vs_table::EXPAND | vs_table::FILL | vs_table::SHRINK,
					   style(),
					   true));

  basic_format.push_back(package_view_item("static2",
					   parse_columns(transcode(aptcfg->Find(PACKAGE "::UI::Package-Status-Format", default_pkgstatusdisplay)),
							 pkg_item::pkg_columnizer::parse_column_type,
							 pkg_item::pkg_columnizer::defaults),
					   PACKAGE "::UI::Package-Status-Format",
					   2, 0, 1, 1,
					   vs_table::ALIGN_CENTER | vs_table::EXPAND | vs_table::FILL | vs_table::SHRINK,
					   vs_table::ALIGN_CENTER,
					   get_style("Status"),
					   "", "",
					   true));

  basic_format.push_back(package_view_item("desc", 3, 0, 1, 1,
					   vs_table::EXPAND | vs_table::FILL | vs_table::SHRINK,
					   vs_table::EXPAND | vs_table::FILL | vs_table::SHRINK,
					   style(),
					   "ShowHideDescription", "",
					   allow_visible_desc && aptcfg->FindB(PACKAGE "::UI::Description-Visible-By-Default", true)));

  return make_package_view(basic_format, mainwidget,
			   mainwidget.unsafe_get_ref(),
			   sig, desc_sig,
			   show_reason_first);
}

void do_new_package_view(OpProgress &progress)
{
  pkg_grouppolicy_factory *grp=NULL;
  std::string grpstr="";

  if(aptcfg->Exists(PACKAGE "::UI::Default-Grouping"))
    {
      grpstr=aptcfg->Find(PACKAGE "::UI::Default-Grouping");
      grp=parse_grouppolicy(grpstr);
    }

  if(!grp) // Fallback
    {
      grpstr=default_grpstr;
      grp=parse_grouppolicy(grpstr);

      if(!grp)
	// Eek!  The default grouping failed to parse.  Fall all the
	// way back.
	grp=new pkg_grouppolicy_task_factory(new pkg_grouppolicy_status_factory(new pkg_grouppolicy_section_factory(pkg_grouppolicy_section_factory::split_subdir,true,new pkg_grouppolicy_section_factory(pkg_grouppolicy_section_factory::split_topdir,false,new pkg_grouppolicy_end_factory()))));
    }

  pkg_tree_ref tree=pkg_tree::create(grpstr.c_str(), grp);

  add_main_widget(make_default_view(tree,
				    &tree->selected_signal,
				    &tree->selected_desc_signal),
		  _("Packages"),
		  _("View available packages and choose actions to perform"),
		  _("Packages"));

  tree->build_tree(progress);
}

// For signal connections.
static void do_new_package_view_with_new_bar()
{
  vs_progress_ref p = gen_progress_bar();
  do_new_package_view(*p.unsafe_get_ref());
  p->destroy();
}

static void do_new_recommendations_view()
{
  vs_progress_ref p = gen_progress_bar();

  pkg_grouppolicy_factory *grp = new pkg_grouppolicy_end_factory();
  std::string grpstr="section(subdir, passthrough)";

  pkg_tree_ref tree=pkg_tree::create(grpstr.c_str(), grp,
				     L"!~v!~i~RBrecommends:~i");

  add_main_widget(make_default_view(tree,
				    &tree->selected_signal,
				    &tree->selected_desc_signal,
				    true,
				    true),
		  _("Recommended Packages"),
		  _("View packages that it is recommended that you install"),
		  _("Recommendations"));

  tree->build_tree(*p.unsafe_get_ref());
  p->destroy();
}

static void do_new_flat_view_with_new_bar()
{
  vs_progress_ref p = gen_progress_bar();

  pkg_grouppolicy_factory *grp = new pkg_grouppolicy_end_factory;
  pkg_tree_ref tree = pkg_tree::create("", grp);
  tree->set_limit(transcode("!~v"));

  add_main_widget(make_default_view(tree,
				    &tree->selected_signal,
				    &tree->selected_desc_signal),
		  _("Packages"),
		  _("View available packages and choose actions to perform"),
		  _("Packages"));

  tree->build_tree(*p.unsafe_get_ref());
  p->destroy();
}

static void do_new_tag_view_with_new_bar()
{
  vs_progress_ref p = gen_progress_bar();

  pkg_grouppolicy_factory *grp = NULL;
  string grpstr = "tag";
  grp = parse_grouppolicy(grpstr);

  pkg_tree_ref tree = pkg_tree::create(grpstr.c_str(), grp);

  add_main_widget(make_default_view(tree,
				    &tree->selected_signal,
				    &tree->selected_desc_signal),
		  _("Packages"),
		  _("View available packages and choose actions to perform"),
		  _("Packages"));

  tree->build_tree(*p.unsafe_get_ref());
  p->destroy();
}

void do_new_hier_view(OpProgress &progress)
{
  pkg_grouppolicy_factory *grp=NULL;
  std::string grpstr="";

  grpstr="hier";
  grp=parse_grouppolicy(grpstr);

  pkg_tree_ref tree=pkg_tree::create(grpstr.c_str(), grp);
  tree->set_limit(transcode("!~v"));
  //tree->set_hierarchical(false);

  add_main_widget(make_default_view(tree,
				    &tree->selected_signal,
				    &tree->selected_desc_signal),
		  _("Packages"),
		  _("View available packages and choose actions to perform"),
		  _("Packages"));
  tree->build_tree(progress);
}

// For signal connections.
static void do_new_hier_view_with_new_bar()
{
  vs_progress_ref p=gen_progress_bar();
  do_new_hier_view(*p.unsafe_get_ref());
  p->destroy();
}

vs_widget_ref make_info_screen(const pkgCache::PkgIterator &pkg,
			       const pkgCache::VerIterator &ver)
{
  pkg_info_screen_ref w = pkg_info_screen::create(pkg, ver);
  vs_widget_ref rval    = make_default_view(w, w->get_sig(), w->get_desc_sig(), false);
  w->repeat_signal(); // force the status line in the view to update
  return rval;
}

vs_widget_ref make_dep_screen(const pkgCache::PkgIterator &pkg,
			      const pkgCache::VerIterator &ver,
			      bool reverse)
{
  pkg_dep_screen_ref w = pkg_dep_screen::create(pkg, ver, reverse);
  vs_widget_ref rval   = make_default_view(w, w->get_sig(), w->get_desc_sig(), true);
  w->repeat_signal(); // force the status line in the view to update
  return rval;
}

vs_widget_ref make_ver_screen(const pkgCache::PkgIterator &pkg)
{
  pkg_ver_screen_ref w = pkg_ver_screen::create(pkg);
  vs_widget_ref rval   = make_default_view(w, w->get_sig(), w->get_desc_sig(), true);
  w->repeat_signal();
  return rval;
}

static void do_help_about()
{
  fragment *f=fragf(_("Aptitude %s%n%nCopyright 2000-2005 Daniel Burrows.%n%naptitude comes with %BABSOLUTELY NO WARRANTY%b; for details see 'license' in the Help menu.  This is free software, and you are welcome to redistribute it under certain conditions; see 'license' for details."), VERSION);

  vs_widget_ref w=vs_dialog_ok(wrapbox(f));
  w->show_all();

  popup_widget(w);
}

static void do_help_license()
{
  vs_widget_ref w=vs_dialog_fileview(HELPDIR "/COPYING",
				     NULL,
				     arg(sigc::ptr_fun(pager_search)),
				     arg(sigc::ptr_fun(pager_repeat_search)));
  w->show_all();

  popup_widget(w);
}

static void do_help_help()
{
  char buf[512];

  snprintf(buf, 512, HELPDIR "/%s", _("help.txt"));

  const char *encoding=P_("Encoding of help.txt|UTF-8");

  // Deal with missing localized docs.
  if(access(buf, R_OK)!=0)
    {
      strncpy(buf, HELPDIR "/help.txt", 512);
      encoding="UTF-8";
    }

  vs_widget_ref w=vs_dialog_fileview(buf, NULL,
				     arg(sigc::ptr_fun(pager_search)),
				     arg(sigc::ptr_fun(pager_repeat_search)));
  w->show_all();

  popup_widget(w);
}

static void do_help_readme()
{
  char buf[512];

  snprintf(buf, 512, HELPDIR "/%s", _("README")); // README can be translated..

  const char *encoding=P_("Encoding of README|UTF-8");

  // Deal with missing localized docs.
  if(access(buf, R_OK)!=0)
    {
      strncpy(buf, HELPDIR "/README", 512);
      encoding="UTF-8";
    }

  vs_table_ref t      = vs_table::create();
  vs_scrollbar_ref s  = vs_scrollbar::create(vs_scrollbar::VERTICAL);
  vs_file_pager_ref p = vs_file_pager::create(buf, encoding);

  p->line_changed.connect(sigc::mem_fun(s.unsafe_get_ref(), &vs_scrollbar::set_slider));
  s->scrollbar_interaction.connect(sigc::mem_fun(p.unsafe_get_ref(), &vs_pager::scroll_page));
  p->scroll_top(); // Force a scrollbar update.

  t->add_widget_opts(p, 0, 0, 1, 1,
		     vs_table::EXPAND | vs_table::FILL | vs_table::SHRINK,
		     vs_table::EXPAND | vs_table::FILL);
  t->add_widget_opts(s, 0, 1, 1, 1,
		     vs_table::EXPAND, vs_table::EXPAND | vs_table::FILL);

  s->show();
  p->show();

  add_main_widget(t, _("User's Manual"), _("Read the full user's manual of aptitude"), _("Manual"));
}

static void do_help_faq()
{
  vs_widget_ref w=vs_dialog_fileview(HELPDIR "/FAQ", NULL,
				     arg(sigc::ptr_fun(pager_search)),
				     arg(sigc::ptr_fun(pager_repeat_search)));
  w->show_all();

  popup_widget(w);
}

// news isn't translated since it's just a changelog.
static void do_help_news()
{
  vs_widget_ref w=vs_dialog_fileview(HELPDIR "/NEWS", NULL,
				     arg(sigc::ptr_fun(pager_search)),
				     arg(sigc::ptr_fun(pager_repeat_search)));
  w->show_all();

  popup_widget(w);
}

/** Remove the given file/directory and all its children.  Behaves
 *  like rm -fr.
 *
 *  \todo this belongs in util.cc
 */
static bool recursive_remdir(const std::string &dirname)
{
  struct stat stbuf;

  if(lstat(dirname.c_str(), &stbuf) != 0)
    _error->Errno("recursive_rmdir", _("Unable to stat \"%s\""), dirname.c_str());

  if(S_ISLNK(stbuf.st_mode) || !S_ISDIR(stbuf.st_mode))
    {
      if(unlink(dirname.c_str()) != 0)
	{
	  _error->Errno("recursive_rmdir", _("Unable to remove \"%s\""), dirname.c_str());
	  return false;
	}
      else
	return true;
    }

  DIR *dir = opendir(dirname.c_str());
  if(dir == NULL)
    {
      _error->Errno("recursive_rmdir", _("Unable to list files in \"%s\""), dirname.c_str());
      return false;
    }

  bool rval = true;

  for(dirent *dent = readdir(dir); dent != NULL; dent = readdir(dir))
    if(strcmp(dent->d_name, ".") != 0 &&
       strcmp(dent->d_name, "..") != 0)
      rval = (rval && recursive_remdir(dirname + "/" + dent->d_name));

  if(closedir(dir) != 0)
    {
      _error->Errno("recursive_rmdir", _("Failure closing directory \"%s\""), dirname.c_str());
      rval = false;
    }

  if(rmdir(dirname.c_str()) != 0)
    {
      _error->Errno("recursive_rmdir", _("Unable to remove directory \"%s\""), dirname.c_str());
      rval = false;
    }

  return rval;
}

static void do_kill_old_tmp(const std::string old_tmpdir)
{
  if(!recursive_remdir(old_tmpdir))
    show_message(ssprintf(_("Unable to remove the old temporary directory; you should remove %s by hand."), old_tmpdir.c_str()));
}

static void cancel_kill_old_tmp(const std::string old_tmpdir)
{
  show_message(ssprintf(_("Will not remove %s; you should examine the files in it and remove them by hand."), old_tmpdir.c_str()));
  aptcfg->Set(PACKAGE "::Ignore-Old-Tmp", "true");
  apt_dumpcfg(PACKAGE);
}

static void maybe_show_old_tmpdir_message()
{
  std::string tmpdir_path = get_homedir() + "/.aptitude/.tmp";

  if(aptcfg->FindB(PACKAGE "::Ignore-Old-Tmp", false))
    {
      // Watch for the reappearance of this directory.
      if(access(tmpdir_path.c_str(), F_OK) != 0)
	{
	  aptcfg->Set(PACKAGE "::Ignore-Old-Tmp", "false");
	  apt_dumpcfg(PACKAGE);
	}

      return;
    }

  // Remove it silently if it's empty.
  if(rmdir(tmpdir_path.c_str()) == 0)
    return;

  if(access(tmpdir_path.c_str(), F_OK) == 0)
    prompt_yesno_popup(wrapbox(fragf(_("It appears that a previous version of aptitude left files behind in %s.  These files are probably useless and safe to delete.%n%nDo you want to remove this directory and all its contents?  If you select \"No\", you will not see this message again."), tmpdir_path.c_str())),
		       false,
		       arg(sigc::bind(sigc::ptr_fun(do_kill_old_tmp), tmpdir_path)),
		       arg(sigc::bind(sigc::ptr_fun(cancel_kill_old_tmp), tmpdir_path)));
}

// There are some circular references because of the code to test
// for consistency before doing a package run; the last routine called before
// the program starts downloading will verify that the selections are
// consistent and call its predecessors if they are not.
//
// (er, can I disentangle this by rearranging the routines?  I think maybe I
//  can to some degree)


static void finish_install_run(pkgPackageManager::OrderResult res)
{
  if(res != pkgPackageManager::Incomplete)
    {
      cerr << _("Press return to continue.\n");
      int c = getchar();

      while(c != '\n'  && c != EOF)
	c = getchar();
    }

  // libapt-pkg likes to stomp on SIGINT and SIGQUIT.  Restore them
  // here in the simplest possible way.
  vscreen_install_sighandlers();

  vscreen_resume();
}

void install_or_remove_packages()
{
  download_install_manager *m = new download_install_manager(false);

  m->pre_install_hook.connect(sigc::ptr_fun(&vscreen_suspend));
  m->post_install_hook.connect(sigc::ptr_fun(&finish_install_run));
  m->post_forget_new_hook.connect(package_states_changed.make_slot());

  (new ui_download_manager(m, false, false, true,
			   _("Downloading packages"),
			   _("View the progress of the package download"),
			   _("Package Download")))->start();
}

/** Make sure that no trust violations are about to be committed.  If
 *  any are, warn the user and give them a chance to fix the
 *  situation.
 *
 *  Should this warning be displayed when a package is selected instead?
 */
static void check_package_trust()
{
  vector<pkgCache::VerIterator> untrusted;

  for(pkgCache::PkgIterator pkg=(*apt_cache_file)->PkgBegin();
      !pkg.end(); ++pkg)
    {
      pkgDepCache::StateCache &state=(*apt_cache_file)[pkg];

      if(state.Install())
	{
	  pkgCache::VerIterator curr=pkg.CurrentVer();
	  pkgCache::VerIterator cand=state.InstVerIter(*apt_cache_file);

	  if((curr.end() || package_trusted(curr)) &&
	     !package_trusted(cand))
	    untrusted.push_back(cand);
	}
    }

  if(!untrusted.empty())
    {
      vector<fragment *> frags;

      frags.push_back(wrapbox(fragf(_("%BWARNING%b: untrusted versions of the following packages will be installed!%n%n"
				      "Untrusted packages could %Bcompromise your system's security%b.  "
				      "You should only proceed with the installation if you are certain that this is what you want to do.%n%n"), "Error")));

      for(vector<pkgCache::VerIterator>::const_iterator i=untrusted.begin();
	  i!=untrusted.end(); ++i)
	frags.push_back(clipbox(fragf(_("  %S*%N %s [version %s]%n"),
				      "Bullet",
				      i->ParentPkg().Name(), i->VerStr())));

      main_stacked->add_visible_widget(vs_dialog_yesno(sequence_fragment(frags),
						       arg(sigc::ptr_fun(install_or_remove_packages)),
						       transcode(_("Really Continue")),
						       NULL,
						       transcode(_("Abort Installation")),
						       get_style("TrustWarning"),
						       true,
						       false),
				       true);
    }
  else
    install_or_remove_packages();
}

static void actually_do_package_run();

static void reset_preview()
{
  active_preview=NULL;
  active_preview_tree=NULL;
}

// One word: ewwwwwwww.  I REALLY need to get my act together on the
// view-customization stuff.
static void do_show_preview()
{
  if(!active_preview_tree.valid())
    {
      assert(!active_preview.valid());

      pkg_grouppolicy_factory *grp=NULL;
      std::string grpstr;

      if(aptcfg->Exists(PACKAGE "::UI::Default-Preview-Grouping"))
	{
	  grpstr=aptcfg->Find(PACKAGE "::UI::Default-Preview-Grouping");
	  grp=parse_grouppolicy(grpstr);
	}

      //if(!grp && aptcfg->Exists(PACKAGE "::UI::Default-Grouping"))
	//{
	//  grpstr=aptcfg->Find(PACKAGE "::UI::Default-Grouping");
	//  grp=parse_grouppolicy(grpstr);
	//}

      if(!grp)
	{
	  grpstr="action";
	  grp=new pkg_grouppolicy_mode_factory(new pkg_grouppolicy_end_factory);
	}

      if(aptcfg->Exists(PACKAGE "::UI::Preview-Limit"))
	active_preview_tree=pkg_tree::create(grpstr.c_str(), grp, transcode(aptcfg->Find(PACKAGE "::UI::Preview-Limit").c_str()));
      else
	active_preview_tree=pkg_tree::create(grpstr.c_str(), grp);

      active_preview=make_default_view(active_preview_tree,
				       &active_preview_tree->selected_signal,
				       &active_preview_tree->selected_desc_signal,
				       true,
				       true);

      active_preview->destroyed.connect(sigc::ptr_fun(reset_preview));
      active_preview->connect_key("DoInstallRun",
				  &global_bindings,
				  sigc::ptr_fun(actually_do_package_run));
      add_main_widget(active_preview, _("Preview of package installation"),
		      _("View and/or adjust the actions that will be performed"),
		      _("Preview"));

      vs_progress_ref p=gen_progress_bar();
      active_preview_tree->build_tree(*p.unsafe_get_ref());
      p->destroy();
    }
  else
    {
      assert(active_preview.valid());
      active_preview->show();
    }
}

static void fixer_dialog_done()
{
  if(active_preview_tree.valid())
    active_preview_tree->build_tree();
  do_package_run_or_show_preview();
}

static void install_fixer_dialog()
{
  vs_widget_ref w=make_solution_dialog();
  w->destroyed.connect(sigc::ptr_fun(fixer_dialog_done));
  popup_widget(w, true);
}

// FIXME: blocks.
static void auto_fix_broken()
{
  undo_group *undo=new apt_undo_group;

  try
    {
      assert(resman != NULL);
      assert(resman->resolver_exists());

      aptitude_solution sol = resman->get_solution(resman->get_selected_solution(), 0);

      (*apt_cache_file)->apply_solution(sol, undo);
      show_message(fragf("%s%n%n%F",
			 _("Some packages were broken and have been fixed:"),
			 solution_fragment(sol)),
		   NULL);
    }
  catch(NoMoreSolutions)
    {
      show_message(_("No solution to these dependency problems exists!"),
		   NULL,
		   get_style("Error"));
    }
  catch(NoMoreTime)
    {
      show_message(fragf(_("Ran out of time while trying to resolve dependencies (press \"%s\" to try harder)"),
			 global_bindings.readable_keyname("NextSolution").c_str()),
		   NULL,
		   get_style("Error"));
    }

  if(!undo->empty())
    apt_undos->add_item(undo);
  else
    delete undo;

  if(active_preview_tree.valid())
    active_preview_tree->build_tree();
}


//  Huge FIXME: the preview interacts badly with the menu.  This can be solved
// in a couple ways, including having the preview be a popup dialog -- the best
// thing IMO, though, would be to somehow allow particular widgets to override
// the meaning of global commands.  This needs a little thought, though.  (fake
// keys?  BLEACH)
static void actually_do_package_run()
{
  if(apt_cache_file)
    {
      if(!active_download)
	{
	  // whatever we call will chain to the next appropriate
	  // routine.
	  if((*apt_cache_file)->BrokenCount()>0)
	    {
	      if(_config->FindB(PACKAGE "::Auto-Fix-Broken", true))
		{
		  auto_fix_broken();
		  do_show_preview();
		}
	      else
		install_fixer_dialog();

	      return;
	    }

	  if(getuid()==0  || !aptcfg->FindB(PACKAGE "::Warn-Not-Root", true))
	    check_package_trust();
	  else
	    {
	      popup_widget(vs_dialog_yesno(wrapbox(text_fragment(_("Installing/removing packages requires administrative privileges, which you currently do not have.  Would you like to change to the root account?"))),
					   arg(sigc::bind(sigc::ptr_fun(&do_su_to_root),
						      "-i")),
					   transcode(_("Become root")),
					   arg(sigc::ptr_fun(&check_package_trust)),
					   transcode(_("Don't become root")),
					   get_style("Error")));
	    }
	}
      else
	show_message(_("A package-list update or install run is already taking place."), NULL, get_style("Error"));
    }
}

void do_package_run_or_show_preview()
{
  // UI: check that something will actually be done first.
  bool some_action_happening=false;
  bool some_non_simple_keep_happening=false;
  for(pkgCache::PkgIterator i=(*apt_cache_file)->PkgBegin(); !i.end(); ++i)
    {
      pkg_action_state state=find_pkg_state(i);

      if(state!=pkg_unchanged)
	{
	  some_action_happening=true;

	  if(!(state==pkg_hold && !(*apt_cache_file)->is_held(i)))
	    some_non_simple_keep_happening=true;
	}

      if(some_action_happening && some_non_simple_keep_happening)
	break;
    }

  if(!some_action_happening)
    {
      show_message(_("No packages are scheduled to be installed, removed, or upgraded."));
      return;
    }
  else if(!some_non_simple_keep_happening &&
	  !aptcfg->FindB(PACKAGE "::Allow-Null-Upgrade", false))
    {
      show_message(_("No packages will be installed, removed or upgraded.  "
		     "Some packages could be upgraded, but you have not chosen to upgrade them.  "
		     "Type \"U\" to prepare an upgrade."));
      return;
    }

  if(apt_cache_file)
    {
      if(!active_preview.valid() || !active_preview->get_visible())
	{
	  if(aptcfg->FindB(PACKAGE "::Display-Planned-Action", true))
	    do_show_preview();
	  else
	    actually_do_package_run();
	}
      else
	{
	  active_preview_tree->build_tree();
	  // We need to rebuild the tree since this is called after a
	  // broken-fixing operation.  This feels like a hack, though..
	  active_preview->show();
	}
    }
}

static bool can_start_download()
{
  return !active_download;
}

static bool can_start_download_and_install()
{
  return !active_download && apt_cache_file != NULL;
}

void do_package_run()
{
  if(apt_cache_file)
    {
      if(active_preview_tree.valid() && active_preview_tree->get_visible())
	actually_do_package_run();
      else if((*apt_cache_file)->BrokenCount()>0)
	{
	  if(aptcfg->FindB(PACKAGE "::Auto-Fix-Broken", true))
	    {
	      auto_fix_broken();
	      do_package_run_or_show_preview();
	    }
	  else
	    install_fixer_dialog();
	}
      else
	do_package_run_or_show_preview();
    }
}

static void lists_autoclean_msg(download_update_manager *m)
{
  vs_widget_ref msg = vs_center::create(vs_frame::create(vs_label::create(_("Deleting obsolete downloaded files"))));
  m->post_autoclean_hook.connect(sigc::mem_fun(msg.unsafe_get_ref(),
					       &vscreen_widget::destroy));

  popup_widget(msg);
  vscreen_tryupdate();
}

void really_do_update_lists()
{
  download_update_manager *m = new download_update_manager;
  m->pre_autoclean_hook.connect(sigc::bind(sigc::ptr_fun(lists_autoclean_msg),
					   m));
  m->post_forget_new_hook.connect(package_states_changed.make_slot());
  (new ui_download_manager(m, false, true, false,
			   _("Updating package lists"),
			   _("View the progress of the package list update"),
			   _("List Update")))->start();
}

void do_update_lists()
{
  if(!active_download)
    {
      if(getuid()==0 || !aptcfg->FindB(PACKAGE "::Warn-Not-Root", true))
	really_do_update_lists();
      else
	{
	  popup_widget(vs_dialog_yesno(wrapbox(text_fragment(_("Updating the package lists requires administrative privileges, which you currently do not have.  Would you like to change to the root account?"))),
				       arg(sigc::bind(sigc::ptr_fun(&do_su_to_root),
						      "-u")),
				       transcode(_("Become root")),
				       arg(sigc::ptr_fun(&really_do_update_lists)),
				       transcode(_("Don't become root")),
				       get_style("Error")));
	}
    }
  else
    show_message(_("A package-list update or install run is already taking place."), NULL, get_style("Error"));
}

static void do_sweep()
{
  add_main_widget(cmine::create(), _("Minesweeper"), _("Waste time trying to find mines"), _("Minesweeper"));
}

static void do_clean()
{
  if(active_download)
    // Erk!  That's weird!
    _error->Error(_("Cleaning while a download is in progress is not allowed"));
  else
    {
      vs_widget_ref msg=vs_center::create(vs_frame::create(vs_label::create(_("Deleting downloaded files"))));
      msg->show_all();
      popup_widget(msg);
      vscreen_tryupdate();

      if(aptcfg)
	{
	  pkgAcquire fetcher;
	  fetcher.Clean(aptcfg->FindDir("Dir::Cache::archives"));
	  fetcher.Clean(aptcfg->FindDir("Dir::Cache::archives")+"partial/");
	}

      msg->destroy();

      show_message(_("Downloaded package files have been deleted"));
    }
}

// Ok, this is, uh, weird.  Erase has to be overridden to at least
// call unlink()
//
// Should I list what I'm doing like apt-get does?
class my_cleaner:public pkgArchiveCleaner
{
  long total_size;
protected:
  virtual void Erase(const char *file,
		     string pkg,
		     string ver,
		     struct stat &stat)
  {
    if(unlink(file)==0)
      total_size+=stat.st_size;
  }
public:
  my_cleaner();
  long get_total_size() {return total_size;}
};

// g++ bug?  If I include this implementation above in the class
// def'n, it never gets called!
my_cleaner::my_cleaner()
  :total_size(0)
{
}

static bool do_autoclean_enabled()
{
  return apt_cache_file != NULL;
}

static void do_autoclean()
{
  if(apt_cache_file == NULL)
    _error->Error(_("The apt cache file is not available; cannot auto-clean."));
  else if(active_download)
    // Erk!  That's weird!
    _error->Error(_("Cleaning while a download is in progress is not allowed"));
  else
    {
      vs_widget_ref msg=vs_center::create(vs_frame::create(vs_label::create(_("Deleting obsolete downloaded files"))));
      msg->show_all();
      popup_widget(msg);
      vscreen_tryupdate();

      long cleaned_size=0;

      if(aptcfg)
	{
	  my_cleaner cleaner;

	  cleaner.Go(aptcfg->FindDir("Dir::Cache::archives"), *apt_cache_file);
	  cleaner.Go(aptcfg->FindDir("Dir::Cache::archives")+"partial/",
		     *apt_cache_file);

	  cleaned_size=cleaner.get_total_size();
	}

      msg->destroy();

      show_message(ssprintf(_("Obsolete downloaded package files have been deleted, freeing %sB of disk space."),
			    SizeToStr(cleaned_size).c_str()));
    }
}

static bool do_mark_upgradable_enabled()
{
  return apt_cache_file != NULL;
}

static void do_mark_upgradable()
{
  if(apt_cache_file)
    {
      undo_group *undo=new apt_undo_group;

      (*apt_cache_file)->mark_all_upgradable(true, true, undo);

      if(!undo->empty())
	apt_undos->add_item(undo);
      else
	delete undo;

      package_states_changed();
    }
}

static bool forget_new_enabled()
{
  if(!apt_cache_file)
    return false;
  else
    return (*apt_cache_file)->get_new_package_count()>0;
}

void do_forget_new()
{
  if(apt_cache_file)
    {
      undoable *undo=NULL;
      (*apt_cache_file)->forget_new(&undo);
      if(undo)
	apt_undos->add_item(undo);

      package_states_changed();
    }
}

#ifdef WITH_RELOAD_CACHE
static void do_reload_cache()
{
  vs_progress_ref p = gen_progress_bar();
  apt_reload_cache(p.unsafe_get_ref(), true);
  p->destroy();
}
#endif

static void start_solution_calculation();

class interactive_continuation : public resolver_manager::background_continuation
{
  /** The manager associated with this continuation; usually resman. */
  resolver_manager *manager;

  /** Indicate that a new solution is available by invoking the
   *  selection_changed signal.
   */
  class success_event : public vscreen_event
  {
    resolver_manager *manager;
  public:
    success_event(resolver_manager *_manager)
      :manager(_manager)
    {
    }

    void dispatch()
    {
      manager->state_changed();
    }
  };

  class no_more_solutions_event : public vscreen_event
  {
    resolver_manager *manager;
  public:
    no_more_solutions_event(resolver_manager *_manager)
      :manager(_manager)
    {
    }

    void dispatch()
    {
      resolver_manager::state st = manager->state_snapshot();

      if(st.selected_solution == st.generated_solutions)
	manager->select_previous_solution();
      show_message(_("No more solutions."));

      manager->state_changed();
    }
  };
public:
  interactive_continuation(resolver_manager *_manager)
    :manager(_manager)
  {
  }

  void success(const aptitude_solution &sol)
  {
    vscreen_post_event(new success_event(manager));
  }

  void no_more_solutions()
  {
    vscreen_post_event(new no_more_solutions_event(manager));
  }

  void no_more_time()
  {
    start_solution_calculation();
  }

  void interrupted()
  {
  }
};

// If the current solution pointer is past the end of the solution
// list, queue up a calculation for it in the background thread.
static void start_solution_calculation()
{
  resolver_manager::state state = resman->state_snapshot();

  if(state.resolver_exists &&
     state.selected_solution == state.generated_solutions &&
     !state.background_thread_active)
    resman->get_solution_background_blocking(resman->get_selected_solution(),
					     aptcfg->FindI(PACKAGE "::ProblemResolver::StepLimit", 5000),
					     aptcfg->FindI(PACKAGE "::ProblemResolver::WaitSteps", 50),
					     new interactive_continuation(resman));
}

static void do_connect_resolver_callback()
{
  resman->state_changed.connect(sigc::ptr_fun(&start_solution_calculation));
  // We may have missed a signal before making the connection:
  start_solution_calculation();
  resman->state_changed.connect(sigc::ptr_fun(&vscreen_update));
}

static bool do_next_solution_enabled()
{
  if(resman == NULL)
    return false;

  resolver_manager::state state = resman->state_snapshot();

  return
    state.selected_solution < state.generated_solutions &&
    !(state.selected_solution + 1 == state.generated_solutions &&
      state.solutions_exhausted);
}

void do_next_solution()
{
  if(!do_next_solution_enabled())
    beep();
  else
    resman->select_next_solution();
}

static bool do_previous_solution_enabled()
{
  if(resman == NULL)
    return false;

  resolver_manager::state state = resman->state_snapshot();

  return state.selected_solution > 0;
}

void do_previous_solution()
{
  if(!do_previous_solution_enabled())
    beep();
  else
    resman->select_previous_solution();
}

static bool do_first_solution_enabled()
{
  if(resman == NULL)
    return false;

  resolver_manager::state state = resman->state_snapshot();

  return state.resolver_exists && state.selected_solution > 0;
}

static void do_first_solution()
{
  if(!do_first_solution_enabled())
    beep();
  else
    resman->select_solution(0);
}

static bool do_last_solution_enabled()
{
  if(resman == NULL)
    return false;

  resolver_manager::state state = resman->state_snapshot();

  return state.resolver_exists && state.selected_solution + 1 < state.generated_solutions;
}

static void do_last_solution()
{
  if(resman == NULL)
    {
      beep();
      return;
    }

  resolver_manager::state state = resman->state_snapshot();

  if(!(state.resolver_exists && state.selected_solution + 1 < state.generated_solutions))
    beep();
  else
    resman->select_solution(state.generated_solutions - 1);
}

static bool do_apply_solution_enabled_from_state(const resolver_manager::state &state)
{
  return
    state.resolver_exists &&
    state.selected_solution >= 0 &&
    state.selected_solution < state.generated_solutions;
}

static bool do_apply_solution_enabled()
{
  if(resman == NULL)
    return false;

  resolver_manager::state state = resman->state_snapshot();
  return do_apply_solution_enabled_from_state(state);
}

void do_apply_solution()
{
  if(!apt_cache_file)
    return;

  resolver_manager::state state = resman->state_snapshot();

  if(!do_apply_solution_enabled_from_state(state))
    {
      beep();
      return;
    }
  else
    {
      undo_group *undo=new apt_undo_group;
      try
	{
	  (*apt_cache_file)->apply_solution(resman->get_solution(state.selected_solution, aptcfg->FindI(PACKAGE "::ProblemResolver::StepLimit", 5000)),
					    undo);
	}
      catch(NoMoreSolutions)
	{
	  show_message(_("Unable to find a solution to apply."),
		       NULL,
		       get_style("Error"));
	}
      catch(NoMoreTime)
	{
	  show_message(_("Ran out of time while trying to find a solution."),
		       NULL,
		       get_style("Error"));
	}

      if(!undo->empty())
	{
	  apt_undos->add_item(undo);
	  package_states_changed();
	}
      else
	delete undo;
    }
}

static void do_nullify_solver(vs_widget_ref *solver)
{
  *solver = NULL;
}

static bool do_examine_solution_enabled()
{
  return resman != NULL && resman->resolver_exists();
}

void do_examine_solution()
{
  if(!do_examine_solution_enabled())
    {
      beep();
      return;
    }

  static vs_widget_ref solver;

  if(solver.valid())
    solver->show();
  else
    {
      solver = make_solution_screen();
      solver->destroyed.connect(sigc::bind(sigc::ptr_fun(&do_nullify_solver),
					   &solver));

      add_main_widget(solver, _("Resolve Dependencies"),
		      _("Search for solutions to unsatisfied dependencies"),
		      _("Resolve Dependencies"));
    }
}

static void handle_dump_resolver_response(const wstring &s)
{
  if(resman != NULL && resman->resolver_exists())
    {
      ofstream out(transcode(s).c_str());

      if(!out)
	_error->Errno("dump_resolver", _("Unable to open %ls"), s.c_str());
      else
	{
	  resman->dump(out);

	  if(!out)
	    _error->Errno("dump_resolver", _("Error while dumping resolver state"));
	}
    }
}

static void do_dump_resolver()
{
  static vs_editline::history_list history;

  if(resman != NULL && resman->resolver_exists())
    prompt_string(_("File to which the resolver state should be dumped:"),
		  "",
		  arg(sigc::ptr_fun(handle_dump_resolver_response)),
		  NULL,
		  NULL,
		  &history);
}

// NOTE ON TRANSLATIONS!
//
//   Implicitly translating stuff in the widget set would be ugly.  Everything
// would need to make sure its input to the widget set was able to survive
// translation.
//
//   Using N_ here and translating when we need to display the description
// would be ugly.  Everything would need to make sure its input to the UI would
// be able to handle translation.
//
//   What I do is ugly, but it doesn't force other bits of the program to handle
// input to us with velvet gloves, or otherwise break stuff.  So these structures
// just contain a char *.  I can modify the char *.  In particular, I can mark
// these for translation, then walk through them and munge the pointers to point
// to the translated version.  "EWWWWW!" I hear you say, and you're right, but
// the alternatives are worse.

vs_menu_info actions_menu[]={
  //{vs_menu_info::VS_MENU_ITEM, N_("Test ^Errors"), NULL,
  //N_("Generate an APT error for testing purposes"),
  //SigC::bind(SigC::slot(&silly_test_error), "This is a test error item.")},

  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("^Install/remove packages"), "DoInstallRun",
	       N_("Perform all pending installs and removals"), sigc::ptr_fun(do_package_run), sigc::ptr_fun(can_start_download_and_install)),

  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("^Update package list"), "UpdatePackageList",
	       N_("Check for new versions of packages"), sigc::ptr_fun(do_update_lists), sigc::ptr_fun(can_start_download)),

  // FIXME: this is a bad name for the menu item.
  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("^Forget new packages"), "ForgetNewPackages",
	       N_("Forget which packages are \"new\""),
	       sigc::ptr_fun(do_forget_new), sigc::ptr_fun(forget_new_enabled)),

  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("^Clean package cache"), NULL,
	       N_("Delete package files which were previously downloaded"),
	       sigc::ptr_fun(do_clean)),

  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("Clean ^obsolete files"), NULL,
	       N_("Delete package files which can no longer be downloaded"),
	       sigc::ptr_fun(do_autoclean), sigc::ptr_fun(do_autoclean_enabled)),

  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("Mark ^Upgradable"), "MarkUpgradable",
	       N_("Mark all upgradable packages which are not held for upgrade"),
	       sigc::ptr_fun(do_mark_upgradable), sigc::ptr_fun(do_mark_upgradable_enabled)),

  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("^Play Minesweeper"), NULL,
	       N_("Waste time trying to find mines"), sigc::ptr_fun(do_sweep)),

  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("^Become root"), NULL,
	       N_("Run 'su' to become root; this will restart the program, but your settings will be preserved"), sigc::bind(sigc::ptr_fun(do_su_to_root), ""), sigc::ptr_fun(su_to_root_enabled)),

#ifdef WITH_RELOAD_CACHE
  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("^Reload package cache"), NULL,
	       N_("Reload the package cache"),
	       sigc::ptr_fun(do_reload_cache)),
#endif

  VS_MENU_SEPARATOR,

  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("^Quit"), "QuitProgram",
	       N_("Exit the program"), sigc::ptr_fun(do_quit)),

  VS_MENU_END
};

vs_menu_info undo_menu[]={
  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("Undo"), "Undo",
	       N_("Undo the last package operation or group of operations"),
	       sigc::hide_return(undo_undo.make_slot()),
	       undo_undo_enabled.make_slot()),

  VS_MENU_END
};

vs_menu_info package_menu[]={
  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("^Install"), "Install",
	       N_("Flag the currently selected package for installation or upgrade"),
	       sigc::hide_return(package_install.make_slot()),
	       package_menu_enabled.make_slot()),
  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("^Remove"), "Remove",
	       N_("Flag the currently selected package for removal"),
	       sigc::hide_return(package_remove.make_slot()),
	       package_menu_enabled.make_slot()),
  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("^Purge"), "Purge",
	       N_("Flag the currently selected package and its configuration files for removal"),
	       sigc::hide_return(package_purge.make_slot()),
	       package_menu_enabled.make_slot()),
  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("^Keep"), "Keep",
	       N_("Cancel any action on the selected package"),
	       sigc::hide_return(package_keep.make_slot()),
	       package_menu_enabled.make_slot()),
  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("^Hold"), "Hold",
	       N_("Cancel any action on the selected package, and protect it from future upgrades"),
	       sigc::hide_return(package_hold.make_slot()),
	       package_menu_enabled.make_slot()),
  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("Mark ^Auto"), "SetAuto",
	       N_("Mark the selected package as having been automatically installed; it will automatically be removed if no other packages depend on it"),
	       sigc::hide_return(package_mark_auto.make_slot()),
	       package_menu_enabled.make_slot()),
  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("Mark ^Manual"), "ClearAuto",
	       N_("Mark the selected package as having been manually installed; it will not be removed unless you manually remove it"),
	       sigc::hide_return(package_unmark_auto.make_slot()),
	       package_menu_enabled.make_slot()),
  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("^Forbid Version"), "ForbidUpgrade",
	       N_("Forbid the candidate version of the selected package from being installed; newer versions of the package will be installed as usual"),
	       sigc::hide_return(package_forbid.make_slot()),
	       package_forbid_enabled.make_slot()),
  VS_MENU_SEPARATOR,
  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("I^nformation"), "InfoScreen",
	       N_("Display more information about the selected package"),
	       sigc::hide_return(package_information.make_slot()),
	       package_information_enabled.make_slot()),
  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("^Changelog"), "Changelog",
	       N_("Display the Debian changelog of the selected package"),
	       sigc::hide_return(package_changelog.make_slot()),
	       package_changelog_enabled.make_slot()),
  VS_MENU_END
};

vs_menu_info resolver_menu[] = {
  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("^Examine Solution"),
	       "ExamineSolution", N_("Examine the currently selected solution to the dependency problems."),
	       sigc::ptr_fun(do_examine_solution),
	       sigc::ptr_fun(do_examine_solution_enabled)),
  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("Apply ^Solution"),
	       "ApplySolution", N_("Perform the actions contained in the currently selected solution."),
	       sigc::ptr_fun(do_apply_solution),
	       sigc::ptr_fun(do_apply_solution_enabled)),
  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("^Next Solution"),
	       "NextSolution", N_("Select the next solution to the dependency problems."),
	       sigc::ptr_fun(do_next_solution),
	       sigc::ptr_fun(do_next_solution_enabled)),
  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("^Previous Solution"),
	       "PrevSolution", N_("Select the previous solution to the dependency problems."),
	       sigc::ptr_fun(do_previous_solution),
	       sigc::ptr_fun(do_previous_solution_enabled)),
  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("^First Solution"),
	       "FirstSolution", N_("Select the first solution to the dependency problems."),
	       sigc::ptr_fun(do_first_solution),
	       sigc::ptr_fun(do_first_solution_enabled)),
  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("^Last Solution"),
	       "LastSolution", N_("Select the last solution to the dependency problems that has been generated so far."),
	       sigc::ptr_fun(do_last_solution),
	       sigc::ptr_fun(do_last_solution_enabled)),

  VS_MENU_SEPARATOR,

  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("Toggle ^Rejected"),
	       "SolutionActionReject", N_("Toggle whether the currently selected action is rejected."),
	       sigc::hide_return(resolver_toggle_rejected.make_slot()),
	       resolver_toggle_rejected_enabled.make_slot()),

  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("Toggle ^Approved"),
	       "SolutionActionApprove", N_("Toggle whether the currently selected action is approved."),
	       sigc::hide_return(resolver_toggle_approved.make_slot()),
	       resolver_toggle_approved_enabled.make_slot()),

  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("^View Target"),
	       "InfoScreen", N_("View the package which will be affected by the selected action"),
	       sigc::hide_return(resolver_view_target.make_slot()),
	       resolver_view_target_enabled.make_slot()),

  VS_MENU_END
};

vs_menu_info search_menu[]={
  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("^Find"), "Search",
	       N_("Search forwards"),
	       sigc::hide_return(find_search.make_slot()),
	       find_search_enabled.make_slot()),
  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("^Find Backwards"), "SearchBack",
	       N_("Search backwards"),
	       sigc::hide_return(find_search_back.make_slot()),
	       find_search_back_enabled.make_slot()),
  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("Find ^Again"), "ReSearch",
	       N_("Repeat the last search"),
	       sigc::hide_return(find_research.make_slot()),
	       find_research_enabled.make_slot()),
  VS_MENU_SEPARATOR,
  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("^Limit Display"),
	       "ChangePkgTreeLimit", N_("Apply a filter to the package list"),
	       sigc::hide_return(find_limit.make_slot()),
	       find_limit_enabled.make_slot()),
  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("^Un-Limit Display"),
	       NULL, N_("Remove the filter from the package list"),
	       sigc::hide_return(find_cancel_limit.make_slot()),
	       find_cancel_limit_enabled.make_slot()),
  VS_MENU_SEPARATOR,
  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("Find ^Broken"),
	       "SearchBroken", N_("Find the next package with unsatisfied dependencies"),
	       sigc::hide_return(find_broken.make_slot()),
	       find_broken_enabled.make_slot()),
  VS_MENU_END
};

vs_menu_info options_menu[]={
  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("^UI options"), NULL,
	       N_("Change the settings which affect the user interface"),
	       sigc::ptr_fun(do_show_ui_options_dlg)),

  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("^Dependency handling"), NULL,
	       N_("Change the settings which affect how package dependencies are handled"),
	       sigc::ptr_fun(do_show_dependency_options_dlg)),

  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("^Miscellaneous"), NULL,
	       N_("Change miscellaneous program settings"),
	       sigc::ptr_fun(do_show_misc_options_dlg)),

  VS_MENU_SEPARATOR,

  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("^Revert options"), NULL,
	       N_("Reset all settings to the system defaults"),
	       sigc::ptr_fun(do_revert_options)),

  //{vs_menu_info::VS_MENU_ITEM, N_("^Save options"), NULL,
  // N_("Save current settings for future sessions"), arg(bind(sigc::ptr_fun(apt_dumpcfg)),
  //							 PACKAGE)},

  VS_MENU_END
};

vs_menu_info views_menu_info[]={
  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("^Next"), "CycleNext",
	       N_("View next display"), sigc::ptr_fun(do_view_next),
	       sigc::ptr_fun(view_next_prev_enabled)),

  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("^Prev"), "CyclePrev",
	       N_("View previous display"), sigc::ptr_fun(do_view_prev),
	       sigc::ptr_fun(view_next_prev_enabled)),

  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("^Close"), "Quit",
	       N_("Close this display"), sigc::ptr_fun(do_destroy_visible),
	       sigc::ptr_fun(any_view_visible)),

  VS_MENU_SEPARATOR,

  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("New Package ^View"), NULL,
	       N_("Create a new default package view"),
	       sigc::ptr_fun(do_new_package_view_with_new_bar)),

  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("Audit ^Recommendations"), NULL,
	       N_("View packages which it is recommended that you install, but which are not currently installed."),
	       sigc::ptr_fun(do_new_recommendations_view)),

  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("New ^Flat Package List"), NULL,
	       N_("View all the packages on the system in a single uncategorized list"),
	       sigc::ptr_fun(do_new_flat_view_with_new_bar)),

  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("New ^Debtags Browser"),
	       NULL,
	       N_("Browse packages using Debtags data"),
	       sigc::ptr_fun(do_new_tag_view_with_new_bar)),

  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("New Categorical ^Browser"),
	       NULL,
	       N_("Browse packages by category"),
	       sigc::ptr_fun(do_new_hier_view_with_new_bar)),

  VS_MENU_SEPARATOR,
  VS_MENU_END
};

vs_menu_info help_menu_info[]={
  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("^About"), NULL,
	       N_("View information about this program"),
	       sigc::ptr_fun(do_help_about)),

  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("^Help"), "Help",
	       N_("View the on-line help"), sigc::ptr_fun(do_help_help)),

  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("User's ^Manual"), NULL,
	       N_("View the detailed program manual"),
	       sigc::ptr_fun(do_help_readme)),

  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("^FAQ"), NULL,
	       N_("View a list of frequently asked questions"),
	       sigc::ptr_fun(do_help_faq)),

  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("^NEWS"), NULL,
	       N_("View the important changes made in each version of " PACKAGE),
	       sigc::ptr_fun(do_help_news)),

  vs_menu_info(vs_menu_info::VS_MENU_ITEM, N_("^License"), NULL,
	       N_("View the terms under which you may copy the program"),
	       sigc::ptr_fun(do_help_license)),

  VS_MENU_END
};

// This is responsible for converting a particular menu-info thingy.
static void munge_menu(vs_menu_info *menu)
{
  while(menu->item_type!=vs_menu_info::VS_MENU_END)
    {
      if(menu->item_description)
	menu->item_description=_(menu->item_description);

      menu->item_name=_(menu->item_name);

      ++menu;
    }
}

static void do_show_menu_description(vs_menu_item *item, const vs_label_ref &label)
{
  if(item && item->get_description().size()>0)
    {
      label->show();
      label->set_text(wrapbox(text_fragment(item->get_description())));
    }
  else
    {
      label->hide();
      label->set_text("");
    }
}

// I want discardable signal arguments!
static void do_setup_columns()
{
  pkg_item::pkg_columnizer::setup_columns(true);
}

static void load_options(string base, bool usetheme)
{
  load_styles(base+"::Styles", usetheme);
  load_bindings(base+"::Keybindings", &global_bindings, usetheme);
  load_bindings(base+"::Keybindings::EditLine", vs_editline::bindings, usetheme);
  load_bindings(base+"::Keybindings::Menu", vs_menu::bindings, usetheme);
  load_bindings(base+"::Keybindings::Menubar", vs_menubar::bindings, usetheme);
  load_bindings(base+"::Keybindings::Minesweeper", cmine::bindings, usetheme);
  load_bindings(base+"::Keybindings::MinibufChoice", vs_statuschoice::bindings, usetheme);
  load_bindings(base+"::Keybindings::Pager", vs_pager::bindings, usetheme);
  load_bindings(base+"::KeyBindings::PkgNode", pkg_tree_node::bindings, usetheme);
  load_bindings(base+"::Keybindings::PkgTree", pkg_tree::bindings, usetheme);
  load_bindings(base+"::Keybindings::Table", vs_table::bindings, usetheme);
  load_bindings(base+"::Keybindings::TextLayout", vs_text_layout::bindings, usetheme);
  load_bindings(base+"::Keybindings::Tree", vs_tree::bindings, usetheme);
}

static vs_menu_ref add_menu(vs_menu_info *info, const std::string &name,
			    const vs_label_ref &menu_description)
{
  munge_menu(info);

  vs_menu_ref menu=vs_menu::create(0, 0, 0, info);

  main_menu->append_item(transcode(name), menu);

  menu->item_highlighted.connect(sigc::bind(sigc::ptr_fun(do_show_menu_description),
					    menu_description));

  return menu;
}

// argh
class help_bar:public vs_label
{
public:
  help_bar(const wstring &txt, const style &st):vs_label(txt, st)
  {
    set_visibility();
  }

  inline void set_visibility()
  {
    set_visible(aptcfg->FindB(PACKAGE "::UI::HelpBar", true));
  }
};

void ui_init()
{
  vscreen_init();
  init_defaults();

  // The basic behavior of the package state signal is to update the
  // display.
  package_states_changed.connect(sigc::ptr_fun(vscreen_update));

  consume_errors.connect(sigc::ptr_fun(check_apt_errors));

  if(aptcfg->Exists(PACKAGE "::Theme"))
    load_options(PACKAGE "::Themes::"+string(aptcfg->Find(PACKAGE "::Theme"))+"::"+PACKAGE "::UI", true);

  load_options(PACKAGE "::UI", false);

  vs_label_ref menu_description=vs_label::create("");

  main_menu=vs_menubar::create(!aptcfg->FindB(PACKAGE "::UI::Menubar-Autohide", false));

  aptcfg->connect(string(PACKAGE "::UI::Menubar-Autohide"),
		  sigc::ptr_fun(update_menubar_autohide));
  aptcfg->connect(string(PACKAGE "::UI::Package-Display-Format"),
		  sigc::ptr_fun(do_setup_columns));

  cache_closed.connect(sigc::ptr_fun(do_show_reload_message));
  cache_reloaded.connect(sigc::ptr_fun(do_hide_reload_message));
  cache_reloaded.connect(sigc::ptr_fun(do_connect_resolver_callback));
  if(apt_cache_file)
    {
      do_connect_resolver_callback();
      start_solution_calculation();
    }
  cache_reload_failed.connect(sigc::ptr_fun(do_hide_reload_message));

  cache_reloaded.connect(sigc::ptr_fun(do_connect_read_only_callbacks));
  if(apt_cache_file)
    do_connect_read_only_callbacks();

  add_menu(actions_menu, _("Actions"), menu_description);
  add_menu(undo_menu, _("Undo"), menu_description);
  add_menu(package_menu, _("Package"), menu_description);
  add_menu(resolver_menu, _("Resolver"), menu_description);
  add_menu(search_menu, _("Search"), menu_description);
  add_menu(options_menu, _("Options"), menu_description);
  views_menu=add_menu(views_menu_info, _("Views"), menu_description);
  add_menu(help_menu_info, _("Help"), menu_description);

  main_stacked=vs_stacked::create();
  main_menu->set_subwidget(main_stacked);
  main_stacked->show();

  main_stacked->connect_key_post("QuitProgram", &global_bindings, sigc::ptr_fun(do_quit));
  main_stacked->connect_key_post("Quit", &global_bindings, sigc::ptr_fun(do_destroy_visible));
  main_stacked->connect_key_post("CycleNext",
				 &global_bindings,
				 sigc::ptr_fun(do_view_next));
  main_stacked->connect_key_post("CyclePrev",
				 &global_bindings,
				 sigc::ptr_fun(do_view_prev));
  main_stacked->connect_key_post("DoInstallRun",
				 &global_bindings,
				 sigc::ptr_fun(do_package_run));
  main_stacked->connect_key_post("UpdatePackageList",
				 &global_bindings,
				 sigc::ptr_fun(do_update_lists));
  main_stacked->connect_key_post("MarkUpgradable",
				 &global_bindings,
				 sigc::ptr_fun(do_mark_upgradable));
  main_stacked->connect_key_post("ForgetNewPackages",
				 &global_bindings,
				 sigc::ptr_fun(do_forget_new));
  main_stacked->connect_key_post("Help",
				 &global_bindings,
				 sigc::ptr_fun(do_help_help));
  main_stacked->connect_key_post("Undo",
				 &global_bindings,
				 sigc::hide_return(undo_undo.make_slot()));
  main_stacked->connect_key_post("NextSolution",
				 &global_bindings,
				 sigc::ptr_fun(do_next_solution));
  main_stacked->connect_key_post("PrevSolution",
				 &global_bindings,
				 sigc::ptr_fun(do_previous_solution));
  main_stacked->connect_key_post("FirstSolution",
				 &global_bindings,
				 sigc::ptr_fun(do_first_solution));
  main_stacked->connect_key_post("LastSolution",
				 &global_bindings,
				 sigc::ptr_fun(do_last_solution));
  main_stacked->connect_key_post("ApplySolution",
				 &global_bindings,
				 sigc::ptr_fun(do_apply_solution));
  main_stacked->connect_key_post("ExamineSolution",
				 &global_bindings,
				 sigc::ptr_fun(do_examine_solution));
  main_stacked->connect_key_post("DumpResolver",
				 &global_bindings,
				 sigc::ptr_fun(do_dump_resolver));

  main_table=vs_table::create();
  main_stacked->add_widget(main_table);
  main_table->show();

  // FIXME: highlight the keys.
  wstring menu_key=global_bindings.readable_keyname("ToggleMenuActive"),
    help_key=global_bindings.readable_keyname("Help"),
    quit_key=global_bindings.readable_keyname("Quit"),
    update_key=global_bindings.readable_keyname("UpdatePackageList"),
    install_key=global_bindings.readable_keyname("DoInstallRun");

  wstring helptext = swsprintf(transcode(_("%ls: Menu  %ls: Help  %ls: Quit  %ls: Update  %ls: Download/Install/Remove Pkgs")).c_str(),
			menu_key.c_str(),
			help_key.c_str(),
			quit_key.c_str(),
			update_key.c_str(),
			install_key.c_str());

  help_bar *help_label=new help_bar(helptext, get_style("Header"));
  main_table->add_widget_opts(help_label, 0, 0, 1, 1,
			      vs_table::EXPAND | vs_table::FILL | vs_table::SHRINK,
			      vs_table::ALIGN_CENTER);
  aptcfg->connect(string(PACKAGE "::UI::HelpBar"),
		  sigc::mem_fun(*help_label, &help_bar::set_visibility));

  main_multiplex=vs_multiplex::create(true);
  main_table->add_widget_opts(main_multiplex, 1, 0, 1, 1,
			      vs_table::EXPAND | vs_table::FILL | vs_table::SHRINK,
			      vs_table::EXPAND | vs_table::FILL | vs_table::SHRINK);
  main_multiplex->show();

  vs_widget_ref b=make_broken_indicator();
  main_table->add_widget_opts(b, 2, 0, 1, 1,
			      vs_table::EXPAND | vs_table::FILL | vs_table::SHRINK,
			      vs_table::ALIGN_CENTER);

  main_status_multiplex=vs_multiplex::create();
  main_status_multiplex->set_bg_style(get_style("Status"));
  main_table->add_widget_opts(main_status_multiplex, 3, 0, 1, 1,
			      vs_table::EXPAND | vs_table::FILL | vs_table::SHRINK,
			      vs_table::ALIGN_CENTER);
  main_status_multiplex->show();

  main_status_multiplex->add_widget(menu_description);

  main_menu->show();

  vscreen_settoplevel(main_menu);

  check_apt_errors();
  main_hook.connect(sigc::ptr_fun(check_apt_errors));

  help_label->set_visibility();

  update_menubar_autohide();

  // Force parsing of the column stuff.
  // FIXME: put this in load_options() and kill all other references
  //       to setup_columns?
  pkg_item::pkg_columnizer::setup_columns();

  // Make sure the broken indicator doesn't annoyingly pop up for a
  // moment. (hack?)
  //
  // Note that it *should* be visible if we enter this code from the
  // command-line (i.e., with the cache already loaded).  More hack?
  if(resman != NULL && resman->resolver_exists())
    b->show();
  else
    b->hide();

  maybe_show_old_tmpdir_message();
}

void ui_main()
{
  vscreen_mainloop();

  if(apt_cache_file &&
     (aptitudeDepCache *) (*apt_cache_file) &&
     apt_cache_file->is_locked())
    {
      vs_progress_ref p=gen_progress_bar();
      (*apt_cache_file)->save_selection_list(*p.unsafe_get_ref());
      p->destroy();
    }

  vscreen_shutdown();
}

void popup_widget(const vs_widget_ref &w, bool do_show_all)
{
  main_stacked->add_widget(w);

  if(do_show_all)
    w->show_all();
  else
    w->show();
}

static void setup_main_widget(const vs_widget_ref &w, const std::wstring &menuref,
			      const std::wstring &menudesc)
{
  vs_menu_item *menuentry=new vs_menu_item(menuref, "", menudesc);

  // FIXME: if w is removed from the multiplexer but not destroyed, this may
  //       break.  Fix for now: Don't Do That Then!
  w->destroyed.connect(sigc::bind(sigc::mem_fun(views_menu.unsafe_get_ref(), &vs_menu::remove_item), menuentry));
  menuentry->selected.connect(sigc::mem_fun(w.unsafe_get_ref(), &vscreen_widget::show));

  views_menu->append_item(menuentry);
}

// Handles the case where the last view is destroyed directly (other than
// through do_destroy_visible); for instance, when a download completes.
static void main_widget_destroyed()
{
  if(aptcfg->FindB(PACKAGE "::UI::Exit-On-Last-Close", true) &&
     main_multiplex->num_children()==0)
    // Don't prompt -- if the last view is destroyed, assume it was by
    // the user's request.
    file_quit();
}

void add_main_widget(const vs_widget_ref &w, const std::wstring &menuref,
		     const std::wstring &menudesc,
		     const std::wstring &tabdesc)
{
  setup_main_widget(w, menuref, menudesc);
  main_multiplex->add_widget(w, tabdesc);
  w->show();
  w->destroyed.connect(sigc::ptr_fun(main_widget_destroyed));

  update_menubar_autohide();
}

void add_main_widget(const vs_widget_ref &w, const std::string &menuref,
		     const std::string &menudesc,
		     const std::string &tabdesc)
{
  add_main_widget(w, transcode(menuref), transcode(menudesc),
		  transcode(tabdesc));
}

void insert_main_widget(const vs_widget_ref &w, const std::wstring &menuref,
			const std::wstring &menudesc,
			const std::wstring &tabdesc)
{
  setup_main_widget(w, menuref, menudesc);
  main_multiplex->add_widget_after(w, main_multiplex->visible_widget(), tabdesc);
  w->show();

  update_menubar_autohide();
}

void insert_main_widget(const vs_widget_ref &w, const std::string &menuref,
			const std::string &menudesc,
			const std::string &tabdesc)
{
  insert_main_widget(w, transcode(menuref),
		     transcode(menudesc), transcode(tabdesc));
}

vs_widget_ref active_main_widget()
{
  return main_multiplex->visible_widget();
}

vs_progress_ref gen_progress_bar()
{
  vs_progress_ref rval=vs_progress::create();

  main_status_multiplex->add_visible_widget(rval, true);

  return rval;
}

fragment *wrapbox(fragment *contents)
{
  if(aptcfg->FindB(PACKAGE "::UI::Fill-Text", false))
    return fillbox(contents);
  else
    return flowbox(contents);
}

static void reset_status_download()
{
  active_status_download=NULL;
}

std::pair<download_signal_log *,
	  vs_widget_ref>
gen_download_progress(bool force_noninvasive,
		      bool list_update,
		      const wstring &title,
		      const wstring &longtitle,
		      const wstring &tablabel,
		      slot0arg abortslot)
{
  download_signal_log *m=new download_signal_log;
  download_list_ref w=NULL;

  if(force_noninvasive ||
     aptcfg->FindB(PACKAGE "::UI::Minibuf-Download-Bar", false))
    {
      w=download_list::create(abortslot, false, !list_update);
      main_status_multiplex->add_visible_widget(w, true);
      active_status_download=w;
      w->destroyed.connect(sigc::ptr_fun(&reset_status_download));
    }
  else
    {
      w=download_list::create(abortslot, true, !list_update);
      add_main_widget(w, title, longtitle, tablabel);
    }

  m->MediaChange_sig.connect(sigc::mem_fun(w.unsafe_get_ref(),
					   &download_list::MediaChange));
  m->IMSHit_sig.connect(sigc::mem_fun(w.unsafe_get_ref(),
				      &download_list::IMSHit));

  m->Fetch_sig.connect(sigc::mem_fun(w.unsafe_get_ref(),
				     &download_list::Fetch));

  m->Done_sig.connect(sigc::mem_fun(w.unsafe_get_ref(),
				    &download_list::Done));

  m->Fail_sig.connect(sigc::mem_fun(w.unsafe_get_ref(),
				    &download_list::Fail));

  m->Pulse_sig.connect(sigc::mem_fun(w.unsafe_get_ref(),
				     &download_list::Pulse));

  m->Start_sig.connect(sigc::mem_fun(w.unsafe_get_ref(),
				     &download_list::Start));

  m->Stop_sig.connect(sigc::mem_fun(w.unsafe_get_ref(),
				    &download_list::Stop));

  m->Complete_sig.connect(sigc::mem_fun(w.unsafe_get_ref(),
					&download_list::Complete));

  return std::pair<download_signal_log *, vs_widget_ref>(m, w);
}

static void do_prompt_string(const wstring &s,
			     vs_editline &e,
			     sigc::slot0<void> realslot)
{
  e.add_to_history(s);
  realslot();
}

std::pair<download_signal_log *, vs_widget_ref>
gen_download_progress(bool force_noninvasive,
		      bool list_update,
		      const string &title,
		      const string &longtitle,
		      const string &tablabel,
		      slot0arg abortslot)
{
  return gen_download_progress(force_noninvasive,
			       list_update,
			       transcode(title),
			       transcode(longtitle),
			       transcode(tablabel),
			       abortslot);
}

void prompt_string(const std::wstring &prompt,
		   const std::wstring &text,
		   slotarg<sigc::slot1<void, wstring> > slot,
		   slotarg<sigc::slot0<void> > cancel_slot,
		   slotarg<sigc::slot1<void, wstring> > changed_slot,
		   vs_editline::history_list *history)
{
  if(aptcfg->FindB(PACKAGE "::UI::Minibuf-Prompts"))
    {
      vs_editline_ref e=vs_editline::create(prompt, text, history);
      if(slot)
	e->entered.connect(*slot);

      e->entered.connect(sigc::bind(sigc::ptr_fun(do_prompt_string),
				    e.weak_ref(),
				    sigc::mem_fun(e.unsafe_get_ref(), &vscreen_widget::destroy)));
      if(changed_slot)
	e->text_changed.connect(*changed_slot);

      e->connect_key("Cancel",
		     &global_bindings,
		     sigc::mem_fun(e.unsafe_get_ref(), &vscreen_widget::destroy));

      if(cancel_slot)
	e->connect_key("Cancel",
		       &global_bindings,
		       *cancel_slot);

      main_status_multiplex->add_visible_widget(e, true);
      main_table->focus_widget(main_status_multiplex);
    }
  else
    main_stacked->add_visible_widget(vs_dialog_string(prompt, text,
						      slot, cancel_slot,
						      changed_slot,
						      history),
				     true);
}

void prompt_string(const std::string &prompt,
		   const std::string &text,
		   slotarg<sigc::slot1<void, wstring> > slot,
		   slotarg<sigc::slot0<void> > cancel_slot,
		   slotarg<sigc::slot1<void, wstring> > changed_slot,
		   vs_editline::history_list *history)
{
  prompt_string(transcode(prompt), transcode(text),
		slot, cancel_slot, changed_slot, history);
}

static void do_prompt_yesno(int cval,
			    bool deflt,
			    slot0arg yesslot,
			    slot0arg noslot)
{
  bool rval;

  if(deflt)
    rval=!cval;
  else
    rval=cval;

  if(rval)
    {
      if(yesslot)
	(*yesslot)();
    }
  else
    {
      if(noslot)
	(*noslot)();
    }
}

void prompt_yesno(const std::wstring &prompt,
		  bool deflt,
		  slot0arg yesslot,
		  slot0arg noslot)
{
  if(aptcfg->FindB(PACKAGE "::UI::Minibuf-Prompts"))
    {
      string yesstring, nostring;

      yesstring+=_("yes_key")[0];
      nostring+=_("no_key")[0];
      string yesnostring=deflt?yesstring+nostring:nostring+yesstring;

      vs_statuschoice_ref c=vs_statuschoice::create(prompt, transcode(yesnostring));
      c->chosen.connect(sigc::bind(sigc::ptr_fun(&do_prompt_yesno),
				   deflt,
				   yesslot,
				   noslot));

      main_status_multiplex->add_visible_widget(c, true);
      main_table->focus_widget(main_status_multiplex);
    }
  else
    main_stacked->add_visible_widget(vs_dialog_yesno(prompt,
						     yesslot,
						     noslot,
						     deflt),
				     true);
}

void prompt_yesno_popup(fragment *prompt,
			bool deflt,
			slot0arg yesslot,
			slot0arg noslot)
{
  main_stacked->add_visible_widget(vs_dialog_yesno(prompt,
						   yesslot,
						   noslot,
						   false,
						   deflt),
				   true);
}

void prompt_yesno(const std::string &prompt,
		  bool deflt,
		  slot0arg yesslot,
		  slot0arg noslot)
{
  return prompt_yesno(transcode(prompt), deflt, yesslot, noslot);
}

void show_message(fragment *msg,
		  slot0arg okslot,
		  const style &st)
{
  msg=wrapbox(msg);
  if(aptcfg->FindB(PACKAGE "::UI::Minibuf-Prompts"))
    {
      vs_text_layout_ref l=vs_text_layout::create(msg);
      l->set_bg_style(get_style("Status")+st);
      if(okslot)
	l->destroyed.connect(*okslot);

      main_status_multiplex->add_visible_widget(vs_transient::create(l), true);
      main_table->focus_widget(main_status_multiplex);
    }
  else
    main_stacked->add_visible_widget(vs_dialog_ok(msg, okslot, st), true);
}

void show_message(const std::string &msg,
		  slot0arg okslot,
		  const style &st)
{
  show_message(text_fragment(msg), okslot, st);
}

void show_message(const std::wstring &msg,
		  slot0arg okslot,
		  const style &st)
{
  show_message(text_fragment(msg), okslot, st);
}
