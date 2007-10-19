// apt_options.cc
//
//   Copyright (C) 2000, 2007 Daniel Burrows
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

#include "apt_options.h"
#include "apt_config_treeitems.h"
#include "apt_config_widgets.h"
#include "pkg_columnizer.h"
#include "ui.h"

#include "aptitude.h"

#include <vscreen/fragment.h>
#include <vscreen/vs_button.h>
#include <vscreen/vs_center.h>
#include <vscreen/vs_frame.h>
#include <vscreen/vs_label.h>
#include <vscreen/vs_scrollbar.h>
#include <vscreen/vs_subtree.h>
#include <vscreen/vs_table.h>
#include <vscreen/vs_text_layout.h>
#include <vscreen/transcode.h>

#include <vscreen/config/keybindings.h>
#include <vscreen/config/colors.h>

#include <generic/apt/apt.h>
#include <generic/apt/config_signal.h>

#include <sigc++/adaptors/bind.h>
#include <sigc++/functors/mem_fun.h>

#include <string>
#include <vector>

using namespace std;

namespace aptitude
{
  namespace ui
  {
    namespace config
    {
      namespace
      {
struct option_item
{
  enum {OPTION_BOOL, OPTION_STRING, OPTION_RADIO, OPTION_END} type;
  const char *description;
  const char *long_description;
  const char *option_name;

  union
  {
    bool b_default;
    const char *s_default;
  };

  /** Used by radio options to store the list of valid values for
   *  the option.
   */
  vector<radio_choice> choices;

  option_item()
    :type(OPTION_END), description(NULL), option_name(NULL)
  {
  }

  option_item(const char *_description,
	      const char *_long_description,
	      const char *_option_name, bool def)
    :type(OPTION_BOOL),
     description(_description),
     long_description(_long_description),
     option_name(_option_name),
     b_default(def)
  {
  }

  option_item(const char *_description,
	      const char *_long_description,
	      const char *_option_name,
	      const char *def)
    :type(OPTION_STRING),
     description(_description),
     long_description(_long_description),
     option_name(_option_name),
     s_default(def)
  {
  }

  /** \brief Construct a radio item from a list of choices,
   *  destroying each choice after it's inserted into the list.
   */
  static option_item radio(const char *option_name,
			   const char *description,
			   const char *long_description,
			   const char *def,
			   const radio_choice *choice_1,
			   ...)
  {
    option_item rval;

    va_list args;
    va_start(args, choice_1);

    rval.type=OPTION_RADIO;
    rval.description=description;
    rval.long_description = long_description;
    rval.option_name=option_name;
    rval.s_default=def;

    while(1)
      {
	radio_choice *choice=va_arg(args, radio_choice *);

	if(choice == NULL)
	  break;

	rval.choices.push_back(*choice);
	delete choice;
      }

    va_end(args);

    return rval;
  }
};

	radio_choice *choice(const char *value,
			     const char *description,
			     const char *long_description)
	{
	  return new radio_choice(value,
				  transcode(_(description)),
				  transcode(_(long_description)));
	}

option_item ui_options[]={
  option_item(N_("Display some available commands at the top of the screen"),
	      N_("If this option is enabled, a brief summary of some "
		 "of the most important aptitude commands will appear "
		 "beneath the menu bar."),
	      PACKAGE "::UI::HelpBar", true),
  option_item(N_("Hide the menu bar when it is not being used"),
	      N_("If this option is enabled, the menu bar will "
		 "only appear when it has been activated by pressing "
		 "the menu key."),
	      PACKAGE "::UI::Menubar-Autohide", false),
  option_item(N_("Use a minibuffer-style prompt when possible"),
	      N_("If this option is enabled, prompts will be "
		 "displayed in a single line at the bottom of "
		 "the screen.  If not, prompts will be displayed "
		 "as pop-up dialog boxes."),
	      PACKAGE "::UI::Minibuf-Prompts", false),
  option_item(N_("Show partial search results (incremental search)"),
	      N_("If this option is enabled, aptitude will "
		 "perform searches within the package list as "
		 "you type them.  This is convenient, but may "
		 "slow the program down, particularly on older "
		 "computers."),
	      PACKAGE "::UI::Incremental-Search", true),
  option_item(N_("Closing the last view exits the program"),
	      N_("If this option is enabled, aptitude will stop "
		 "running when all views (package lists, package "
		 "details, etc) have been closed.  Otherwise, "
		 "aptitude will continue running until you select "
		 "'Quit' from the Actions menu."),
	      PACKAGE "::UI::Exit-On-Last-Close", true),
  option_item(N_("Prompt for confirmation at exit"),
	      N_("If this option is enabled, aptitude will "
		 "not terminate until you confirm that you "
		 "really want to quit."),
	      PACKAGE "::UI::Prompt-On-Exit", true),
  option_item::radio(N_("Pause after downloading files"),
		     N_("This option controls whether aptitude will wait for confirmation after a download before it goes ahead and installs packages."),
		     PACKAGE "::UI::Pause-After-Download",
		     "OnlyIfError",
		     choice("No", _("Never"),
			    _("Never wait for the user after downloading packages: always begin the installation immediately.")),
		     choice("OnlyIfError", _("When an error occurs"),
			    _("Wait for confirmation if an error occurred during the download.  If there were no errors, begin installing packages immediately.")),
		     choice("Yes", _("Always"),
			    _("Always wait for the user to confirm the download before proceeding with the installation.")),
		     NULL),
  option_item(N_("Use a 'status-line' download indicator for all downloads"),
	      N_("If this option is enabled, aptitude will display "
		 "the status of ongoing downloads at the bottom of "
		 "the screen, rather than opening a new view."),
	      PACKAGE "::UI::Minibuf-Download-Bar", false),
  option_item(N_("Display the extended description area by default"),
	      N_("If this option is enabled, the long description "
		 "area (the pane at the bottom of the screen) in the "
		 "package list will be visible when the program "
		 "starts; otherwise, it will be initially hidden."),
	      PACKAGE "::UI::Description-Visible-By-Default", true),
  option_item(N_("Advance to the next item after changing the state of a package"),
	      N_("If this option is enabled, then performing an "
		 "action on a package (for instance, installing or "
		 "removing it) will move the selection to the next "
		 "package in the list."),
	      PACKAGE "::UI::Advance-On-Action", false),
  option_item(N_("Automatically show why packages are broken"),
	      N_("If this option is enabled, then highlighting a "
		 "package that has broken dependencies will "
		 "automatically display the dependencies that "
		 "are unfulfilled in the lower pane of the "
		 "display."),
	      PACKAGE "::UI::Auto-Show-Reasons", true),
  option_item(N_("The default grouping method for package views"),
	      N_("This option controls how aptitude organizes the "
		 "package list.  See the aptitude user's manual for "
		 "information on how to specify a grouping method."),
	      PACKAGE "::UI::Default-Grouping", default_grpstr),
  option_item(N_("The default display-limit for package views"),
	      N_("By default, the limit of each package view will "
		 "be set to the value specified by this option.  "
		 "See the aptitude user's manual for detailed "
		 "information about searches."),
	      PACKAGE "::Pkg-Display-Limit", ""),
  option_item(N_("The display format for package views"),
	      N_("This option controls how aptitude formats lines "
		 "of the package list.  See the aptitude user's "
		 "manual for information on how to specify a "
		 "display format."),
	      PACKAGE "::UI::Package-Display-Format",
	      pkg_item::pkg_columnizer::default_pkgdisplay),
  option_item(N_("The display format for the status line"),
	      N_("This option controls how aptitude formats the "
		 "status line (the line between the package list "
		 "and the lower pane).  See the aptitude user's "
		 "manual for information on how to specify a "
		 "display format."),
	      PACKAGE "::UI::Package-Status-Format",
	      default_pkgstatusdisplay),
  option_item(N_("The display format for the header line"),
	      N_("This option controls how aptitude formats the "
		 "header line (the line above the package list).  "
		 "See the aptitude user's manual for information on "
		 "how to specify a display format."),
	      PACKAGE "::UI::Package-Header-Format",
	      default_pkgheaderdisplay),
  option_item()
};

option_item misc_options[]={
  option_item(N_("Automatically upgrade installed packages"),
	      N_("If this option is enabled, then on startup, "
		 "aptitude will select all upgradable packages for "
		 "upgrade."),
	      PACKAGE "::Auto-Upgrade", false),
  option_item(N_("Remove obsolete package files after downloading new package lists"),
	      N_("If this option is enabled, then after every "
		 "install run, aptitude will delete from the package "
		 "cache any package files that can no longer be "
		 "downloaded from any archive in sources.lst."),
	      PACKAGE "::AutoClean-After-Update", false),
  option_item(N_("URL to use to download changelogs"),
	      N_("This option controls the template that's used to "
		 "download changelogs from the Debian Web site.  You "
		 "should only need to change this if the URL of "
		 "the package archive changes."),
	      PACKAGE "::Changelog-URL-Template",
	      "http://cgi.debian.org/cgi-bin/get-changelog?package=%s"),
  option_item(N_("Display a preview of what will be done before doing it"),
	      N_("If this option is enabled, then when you ask "
		 "aptitude to perform an install run, it will "
		 "first display a summary of the actions it is "
		 "going to perform."),
	      PACKAGE "::Display-Planned-Action",
	      true),
  option_item(N_("Forget which packages are \"new\" whenever the package lists are updated"),
	      N_("If this option is enabled, then aptitude will "
		 "clear the list of new packages after you update "
		 "the package lists (e.g., by pressing 'u')."),
	      PACKAGE "::Forget-New-On-Update",
	      false),
  option_item(N_("Forget which packages are \"new\" whenever packages are installed or removed"),
	      N_("If this option is enabled, then aptitude will "
		 "clear the list of new packages after you perform "
		 "an install run or install or remove packages from "
		 "the command-line."),
	      PACKAGE "::Forget-New-On-Install",
	      false),
  option_item(N_("Do not display a warning when the first change is made in read-only mode"),
	      N_("If this option is %Bnot%b enabled, aptitude will "
		 "display a warning when you modify the state of a "
		 "package if you do not have permissions to apply "
		 "the change to the system."),
	      PACKAGE "::Suppress-Read-Only-Warning",
	      false),
  option_item(N_("Warn when attempting to perform a privileged action as a non-root user"),
	      N_("If this option is enabled, aptitude will "
		 "warn you when you attempt to perform an action "
		 "which you do not have permission to do: for "
		 "instance, installing packages as a non-root user.  "
		 "You will be given the option to log in as root "
		 "and perform the action with root privileges."),
	      PACKAGE "::Warn-Not-Root",
	      true),
  // TODO: support multiple log destinations.
  option_item(N_("File to log actions into"),
	      N_("When you install or remove packages, a summary of "
		 "what aptitude does will be written to this file.  "
		 "If the first character of the file name is a pipe "
		 "character ('%B|%b'), the remainder of the name will "
		 "be interpreted as a shell command that is to "
		 "receive the log on standard input."),
	      PACKAGE "::Log",
	      "/var/log/aptitude"),
  option_item()
};

option_item dependency_options[]={
  option_item(N_("Automatically resolve dependencies of a package when it is selected"),
	      N_("If this option is enabled, aptitude will "
		 "use a simple heuristic to immediately resolve "
		 "the dependencies of each package you flag for "
		 "installation.  This is much faster than the "
		 "built-in dependency resolver, but may produce "
		 "suboptimal results or fail entirely in some "
		 "scenarios."),
	      PACKAGE "::Auto-Install", true),
  option_item(N_("Automatically fix broken packages before installing or removing"),
	      N_("If this option is enabled, and you perform an "
		 "install run while some packages are broken, "
		 "aptitude will automatically apply the current "
		 "suggestion of the problem resolver.  Otherwise,"
		 "aptitude will prompt you for a solution to the "
		 "broken dependencies."),
	      PACKAGE "::Auto-Fix-Broken", true),
  option_item(N_("Install recommended packages automatically"),
	      N_("If this option is enabled  and \"automatically "
		 "resolve dependencies\" is also enabled, aptitude "
		 "will attempt to install the recommendations of "
		 "newly installed packages in addition to their "
		 "outright dependencies.  Suggestions will not "
		 "be automatically installed."
		 "\n"
		 "If this option is enabled and \"Remove unused "
		 "packages automatically\" is enabled, packages "
		 "that are recommended by an installed package "
		 "will not automatically be removed."),
	      PACKAGE "::Recommends-Important", true),
  option_item(N_("Remove unused packages automatically"),
	      N_("If this option is enabled, packages that are "
		 "automatically installed and that no manually "
		 "installed package depends on will be removed "
		 "from the system.  Cancelling the removal will "
		 "flag the package as manually installed."
		 "\n"
		 "If this option is enabled and \"Install recommended "
		 "packages automatically\" is enabled, "
		 "automatically installed packages will not be "
		 "removed if any installed package recommends them."),
	      PACKAGE "::Delete-Unused", true),
  option_item(N_("Packages that should never be automatically removed"),
	      N_("Packages matching this search pattern will "
		 "always be treated as if an installed package "
		 "depends on them: they will never be targeted "
		 "for removal as unused packages."),
	      PACKAGE "::Keep-Unused-Pattern", ""),
  option_item()
};

	class dummy_subtree : public vs_subtree<vs_treeitem>
	{
	  std::wstring text;
	public:
	  dummy_subtree(const std::wstring &_text)
	    : vs_subtree<vs_treeitem>(true),
	      text(_text)
	  {
	  }

	  void paint(vs_tree *win, int y,
		     bool hierarchical, const style &)
	  {
	    vs_subtree<vs_treeitem>::paint(win, y, hierarchical, text);
	  }

	  const wchar_t *tag()
	  {
	    return L"";
	  }

	  const wchar_t *label()
	  {
	    return L"";
	  }
	};

	vs_treeitem *parse_option(const option_item &option)
	{
	  eassert(option.type != option_item::OPTION_END);
	  switch(option.type)
	    {
	    case option_item::OPTION_BOOL:
	      return make_boolean_item(transcode(_(option.description)),
				       transcode(_(option.long_description)),
				       option.option_name,
				       option.b_default);
	    case option_item::OPTION_STRING:
	      return make_string_item(transcode(_(option.description)),
				      transcode(_(option.long_description)),
				      option.option_name,
				      option.s_default);
	    case option_item::OPTION_RADIO:
	      return make_radio_item(transcode(_(option.description)),
				     transcode(_(option.long_description)),
				     option.option_name,
				     option.choices,
				     option.s_default);
	    default:
	      eassert(!"This should never happen.");
	      return NULL;
	    }
	}

	class apt_options_view : public vs_table
	{
	  vs_text_layout_ref desc_area;
	  vs_tree_ref tree;

	  sigc::connection last_connection;

	  void handle_selection_changed(vs_treeitem *selected)
	  {
	    last_connection.disconnect();

	    config_treeitem *configitem = dynamic_cast<config_treeitem *>(selected);
	    if(configitem == NULL)
	      desc_area->set_fragment(newline_fragment());
	    else
	      {
		last_connection = configitem->description_changed.connect(sigc::mem_fun(this, &apt_options_view::handle_description_changed));
		desc_area->set_fragment(configitem->get_long_description());
	      }
	  }

	  void handle_description_changed()
	  {
	    vs_treeiterator selected = tree->get_selection();
	    config_treeitem *configitem;

	    if(selected == tree->get_end())
	      configitem = NULL;
	    else
	      configitem = dynamic_cast<config_treeitem *>(&*selected);

	    if(configitem == NULL)
	      desc_area->set_fragment(newline_fragment());
	    else
	      desc_area->set_fragment(configitem->get_long_description());
	  }

	  void make_children(dummy_subtree *parent, option_item *children)
	  {
	    for(option_item *it = children;
		it->type != option_item::OPTION_END;
		++it)
	      parent->add_child(parse_option(*it));
	  }

	  apt_options_view()
	  {
	    dummy_subtree *root = new dummy_subtree(L"");

	    dummy_subtree *ui_tree =
	      new dummy_subtree(transcode(_("UI options")));
	    dummy_subtree *dep_tree =
	      new dummy_subtree(transcode(_("Dependency handling")));
	    dummy_subtree *misc_tree =
	      new dummy_subtree(transcode(_("Miscellaneous")));

	    root->add_child(ui_tree);
	    root->add_child(dep_tree);
	    root->add_child(misc_tree);

	    make_children(ui_tree,   ui_options);
	    make_children(dep_tree,  dependency_options);
	    make_children(misc_tree, misc_options);

	    tree = vs_tree::create(root);

	    // Use an empty label to produce a "bar" dividing the two
	    // halves of the screen.
	    vs_label_ref middle_label = vs_label::create("", get_style("Status"));
	    desc_area = vs_text_layout::create();

	    vs_scrollbar_ref desc_area_scrollbar = vs_scrollbar::create(vs_scrollbar::VERTICAL);

	    tree->selection_changed.connect(sigc::mem_fun(this, &apt_options_view::handle_selection_changed));
	    tree->highlight_current();

	    add_widget_opts(tree, 0, 0, 1, 2,
			    vs_table::EXPAND | vs_table::FILL | vs_table::SHRINK | vs_table::IGNORE_SIZE_REQUEST,
			    vs_table::EXPAND | vs_table::FILL | vs_table::SHRINK | vs_table::IGNORE_SIZE_REQUEST);

	    add_widget_opts(middle_label, 1, 0, 1, 2,
			    vs_table::ALIGN_CENTER | vs_table::EXPAND | vs_table::FILL | vs_table::SHRINK,
			    vs_table::ALIGN_CENTER);

	    add_widget_opts(desc_area, 2, 0, 1, 1,
			    vs_table::EXPAND | vs_table::FILL | vs_table::SHRINK | vs_table::IGNORE_SIZE_REQUEST,
			    vs_table::EXPAND | vs_table::FILL | vs_table::SHRINK | vs_table::IGNORE_SIZE_REQUEST);
	    add_widget_opts(desc_area_scrollbar, 2, 1, 1, 1,
			    vs_table::FILL | vs_table::SHRINK,
			    vs_table::EXPAND | vs_table::FILL | vs_table::SHRINK);

	    desc_area->location_changed.connect(sigc::mem_fun(*desc_area_scrollbar.unsafe_get_ref(), &vs_scrollbar::set_slider));

	    tree->connect_key("DescriptionUp", &global_bindings,
			      sigc::mem_fun(*desc_area.unsafe_get_ref(),
					    &vs_text_layout::line_up));
	    tree->connect_key("DescriptionDown", &global_bindings,
			      sigc::mem_fun(*desc_area.unsafe_get_ref(),
					    &vs_text_layout::line_down));
	  }

	public:
	  static ref_ptr<apt_options_view> create()
	  {
	    return ref_ptr<apt_options_view>(new apt_options_view);
	  }
	};
	typedef ref_ptr<apt_options_view> apt_options_view_ref;
      }

      vs_widget_ref make_options_tree()
      {
	return apt_options_view::create();
      }
    }
  }
}
