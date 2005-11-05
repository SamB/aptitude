// apt_options.cc
//
//  Copyright 2000 Daniel Burrows

#include "apt_options.h"
#include "apt_config_widgets.h"
#include "pkg_columnizer.h"
#include "ui.h"

#include "aptitude.h"

#include <vscreen/fragment.h>
#include <vscreen/vs_button.h>
#include <vscreen/vs_center.h>
#include <vscreen/vs_frame.h>
#include <vscreen/vs_label.h>
#include <vscreen/vs_table.h>

#include <vscreen/config/keybindings.h>
#include <vscreen/config/colors.h>

#include <generic/apt/apt.h>
#include <generic/apt/config_signal.h>

#include <sigc++/adaptors/bind.h>
#include <sigc++/functors/mem_fun.h>

#include <string>
#include <vector>

using namespace std;

struct option_item
{
  enum {OPTION_BOOL, OPTION_STRING, OPTION_RADIO, OPTION_END} type;
  const char *description;
  const char *option_name;

  union
  {
    bool b_default;
    const char *s_default;
  };

  /** Used by radio options: the valid values for this option. */
  vector<string> choices;

  /** Used by radio options: the descriptions corresponding to
      elements of "choices". */
  vector<string> choice_descriptions;

  option_item()
    :type(OPTION_END), description(NULL), option_name(NULL)
  {
  }

  option_item(const char *_description, const char *_option_name,
	      bool def)
    :type(OPTION_BOOL), description(_description), option_name(_option_name),
     b_default(def)
  {
  }

  option_item(const char *_description, const char *_option_name,
	      const char *def)
    :type(OPTION_STRING), description(_description), option_name(_option_name),
     s_default(def)
  {
  }

  static option_item radio(const char *description,
			   const char *option_name,
			   const char *def,
			   ...)
  {
    option_item rval;

    va_list args;
    va_start(args, def);

    rval.type=OPTION_RADIO;
    rval.description=description;
    rval.option_name=option_name;
    rval.s_default=def;

    while(1)
      {
	const char *choice=va_arg(args, const char *);

	if(choice == NULL)
	  break;

	const char *description=va_arg(args, const char *);

	if(description == NULL)
	  break;

	rval.choices.push_back(choice);

	rval.choice_descriptions.push_back(description);
      }

    va_end(args);

    return rval;
  }
};

option_item ui_options[]={
  option_item(N_("Display some available commands at the top of the screen"),
	      PACKAGE "::UI::HelpBar", true),
  option_item(N_("Hide the menubar when it is not being used"),
	      PACKAGE "::UI::Menubar-Autohide", false),
  option_item(N_("Use a minibuffer-style prompt when possible"),
	      PACKAGE "::UI::Minibuf-Prompts", false),
  option_item(N_("Show partial search results (incremental search)"),
	      PACKAGE "::UI::Incremental-Search", true),
  option_item(N_("Closing the last view exits the program"),
	      PACKAGE "::UI::Exit-On-Last-Close", true),
  option_item(N_("Prompt for confirmation at exit"),
	      PACKAGE "::UI::Prompt-On-Exit", true),
  option_item::radio(N_("Pause after downloading files"),
		     PACKAGE "::UI::Pause-After-Download", "OnlyIfError",
		     "No", _("Never"),
		     "OnlyIfError", _("When an error occurs"),
		     "Yes", _("Always"),
		     NULL, NULL),
  option_item(N_("Use a 'status-line' download indicator for all downloads"),
	      PACKAGE "::UI::Minibuf-Download-Bar", false),
  option_item(N_("Display the extended description area by default"),
	      PACKAGE "::UI::Description-Visible-By-Default", true),
  option_item(N_("Advance to the next item after changing the state of a package"),
	      PACKAGE "::UI::Advance-On-Action", false),
  option_item(N_("Automatically show why packages are broken"),
	      PACKAGE "::UI::Auto-Show-Reasons", true),
  option_item(N_("The default grouping method for package views"),
	      PACKAGE "::UI::Default-Grouping", default_grpstr),
  option_item(N_("The default display-limit for package views"),
	      PACKAGE "::Pkg-Display-Limit", ""),
  option_item(N_("The display format for package views"),
	      PACKAGE "::UI::Package-Display-Format",
	      pkg_item::pkg_columnizer::default_pkgdisplay),
  option_item(N_("The display format for the status line"),
	      PACKAGE "::UI::Package-Status-Format",
	      default_pkgstatusdisplay),
  option_item(N_("The display format for the header line"),
	      PACKAGE "::UI::Package-Header-Format",
	      default_pkgheaderdisplay),
  //option_item(N_("Use new (idempotent) package command behavior"),
  //	      PACKAGE "::UI::New-Package-Commands", true),
  option_item()
};

option_item misc_options[]={
  option_item(N_("Automatically upgrade installed packages"),
	      PACKAGE "::Auto-Upgrade", false),
  option_item(N_("Remove obsolete package files after downloading new package lists"),
	      PACKAGE "::AutoClean-After-Update", false),
  option_item(N_("URL to use to download changelogs"),
	      PACKAGE "::Changelog-URL-Template",
	      "http://cgi.debian.org/cgi-bin/get-changelog?package=%s"),
  option_item(N_("Display a preview of what will be done before doing it"),
	      PACKAGE "::Display-Planned-Action",
	      true),
  option_item(N_("Forget which packages are \"new\" whenever the package lists are updated"),
	      PACKAGE "::Forget-New-On-Update",
	      false),
  option_item(N_("Forget which packages are \"new\" whenever packages are installed or removed"),
	      PACKAGE "::Forget-New-On-Install",
	      false),
  option_item(N_("Do not display a warning when the first change is made in read-only mode."),
	      PACKAGE "::Suppress-Read-Only-Warning",
	      false),
  option_item(N_("Warn when attempting to perform a privileged action as a non-root user"),
	      PACKAGE "::Warn-Not-Root",
	      true),
  option_item(N_("File to log actions into"),
	      PACKAGE "::Log",
	      "/var/log/aptitude"),
  option_item()
};

option_item dependency_options[]={
  option_item(N_("Automatically resolve dependencies of a package when it is selected"),
	      PACKAGE "::Auto-Install", true),
  option_item(N_("Automatically fix broken packages before installing or removing"),
	      PACKAGE "::Auto-Fix-Broken", true),
  option_item(N_("Install Recommended packages automatically"),
	      PACKAGE "::Recommends-Important", true),
  option_item(N_("Remove unused packages automatically"),
	      PACKAGE "::Delete-Unused", true),
  option_item(N_("Don't automatically remove unused packages matching this filter"),
	      PACKAGE "::Keep-Unused-Pattern", ""),
  option_item()
};

// Commits all the entries of an options dialog, then saves the
// present settings and destroys the dialog.
class dialog_manager:public sigc::trackable
{
public:
  dialog_manager(const vs_widget_ref &_dialog):dialog(_dialog) {}

  ~dialog_manager()
  {
    for(vector<apt_config_widget *>::iterator i=widgets.begin();
	i!=widgets.end(); ++i)
      delete *i;

    widgets.clear();
  }

  void add_widget(apt_config_widget *w) {widgets.push_back(w);}

  void commit()
  {
    for(vector<apt_config_widget *>::iterator i=widgets.begin();
	i!=widgets.end(); ++i)
      (*i)->commit();

    apt_dumpcfg(PACKAGE);

    destroy();
  }

  void destroy()
  {
    dialog->destroy();

    delete this;
  }

private:
  vector<apt_config_widget *> widgets;

  vs_widget_ref dialog;
};

static vs_widget_ref realize_options_dialog(option_item *items)
{
  vs_center_ref center=vs_center::create();

  // Create general infrastructure (FIXME: make a plugin dialog widget thingy
  // that does this?
  vs_table_ref table=vs_table::create();
  table->set_colsep(1);

  vs_button_ref okbutton=vs_button::create(_("Ok"));
  vs_button_ref cancelbutton=vs_button::create(_("Cancel"));

  table->connect_key("Confirm", &global_bindings, okbutton->pressed.make_slot());
  table->connect_key("Cancel", &global_bindings, cancelbutton->pressed.make_slot());

  dialog_manager *manager=new dialog_manager(center);

  okbutton->pressed.connect(sigc::mem_fun(*manager, &dialog_manager::commit));
  cancelbutton->pressed.connect(sigc::mem_fun(*manager, &dialog_manager::destroy));

  int row=0;

  while(items->type!=option_item::OPTION_END)
    {
      vs_label_ref l=NULL;

      switch(items->type)
	{
	case option_item::OPTION_BOOL:
	  {
	    apt_bool_widget *w=new apt_bool_widget(_(items->description),
						   items->option_name,
						   items->b_default);

	    table->add_widget_opts(w->cb, row, 0, 1, 2,
				   vs_table::EXPAND | vs_table::FILL | vs_table::SHRINK,
				   vs_table::EXPAND);

	    manager->add_widget(w);
	  }
	  ++row;
	  break;
	case option_item::OPTION_STRING:
	  {
	    apt_string_widget *w=new apt_string_widget(items->option_name,
						       items->s_default);

	    l = vs_label::create(flowbox(text_fragment(_(items->description))));

	    table->add_widget_opts(l, row, 0, 1, 1,
			      vs_table::EXPAND | vs_table::FILL | vs_table::SHRINK,
			      vs_table::EXPAND);
	    table->add_widget_opts(w->w, row, 1, 1, 1,
				   vs_table::EXPAND | vs_table::FILL | vs_table::SHRINK,
				   vs_table::EXPAND);

	    manager->add_widget(w);
	  }
	  ++row;
	  break;
	case option_item::OPTION_RADIO:
	  {
	    eassert(items->choices.size()==items->choice_descriptions.size());

	    eassert(items->choices.size()>0);

	    apt_radio_widget *w=new apt_radio_widget(items->option_name,
						     items->choices,
						     items->s_default);

	    string curr=aptcfg->Find(items->option_name,
				     items->s_default);

	    l = vs_label::create(flowbox(text_fragment(_(items->description))));

	    for(vector<string>::size_type i=0; i<items->choices.size(); ++i)
	      {
		string choice=items->choices[i];
		string description=items->choice_descriptions[i];

		vs_togglebutton_ref b=vs_radiobutton::create(_(description.c_str()),
							     choice == curr);

		table->add_widget(b, row+i, 1, 1, 1,
				  vs_table::EXPAND | vs_table::FILL | vs_table::SHRINK,
				  vs_table::EXPAND);

		w->rg.add_button(b, i);
	      }

	    if(!w->rg.selection_valid())
	      {
		// Hm, if it didn't work the first time, why would it
		// work the second time?
		for(vector<string>::size_type i=0; i<items->choices.size(); ++i)
		  if(items->choices[i]==curr)
		    {
		      w->rg.select(i);
		      break;
		    }
	      }

	    table->add_widget(l, row+(items->choices.size()-1)/2, 0, 1, 1, false, false);
	    row+=items->choices.size();
	    manager->add_widget(w);
	  }
	  break;

	default:
	  eassert(0);  // Someone probably goofed in setting the tables up
	}

      items++;
    }

  vs_table_ref bt=vs_table::create();
  bt->add_widget_opts(okbutton, 0, 0, 1, 1,
		      vs_table::ALIGN_CENTER | vs_table::EXPAND,
		      vs_table::ALIGN_CENTER);
  bt->add_widget_opts(cancelbutton, 0, 1, 1, 1,
		      vs_table::ALIGN_CENTER | vs_table::EXPAND,
		      vs_table::ALIGN_CENTER);
  table->add_widget_opts(bt, row, 0, 1, 2,
			 vs_table::ALIGN_CENTER | vs_table::EXPAND | vs_table::FILL | vs_table::SHRINK,
			 vs_table::ALIGN_CENTER);
  
  vs_frame_ref frame=vs_frame::create(table);

  center->add_widget(frame);
  center->set_bg_style(style_attrs_flip(A_REVERSE));

  return center;
}

vs_widget_ref make_ui_options_dialog()
{
  return realize_options_dialog(ui_options);
}

vs_widget_ref make_misc_options_dialog()
{
  return realize_options_dialog(misc_options);
}

vs_widget_ref make_dependency_options_dialog()
{
  return realize_options_dialog(dependency_options);
}
