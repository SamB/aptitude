// apt_config_treeitems.cc      -*-c++-*-
//
//   Copyright (C) 2007 Daniel Burrows
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

#include "apt_config_treeitems.h"

#include "ui.h"

#include <aptitude.h>

#include <generic/apt/apt.h>
#include <generic/apt/config_signal.h>

#include <generic/util/util.h>

#include <vscreen/config/keybindings.h>

#include <vscreen/fragment.h>
#include <vscreen/transcode.h>
#include <vscreen/vscreen_widget.h>
#include <vscreen/vs_layout_item.h>
#include <vscreen/vs_treeitem.h>
#include <vscreen/vs_subtree.h>
#include <vscreen/vs_tree.h>

namespace aptitude
{
  namespace ui
  {
    namespace config
    {
      namespace
      {
	fragment *drophardwrapbox(fragment *header, const std::string &contents)
	{
	  return dropbox(header, hardwrapbox(text_fragment(contents)));
	}
      }

      class boolean_config_treeitem : public config_treeitem
      {
	const std::string  item;
	const std::wstring description;
	const std::wstring long_description;
	const bool         dflt;

	void value_changed()
	{
	  vscreen_update();
	  description_changed();
	}
      public:
	boolean_config_treeitem(const std::string &_item,
				const std::wstring &_description,
				const std::wstring &_long_description,
				bool _dflt)
	  : item(_item),
	    description(_description),
	    long_description(_long_description),
	    dflt(_dflt)
	{
	  aptcfg->connect(item, sigc::mem_fun(*this, &boolean_config_treeitem::value_changed));
	}

	fragment *get_long_description() const
	{
	  bool value = aptcfg->FindB(item, dflt);

	  std::vector<fragment *> fragments;
	  fragments.push_back(fragf(_("%BOption:%b  %s\n"
				      "%BDefault:%b %s\n"
				      "%BValue:%b   %s\n"),
				    item.c_str(),
				    dflt  ? _("True") : _("False"),
				    value ? _("True") : _("False")));
	  fragments.push_back(newline_fragment());
	  fragments.push_back(wrapbox(text_fragment(description.c_str())));

	  return sequence_fragment(fragments);
	}

	void toggle()
	{
	  const bool old_value = aptcfg->FindB(item, dflt);
	  const bool new_value = !old_value;
	  aptcfg->Set(item, new_value ? "true" : "false");

	  apt_dumpcfg(PACKAGE);
	}

	bool dispatch_key(const key &k, vs_tree *owner)
	{
	  if(global_bindings.key_matches(k, "Confirm"))
	    toggle();
	  else
	    return config_treeitem::dispatch_key(k, owner);

	  return true;
	}

	void dispatch_mouse(short id, int x, mmask_t bstate, vs_tree *owner)
	{
	  if((bstate & BUTTON1_CLICKED) != 0 &&
	     x >= 2 * get_depth() &&
	     x <  2 * get_depth() + 3)
	    toggle();
	}

	void paint(vs_tree *win, int y, bool hierarchical, const style &st)
	{
	  bool checked = aptcfg->FindB(item, dflt);
	  const char *box = checked ? "[X]" : "[ ]";

	  vs_treeitem::paint(win, y, hierarchical,
			     swsprintf(L"%s %ls", box, description.c_str()));
	}

	const wchar_t *tag()
	{
	  return description.c_str();
	}

	const wchar_t *label()
	{
	  return tag();
	}
      };

      vs_treeitem *make_boolean_item(const std::wstring &description,
				     const std::wstring &long_description,
				     const std::string  &item,
				     const bool          dflt)
      {
	return new boolean_config_treeitem(item,
					   description,
					   long_description,
					   dflt);
      }

      namespace
      {
	// This is just the line containing the text of the item, not the
	// description that precedes it.
	class string_config_treeitem : public config_treeitem
	{
	  const std::string  item;
	  const std::wstring witem;
	  const std::wstring description;
	  const std::wstring long_description;
	  const std::string  dflt;

	  void value_changed()
	  {
	    vscreen_update();
	    description_changed();
	  }

	public:
	  string_config_treeitem(const std::string  &_item,
				 const std::wstring &_description,
				 const std::wstring &_long_description,
				 const std::string  &_dflt)
	    : item(_item),
	      witem(transcode(_item)),
	      description(_description),
	      long_description(_long_description),
	      dflt(_dflt)
	  {
	    aptcfg->connect(item, sigc::mem_fun(*this, &string_config_treeitem::value_changed));
	  }

	  fragment *get_long_description() const
	  {
	    const std::string value = aptcfg->Find(item, dflt.c_str());

	    std::vector<fragment *> fragments;


	    fragments.push_back(drophardwrapbox(fragf(_("%BOption:%b  ")),
						item));
	    fragments.push_back(drophardwrapbox(fragf(_("%BDefault:%b ")),
						dflt));
	    fragments.push_back(drophardwrapbox(fragf(_("%BValue:%b   ")),
						value));


	    fragments.push_back(newline_fragment());
	    fragments.push_back(wrapbox(text_fragment(long_description)));

	    return sequence_fragment(fragments);
	  }

	  void paint(vs_tree *win, int y, bool hierarchical, const style &st)
	  {
	    win->move(y, 0);
	    int x = 0;
	    const int maxx = win->getmaxx();

	    std::wstring value = transcode(aptcfg->Find(item, dflt.c_str()));
	    std::wstring::const_iterator valueIt = value.begin();
	    while(x < maxx)
	      {
		if(x < get_depth())
		  {
		    win->addch(' ');
		    ++x;
		  }
		else
		  {
		    if(valueIt == value.end())
		      {
			win->addch(' ');
			++x;
		      }
		    else
		      {
			win->add_wch(*valueIt);
			x += wcwidth(*valueIt);
			++valueIt;
		      }
		  }
	      }

	    if(valueIt != value.end())
	      {
		win->move(y, maxx - 5);
		win->addnstr(" ... ", 5);
	      }
	  }

	  const wchar_t *tag()
	  {
	    return witem.c_str();
	  }

	  const wchar_t *label()
	  {
	    return witem.c_str();
	  }

	  void set_text(const std::wstring &text)
	  {
	    aptcfg->Set(item, transcode(text));
	  }

	  void edit()
	  {
	    prompt_string(ssprintf(_("Editing \"%ls\""), description.c_str()),
			  aptcfg->Find(item, dflt.c_str()),
			  arg(sigc::mem_fun(this, &string_config_treeitem::set_text)),
			  NULL,
			  NULL,
			  NULL);
	  }

	  bool dispatch_key(const key &k, vs_tree *owner)
	  {
	    if(global_bindings.key_matches(k, "Confirm"))
	      edit();
	    else
	      return config_treeitem::dispatch_key(k, owner);

	    return true;
	  }
	};

	class dummy_subtree : public vs_subtree<config_treeitem>
	{
	  const std::wstring description;
	public:
	  dummy_subtree(const std::wstring &_description)
	    : vs_subtree<config_treeitem>(true),
	      description(_description)
	  {
	    set_selectable(false);
	  }

	  void paint(vs_tree *win, int y,
		     bool hierarchical, const style &)
	  {
	    vs_subtree<config_treeitem>::paint(win, y, hierarchical, description);
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
      }

      vs_treeitem *make_string_item(const std::wstring &description,
				    const std::wstring &long_description,
				    const std::string  &item,
				    const std::string  &dflt)
      {
	dummy_subtree *rval = new dummy_subtree(description);
	rval->add_child(new string_config_treeitem(item,
						   description,
						   long_description,
						   dflt));
	return rval;
      }
    }
  }
}
