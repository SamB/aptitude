// menu_text_layout.cc
//
//   Copyright (C) 2005 Daniel Burrows
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

#include "menu_text_layout.h"

#include "aptitude.h"
#include "ui.h"

#include <generic/util/slotarg.h>

#include <vscreen/config/keybindings.h>

bool menu_text_layout::find_search_enabled()
{
  return true;
}

bool menu_text_layout::find_search()
{
  prompt_string(_("Search for: "),
		"",
		arg(sigc::mem_fun(this, &menu_text_layout::do_find_search)),
		NULL,
		NULL,
		&search_history);

  return true;
}

void menu_text_layout::do_find_search(const std::wstring &s)
{
  last_search_forward = true;

  if(!s.empty())
    {
      last_search = s;
      search_for(s, true);
    }
  else if(!last_search.empty())
    search_for(last_search, true);
  else
    beep();
}

bool menu_text_layout::find_search_back_enabled()
{
  return true;
}

bool menu_text_layout::find_search_back()
{
  prompt_string(_("Search backwards for: "),
		"",
		arg(sigc::mem_fun(this, &menu_text_layout::do_find_search_back)),
		NULL,
		NULL,
		&search_history);

  return true;
}

void menu_text_layout::do_find_search_back(const std::wstring &s)
{
  last_search_forward = false;

  if(!s.empty())
    {
      last_search = s;
      search_for(s, false);
    }
  else if(!last_search.empty())
    search_for(last_search, false);
  else
    beep();
}

bool menu_text_layout::find_research_enabled()
{
  return !last_search.empty();
}

bool menu_text_layout::find_research()
{
  if(last_search.empty())
    beep();
  else
    search_for(last_search, last_search_forward);

  return true;
}

bool menu_text_layout::handle_key(const key &k)
{
  if(global_bindings.key_matches(k, "Search"))
    find_search();
  else if(global_bindings.key_matches(k, "SearchBack"))
    find_search_back();
  else if(global_bindings.key_matches(k, "ReSearch"))
    find_research();
  else
    return vs_text_layout::handle_key(k);

  return true;
}
