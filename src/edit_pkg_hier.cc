// edit_pkg_hier.cc
//
//   Copyright (C) 2000-2001, 2004-2006 Daniel Burrows
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


#include "edit_pkg_hier.h"

#include "aptitude.h"
#include "ui.h"

#include <apt-pkg/error.h>

#include <sigc++/adaptors/bind.h>
#include <sigc++/functors/mem_fun.h>
#include <sigc++/trackable.h>

#include <generic/apt/apt.h>

#include <generic/util/util.h>

#include <vscreen/config/keybindings.h>
#include <vscreen/vs_subtree.h>
#include <vscreen/transcode.h>

using namespace std;
using namespace __gnu_cxx;

// Stores a group (name) and a Y/N state
class vs_hier_editor::vs_hier_item:public sigc::trackable, public vs_treeitem
{
  bool selected;
  pkg_hier::group *group;
  pkg_hier::item *item;
  // whatever
  wstring group_name;
public:
  vs_hier_item(pkg_hier::group *_group, pkg_hier::item *_item)
    :vs_treeitem(true), group(_group), group_name(transcode(group->name, "ASCII"))
  {
    set_item(_item);
  }

  bool dispatch_key(const key &k, vs_tree *owner)
  {
    if(global_bindings.key_matches(k, "PushButton") ||
       global_bindings.key_matches(k, "Confirm"))
      {
	selected=!selected;
	vscreen_update();
      }
    else
      return vs_treeitem::dispatch_key(k, owner);

    return true;
  }

  void dispatch_mouse(short id, int x, mmask_t bstate, vs_tree *owner)
  {
    if(bstate&BUTTON1_DOUBLE_CLICKED || bstate & BUTTON3_PRESSED ||
       bstate & BUTTON3_CLICKED)
      {
	selected=!selected;
	vscreen_update();
      }
    else
      vs_treeitem::dispatch_mouse(id, x, bstate, owner);
  }

  void paint(vs_tree *win, int y, bool hierarchical, const style &st)
  {
    string::size_type width=win->get_width();
    string todisp=" ";

    if(selected)
      todisp+='*';
    else
      todisp+=' ';

    todisp+=" ";
    todisp+=group->name;
    todisp+=" : ";
    todisp+=group->description;

    while(todisp.size()<width)
      todisp+=" ";

    win->mvaddnstr(y, 0, transcode(todisp, "ASCII"), width);
  }

  const wchar_t *tag()
  {
    return group_name.c_str();
  }

  const wchar_t *label()
  {
    return group_name.c_str();
  }

  void commit()
  {
    if(selected)
      item->parents.insert(group->name);
    else
      item->parents.erase(group->name);
  }

  void set_item(pkg_hier::item *_item)
  {
    if(item!=_item)
      // Don't unnecessarily clobber our state.
      {
	item=_item;

	if(item)
	  selected=item->parents.find(group->name)!=item->parents.end();
	else
	  selected=false;
      }
  }
};

// FIXME: I shouldn't have to do this.
class silly_subtree:public vs_subtree_generic
{
  wstring txt;
public:
  silly_subtree(bool expanded, const wstring &_txt)
    :vs_subtree_generic(expanded), txt(_txt) {}

  void paint(vs_tree *win, int y, bool hierarchical, const style &st)
  {
    vs_subtree_generic::paint(win, y, hierarchical, txt);
  }

  const wchar_t *tag() {return txt.c_str();}
  const wchar_t *label() {return txt.c_str();}
};

vs_hier_editor::vs_hier_editor():item(NULL)
{
  hier_reloaded.connect(sigc::mem_fun(*this, &vs_hier_editor::handle_reload));
}

void vs_hier_editor::handle_reload()
{
  vs_widget_ref tmpref(this);

  set_root(NULL);
  items.clear();

  item=NULL;

  set_package(pkgCache::PkgIterator(),
	      pkgCache::VerIterator(*apt_cache_file));
}

bool vs_hier_editor::get_cursorvisible()
{
  if(!item)
    return false;
  else
    return true;
}

void vs_hier_editor::paint(const style &st)
{
  if(!item)
    mvaddnstr(0, 0, _("No hierarchy information to edit"), get_width());
  else
    vs_tree::paint(st);
}

// Creates a new list of edit-widgets if pkg isn't an end iterator.
//
// (yes, it would be nicer in some ways to not recreate the tree continually)
void vs_hier_editor::set_package(const pkgCache::PkgIterator &pkg)
{
  vs_widget_ref tmpref(this);

  shown_conn.disconnect();
  if(get_visible())
    {
      pkg_hier *user_pkg_hier=get_user_pkg_hier();

      if(user_pkg_hier && !pkg.end())
	{
	  // Get a reference to the item stored in the hierarchy, or
	  // create it if it isn't there.
	  pkg_hier::pkgmap::iterator found=user_pkg_hier->pkgs.find(pkg.Name());
	  item=NULL;

	  if(found==user_pkg_hier->pkgs.end())
	    {
	      user_pkg_hier->pkgs[pkg.Name()]=pkg_hier::item(pkg.Name());
	      item=&user_pkg_hier->pkgs[pkg.Name()];
	    }
	  else
	    item=&found->second;

	  if(items.empty())
	    {
	      silly_subtree *newroot=new silly_subtree(true, L"All groups");

	      // Add all available groups to the list.
	      for(pkg_hier::groupmap::iterator i=user_pkg_hier->groups.begin();
		  i!=user_pkg_hier->groups.end();
		  ++i)
		{
		  vs_hier_item *tmp=new vs_hier_item(&i->second, item);
		  commit_changes.connect(sigc::mem_fun(*tmp, &vs_hier_item::commit));

		  newroot->add_child(tmp);
		  items.push_back(tmp);
		}

	      newroot->sort();

	      set_root(newroot);
	    }

	  for(vector<vs_hier_item *>::iterator i=items.begin();
	      i!=items.end(); ++i)
	    (*i)->set_item(item);
	}
      else
	item=NULL;
    }
  else
    {
      string name=pkg.end()?"":pkg.Name();

      shown_conn=shown_sig.connect(sigc::bind(sigc::mem_fun(*this,
							    (void (vs_hier_editor::*) (string)) &vs_hier_editor::set_package),
					      name));
    }
}

void vs_hier_editor::set_package(const pkgCache::PkgIterator &pkg,
				 const pkgCache::VerIterator &ver)
{
  set_package(pkg);
}

void vs_hier_editor::set_package(std::string name)
{
  if(apt_cache_file)
    set_package((*apt_cache_file)->FindPkg(name));
}

// Lets us sort stuff by name:
struct item_cmp
{
  bool operator()(const pkg_hier::item *a, const pkg_hier::item *b)
  {
    return a->name<b->name;
  }
};

void vs_hier_editor::save_hier(string file)
{
  // We copy references to items into a list, then sort it (so that the
  // output is in a predictable order -- this makes diffs against an
  // autogenerated file much more useful)
  vector<pkg_hier::item *> items;

  FILE *f=fopen(file.c_str(), "w");

  if(!f)
    {
      _error->Errno("save_hier", _("Couldn't open \"%s\" for writing"), file.c_str());
      return;
    }

  for(pkg_hier::pkgmap::iterator i=get_user_pkg_hier()->pkgs.begin();
      i!=get_user_pkg_hier()->pkgs.end();
      ++i)
    // Don't save items with no parents, it's pointless.
    if(!i->second.parents.empty())
      items.push_back(&i->second);

  sort(items.begin(), items.end(), item_cmp());

  for(vector<pkg_hier::item *>::iterator i=items.begin();
      i!=items.end();
      ++i)
    {
      string parentsstr;

      for(hash_set<string>::iterator j=(*i)->parents.begin();
	  j!=(*i)->parents.end(); ++j)
	{
	  if(j==(*i)->parents.begin())
	    parentsstr+=*j;
	  else
	    parentsstr+=", "+*j;
	}

      fprintf(f, "%sPackage: %s\nParents: %s\n",
	      i==items.begin()?"":"\n",
	      (*i)->name.c_str(), parentsstr.c_str());
    }

  fclose(f);
}

bool vs_hier_editor::handle_key(const key &k)
{
  if(global_bindings.key_matches(k, "SaveHier"))
    {
      string homedir = get_homedir();
      string cfgfile;

      if(homedir.empty())
	{
	  show_message(_("Unable to look up your home directory, saving to /tmp/function_pkgs!"),
		       NULL,
		       get_style("Error"));
	  cfgfile = "/tmp/function_pkgs";
	}
      else
	cfgfile = homedir + "/.aptitude/function_pkgs";
      save_hier(cfgfile);
    }
  else if(global_bindings.key_matches(k, "Quit"))
    {
      if(item)
	{
	  item->parents.clear();

	  for(vector<vs_hier_item *>::iterator i=items.begin();
	      i!=items.end(); ++i)
	    (*i)->commit();
	}

      hide();
    }
  else if(global_bindings.key_matches(k, "Commit"))
    {
      if(item)
	{
	  item->parents.clear();

	  for(vector<vs_hier_item *>::iterator i=items.begin();
	      i!=items.end(); ++i)
	    (*i)->commit();
	}

      commit_changes();
    }
  else if(global_bindings.key_matches(k, "Abort"))
    hide();
  else
    return vs_tree::handle_key(k);

  return true;
}
