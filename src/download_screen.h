// download_screen.h  (this is -*-c++-*- )
//
//  Copyright 1999-2001, 2003-2005 Daniel Burrows
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; see the file COPYING.  If not, write to
//  the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//  Boston, MA 02111-1307, USA.
//
//  This acts as a progress meter for a download.

#ifndef DOWNLOAD_SCREEN_H
#define DOWNLOAD_SCREEN_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <apt-pkg/acquire.h>
#ifdef HAVE_HASH_MAP
#include <hash_map>
#else
#ifdef HAVE_EXT_HASH_MAP
#include <ext/hash_map>
#else
// Fallback to the non-hashing map class
#include <map>
#define hash_map map
#endif
#endif

#include <vscreen/vs_table.h>
#include <vscreen/vs_tree.h>
#include <vscreen/vs_subtree.h>

class download_item;

#if defined(HAVE_HASH_MAP) || defined(HAVE_EXT_HASH_MAP)
namespace HASH_NAMESPACE {
  template <>
  struct hash<void *>
  {
    size_t operator()(void * __x) const { return (size_t) __x; }
  };
}
#endif

class download_tree:public vs_subtree_generic
{
public:
  download_tree():vs_subtree_generic(true) {}

  void paint(vs_tree *win, int y, bool hierarchical, const style &style)
  {vs_subtree_generic::paint(win, y, hierarchical, L"ERROR: SHOULD NOT APPEAR");}
  const wchar_t * tag() {return L"download tree";}
  const wchar_t * label() {return L"download tree";}
};

class download_screen:public vs_tree, public pkgAcquireStatus
{
  typedef HASH_NAMESPACE::hash_map<void *, download_item *> downloadmap;
  downloadmap active_items;
  // Makes it easy to find a currently downloading item when we get a hit
  // for it.

  vscreen_widget *prev;
  // The screen that was being displayed before we started running the
  // download.

  bool finished;
  // If this is true, the status bar will be displayed as usual (as opposed to
  // being a progress meter)

  bool cancelled;
  // True if the user cancelled the download.

  download_tree *contents;

  download_item *get_itm(pkgAcquire::ItemDesc &itmdesc)
  {
    downloadmap::iterator found=active_items.find(itmdesc.Owner);
    eassert(found!=active_items.end());

    return found->second;
  }

protected:
  bool handle_key(const key &k);

public:
  download_screen():prev(NULL),finished(false),cancelled(false) {contents=new download_tree; set_root(contents);}

  bool MediaChange(string media, string drive);
  void IMSHit(pkgAcquire::ItemDesc &itmdesc);
  void Fetch(pkgAcquire::ItemDesc &itmdesc);
  void Done(pkgAcquire::ItemDesc &itmdesc);
  void Fail(pkgAcquire::ItemDesc &itmdesc);
  bool Pulse(pkgAcquire *Owner);
  void Start();
  void Stop();

  bool get_cursorvisible() {return false;}

  //void paint_status();

  virtual ~download_screen();
};

#endif
