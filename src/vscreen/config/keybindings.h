// keybindings.h, -*-c++-*-
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

#ifndef KEYBINDINGS_H
#define KEYBINDINGS_H

#include <string>
#include <list>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

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

#include <generic/util/strhash.h>
#include <vscreen/curses++.h>

/** Represents a keystroke as seen by curses.  Since the function keys
 *  can overlap Unicode codepoints, we need to include a value that
 *  distinguishes them.
 */
struct key
{
  /** The key code. */
  wint_t ch;

  /** If \b true, this is a function key. */
  bool function_key;

  key()
    :ch((wint_t) ERR), function_key(true)
  {
  }

  key(wint_t _ch, bool _function_key)
    :ch(_ch), function_key(_function_key)
  {
  }

  /** Lexicographic ordering on keys. */
  bool operator<(const key &other) const
  {
    return ch < other.ch || (ch == other.ch &&
			     !function_key && other.function_key);
  }

  bool operator==(const key &other) const
  {
    return ch == other.ch && function_key == other.function_key;
  }
};

typedef std::vector<key> keybinding;

class keybindings
{
  HASH_NAMESPACE::hash_map<std::string, keybinding> keymap;

  keybindings *parent;

  // It's way too easy to accidentally invoke the automatic copy
  // constructor instead of the real one.
  keybindings(const keybindings &_parent);
public:
  keybindings(keybindings *_parent=NULL):parent(_parent) {}

  /** \return the first binding of the given key, in a form suitable
   *   for inclusion in a config file.
   */
  std::wstring keyname(const std::string &tag);


  /** \return a human-readable string identifying the given keystroke
   *   (as opposed to 'keyname', which is a strict reverse mapping).
   */
  std::wstring readable_keyname(const std::string &tag);

  keybinding get(std::string tag)
    // Returns the keybinding for the given string.  Almost never needed.
  {
    HASH_NAMESPACE::hash_map<std::string, keybinding>::iterator found=keymap.find(tag);

    if(found==keymap.end())
      return keybinding();
    else
      return found->second;
  }

  void set(std::string tag, keybinding strokes);
  // Adds a setting for the given binding, clobbering whatever was there
  // previously.

  void set(std::string tag, const key &stroke)
  {
    keybinding strokes;
    strokes.push_back(stroke);
    set(tag, strokes);
  }

  bool key_matches(const key &k, std::string tag);
  // Tests whether the given keystroke matches the keybinding with the given
  // name.  If no keybinding by that name exists, the match fails.
};

key parse_key(std::wstring keystr);
// Parses a string to a keycode.  Returns ERR if the parse fails.

std::wstring keyname(const key &k);
// Returns a string identifying the given keystroke.

/** \return a human-readable string identifying the given keystroke
 *   (as opposed to 'keyname', which is a strict reverse mapping).
 */
std::wstring readable_keyname(const key &k);

extern keybindings global_bindings;
// For now, this is where the global bindings are stored (I might want to move
// it in the future, hmmm..)

// Stolen from pinfo.  I don't like the looks of it, but presumably it works
// (in some circumstances).  This is a FIXME, btw :)
/* adapted from Midnight Commander */

// Having read a bit more, it appears that the control modifier
// clears bits 5 and 4.  I think KEY_ALT is utterly broken.
#define KEY_CTRL(x) key(((x)&~(64|32)), false)
#define KEY_ALT(x) key((0x200 | (x)), false)


#endif
