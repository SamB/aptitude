// colors.h  -*-c++-*-
//
//  Copyright 1999-2001, 2004-2005 Daniel Burrows
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
//  Manages color allocation so as to allow any combination of
//  foreground/background colors to be used.  If there aren't enough
//  color pairs available to handle all color combinations, this will
//  act as though no colors are available.  NOTE: colors whose
//  foreground and background are the same will be reduced to an
//  arbitrary color of that background; it is expected that the caller
//  will apply A_INVIS to such colors.  This is done to conserve color
//  pairs so as to allow the use of the 'default' color.

#ifndef COLORS_H
#define COLORS_H

/** Set up the colors as we expect them to be.  Call this once
 *  when the program starts.
 */
void init_colors();

/** \return a color pair for the given foreground and background. */
int get_color_pair(short fg, short bg);

/** \param color attributes containing the starting color value
 *  \param fg the new foreground (-1 to use color)
 *  \param bg the new background (-2 to use color; -1 to use the default background)
 *
 *  \return a color pair created by mixing the given foreground
 *  and background into color.
 */
int mix_color(short color, short fg, short bg);

#endif
