# Copyright (C) 2010 Daniel Burrows
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; see the file COPYING.  If not, write to the
# Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
# MA 02111-1307, USA.



# Replacement for the built-in Install builder, supporting DESTDIR and
# automatically adding the nodes it generates to the "install" alias.

from SCons.Script import Copy, Entry, COMMAND_LINE_TARGETS
import os.path

def exists():
    return True

def generate(env):
    env.AddMethod(Install)

def Install(env, targetdir, *sources):
    def add_destdir(path):
        '''Add DESTDIR to a path.

os.path.join won't work because it doesn't join to absolute paths;
join('a', '/b') returns '/b'.  And concatenation doesn't work right
if DESTDIR lacks a trailing space.  This deals with both cases
properly.'''
        destdir = env['DESTDIR']
        if destdir == '':
            return path

        if not destdir.endswith('/'):
            destdir += '/'

        return os.path.normpath('%s%s' % (destdir, path))

    # Only set up the alias if the user asked for an install, so
    # "scons ." works to build everything with no surprises and so
    # that multiple variants can be built without trying to create
    # install targets for the same file.
    if 'install' in COMMAND_LINE_TARGETS:
        result = []
        for source in env.Flatten(sources):
            output = add_destdir(os.path.join(targetdir, os.path.basename(Entry(source).path)))
            result.append(env.Command(output,
                                      source,
                                      Copy('$TARGET', '$SOURCE')))

        env.Alias('install', result)

        return result
    else:
        return []
