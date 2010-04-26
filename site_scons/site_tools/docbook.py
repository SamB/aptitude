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

from SCons.Script import Entry, File
import re

def exists():
    return True

def generate(env):
    env.AddMethod(Docbook)

def Docbook(env, target, xml,
            stylesheet = None,
            target_is_directory = False):
    '''Build a Docbook document from its input XML file.

If target_is_directory is True, the target output will be stored in a
directory of the given name.  This is equivalent to passing a filename
ending in "/" and leaving this parameter False.'''

    if stylesheet is not None:
        args = [File(stylesheet), File(xml)]
    else:
        args = [File(xml)]

    if target_is_directory:
        target_subst = '${TARGET}/'
    else:
        target_subst = '$TARGET'

    return env.Command(Entry(target), args,
                       [['xsltproc', '-o', target_subst] + args])

