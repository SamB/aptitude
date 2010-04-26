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

def exists():
    return True

def generate(env):
    env.AddMethod(Html2Text)

# TODO: support multiple html2text commands, not just elinks.
def Html2Text(env, target, source):
    '''Convert an HTML file to a text file using elinks.'''
    return env.Command(target, source,
                       'elinks -dump -no-references -no-numbering $SOURCE > $TARGET')
