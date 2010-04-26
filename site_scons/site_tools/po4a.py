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
    env.AddMethod(Po4A)

def Po4A(env, target, master, pofile,
         addendum = None,
         percent_translated = 75,
         master_charset = 'utf-8',
         format = 'docbook'):
    '''Run po4a over a document to produce a translated document.'''
    extra_args = []
    deps = [master, pofile]
    if addendum is not None:
        # This is what we should do, but the addendums were never used
        # in the old system and they break the build!  Disabling them
        # for now.
        #
        #extra_args += ['-a', addendum]
        #deps.append(addendum)
        pass
    return env.Command(target, deps,
                       [[
            'po4a-translate',
            '-k', str(percent_translated),
            '-M', master_charset,
            '-f', format,
            '-m', master,
            '-p', pofile,
            '-l', '$TARGET'
            ] + extra_args])
