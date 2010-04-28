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

from SCons.Script import AddOption, Builder, GetOption, Move

def exists():
    return True

def generate(env):
    env.Append(BUILDERS = { 'Xgettext' : XgettextBuilder })

# A more general solution would pass a lot of this stuff as parameters
# or environment variables.  In aptitude, a lot of these arguments are
# always the same, so I can get away with hardcoding them (but
# eventually they should be lifted out to be more hygenic).  Also, the
# environment variables here are fairly specific to aptitude's build
# ($PACKAGE and $XGETTEXT).
XgettextBuilder = Builder(action = [['$XGETTEXT',
                                     '--default-domain=$PACKAGE',
                                     '--add-comments=TRANSLATORS:',
                                     '--keyword=_',
                                     '--keyword=N_',
                                     '--keyword=P_',
                                     '--keyword=W_',
                                     '--sort-output',
                                     '--add-comments=ForTranslators:',
                                     '--copyright-holder=Daniel Burrows <dburrows@debian.org>',
                                     '--msgid-bugs-address=aptitude@packages.debian.org',
                                     '-o', '${TARGET}.new',
                                     '$SOURCES'],
                                    Move('$TARGET', '${TARGET}.new')],
                          suffix = '.pot')
