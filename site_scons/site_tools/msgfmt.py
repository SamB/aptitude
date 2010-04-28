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

from SCons.Script import Builder, Move
import msgmerge

def exists():
    return True

def generate(env):
    env.Append(BUILDERS = { 'Msgfmt' : MsgfmtBuilder })

# TODO: could add rules for building .mo (is that for non-GNU
# systems?)  Maybe even build either one, depending on which ones are
# available?

# Note that we can't automatically build the .po from the .pot since
# we can't compute the .pot name from the .po name.
MsgfmtBuilder = Builder(
    action = [[
        '$MSGFMT', '-c', '--statistics',
        '-o', '${TARGET}.new',
        '$SOURCE'
        ],
              Move('${TARGET}', '${TARGET}.new')],
    suffix = '.gmo',
    source_suffix = '.po')
