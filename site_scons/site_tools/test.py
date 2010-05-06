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

from SCons.Script import File

def exists():
    return True

def generate(env):
    env.AddMethod(Test)


def Test(env, target, test_inputs = [], *args, **kwargs):
    '''Compile the given test and run it as part of the "test" target.
Returns the Node for the alias.'''

    if 'test_target' in kwargs:
        test_target = kwargs['test_target']
        del kwargs['test_target']
    else:
        test_target = 'test'

    program = env.Program(target, *args, **kwargs)
    stampfile = File('%s.stamp' % target)
    program_stamp = env.Command(stampfile,
                                [program] + test_inputs,
                                [ program[0].abspath,
                                  ['touch', stampfile.abspath] ])
    return env.Alias(test_target, program_stamp)


