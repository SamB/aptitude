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

# Contains utility code dealing with writing and invoking configure
# tests, plus some very generic configure tests.  Configure tests for
# specific libraries are in aptitude_configure.py.

from SCons import SConf
import SCons.Script

# Used by submodules to stage code that should run at the end of the
# configure step.  They're passed the configure object as a parameter.
configure_finish_hooks = []

def AddConfigureFinishHook(f):
    configure_finish_hooks.append(f)

def RunConfigureFinishHooks(configure):
    for hook in configure_finish_hooks:
        hook(configure)


def RequireCheck(check, failure_message):
    """If the given configure check fails, print a message and exit."""
    if not check:
        print failure_message
        SCons.Script.Exit(1)

