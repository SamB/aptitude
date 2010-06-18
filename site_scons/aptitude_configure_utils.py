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


# Custom configure checks for aptitude and the code to register and
# configure them.

custom_tests = {}

def Configure(env):
    """Creates an aptitude-flavored configuration object targeting
the given environment."""

    result = SCons.Script.Configure(env, custom_tests, config_h = 'build/config/config.h')
    result.Define('PACKAGE', '"%s"' % env['PACKAGE'])
    result.Define('VERSION', '"%s"' % env['VERSION'])
    # Need to inform the source code that we have a config.h file:
    env.MergeFlags(env.ParseFlags('-DHAVE_CONFIG_H'))
    return result

def RegisterCheck(f, name = None):
    """Decorates a custom configure check by registering it in the
    global dictionary of checks under its name."""

    if name is None:
        name = f.__name__
    custom_tests[name] = f
    return f


def ConfigureCheck(message, register = True, name = None):
    """Decorates a custom configure function by modifying its context
as specified in kwargs before running it.  If the test succeeds
by returning a true value, the environment is preserved; if the test
fails, the environment is restored.

The "tries" keyword argument can be used to specify an explicit series
of checks.  The advantage of doing this versus simply invoking the
check multiple times is that you get better messages: instead of

Checking for ncursesw... no
Checking for ncursesw... yes

you get:

Checking for ncursesw...
  In /usr/include... no
  In /usr/include/ncursesw... yes

Each entry of "tries" has the form (msg, vars).  For instance, in the
above example. "msg" would be "In /usr/include" or "In /usr/include/ncursesw".
If "tries" is not present, one run is performed with the values in kwargs.

This decorator also adds the test to the custom_tests dictionary."""
    def decorator(f, name = name):
        def check(context, tries = None, *args, **kwargs):
            context.Message('%s...' % message)

            if tries is None:
                show_msg = False
                tries = [("", {})]
            else:
                if len(tries) == 0:
                    raise Exception('Configure checks must have at least one test.')
                context.Result('')
                show_msg = True

            for msg, bindings in tries:
                if show_msg:
                    context.Message('  %s...' % msg)

                env2 = context.env.Clone()
                context.env.Append(**kwargs)
                context.env.Append(**bindings)
                result = f(context, *args)

                context.Result(bool(result))

                if not result:
                    # TODO: this might not work if variables were changed
                    # that weren't in the original environment.  What to
                    # do then?
                    context.env.Replace(**env2.Dictionary())
                else:
                    return result

            return result

        if name is None:
            name = f.__name__
        if register:
            if name in custom_tests:
                raise Exception('Duplicate function name \"%s\".' % f)
            else:
                custom_tests[name] = check

        check.__name__ = name
        return check

    return decorator

def RequireCheck(check, failure_message):
    """If the given configure check fails, print a message and exit."""
    if not check:
        print failure_message
        SCons.Script.Exit(1)

def TryCompileCXX(context, code):
    """Compile some code as a C++ program."""
    return context.TryCompile(code, context.env['CXXFILESUFFIX'])
