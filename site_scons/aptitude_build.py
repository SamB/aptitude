from SCons import SConf
from SCons.Script import *

import os
import os.path

custom_tests = {}

def Configure(env):
    """Creates an aptitude-flavored configuration object targeting
the given environment."""

    return SCons.Script.Configure(env, custom_tests)

def RequireCheck(check, failure_message):
    """If the given configure check fails, print a message and exit."""
    if not check:
        print failure_message
        Exit(1)

def RegisterCheck(f):
    """Decorates a custom configure check by registering it in the
    global dictionary of checks under its name."""

    custom_tests[f.__name__] = f

def ConfigureCheck(message):
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
    def decorator(f):
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

        if f.__name__ in custom_tests:
            raise Exception('Duplicate function name \"%s\".' % f)
        else:
            custom_tests[f.__name__] = check

        check.__name__ = f.__name__
        return check

    return decorator

def TryInclude(d):
    """Generate a single entry in "tries" that outputs an appropriate message."""

    return ("In %s" % d, { 'CPPPATH' : [ d ] })

@ConfigureCheck("Checking for apt")
def CheckForApt(context):
    """Look for apt in the given directory."""

    context.env.Append(LIBS = [ 'apt-pkg' ])

    return context.TryLink('''
#include <apt-pkg/init.h>

int main(int argc, char **argv)
{
  pkgInitSystem(*_config, _system);
  return 0;
}''', context.env['CXXFILESUFFIX'])

@ConfigureCheck("Checking for libncursesw")
def CheckForNCursesW(context):
    """Look for NCursesW in the system header directory."""

    context.env.Append(LIBS = [ 'ncursesw' ])

    return context.TryLink('''
#include <ncurses.h>

int main(int argc, char **argv)
{
  wget_wch(0, 0);
  return 0;
}''', context.env['CXXFILESUFFIX'])

@RegisterCheck
def CheckForPo4A(context):
    """Look for po4a in $PO4APATH and set $PO4A accordingly.
PO4APATH defaults to $PATH."""
    context.Message("Checking for po4a...")
    context.Result("")

    po4apath = context.env.get('PO4APATH', '$PATH')
    for bindir in po4apath:
        context.Message("  In %s..." % bindir)
        po4a = os.path.abspath(os.path.join(po4apath, po4a))
        if os.access(po4a, os.X_OK):
            context.Result("yes")
            context.env['PO4A'] = po4a
            context.env['HAVE_PO4A'] = True
            return True

    context.Result("no")
    context.env['HAVE_PO4A'] = False
    return False
