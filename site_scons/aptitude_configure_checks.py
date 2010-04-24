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

import SCons.Script
import subprocess

# Custom configure checks for aptitude and the code to register and
# configure them.

custom_tests = {}

def Configure(env):
    """Creates an aptitude-flavored configuration object targeting
the given environment."""

    result = SCons.Script.Configure(env, custom_tests, config_h = 'config.h')
    result.Define('PACKAGE', '"%s"' % env['PACKAGE'])
    result.Define('VERSION', '"%s"' % env['VERSION'])
    # Need to inform the source code that we have a config.h file:
    env.MergeFlags(env.ParseFlags('-DHAVE_CONFIG_H'))
    return result

def RegisterCheck(f):
    """Decorates a custom configure check by registering it in the
    global dictionary of checks under its name."""

    custom_tests[f.__name__] = f
    return f


def ConfigureCheck(message, register = True):
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

        if register:
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

@RegisterCheck
def CheckForExecutable(context, filename, var):
    """Look for the given filename in $PATH.  If var is set in the
environment, use its value; otherwise find the program and set var to
its absolute path."""
    context.Message("Checking for %s..." % filename)

    location = context.env.get(var)
    if location is None:
        location = context.env.WhereIs(filename)

    if location is None:
        context.Result('no')
        return False
    else:
        context.env[var] = location
        context.Result(location)
        return True


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

@ConfigureCheck("Checking for libpthread")
def CheckForPThread(context):
    """Look for POSIX thread support."""

    context.env.Append(LIBS = [ 'pthread' ])

    return context.TryLink('''
#include <pthread.h>

int main(int argc, char **argv)
{
  pthread_mutex_init(0, 0);
  return 0;
}''', context.env['CXXFILESUFFIX'])

@ConfigureCheck("Checking for Boost.IOStreams")
def CheckForBoostIOStreams(context):
    """Look for Boost.IOStreams."""

    context.env.Append(LIBS = [ 'boost_iostreams' ])

    return context.TryLink('''
#include <boost/iostreams/device/file.hpp>
#include <boost/iostreams/filter/gzip.hpp>
#include <boost/iostreams/filtering_stream.hpp>

int main(int argc, char **argv)
{
  boost::iostreams::file_sink devnull("/dev/null");
  boost::iostreams::filtering_ostream compressed_devnull(boost::iostreams::zlib_compressor(9) | devnull);
}''', context.env['CXXFILESUFFIX'])

@ConfigureCheck("Checking for Boost.Test")
def CheckForBoostTest(context):
    """Look for Boost.Test."""

    context.env.Append(LIBS = [ 'boost_unit_test_framework' ],
                       CPPDEFINES = [ 'BOOST_TEST_DYN_LINK',
                                      'BOOST_TEST_NO_MAIN' ])

    return context.TryLink('''
#include <boost/test/unit_test.hpp>

BOOST_AUTO_TEST_CASE(dummy)
{
}

bool init_unit_test()
{
  return true;
}

int main(int argc, char **argv)
{
  return boost::unit_test::unit_test_main(init_unit_test, argc, argv);
}''', context.env['CXXFILESUFFIX'])

@ConfigureCheck("Checking for CPPUnit")
def CheckForCPPUnit(context):
    """Look for CPPUnit."""

    context.env.Append(LIBS = 'cppunit')

    return context.TryLink('''
#include <cppunit/extensions/HelperMacros.h>
#include <cppunit/extensions/TestFactoryRegistry.h>
#include <cppunit/ui/text/TestRunner.h>

class FooTest : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(FooTest);

  CPPUNIT_TEST(testDummy);

  CPPUNIT_TEST_SUITE_END();

public:
  void testDummy()
  {
  }
};

CPPUNIT_TEST_SUITE_REGISTRATION(FooTest);

int main(int argc, char **argv)
{
  CppUnit::TextTestRunner runner;
  CppUnit::TestFactoryRegistry &registry =
    CppUnit::TestFactoryRegistry::getRegistry();

  runner.addTest(registry.makeTest());

  bool wasSuccessful = runner.run("", false);

  return wasSuccessful ? 0 : -255;
}''', context.env['CXXFILESUFFIX'])

@RegisterCheck
def PkgConfig(context, pkg):
    context.Message('Checking for %s' % pkg)

    pipe = subprocess.Popen(['pkg-config',
                             '--cflags',
                             '--libs',
                             pkg],
                            stdout = subprocess.PIPE)

    output = pipe.stdout.read()
    pipe.wait()

    if pipe.returncode == 0:
        context.env.MergeFlags(output)
        result = True
    else:
        result = False

    context.Result(result)
    return result

@ConfigureCheck("Checking for po4a")
def CheckForPo4A(context):
    """Look for po4a in $PATH and set $PO4A accordingly."""

    return CheckForExecutable(context, 'po4a', 'PO4A')

@ConfigureCheck("Checking for libintl in libc")
def CheckForLibintlInLibc(context):
    """Check whether libintl is already included in libc."""

    if context.TryLink('''
#include <libintl.h>

int main(int argc, char **argv)
{
  const char * const foo = gettext("Foo");
}''', context.env['CXXFILESUFFIX']):
        context.Result('yes')
        return True
    else:
        context.Result('no')
        return False

@ConfigureCheck('Checking for setlocale in libintl.h')
def CheckSetLocale(context):
    if context.TryLink('''
#include <locale.h>

int main(int argc, char **argv)
{
  setlocale(0, 0);
}''', context.env['CXXFILESUFFIX']):
        context.Result('yes')
        return True
    else:
        context.Result('no')
        return False

@ConfigureCheck('Checking whether apt supports ddtp')
def CheckDDTP(context):
    if context.TryLink('''
#include <apt-pkg/pkgcache.h>

int main(int argc, char **argv)
{
  pkgCache::DescIterator d;
}''', context.env['CXXFILESUFFIX']):
        context.Result('yes')
        return True
    else:
        context.Result('no')
        return False


def FindGettext(configure):
    """Look for gettext-related utilities."""

    result = True

    result = configure.CheckForLibintlInLibc() and result
    result = configure.CheckForExecutable('gettext', 'GETTEXT') and result
    result = configure.CheckForExecutable('msgmerge', 'MSGMERGE') and result
    result = configure.CheckHeader('libintl.h') and result
    result = configure.CheckHeader('locale.h') and result

    if configure.CheckSetLocale():
        configure.Define("HAVE_SETLOCALE")
    else:
        result = False

    if result:
        configure.Define("ENABLE_NLS", 1)

    return result
