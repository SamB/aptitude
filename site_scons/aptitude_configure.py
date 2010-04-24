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


# Contains the logic to configure aptitude.
from SCons.Script import DefaultEnvironment
from SCons import SConf
import os

import aptitude_configure_checks
import aptitude_configure_utils
from aptitude_configure_checks import TryInclude
from aptitude_configure_utils import RequireCheck

languages = [
    "ar",
    "ast",
    "bs",
    "ca",
    "cs",
    "da",
    "de",
    "dz",
    "el",
    "es",
    "eu",
    "fi",
    "fr",
    "gl",
    "hu",
    "it",
    "ja",
    "km",
    "ku",
    "lt",
    "mr",
    "nb",
    "ne",
    "nl",
    "nn",
    "pl",
    "pt",
    "pt_BR",
    "ro",
    "ru",
    "sk",
    "sv",
    "th",
    "tl",
    "tr",
    "uk",
    "vi",
    "zh_CN",
    "zh_TW",
    ]

def DoConfigureBuild(env):
    """Configure the build environment 'env' with the libraries that
all executable targets need."""
    conf = aptitude_configure_checks.Configure(env)

    RequireCheck(conf.CheckForNCursesW(tries = [ TryInclude('/usr/include'),
                                                 TryInclude('/usr/include/ncursesw') ]),
                 "Can't find libncursesw -- please install libncursesw5-dev.")
    RequireCheck(conf.CheckForApt(),
                 "Can't find the APT libraries -- please install libapt-pkg-dev.")
    RequireCheck(conf.CheckForPThread(),
                 "Can't find the POSIX thread libraries.")
    RequireCheck(conf.CheckForBoostIOStreams(),
                 "Can't find Boost.IOStreams")
    if conf.CheckDDTP():
        conf.Define('HAVE_DDTP', 1)

    conf.CheckForPo4A()
    aptitude_configure_checks.FindGettext(conf)

    conf.Define('SIGC_VERSION',
                         '"%s"' % os.popen('pkg-config --modversion sigc++-2.0').read().strip())

    aptitude_configure_utils.RunConfigureFinishHooks(conf)

    conf.Finish()

def DoConfigureBoostTests(env):
    '''Configure the build environment "env" with the libraries that
the Boost unit tests need.'''

    conf = aptitude_configure_checks.Configure(env)

    RequireCheck(conf.CheckForBoostTest(),
                 "Can't find Boost.Test")

    conf.Finish()

def DoConfigureCppunitTests(env):
    '''Configure the build environment "env" with the libraries that
the CPPUnit unit tests need.'''

    conf = aptitude_configure_checks.Configure(env)

    RequireCheck(conf.CheckForCPPUnit(),
                 "Can't find CPPUnit")

    conf.Finish()

class ConfigureResult:
    '''Holder for the result of Configure(); see its documentation for
details.'''

    def __init__(self, **kwargs):
        for key, val in kwargs.iteritems():
            setattr(self, key, val)

def Configure(PACKAGE, VERSION):
    '''Create the build environments needed for various parts of
aptitude.

Returns an object with the following fields:
  - base: a bare environment with minimal customization
  - programs: an environment for building programs
  - tests: an environment for building unit tests
'''

    base = DefaultEnvironment(ENV = { 'PATH' : os.environ['PATH'] },
                              PACKAGE = PACKAGE,
                              VERSION = VERSION)

    base.Tool('define_directories')
    base.Tool('variant_builds')

    base.DefineVariants(axes = [
        base.VariantAxis('Compile flags',
                         base.Variant('debug', flags = '-g -O0 -fno-inline'),
                         base.Variant('optimized', flags = '-g -O2'),
                         base.Variant('profiling', flags = '-g -O2 -pg')),
        base.VariantAxis('Interface',
                         base.Variant('curses', helptext = 'Command-line and curses only'),
                         base.Variant('gtk', helptext = 'Command-line, curses, and gtk')),
        ],
        default = 'debug-gtk')

    base.DefineDirectory('prefix',
                         default = '/usr/local',
                         help = 'installation prefix')
    base.DefineDirectory('datadir',
                         default = '$PREFIX/share',
                         help = 'data installation prefix')
    base.DefineDirectory('pkgdatadir',
                         default = '$DATADIR/$PACKAGE',
                         help = 'package-specific data installation prefix')
    base.DefineDirectory('helpdir',
                         default = '$PKGDATADIR',
                         help = 'help file installation prefix')
    base.DefineDirectory('localedir',
                         default = '$DATADIR/locale',
                         help = 'installation prefix for locale files')
    base.DefineDirectory('statedir',
                         default = '/var/lib/$PACKAGE',
                         help = 'the location in which aptitude should store its state (default /var/lib/$PACKAGE)')
    base.DefineDirectory('lockfile',
                         default = '/var/lock/$PACKAGE',
                         help = 'the file that aptitude should use to lock out other instances of itself (default /var/lock/$PACKAGE).')

    all_build_envs = base.Clone(CPPPATH = [ '#', '#/src' ],
                                CPPDEFINES = [ '_REENTRANT' ])
    DoConfigureBuild(all_build_envs)

    programs = all_build_envs.Clone()
    boost_tests = all_build_envs.Clone()
    cppunit_tests = all_build_envs.Clone()

    DoConfigureBoostTests(boost_tests)
    DoConfigureCppunitTests(cppunit_tests)

    return ConfigureResult(base = base,
                           programs = programs,
                           boost_tests = boost_tests,
                           cppunit_tests = cppunit_tests)
