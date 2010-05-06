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
from SCons.Script import ARGUMENTS, DefaultEnvironment, Exit
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
    conf.CheckGTK()

    conf.CheckForXsltproc()
    conf.CheckForPo4A()
    aptitude_configure_checks.FindGettextTools(conf)
    aptitude_configure_checks.FindGettext(conf)

    conf.Define('SIGC_VERSION',
                         '"%s"' % os.popen('pkg-config --modversion sigc++-2.0').read().strip())

    aptitude_configure_utils.RunConfigureFinishHooks(conf)


    pkgconfig_packages = [
        'cwidget',
        'libept',
        'liblog4cxx',
        'sigc++-2.0',
        'sqlite3',
        'vte',
        ]

    for pkg in pkgconfig_packages:
        RequireCheck(conf.PkgConfig(pkg),
                     "Can't find %s" % pkg)

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
                              # We need -fno-strict-aliasing to avoid
                              # compiler errors that I can't figure
                              # out how to fix.
                              CXXFLAGS = [ '-fno-strict-aliasing', '-Wall', '-Werror' ],
                              PACKAGE = PACKAGE,
                              VERSION = VERSION)

    # Complain if the user passed command-line build variables;
    # AddOption is much more modular, and so we prefer it everywhere,
    # even for stuff that traditionally (under automake) was a build
    # variable, like DESTDIR.
    if len(ARGUMENTS) > 0:
        print 'var=value arguments are not supported; see --help.'
        Exit(1)

    base.Tool('aptitude_doc')
    base.Tool('better_install')
    base.Tool('define_directories')
    base.Tool('distribute')
    base.Tool('docbook')
    base.Tool('docbook-xsl-import-scanner')
    base.Tool('html2text')
    base.Tool('msgfmt')
    base.Tool('msgmerge')
    base.Tool('pkg_config')
    base.Tool('po4a')
    base.Tool('rsvg')
    base.Tool('test')
    base.Tool('variant_builds')
    base.Tool('xgettext')
    base.Tool('xml-external-entity-scanner')

    def DisableVariants(env):
        if not env.get('HAVE_GTK', False):
            return 'gtk'
        else:
            return None

    base.DefineVariants(axes = [
        base.VariantAxis('Compile flags',
                         base.Variant('debug', flags = '-g -O0 -fno-inline'),
                         base.Variant('optimized', flags = '-g -O2'),
                         base.Variant('profiling', flags = '-g -O2 -pg')),
        base.VariantAxis('Interface',
                         base.Variant('curses', helptext = 'Command-line and curses only'),
                         base.Variant('gtk', helptext = 'Command-line, curses, and gtk',
                                      flags = '$GTKFLAGS')),
        ],
        default = 'debug-gtk',
        disabledf = DisableVariants)

    base.DefineDirectory('destdir',
                         default = '',
                         help = 'path prepended to all install paths (e.g., for packaging)')
    base.DefineDirectory('prefix',
                         default = '/usr/local',
                         help = 'installation prefix')
    base.DefineDirectory('bindir',
                         default = '$PREFIX/bin',
                         help = 'installation prefix for executable files')
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
    base.DefineDirectory('mandir',
                         default = '$PREFIX/man',
                         help = 'the location in which manpages should be installed (default /usr/share/man).')
    base.DefineDirectory('docdir',
                         default = '$DATADIR/doc',
                         help = 'system prefix for documentation.')
    base.DefineDirectory('pkgdocdir',
                         default = '$DOCDIR/$PACKAGE',
                         help = 'the location in which aptitude\'s documentation should be installed.')

    all_build_envs = base.Clone(CPPPATH = [ '#/src' ],
                                CPPDEFINES = [ '_REENTRANT' ])
    DoConfigureBuild(all_build_envs)

    programs = all_build_envs.Clone()
    boost_tests = all_build_envs.Clone()
    cppunit_tests = all_build_envs.Clone()

    DoConfigureBoostTests(boost_tests)
    DoConfigureCppunitTests(cppunit_tests)

    return ConfigureResult(base = all_build_envs,
                           programs = programs,
                           boost_tests = boost_tests,
                           cppunit_tests = cppunit_tests)
