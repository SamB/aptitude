PACKAGE = 'aptitude'
VERSION = '0.6.2.1'

import aptitude_build
import os

from aptitude_build import RequireCheck
from aptitude_build import TryInclude

# A generic environment used to build all aptitude's programs.
programs_env = DefaultEnvironment(ENV = { 'PATH' : os.environ['PATH'] },
                                  PACKAGE = PACKAGE,
                                  VERSION = VERSION,
                                  CPPPATH = [ '#', '#/src' ],
                                  CPPDEFINES = [ '_REENTRANT' ])

programs_env.Tool('define_directories')
programs_env.Tool('variant_builds')

programs_env.DefineVariants(axes = [
    programs_env.VariantAxis('Compile flags',
                             programs_env.Variant('debug', flags = '-g -O0 -fno-inline'),
                             programs_env.Variant('optimized', flags = '-g -O2')),
    programs_env.VariantAxis('Interface',
                             programs_env.Variant('curses', helptext = 'Command-line and curses only'),
                             programs_env.Variant('gtk', helptext = 'Command-line, curses, and gtk')),
    ],
                            default = 'debug-gtk')

programs_env.DefineDirectory('prefix',
                             default = '/usr/local',
                             help = 'installation prefix')
programs_env.DefineDirectory('datadir',
                             default = '$PREFIX/share',
                             help = 'data installation prefix')
programs_env.DefineDirectory('pkgdatadir',
                             default = '$DATADIR/$PACKAGE',
                             help = 'package-specific data installation prefix')
programs_env.DefineDirectory('helpdir',
                             default = '$PKGDATADIR',
                             help = 'help file installation prefix')
programs_env.DefineDirectory('localedir',
                             default = '$DATADIR/locale',
                             help = 'installation prefix for locale files')
programs_env.DefineDirectory('statedir',
                             default = '/var/lib/$PACKAGE',
                             help = 'the location in which aptitude should store its state (default /var/lib/$PACKAGE)')
programs_env.DefineDirectory('lockfile',
                             default = '/var/lock/$PACKAGE',
                             help = 'the file that aptitude should use to lock out other instances of itself (default /var/lock/$PACKAGE).')

programs_conf = aptitude_build.Configure(programs_env)

RequireCheck(programs_conf.CheckForNCursesW(tries = [ TryInclude('/usr/include'),
                                                      TryInclude('/usr/include/ncursesw') ]),
             "Can't find libncursesw -- please install libncursesw5-dev.")
RequireCheck(programs_conf.CheckForApt(),
             "Can't find the APT libraries -- please install libapt-pkg-dev.")
RequireCheck(programs_conf.CheckForPThread(),
             "Can't find the POSIX thread libraries.")
RequireCheck(programs_conf.CheckForBoostIOStreams(),
             "Can't find Boost.IOStreams")
if programs_conf.CheckDDTP():
    programs_conf.Define('HAVE_DDTP', 1)

programs_conf.CheckForPo4A()
aptitude_build.FindGettext(programs_conf)

programs_conf.Define('SIGC_VERSION',
                     '"%s"' % os.popen('pkg-config --modversion sigc++-2.0').read().strip())

aptitude_build.RunConfigureFinishHooks(programs_conf)

programs_conf.Finish()

pkgconfig_packages = [
    'cwidget',
    'libept',
    'liblog4cxx',
    'sigc++-2.0',
    'sqlite3',
    'vte',
    ]

for pkg in pkgconfig_packages:
    programs_env.ParseConfig('pkg-config %s --cflags --libs' % pkg)


for variant_env in programs_env.AllVariantEnvs():
    Export(programs_env = variant_env)
    SConscript(['src/SConscript'], variant_dir = 'build/%s' % variant_env.GetVariantName())
