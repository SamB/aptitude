PACKAGE = 'aptitude'
VERSION = '0.6.2'

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

programs_conf = aptitude_build.Configure(programs_env)

RequireCheck(programs_conf.CheckForNCursesW(tries = [ TryInclude('/usr/include'),
                                                      TryInclude('/usr/include/ncursesw') ]),
             "Can't find libncursesw -- please install libncursesw5-dev.")
RequireCheck(programs_conf.CheckForApt(),
             "Can't find the APT libraries -- please install libapt-pkg-dev.")
RequireCheck(programs_conf.CheckForPThread(),
             "Can't find the POSIX thread libraries.")
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




Export('programs_env')

SConscript(['src/SConscript'])
