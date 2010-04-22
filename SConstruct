PACKAGE = 'aptitude'
VERSION = '0.6.2'

import aptitude_build
import os

from aptitude_build import RequireCheck
from aptitude_build import TryInclude

AddOption('--prefix',
          dest = 'prefix',
          nargs = 1,
          type = 'string',
          action = 'store',
          metavar = 'DIR',
          default = '/usr/local',
          help = 'installation prefix')

AddOption('--datadir',
          dest = 'datadir',
          nargs = 1,
          type = 'string',
          action = 'store',
          metavar = 'DIR',
          default = '$PREFIX/share',
          help = 'data installation prefix')

AddOption('--localedir',
          dest = 'localedir',
          nargs = 1,
          type = 'string',
          action = 'store',
          metavar = 'DIR',
          default = '$DATADIR/locale',
          help = 'installation prefix for locale files')

# A generic environment used to build all aptitude's programs.
programs_env = DefaultEnvironment(ENV = { 'PATH' : os.environ['PATH'] },
                                  PACKAGE = PACKAGE,
                                  VERSION = VERSION,
                                  CPPPATH = [ '#', '#/src' ],
                                  CPPDEFINES = [ '_REENTRANT' ],
                                  PREFIX = GetOption('prefix'),
                                  DATADIR = GetOption('datadir'),
                                  LOCALEDIR = GetOption('localedir'))

prefix = programs_env['PREFIX']
datadir = programs_env['DATADIR']
localedir = programs_env['LOCALEDIR']

Export('prefix', 'datadir', 'localedir')

if 'LOCALEDIR' not in programs_env:
    programs_env['LOCALEDIR'] = '/usr/share/locale'

programs_conf = aptitude_build.Configure(programs_env)

RequireCheck(programs_conf.CheckForNCursesW(tries = [ TryInclude('/usr/include'),
                                                      TryInclude('/usr/include/ncursesw') ]),
             "Can't find libncursesw -- please install libncursesw5-dev.")
RequireCheck(programs_conf.CheckForApt(),
             "Can't find the APT libraries -- please install libapt-pkg-dev.")
RequireCheck(programs_conf.CheckForPThread(),
             "Can't find the POSIX thread libraries.")

programs_conf.CheckForPo4A()
aptitude_build.FindGettext(programs_conf)

programs_conf.Define('SIGC_VERSION',
                     '"%s"' % os.popen('pkg-config --modversion sigc++-2.0').read().strip())
programs_conf.Define('LOCALEDIR', localedir)
programs_conf.Define('PREFIX', prefix)
programs_conf.Define('DATADIR', datadir)

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
