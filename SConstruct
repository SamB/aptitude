import aptitude_build
import os

from aptitude_build import RequireCheck
from aptitude_build import TryInclude

# A generic environment used to build all aptitude's programs.
programs_env = DefaultEnvironment(ENV = { 'PATH' : os.environ['PATH'] },
                                  CPPPATH = [ '#', '#/src' ])
programs_conf = aptitude_build.Configure(programs_env)


RequireCheck(programs_conf.CheckCXX(), 'No working C++ compiler found.')
RequireCheck(programs_conf.CheckForNCursesW(tries = [ TryInclude('/usr/include'),
                                                      TryInclude('/usr/include/ncursesw') ]),
             "Can't find libncursesw -- please install libncursesw5-dev.")
RequireCheck(programs_conf.CheckForApt(),
             "Can't find the APT libraries -- please install libapt-pkg-dev.")

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

print programs_env
