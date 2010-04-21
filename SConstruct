import aptitude_build
import os

from aptitude_build import TryInclude

# A generic environment used to build all aptitude's programs.
programs_env = DefaultEnvironment(ENV = { 'PATH' : os.environ['PATH'] },
                                  CPPPATH = [ '#', '#/src' ])
programs_conf = aptitude_build.Configure(programs_env)


programs_conf.CheckForNCursesW(tries = [ TryInclude('/usr/include'),
                                         TryInclude('/usr/include/ncursesw') ])
programs_conf.CheckForApt()


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
