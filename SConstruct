PACKAGE = 'aptitude'
VERSION = '0.6.2.1'

import aptitude_configure
import os

from aptitude_configure_utils import RequireCheck

# A generic environment used to build all aptitude's programs.

envs = aptitude_configure.Configure(PACKAGE, VERSION)

pkgconfig_packages = [
    'cwidget',
    'libept',
    'liblog4cxx',
    'sigc++-2.0',
    'sqlite3',
    'vte',
    ]

for pkg in pkgconfig_packages:
    envs.programs.ParseConfig('pkg-config %s --cflags --libs' % pkg)


for variant_env in envs.programs.AllVariantEnvs():
    Export(programs_env = variant_env)
    SConscript(['src/SConscript'], variant_dir = 'build/%s' % variant_env.GetVariantName())
