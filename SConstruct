PACKAGE = 'aptitude'
VERSION = '0.6.2.1'

import aptitude_configure
import os

from aptitude_configure_utils import RequireCheck

# A generic environment used to build all aptitude's programs.

envs = aptitude_configure.Configure(PACKAGE, VERSION)

for variant_env in envs.programs.AllVariantEnvs():
    Export(programs_env = variant_env)
    aptitude = SConscript(['src/SConscript'], variant_dir = 'build/%s/src' % variant_env.GetVariantName())
    Default(aptitude)

# NB: I know because of how the variant directories are set up that we
# always have the same number of entries in the two lists.
for cppunit_tests_env, boost_tests_env in zip(envs.cppunit_tests.AllVariantEnvs(), envs.boost_tests.AllVariantEnvs()):
    Export(cppunit_tests_env = cppunit_tests_env, boost_tests_env = boost_tests_env)
    SConscript(['tests/SConscript'], variant_dir = 'build/%s/tests' % variant_env.GetVariantName())
    AlwaysBuild('test')
    # For convenience, make "scons check" the same as "make check".
    Alias('check', 'test')
