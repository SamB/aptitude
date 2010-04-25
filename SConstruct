PACKAGE = 'aptitude'
VERSION = '0.6.2.1'

import aptitude_configure
import os
import os.path

from aptitude_configure_utils import RequireCheck

# A generic environment used to build all aptitude's programs.

envs = aptitude_configure.Configure(PACKAGE, VERSION)

# Put files from the top-level into the source distribution.
envs.base.Dist(
    'SConstruct',
    )





############# Code to build source & tests in each variant #############
for variant_env in envs.programs.AllVariantEnvs():
    Export(programs_env = variant_env)
    aptitude = SConscript(['src/SConscript'], variant_dir = 'build/%s/src' % variant_env.GetVariantName())
    Default(aptitude)

# NB: I know because of how the variant directories are set up that we
# always have the same number of entries in the two lists.
for cppunit_tests_env, boost_tests_env in zip(envs.cppunit_tests.AllVariantEnvs(), envs.boost_tests.AllVariantEnvs()):
    Export(cppunit_tests_env = cppunit_tests_env, boost_tests_env = boost_tests_env)
    # If this isn't true, something has gone horribly wrong:
    assert(cppunit_tests_env['VARIANT'] == boost_tests_env['VARIANT'])
    SConscript(['tests/SConscript'], variant_dir = 'build/%s/tests' % cppunit_tests_env.GetVariantName())
    AlwaysBuild('test')
    # For convenience, make "scons check" the same as "make check".
    Alias('check', 'test')


# Don't generate the "dist" stuff unless it's been explicitly
# requested.  This allows "dist" to build all targets with '.'
# without having to worry about infinite recurrence.
if 'dist' in COMMAND_LINE_TARGETS:
    archives = envs.base.DistArchive(envs.base.subst('$PACKAGE-$VERSION'),
                                     scons_args = [ '--variants=all', '.' ])
    Alias('dist', archives)
