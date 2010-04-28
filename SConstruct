PACKAGE = 'aptitude'
VERSION = '0.6.2.1'

import aptitude_configure
import os
import os.path

from aptitude_configure_utils import RequireCheck

# A generic environment used to build all aptitude's programs.

envs = aptitude_configure.Configure(PACKAGE, VERSION)

manpage_locales = [
    'gl',
    'it',
    'pl'
    ]

tl_manpages = Glob('aptitude.??.8')
tl_helptexts = [
    'help.txt',
    Glob('help-??.txt'),
    Glob('help-??_??.txt')
    ]
tl_defaults = [
    Glob('aptitude-defaults.??'),
    Glob('aptitude-defaults.??_??')
    ]

# Put files from the top-level into the source distribution.
envs.base.Dist(
    'COPYING',
    'ChangeLog.SVN',
    'Doxyfile.in',
    'FAQ',
    'FAQ',
    'NEWS',
    'README.CWIDGET',
    'README.SMART-POINTERS',
    'README.THREADS',
    'README.i18n',
    'SConstruct',
    'aclocal.m4',
    'aptitude-defaults',
    'aptitude-hackers-guide.txt',
    'check_boost.sh',
    'function_groups',
    'function_pkgs',
    'section-descriptions',
    tl_defaults,
    tl_helptexts,
    tl_manpages,
    )


base_env = envs.base
Export('base_env')
SConscript(dirs = [ 'site_scons' ])


# Define everything that should be installed from the top-level.
envs.base.Install(envs.base['PKGDATADIR'], [
    'COPYING',
    'FAQ',
    'NEWS',
    'aptitude-defaults',
    'function_groups',
    'function_pkgs',
    'section-descriptions',
    tl_defaults,
    tl_helptexts,
    ])

envs.base.Install(envs.base['BINDIR'], [
        'aptitude-create-state-bundle',
        'aptitude-run-state-bundle'
        ])

for locale in manpage_locales:
    envs.base.Install(os.path.join(envs.base['MANDIR'], locale, 'man8'),
                      'aptitude.%s.8' % locale)

# Create the state directory on "install".
make_statedir = Command(envs.base['STATEDIR'], [],
                        Mkdir('$TARGET'))
envs.base.Alias('install', make_statedir)

all_aptitude_srcs = None

############# Code to build source & tests in each variant #############
for variant_env in envs.programs.AllVariantEnvs():
    Export(programs_env = variant_env)
    variant_dir = 'build/%s/src' % variant_env.GetVariantName()
    aptitude, all_aptitude_srcs_v = SConscript(['src/SConscript'],
                                               variant_dir = variant_dir)
    all_aptitude_srcs_v = set(Flatten(all_aptitude_srcs_v))
    if all_aptitude_srcs is None:
        all_aptitude_srcs = all_aptitude_srcs_v
    elif all_aptitude_srcs_v != all_aptitude_srcs:
        print 'Build script error: not all variants produced the same list of source files.'
        Exit(1)
    Default(aptitude)

# De-setify the list of sources.
all_aptitude_srcs = list(all_aptitude_srcs)
# Sort it so we don't get spurious changes in order due to hashing
# blips.
all_aptitude_srcs.sort()
Export('all_aptitude_srcs')

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


###### Build the documentation in build/doc ######
docs_env = envs.base
Export('docs_env')
SConscript(['doc/SConscript'], variant_dir = 'build/doc', duplicate = True)
Alias('docs', 'doc-html')
Alias('docs', 'doc-text')
Alias('docs', 'doc-man')

###### Build the gettext translation files ######
po_env = envs.base
Export('po_env')
# No variant_dir because the pofile workflow requires the outputs to
# be in the source directory and in distributed archives.
SConscript(['po/SConscript'])


# Don't generate the "dist" stuff unless it's been explicitly
# requested.  This allows "dist" to build all targets with '.'
# without having to worry about infinite recurrence.
if 'dist' in COMMAND_LINE_TARGETS:
    archives = envs.base.DistArchive(envs.base.subst('$PACKAGE-$VERSION'),
                                     scons_args = [ '--variants=all', '.',
                                                    '-j', GetOption('num_jobs') ])
    Alias('dist', archives)

    # Rebuild the pofiles *before* attempting to build the
    # distribution archive.
    po_env.Depends('dist', 'update-po')
