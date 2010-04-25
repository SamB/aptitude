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

from SCons.Script import Copy, Delete, Dir, Entry

import os.path

# Emulation of "make dist" for scons.
def exists():
    return True

def generate(env):
    env.AddMethod(Archive)
    env.AddMethod(Dist)
    env.AddMethod(DistArchive)
    env.AddMethod(DistDirectory)
    env._dist = set()

def Dist(env, *sources):
    '''Register the given files for inclusion in the distribution
archive.'''

    # I could instead make '.' require them.  That would make builds
    # fail if they were missing, which I kind of like; it would also
    # mean that if they were not actually leaves, they would be built
    # by default and not included in the distribution, which I don't
    # like.
    for f in env.Flatten(sources):
        if isinstance(f, basestring):
            f = Entry(f)

        # Do this to collapse away variant builds.
        env._dist.add(f.srcnode())

def DistDirectory(env, dir_name):
    '''Find all source files under \'.\' and copy them into the named
directory.  Returns the node for the whole process.'''
    # Early versions of this code used FindSourceFiles.  However, that
    # has some significant drawbacks; most notably, it's very
    # difficult to control what gets put into the distribution
    # archive.  I'd rather specify the contents "by hand".
    sources = env._dist
    directory = Dir(dir_name)
    for source in sources:
        target = os.path.join(directory.path, source.path)
        env.Command(target,
                    source,
                    Copy("$TARGET", "$SOURCE"))
        env.Depends(directory, target)

    return directory

def Archive(env, target, sources, type = None):
    '''Create an archive from one or more files or directories.

The given "type" is an archive format suffix.  If it is not present, the
suffix of the target file is used to determine the archive format.

Recognized suffixes include:
  .7z
  .tar
  .tar.bz2
  .tar.gz
  .tar.lzma
  .zip

Returns the node for the target.'''

    sources = env.Flatten(sources)

    class Tool:
        def __init__(self, suffix, command):
            self.suffix = suffix
            self.command = command

    tools = [
        Tool('.7z', '7z a -mx=9 $TARGET $SOURCES'),
        Tool('.tar', 'tar cf $TARGET $SOURCES'),
        Tool('.tar.bz2', 'tar c $SOURCES | bzip2 -9 > $TARGET'),
        Tool('.tar.gz', 'tar c $SOURCES | gzip -9 > $TARGET'),
        Tool('.tar.lzma', 'tar c $SOURCES | lzma --best > $TARGET'),
        Tool('.zip', 'zip -r $TARGET $SOURCES')
        ]

    tool = None
    if type is None:
        for tool_test in tools:
            if target.endswith(tool_test.suffix):
                tool = tool_test
                break
        if tool is None:
            raise Exception('Unable to determine an archive format for %s' % `target`)
    else:
        for tool_test in tools:
            if type == tool_test.suffix:
                tool = tool_test
                break

        if tool is None:
            raise Exception('Unknown archive format %s' % `type`)

    try:
        return env.Command(target, sources, tool.command)
    except Exception, e:
        print e
        raise e

# archive-stem used to default to ${PACKAGE}-${VERSION}, but there was
# weirdness due to the ${} not always getting expanded.  Now we just
# rely on the caller doing it for us.
def DistArchive(env,
                archive_stem,
                archive_directory = '#/build/dist',
                distcheck_directory = '#/build/distcheck',
                suffixes = [ '.tar.gz' ],
                scons_args = [ ]):
    '''Create a directory containing the distributed source and
archive it.  "suffixes" is a list of archive suffixes such as
".tar.gz", ".tar.bz2", etc.

By default, the variables PACKAGE and VERSION are used to name the
generated archive.

If scons_args is not None, then scons will invoke itself in the
subdirectory, passing the given list of arguments; the archive is only
created if the subsidiary scons succeeds.  This can be used, for
instance, to verify that the source compiles and passes its unit
tests.

Returns a list of targets, one for each entry in "suffixes".'''
    archive_filename = os.path.join(archive_directory, archive_stem)
    dist_dir = env.DistDirectory(archive_filename)

    if scons_args is not None:
        dist_dir_pristine_copy_filename = Dir(os.path.join(distcheck_directory, archive_stem))
        dist_dir_pristine_copy = env.Command(dist_dir_pristine_copy_filename,
                                             dist_dir,
                                             [ Delete("$TARGET"),
                                               Copy("$TARGET", "$SOURCE") ])

        tested_dist_dir_stamp = os.path.join(distcheck_directory, '%s.stamp' % archive_stem)
        tested_dist_dir = env.Command(tested_dist_dir_stamp,
                                      dist_dir_pristine_copy,
                                      [ [ 'scons' ] + scons_args,
                                        "echo ${SOURCE.get_csig()} > $TARGET" ],
                                      chdir = Dir(dist_dir_pristine_copy_filename).abspath)
    else:
        # Pretend we tested it.
        tested_dist_dir = dist_dir

    result = []
    for suffix in suffixes:
        archive = env.Archive(os.path.join(archive_directory,
                                           '%s%s' % (archive_stem, suffix)),
                              [ dist_dir ])
        env.Depends(archive, tested_dist_dir)

        result.append(archive)

    return result
