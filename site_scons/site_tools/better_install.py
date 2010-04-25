# Replacement for the built-in Install builder, supporting DESTDIR and
# automatically adding the nodes it generates to the "install" alias.

from SCons.Script import Copy, Entry, COMMAND_LINE_TARGETS
import os.path

def exists():
    return True

def generate(env):
    env.AddMethod(Install)

def Install(env, targetdir, *sources):
    def add_destdir(path):
        '''Add DESTDIR to a path.

os.path.join won't work because it doesn't join to absolute paths;
join('a', '/b') returns '/b'.  And concatenation doesn't work right
if DESTDIR lacks a trailing space.  This deals with both cases
properly.'''
        destdir = env['DESTDIR']
        if destdir == '':
            return path

        if not destdir.endswith('/'):
            destdir += '/'

        return os.path.normpath('%s%s' % (destdir, path))

    # Only set up the alias if the user asked for an install, so
    # "scons ." works to build everything with no surprises and so
    # that multiple variants can be built without trying to create
    # install targets for the same file.
    if 'install' in COMMAND_LINE_TARGETS:
        result = []
        for source in env.Flatten(sources):
            output = add_destdir(os.path.join(targetdir, os.path.basename(Entry(source).path)))
            result.append(env.Command(output,
                                      source,
                                      Copy('$TARGET', '$SOURCE')))

        env.Alias('install', result)

        return result
    else:
        return []
