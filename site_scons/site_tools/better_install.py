# Replacement for the built-in Install builder, supporting DESTDIR and
# automatically adding the nodes it generates to the "install" alias.

from SCons.Script import Copy, Entry, COMMAND_LINE_TARGETS
import os.path

def exists():
    return True

def generate(env):
    env.AddMethod(Install)

def Install(env, targetdir, *sources):
    # Only set up the alias if the user asked for an install, so
    # "scons ." works to build everything with no surprises and so
    # that multiple variants can be built without trying to create
    # install targets for the same file.
    if 'install' in COMMAND_LINE_TARGETS:
        result = []
        for source in env.Flatten(sources):
            output = os.path.join(targetdir, os.path.basename(Entry(source).path))
            result.append(env.Command(output,
                                      source,
                                      Copy('$TARGET', '$DESTDIR$SOURCE')))

        env.Alias('install', result)

        return result
    else:
        return []
