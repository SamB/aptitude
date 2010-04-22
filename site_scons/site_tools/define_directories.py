import aptitude_build

from SCons.Script import *

def exists(env):
    return True

def generate(env):
    env.AddMethod(DefineDirectory)

def DefineDirectory(env, key, default = None, help = None):
    """Define a new directory variable.

\"key\" is the name of the new variable; for instance, \"prefix\" or
\"datadir\".  A command-line option \"--key\" will be created to
define a new value for the variable, a build environment variable $KEY
will be created to allow it to be substituted in various places, and a
config.h symbol KEY will be defined to allow it to be referenced from
C++ code.  SConscript files can import the variable under the name
\"key\"."""

    while key[:2] == '--':
        key = key[2:]

    lowercase_key = key.lower()
    uppercase_key = key.upper()
    del key # Avoid accidentally referencing it.

    AddOption('--%s' % lowercase_key,
              dest = lowercase_key,
              nargs = 1,
              type = 'string',
              action = 'store',
              metavar = 'DIR',
              default = default,
              help = help or 'set %s' % lowercase_key)

    env.Replace(**{uppercase_key: GetOption(lowercase_key)})

    def finish(configure):
        value = configure.env.subst('$%s' % uppercase_key)
        Export({lowercase_key : value})
        configure.Define(uppercase_key, '"%s"' % value)

    aptitude_build.AddConfigureFinishHook(finish)
