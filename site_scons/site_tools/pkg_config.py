import subprocess

def exists():
    return True

def generate(env):
    env.AddMethod(GetPkgConfigFlags)

def GetPkgConfigFlags(env, *pkgs):
    """Return the cflags and libs for the given packages,
or None if one of the packages can't be found."""
    pkg_config = env.get('PKG_CONFIG', 'pkg-config')

    pipe = subprocess.Popen([pkg_config,
                             '--cflags',
                             '--libs'] + list(pkgs),
                            stdout = subprocess.PIPE)

    output = pipe.stdout.read()
    pipe.wait()

    if pipe.returncode == 0:
        return output
    else:
        return None

