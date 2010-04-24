def exists():
    return True

def generate(env):
    env.AddMethod(Test)


def Test(env, target, *args, **kwargs):
    '''Compile the given test and run it as part of the "test" target.
Returns the Node for the alias.'''

    if 'test_target' in kwargs:
        test_target = kwargs['test_target']
        del kwargs['test_target']
    else:
        test_target = 'test'

    env.Program(target, *args, **kwargs)
    return env.Alias(test_target, [result], result[0].abspath)


