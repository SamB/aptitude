def exists():
    return True

def generate(env):
    env.AddMethod(Test)


def Test(env, target, *args, **kwargs):
    '''Compile the given test and run it as part of the "test" target.
Returns the Node for the program.'''

    if 'test_target' in kwargs:
        test_target = kwargs['test_target']
        del kwargs['test_target']
    else:
        test_target = 'test'

    result = env.Program(target, *args, **kwargs)
    env.Alias(test_target, [result], result[0].abspath)
    return result


