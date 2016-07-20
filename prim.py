from rpython.rlib import jit

class Value(object):
    _attrs_ = []
    def evaluate(self, exp, env, k):
        raise NotImplementedError("Abstract base class")

class Cont(Value):
    _attrs_ = ['exp', 'env', 'k']
    _immutable_fields_ = ['exp', 'env', 'k']

    def plug_reduce(self, v):
        raise NotImplementedError("Abstract base class")

class Number(Value):
    _attrs = []
    _immutable_fields_ = ['number_value']

    def __init__(self, num):
        self.number_value = num

    def tostring(self):
        return str(self.number_value)

class EnvironmentStructure(object):
    _immutable_ = True
    _attrs_ = ['elems', 'prev']
    _immutable_fields_ = ['elems[*]', 'prev']

    def __init__(self, elems, prev=None):
        self.elems = elems
        self.prev = prev

    @jit.elidable
    def get_index(self, var):
        for i, v in enumerate(self.elems):
            if v == var:
                return i
        return -1
    
class EnvironmentValues(Value):
    _immutable_ = True
    _attrs_ = ['values', 'prev']
    _immutable_fields_ = ['values[*]', 'prev']
    def __init__(self, values, prev=None):
        self.values = values
        self.prev = prev

    def get_at_index(self, index):
        return self.values[index]

@jit.unroll_safe
def env_lookup(env, key):
    env_struct, env_values = env
    while env_struct is not None:
        index = env_struct.get_index(key)
        if index == -1:
            env_struct = env_struct.prev
            env_values = env_values.prev
        else:
            return env_values.get_at_index(index)
    raise Exception('unbound variable')

def env_extend(env, vars, values):
    env_struct_prev, env_values_prev = env
    new_env_struct = EnvironmentStructure(vars, env_struct_prev)
    new_env_values = EnvironmentValues(values, env_values_prev)
    return (new_env_struct, new_env_values)

def create_top_level_env(names, values):
    env_struct = EnvironmentStructure(names)
    env_values = EnvironmentValues(values)
    return (env_struct, env_values)

class app_k(Cont):
    def __init__(self, exp, env, k):
        self.exp = exp
        self.env = env
        self.k = k
    def plug_reduce(self, v):
        return v.evaluate(self.exp, self.env, self.k)

