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

def make_env_map(vars):
    return vars

@jit.unroll_safe
def get_env_index(var_map, var):
    for i, v in enumerate(var_map):
        if v == var:
            return i
    return -1

@jit.unroll_safe
def env_lookup(env, key):
    while env is not None:
        index = get_env_index(env.var_map, key)
        if index == -1:
            env = env.prev
        else:
            return env.val_arr[index]
    raise Exception('Unbound variable ')#get symbol from interned
        
class Environment(Value):
    _immutable_ = True
    _attrs_ = ['val_arr', 'var_map', 'prev']
    _immutable_fields_ = ['val_arr[*]', 'var_map[*]', 'prev']
    def __init__(self, var_map, values, prev=None):
        self.val_arr = values
        self.var_map = var_map
        self.prev = prev

    def lookup(self, key):
        return env_lookup(self, key)

    def extend(self, key, value):
        evm = make_env_map([key])
        ne = Environment(evm, [value], self)
        return ne

class app_k(Cont):
    def __init__(self, exp, env, k):
        self.exp = exp
        self.env = env
        self.k = k
    def plug_reduce(self, v):
        return v.evaluate(self.exp, self.env, self.k)

