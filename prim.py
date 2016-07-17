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

class Environment1(Value):
    _immutable_ = True
    _immutable_fields_ = ['val', 'key', 'prev']
    def __init__(self, var, val, prev):

        self.key = var
        self.val = val
        self.prev = prev

    def lookup(self, key):
        if key == self.key:
            return self.val
        else:
            return self.prev.lookup(key)

    def extend(self, key, value):
        ne = Environment1(key, val, self)
        return ne

class Environment(Value):
    _immutable_ = True
    _attrs_ = ['val_arr', 'var_map', 'prev']
    _immutable_fields_ = ['val_arr[*]', 'var_map[*]', 'prev']
    def __init__(self, var_map, values, prev=None):
        self.val_arr = values
        self.var_map = var_map
        self.prev = prev

    def get_index(self, var):
        for i, v in enumerate(self.var_map):
            if v == var:
                return i
        return -1

    @jit.unroll_safe
    def lookup(self, key):
        for i, v in enumerate(self.var_map):
            if v == key:
                return self.val_arr[i]
        return self.prev.lookup(key)

@jit.elidable
def get_topenv_index(var_map, key):
    return var_map.get(key, -1)

class TopLevelEnvironment(Value):
    _immutable_fields_ = ['val_arr[*]', 'var_map']
    def __init__(self, names, values):
        self.var_map = {}
        for i, v in enumerate(names):
            self.var_map[v] = i
        self.val_arr = values

    @jit.elidable
    def lookup(self, key):
        index = get_topenv_index(self.var_map, key)
        if index == -1:
            raise Exception('variable not found ')
        else:
            return self.val_arr[index]

class app_k(Cont):
    def __init__(self, exp, env, k):
        self.exp = exp
        self.env = env
        self.k = k
    def plug_reduce(self, v):
        return v.evaluate(self.exp, self.env, self.k)

