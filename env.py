from rpython.rlib import jit

class EnvironmentStructure(object):
    _immutable_ = True
    _attrs_ = ['elems', 'prev']
    _immutable_fields_ = ['elems[*]', 'prev']

    def __init__(self, elems, prev=None):
        self.elems = elems
        self.prev = prev

    @jit.unroll_safe
    def get_index(self, var):
        for i, v in enumerate(self.elems):
            if v == var:
                return i
        return -1

    def __str__(self):
        if self.prev is None:
            return str(map(get_ast_string, self.elems))
        else:
            return str(map(get_ast_string, self.elems)) + '->' + str(self.prev)

def create_new_env_cache(elem_len):
    class EnvironmentStructureCache(object):
        def __init__(self, elem_len):
            self._cache = {}
            self.elem_len = elem_len
        def contains(self, t):
            return t in self._cache
        def _get(self, t):
            return self._cache[t]
        def _set(self, t, v):
            self._cache[t] = v
        def get_env_s(self, t, elems, prev):
            if self.contains(t):
                return self._get(t)
            else:
                new_s = EnvironmentStructure(elems, prev)
                self._set(t, new_s)
                return new_s
    return EnvironmentStructureCache(elem_len)

_1envsc = create_new_env_cache(1)
_2envsc = create_new_env_cache(2)
_3envsc = create_new_env_cache(3)
_4envsc = create_new_env_cache(4)

@jit.elidable_promote('all')
def create_new_env_structure(elems, prev_s):
    n = len(elems)
    if n == 1:
        t = (elems[0], prev_s)
        return _1envsc.get_env_s(t, elems, prev_s)
    elif n == 2:
        t = (elems[0], elems[1], prev_s)
        return _2envsc.get_env_s(t, elems, prev_s)
    elif n == 3:
        t = (elems[0], elems[1], elems[2], prev_s)
        return _3envsc.get_env_s(t, elems, prev_s)
    elif n == 4:
        t = (elems[0], elems[1], elems[2], prev_s)
        return _4envsc.get_env_s(t, elems, prev_s)
    else:
        new_s = EnvironmentStructure(elems, prev_s)
        return new_s

class EnvironmentValues(object):
    _immutable_ = True
    _attrs_ = ['values', 'prev']
    _immutable_fields_ = ['values[*]', 'prev']
    def __init__(self, values, prev=None):
        self.values = values
        self.prev = prev

    def get_at_index(self, index):
        return self.values[index]

    def __str__(self):
        if self.prev is None:
            return str(self.values)
        else:
            return str(self.values) + '->' + str(self.prev)

def env_extend(env_s_prev, env_v_prev, vars, values):
    env_struct_prev, env_values_prev = env_s_prev, env_v_prev
    new_env_struct = create_new_env_structure(vars, env_struct_prev)
    new_env_values = EnvironmentValues(values, env_values_prev)
    return (new_env_struct, new_env_values)

def create_top_level_env(names, values):
    env_struct = EnvironmentStructure(names)
    env_values = EnvironmentValues(values)
    return (env_struct, env_values)

