from rpython.rlib import jit

class Value(object):
    _attrs_ = []
    def evaluate(self, exp, env_s , env_v, k):
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

class MultiValue(Value):
    _attrs_ = ['values']
    _immutable_fields_ = ['values[*]']
    def __init__(self, values):
        self.values = values

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

    def __str__(self):
        if self.prev is None:
            return str(self.elems)
        else:
            return str(self.elems) + '->' + str(self.prev)
class EnvironmentValue(Value):
    _attrs_ = _immutable_fields_ = ['env_v', 'env_s']
    def __init__(self, env_s, env_v):
        self.env_s = env_s
        self.env_v = env_v

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

class EnvironmentValues(Value):
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
            return str(['*' for v in self.values])
        else:
            return str(['*' for v in self.values]) + '->' + str(self.prev)


@jit.unroll_safe
def env_lookup(env_struct, env_values, key):
    while env_struct is not None:
        index = env_struct.get_index(key)
        if index == -1:
            env_struct = env_struct.prev
            env_values = env_values.prev
        else:
            return env_values.get_at_index(index)
    raise Exception('unbound variable')

def env_extend(env_s_prev, env_v_prev, vars, values):
    env_struct_prev, env_values_prev = env_s_prev, env_v_prev
    new_env_struct = create_new_env_structure(vars, env_struct_prev)
    new_env_values = EnvironmentValues(values, env_values_prev)
    return (new_env_struct, new_env_values)

def create_top_level_env(names, values):
    env_struct = EnvironmentStructure(names)
    env_values = EnvironmentValues(values)
    return (env_struct, env_values)

class app_k(Cont):
    def __init__(self, exp, env_s, env_v, k):
        self.exp = exp
        self.env_s = env_s
        self.env_v = env_v
        self.k = k
    def plug_reduce(self, v):
        return v.evaluate(self.exp, self.env_s, self.env_v, self.k)


##AST
class AST(object):
    _attrs_ = ['should_enter', 'string_value']
    #TODO string_value should not be here but rpython doesn't believe my isinstance checks
    _immutable_fields_ = ['should_enter', 'string_value']
    pass

class SymbolAST(AST):
    _attrs_ = ['string_value', 'should_enter']
    _immutable_fields_ = ['string_value', 'should_enter']
    def __init__(self, symbol_str):
        self.string_value = symbol_str
        self.should_enter = False
    def tostring(self):
        return global_symbol_table.num_to_symbol[self.string_value]

    def eval(self, env_s, env_v, k):
        return k.plug_reduce(env_lookup(env_s, env_v, self.string_value))

    def simple_eval(self, env_s, env_v):
        return env_lookup(env_s, env_v, self.string_value)

class SymbolTable(object):
    def __init__(self):
        self.symbol_to_ast = {}
        self.num_to_symbol = {}
        self.count = 1

    def make_symbol_ast(self, symbol):
        if symbol in self.symbol_to_ast:
            return self.symbol_to_ast[symbol]
        else:
            ast = SymbolAST(self.count)
            self.symbol_to_ast[symbol] = ast
            self.num_to_symbol[self.count] = symbol
            self.count += 1
            return ast

global_symbol_table = SymbolTable()

def get_string_value(s_ast):
    return s_ast.string_value

class NumberAST(AST):
    _attrs_ = ['number_value', 'should_enter']
    _immutable_fields_ = ['number_value', 'should_enter']
    def __init__(self, num):
        self.number_value = Number(num)
        self.should_enter = False
    def tostring(self):
        return str(self.number_value.number_value)
    def eval(self, env_s, env_v, k):
        return k.plug_reduce(self.number_value)
    def simple_eval(self, env_s, env_v):
        return self.number_value

def simple_sexp_eval(self, env_s, env_v, k):
    ev = env_lookup(env_s, env_v, self.children[0].string_value)
    return ev.evaluate(self.children, env_s, env_v, k)

def complex_sexp_eval(self, env_s, env_v, k):
    return self.children[0], env_s, env_v, app_k(self.children, env_s, env_v, k)

class SexpAST(AST):
    _attrs_ = ['children', '_eval', 'should_enter']
    _immutable_fields_ = ['children[*]', '_eval', 'should_enter']

    def __init__(self, children, captured_env=False):
        self.children = children
        self.should_enter = False

        if type(self.children[0]) is SymbolAST:
            self._eval = simple_sexp_eval
        else:
            self._eval = complex_sexp_eval

    def __getitem__(self, key):
        return self.children[key]

    def tostring(self):
        return '(' + ' '.join([ast.tostring() for ast in self.children]) + ')'

    def eval(self, env_s, env_v, k):
        return self._eval(self, env_s, env_v, k)

class PrimSexpAST(AST):
    _attrs_ = ['children', 'should_enter', '_evaluator']
    _immutable_fields_ = ['children[*]', '_evaluator', 'should_enter']
    def __init__(self, children, init_env):
        self.children = children
        self.should_enter = False
        self._evaluator = env_lookup(init_env[0], init_env[1], self.children[0].string_value)

    def __getitem__(self, key):
        return self.children[key]

    def tostring(self):
        return '(' + ' '.join([ast.tostring() for ast in self.children]) + ')'

    def eval(self, env_s, env_v, k):
        return self._evaluator.evaluate(self.children, env_s, env_v, k)

class PrimFunc1AST(AST):
    _attrs_ = _immutable_fields_ = ['func']
    def __init__(self, func):
        self.func = func

    def eval(self, env_s, env_v, k):
        args = env_v.values
        return k.plug_reduce(self.func(args[0]))

class PrimFunc2AST(AST):
    _attrs_ = _immutable_fields_ = ['func']
    def __init__(self, func):
        self.func = func

    def eval(self, env_s, env_v, k):
        args = env_v.values
        return k.plug_reduce(self.func(args[0], args[1]))
