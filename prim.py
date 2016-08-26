from rpython.rlib import jit

class State(Exception):
    _attrs_ = []
    pass
class EvalExp(State):
    _attrs_ = _immutable_fields_ = ['t']
    def __init__(self, t):
        self.t = t

class PlugReduce(State):
    _attrs_ = _immutable_fields_ = ['t']
    def __init__(self, t):
        self.t = t

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
            return str(map(get_ast_string, self.elems))
        else:
            return str(map(get_ast_string, self.elems)) + '->' + str(self.prev)

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
            return str(self.values)
        else:
            return str(self.values) + '->' + str(self.prev)


@jit.unroll_safe
def env_lookup(env_struct, env_values, key):
    while env_struct is not None:
        index = env_struct.get_index(key)
        if index == -1:
            env_struct = env_struct.prev
            env_values = env_values.prev
        else:
            return env_values.get_at_index(index)
    
    raise Exception('unbound variable', get_ast_string(key))

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

def get_ast_string(string_value):
    return global_symbol_table.num_to_symbol[string_value]

def get_string_value(s_ast):
    return s_ast.string_value

def get_symbol_ast(symbol):
    return global_symbol_table.make_symbol_ast(symbol)

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

@jit.unroll_safe
def find_env_in_chain_speculate(target_env_structure, target_env_values, env_structure, env_values):
    """
    finds the Environment Structure of shape target_env_structure in env_structure
    and returns the Environment Values for that environment structure. Used to tell jit that
    the environment closure is evaluating can be get from the environment where closure was
    constructed, which happens most of the time.
    """
    jit.promote(target_env_structure)
    jit.promote(env_structure)
    while env_structure is not None:
        if env_structure is target_env_structure:
            if env_values is target_env_values:
                return env_values
        env_values = env_values.prev
        env_structure = env_structure.prev
    return target_env_values

def build_prim_eval_cont(exp_array, exp_offset, value_array,
                         current_index, end_index, env_s, env_v, k):
    #we are using the same value array across the whole evaluation of arguments. 
    #If the continuation gets captured and is used in a weird way could cause problems.
    i = current_index
    while i < end_index:
        e = exp_array[i+exp_offset]
        if can_simple_eval(e):
            v = e.simple_eval(env_s, env_v)
            value_array[i] = v
            i+=1
        else:
            break
    if i == end_index:
        return k.plug_reduce(MultiValue(value_array))
    elif i == end_index-1:
        return (exp_array[i+exp_offset], env_s, env_v, _prim_n_end(value_array, i, k))
    else:
        return (exp_array[i+exp_offset], env_s, env_v,
                           _prim_n(exp_array, exp_offset, value_array,
                                   i, end_index, env_s, env_v, k))
class _prim_n(Cont):
    _immutable_fields_ = ['exp_array[*]', 'exp_offset', 
                          'current_index', 'end_index', 'env_s', 'env_v', 'k']
    def __init__(self, exp_array, exp_offset,
                       value_array, current_index, end_index,
                       env_s, env_v, k):
        self.exp_array = exp_array
        self.exp_offset = exp_offset
        self.value_array = value_array
        self.current_index = current_index
        self.end_index = end_index
        self.env_s = env_s
        self.env_v = env_v
        self.k = k

    def plug_reduce(self, v):
        i = self.current_index
        self.value_array[i] = v
        return build_prim_eval_cont(self.exp_array, self.exp_offset,
                                    self.value_array,
                                    i+1, self.end_index, self.env_s, self.env_v, self.k)

class _prim_n_end(Cont):
    def __init__(self, val_array, index, k):
        self.value_array = val_array
        self.current_index = index
        self.k = k
    def plug_reduce(self, v):
        self.value_array[self.current_index] = v
        return self.k.plug_reduce(MultiValue(self.value_array))

class LambdaAST(AST):
    def __init__(self, vars, body):
        self.vars = vars
        self.body = body
        self.body.should_enter = True
        self.should_enter = False
        self.top_env_s = [var.string_value for var in vars]
    def eval(self, env_s, env_v, k):
        return k.plug_reduce(Closure(self, env_s, env_v))
def indentity(x):
    return x

class Closure(Value):
    def __init__(self, lambda_ast, env_s, env_v, fix=0):
        assert isinstance(lambda_ast, LambdaAST)
        self.lambda_ast = lambda_ast
        if fix == 0:
            self.env_s = env_s
            self.env_v = env_v
        else:
            self.env_s = create_new_env_structure([fix], env_s)
            self.env_v = EnvironmentValues([self], env_v)
    def evaluate(self, exp, env_s, env_v, k):
        vals = [None]*(len(exp)-1)
        new_env_s = create_new_env_structure(self.lambda_ast.top_env_s, self.env_s)
        prev_env_v = find_env_in_chain_speculate(new_env_s.prev, self.env_v, env_s, env_v)

        return build_prim_eval_cont(exp, 1, vals,
                                    0, len(exp)-1,
                                    env_s, env_v,
                                    closure_k(self, k, new_env_s, prev_env_v))
class closure_k(Cont):
    def __init__(self, closure, k, env_s, prev_env_v):
        assert isinstance(closure, Closure)
        self.closure = closure
        self.k = k
        self.env_s = env_s
        self.prev_env_v = prev_env_v
    def plug_reduce(self, v):
        assert isinstance(v, MultiValue)
        return (self.closure.lambda_ast.body,
                self.env_s, EnvironmentValues(v.values, self.prev_env_v),
                self.k)

class LetAST(AST):
    def __init__(self, vars, var_vals, body):
        self.vars = vars
        self.var_vals = var_vals
        self.body = body
        self.should_enter = False
        self.top_env_s = [var.string_value for var in vars]
    def eval(self, env_s, env_v, k):
        vals = [None]*len(self.vars)
        return build_prim_eval_cont(self.var_vals, 0, vals,
                                    0, len(self.var_vals),
                                    env_s, env_v,
                                    let_k(self, env_s, env_v, k))

class let_k(Cont):
    def __init__(self, let_ast, env_s, env_v, k):
        self.let_ast = let_ast
        self.env_s = env_s
        self.env_v = env_v
        self.k = k
    def plug_reduce(self, v):
        assert isinstance(v, MultiValue)
        new_env_s = create_new_env_structure(self.let_ast.top_env_s, self.env_s)
        return (self.let_ast.body, new_env_s, EnvironmentValues(v.values, self.env_v), self.k)

class LetrecAST(AST):
    def __init__(self, var, var_val, body):
        self.var = var
        self.var_val = var_val
        self.body = body
        self.should_enter = False
        self.top_env_s = [var.string_value]
    def eval(self, env_s, env_v, k):
        return (self.var_val, env_s, env_v, letrec_k(self, env_s, env_v, k))

class letrec_k(Cont):
    def __init__(self, letrec_ast, env_s, env_v, k):
        self.letrec_ast = letrec_ast
        self.env_s = env_s
        self.env_v = env_v
        self.k = k
    def plug_reduce(self, v):
        assert isinstance(v, Closure)
        new_c = Closure(v.lambda_ast, v.env_s, v.env_v, self.letrec_ast.var.string_value)
        #FIXME need separate env for letrec if the closure comes from inside some other expression
        return self.letrec_ast.body, new_c.env_s, new_c.env_v, self.k

class Bool(Value):
    pass

true = Bool()
false = Bool()
class Void(Value):
    pass
void = Void()
nil = None

class IfAST(AST):
    def __init__(self, ch, el, th):
        self.ch = ch
        self.el = el
        self.th = th
        self.should_enter = False
    def eval(self, env_s, env_v, k):
        return (self.ch, env_s, env_v, if_k(self, env_s, env_v, k))
class if_k(Cont):
    def __init__(self, if_ast, env_s, env_v, k):
        self.if_ast = if_ast
        self.env_s = env_s
        self.env_v = env_v
        self.k = k
    def plug_reduce(self, v):
        if v == true:
            return (self.if_ast.th, self.env_s, self.env_v, self.k)
        else:
            return (self.if_ast.el, self.env_s, self.env_v, self.k)

class BeginAST(AST):
    def __init__(self, exps):
        self.exps = exps
    def eval(self, env_s, env_v, k):
        return (self.exps[0], env_s, env_v, begin_k(self.exps, 1, len(self.exps), env_s, env_v, k))

class begin_k(Cont):
    def __init__(self, exps, index, end_index, env_s, env_v, k):
        self.exps = exps
        self.index = index
        self.end_index = end_index
        self.env_s = env_s
        self.env_v = env_v
        self.k = k
    def plug_reduce(self, v):
        if self.index == self.end_index:
            return k.plug_reduce(v)
        else:
            return (self.exps[self.index],
                    env_s, env_v,
                    begin_k(self.exps, self.index+1, self.end_index, self.env_s, self.env_v, k))

def can_simple_eval(exp):
    return isinstance(exp, SymbolAST) or isinstance(exp, NumberAST)

def simple_interpret(exp, env_s, env_v):
    return env_lookup(env_s, env_v, exp.string_value)

