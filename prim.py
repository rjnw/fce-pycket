from rpython.rlib import jit
from env import *

class State(object):
    _attrs_ = []
    pass

class ExpState(State):
    _attrs_ = _immutable_fields_ = ['exp', 'env_s', 'env_v', 'k']
    def __init__(self, exp, env_s, env_v, k):
        self.exp = exp
        self.env_s = env_s
        self.env_v = env_v
        self.k = k
    def get_jit_vars(self):
        return (self.exp, self.env_s, self.env_v, self.k)
    def should_enter(self):
        return self.exp.should_enter
    def merge_point(self, jitdriver):
        jitdriver.jit_merge_point(exp=self.exp,
                                  env_struct=self.env_s, env_values=self.env_v,
                                  k=self.k, state=self)
    def enter_jit(self, jitdriver):
        if self.exp.should_enter:
            jitdriver.can_enter_jit(exp=self.exp,
                                    env_struct=self.env_s, env_values=self.env_v,
                                    k=self.k, state=self)
    def step(self):
        return self.exp.eval(self.env_s, self.env_v, self.k)

class ContState(State):
    _attrs_ = _immutable_fields_ = ['k', 'v']
    def __init__(self, k, v):
        self.k = k
        self.v = v
    def get_jit_vars(self):
        return (self.k.exp, self.k.env_s, self.k.env_v, self.k.k)
    def should_enter(self):
        return True
    def merge_point(self, jitdriver):
        jitdriver.jit_merge_point(exp=self.k.exp,
                                  env_struct=self.k.env_s, env_values=self.k.env_v,
                                  k=self.k.k, state=self)
    def enter_jit(self, jitdriver):
        jitdriver.can_enter_jit(exp=self.k.exp,
                                env_struct=self.k.env_s, env_values=self.k.env_v,
                                k=self.k.k, state=self)
    def step(self):
        return self.k.k.plug_reduce(self.v)

class Value(object):
    _attrs_ = []
    def evaluate(self, exp, env_s , env_v, k):
        raise NotImplementedError("Abstract base class")

class Cont(Value):
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

class EnvironmentValue(Value):
    _attrs_ = _immutable_fields_ = ['env_v', 'env_s']
    def __init__(self, env_s, env_v):
        self.env_s = env_s
        self.env_v = env_v

@jit.unroll_safe
def env_lookup(env_struct, env_values, key):
    #print 'lookup', key, env_struct, env_values
    while env_struct is not None:
        index = env_struct.get_index(key)
        if index == -1:
            env_struct = env_struct.prev
            env_values = env_values.prev
        else:
            return env_values.get_at_index(index)
    
    raise Exception('unbound variable', get_ast_string(key))

class app_k(Cont):
    def __init__(self, exp, env_s, env_v, k):
        self.exps = exp
        self.env_s = env_s
        self.env_v = env_v
        self.k = k
    def plug_reduce(self, v):
        return v.evaluate(self.exps, self.env_s, self.env_v, self.k)


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
    return ExpState(self.children[0], env_s, env_v, app_k(self.children, env_s, env_v, k))

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

@jit.unroll_safe
def build_prim_eval_cont_with_store(prim_store, current_index):
    ps = prim_store
    i = current_index
    while i < ps.end_index:
        e = ps.exp_array[i+ps.exp_offset]
        if can_simple_eval(e):
            v = e.simple_eval(ps.env_s, ps.env_v)
            ps.value_array[i] = v
            i+=1
        else:
            break
    if i == ps.end_index:
        return ps.k.plug_reduce(MultiValue(ps.value_array))
        #return ps.k.plug_reduce(MultiValue(ps.value_array))
    elif i == ps.end_index-1:
        return ExpState(ps.exp_array[i+ps.exp_offset],
                        ps.env_s, ps.env_v,
                        _prim_n_end(ps.value_array, i, ps.k))
    else:
        return ExpState(ps.exp_array[i+ps.exp_offset],
                        ps.env_s, ps.env_v,
                        _prim_n(ps, i))

@jit.unroll_safe    
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
        #return raise_plug(k, MultiValue(value_array))
    elif i == end_index-1:
        return ExpState(exp_array[i+exp_offset], env_s, env_v, _prim_n_end(value_array, i, k))
    else:
        prim_store = _prim_n_store(exp_array, exp_offset, value_array, end_index, env_s, env_v, k)
        return ExpState(exp_array[i+exp_offset], env_s, env_v,
                        _prim_n(prim_store, i))
    
class _prim_n_store(object):
    _immutable_fields_ = ['exp_array[*]', 'exp_offset', 
                          'end_index', 'env_s', 'env_v', 'k']
    def __init__(self, exp_array, exp_offset, value_array, end_index, env_s, env_v, k):
        self.exp_array = exp_array
        self.exp_offset = exp_offset
        self.value_array = value_array
        self.end_index = end_index
        self.env_s = env_s
        self.env_v = env_v
        self.k = k
        
class _prim_n(Cont):
    _immutable_fields_ = ['prim_store', 'current_index']
    def __init__(self, prim_store, current_index):
        self.current_index = current_index
        self.prim_store = prim_store

    def plug_reduce(self, v):
        i = self.current_index
        self.prim_store.value_array[i] = v
        return build_prim_eval_cont_with_store(self.prim_store, i+1)

class _prim_n_end(Cont):
    _immutable_fields_ = ['current_index', 'k']
    def __init__(self, val_array, index, k):
        self.value_array = val_array
        self.current_index = index
        self.k = k
    def plug_reduce(self, v):
        self.value_array[self.current_index] = v
        return self.k.plug_reduce(MultiValue(self.value_array))
        #return self.k.plug_reduce(MultiValue(self.value_array))

class LambdaAST(AST):
    _immutable_fields_ = ['vars[*]', 'body', 'should_enter', 'top_env_s[*]']
    def __init__(self, vars, body):
        self.vars = vars
        self.body = body
        self.body.should_enter = True
        self.should_enter = False
        self.top_env_s = [var.string_value for var in vars]
    def eval(self, env_s, env_v, k):
        return k.plug_reduce(Closure(self, env_s, env_v))

    def tostring(self):
        return '(lambda ('+' '.join([v.tostring() for v in self.vars])+') '+self.body.tostring()+')'

def indentity(x):
    return x

class Closure(Value):
    _immutable_fields_ = ['lambda_ast', 'env_s', 'env_v']
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
        jit.promote(new_env_s)
        prim_store = _prim_n_store(exp, 1, vals, len(exp)-1,
                                   env_s, env_v, closure_k(self, k, new_env_s, prev_env_v))
        return build_prim_eval_cont(exp, 1, vals,
                                    0, len(exp)-1,
                                    env_s, env_v,
                                    closure_k(self, k, new_env_s, prev_env_v))
class closure_k(Cont):
    _immutable_fields_ = ['closure', 'k', 'env_s', 'prev_env_v']
    def __init__(self, closure, k, env_s, prev_env_v):
        assert isinstance(closure, Closure)
        self.closure = closure
        self.k = k
        self.env_s = env_s
        jit.promote(self.env_s)
        self.prev_env_v = prev_env_v
        self.plug_loop = False
    def plug_reduce(self, v):
        assert isinstance(v, MultiValue)
        return ExpState(self.closure.lambda_ast.body,
                        self.env_s, EnvironmentValues(v.values, self.prev_env_v),
                        self.k)

class LetAST(AST):
    _immutable_fields_ = ['vars[*]', 'var_vals[*]', 'body', 'should_enter', 'top_env_s[*]']
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
    def tostring(self):
        return '(let '+'v: '+\
            ','.join([v.tostring() for v in self.vars])+\
            'vals:'+','.join([v.tostring() for v in self.var_vals])+\
            self.body.tostring()+')'

class let_k(Cont):
    _immutable_fields_ = ['let_ast', 'env_s', 'env_v', 'k']
    def __init__(self, let_ast, env_s, env_v, k):
        self.let_ast = let_ast
        self.env_s = env_s
        self.env_v = env_v
        self.k = k
        self.plug_loop = False
    def plug_reduce(self, v):
        assert isinstance(v, MultiValue)
        new_env_s = create_new_env_structure(self.let_ast.top_env_s, self.env_s)
        jit.promote(new_env_s)
        return ExpState(self.let_ast.body,
                        new_env_s, EnvironmentValues(v.values, self.env_v),
                        self.k)

class LetrecAST(AST):
    _immutable_fields_ = ['var', 'var_val', 'body', 'should_enter', 'top_env_s[*]']
    def __init__(self, var, var_val, body):
        self.var = var
        self.var_val = var_val
        self.body = body
        self.should_enter = False
        self.top_env_s = [var.string_value]
    def eval(self, env_s, env_v, k):
        return ExpState(self.var_val, env_s, env_v, letrec_k(self, env_s, env_v, k))
    def tostring(self):
        return 'letrec'

class letrec_k(Cont):
    _immutable_fields_ = ['letrec_ast', 'env_s', 'env_v', 'k']
    def __init__(self, letrec_ast, env_s, env_v, k):
        self.letrec_ast = letrec_ast
        self.env_s = env_s
        self.env_v = env_v
        self.k = k
        self.plug_loop = False
    def plug_reduce(self, v):
        assert isinstance(v, Closure)
        new_c = Closure(v.lambda_ast, v.env_s, v.env_v, self.letrec_ast.var.string_value)
        #FIXME need separate env for letrec if the closure comes from inside some other expression
        return ExpState(self.letrec_ast.body, new_c.env_s, new_c.env_v, self.k)

class Bool(Value):
    pass

true = Bool()
false = Bool()
class Void(Value):
    pass
void = Void()
nil = None

class IfAST(AST):
    _immutable_fields_ =['ch', 'th', 'el']
    def __init__(self, ch, th, el):
        self.ch = ch
        self.th = th
        self.el = el
        self.should_enter = False
    def eval(self, env_s, env_v, k):
        return ExpState(self.ch, env_s, env_v, if_k(self, env_s, env_v, k))
    def tostring(self):
        return '(if '+self.ch.tostring()+' '+self.th.tostring()+' '+self.el.tostring()+')'

class if_k(Cont):
    _immutable_fields_ = ['if_ast', 'env_s', 'env_v', 'k']
    def __init__(self, if_ast, env_s, env_v, k):
        self.if_ast = if_ast
        self.env_s = env_s
        self.env_v = env_v
        self.k = k
        self.plug_loop = False
    def plug_reduce(self, v):
        if v == true:
            return ExpState(self.if_ast.th, self.env_s, self.env_v, self.k)
        else:
            return ExpState(self.if_ast.el, self.env_s, self.env_v, self.k)

class BeginAST(AST):
    _immutable_fields_ = ['exps[*]']
    def __init__(self, exps):
        self.exps = exps
        self.should_enter = False
    def eval(self, env_s, env_v, k):
        return ExpState(self.exps[0], env_s, env_v,
                        begin_k(self.exps, 1, len(self.exps), env_s, env_v, k))
    def tostring(self):
        return '(begin '+' '.join([b.tostring() for b in self.exps])+')'

class begin_k(Cont):
    _immutable_fields_ = ['exps[*]', 'index', 'end_index', 'env_s', 'env_v', 'k']
    def __init__(self, exps, index, end_index, env_s, env_v, k):
        self.exps = exps
        self.index = index
        self.end_index = end_index
        self.env_s = env_s
        self.env_v = env_v
        self.k = k
        self.plug_loop = False
    def plug_reduce(self, v):
        if self.index == self.end_index:
            return self.k.plug_reduce(v)
        else:
            return ExpState(self.exps[self.index],
                            self.env_s, self.env_v,
                            begin_k(self.exps, self.index+1,
                                    self.end_index, self.env_s, self.env_v, self.k))

def can_simple_eval(exp):
    return isinstance(exp, SymbolAST) or isinstance(exp, NumberAST)

def simple_interpret(exp, env_s, env_v):
    return env_lookup(env_s, env_v, exp.string_value)
