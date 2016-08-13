#! /usr/bin/env python
# -*- coding: utf-8 -*-
#

from prim import *
from ast import *
from rpython.rlib import streamio, jit
from rpython.rlib.objectmodel import specialize
import time
from ast import global_symbol_table, get_string_value

def can_simple_eval(exp):
    return isinstance(exp, SymbolAST) or isinstance(exp, NumberAST)

def simple_interpret(exp, env_s, env_v):
    return env_lookup(env_s, env_v, exp.string_value)

class _prim_n(Cont):
    _immutable_fields_ = ['exp_array[*]', 'exp_offset', 'exp_getter', 'current_index', 'end_index', 'env_s', 'env_v', 'k']
    def __init__(self, exp_array, exp_offset, exp_getter,
                       value_array, current_index, end_index,
                       env_s, env_v, k):
        self.exp_array = exp_array
        self.exp_offset = exp_offset
        self.exp_getter = exp_getter
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
                                    self.exp_getter, self.value_array,
                                    i+1, self.end_index, self.env_s, self.env_v, self.k)

class _prim_n_end(Cont):
    def __init__(self, val_array, index, k):
        self.value_array = val_array
        self.current_index = index
        self.k = k
    def plug_reduce(self, v):
        self.value_array[self.current_index] = v
        return self.k.plug_reduce(MultiValue(self.value_array))

@jit.unroll_safe
def build_prim_eval_cont(exp_array, exp_offset, exp_getter, value_array,
                         current_index, end_index, env_s, env_v, k):
    i = current_index
    while i <= end_index:
        e = exp_getter(exp_array[i+exp_offset])
        if can_simple_eval(e):
            v = e.simple_eval(env_s, env_v)
            value_array[i] = v
            i+=1
        else:
            break
    if i > end_index:
        return k.plug_reduce(MultiValue(value_array))
    elif i == end_index:
        return (exp_getter(exp_array[i+exp_offset]), env_s, env_v, _prim_n_end(value_array, i, k))
    else:
        return (exp_getter(exp_array[i+exp_offset]), env_s, env_v,
                           _prim_n(exp_array, exp_offset, exp_getter, value_array,
                                   i, end_index, env_s, env_v, k))

def prim_one_arg(func):
    class prim_k(Cont):
        def __init__(self, env_s, env_v, k):
            self.k = k
        def plug_reduce(self, v):
            return self.k.plug_reduce(func(v))

    class prim_eval(Value):
        def evaluate(self, exp, env_s, env_v, k):
            if isinstance(exp[1], SymbolAST):
                v = simple_interpret(exp[1], env_s, env_v)
                return k.plug_reduce(func(v))
            else:
                return (exp[1], env_s, env_v, prim_k(env_s, env_v, k))
    return prim_eval

def prim_two_arg(func):
    class prim_k1(Cont):
        _attrs_ = ['exp', 'env_s', 'env_v', 'k']
        _immutable_fields_ = ['exp', 'env_s', 'env_v', 'k']
        def __init__(self, exp2, env_s, env_v, k):
            self.exp = exp2
            self.env_s = env_s
            self.env_v = env_v
            self.k = k
        def plug_reduce(self, v):
            if isinstance(self.exp, SymbolAST):
                v2 = simple_interpret(self.exp, self.env_s, self.env_v)
                return self.k.plug_reduce(func(v, v2))
            else:
                return (self.exp, self.env_s, self.env_v,
                        prim_k2(v, self.env_s, self.env_v, self.k))

    class prim_k2(Cont):
        _attrs_ = ['v1', 'k']
        _immutable_fields_ = ['v1', 'k']
        def __init__(self, v1, env_s, env_v, k):
            self.v1 = v1
            self.k = k
        def plug_reduce(self, v):
            return self.k.plug_reduce(func(self.v1, v))

    class prim_eval(Value):
        def evaluate(self, exp, env_s, env_v, k):
            if isinstance(exp[1], SymbolAST) and isinstance(exp[2], SymbolAST):
                v1 = simple_interpret(exp[1], env_s, env_v)
                v2 = simple_interpret(exp[2], env_s, env_v)
                return k.plug_reduce(func(v1, v2))
            elif isinstance(exp[1], SymbolAST):
                v = simple_interpret(exp[1], env_s, env_v)
                return (exp[2], env_s, env_v, prim_k2(v, env_s, env_v, k))
            else:
                return (exp[1], env_s, env_v, prim_k1(exp[2], env_s, env_v, k))
    return prim_eval

class Define(Value):
    def evaluate(self, exp, env_s, env_v, k):
        body = exp[2]
        arg = exp[1]
        assert isinstance(arg, SymbolAST)
        return (body, env_s, env_v, define_k(env_s, env_v, arg, k))

class define_k(Cont):
    def __init__(self, arg, env_s, env_v, k):
        self.arg = arg
        self.env_s = env_s
        self.env_v = env_v
        self.k = k
    def plug_reduce(self, v):
        e = env_extend(self.env_s, self.env_v, [self.arg.string_value], [v])
        return k.plug_reduce(e)

class Lambda(Value):
    def evaluate(self, exp, env_s, env_v, k):
        args = exp[1]
        body = exp[2]
        body.should_enter = True
        return k.plug_reduce(Closure(body, args, env_s, env_v))

class Closure(Value):
    _attrs_ = ['body', 'args', 'env_s', 'env_v', 'new_env_s']
    _immutable_fields_ = ['body', 'args', 'env_s', 'env_v', 'new_env_s']
    def __init__(self, body, args, env_s, env_v, fix=0):
        self.body = body
        self.args = args
        if fix is not 0:
            env_s, env_v = env_extend(env_s, env_v, [fix], [self])
        self.env_s = env_s
        self.env_v = env_v
        if isinstance(args, SexpAST):
            self.new_env_s = EnvironmentStructure([var.string_value for var in args.children],
                                                   self.env_s)
        else:
            raise Exception('list argument not implemented')

    def evaluate(self, exp, env_s, env_v, k):
        ck_prev_env_v = find_env_in_chain_speculate(self.new_env_s.prev, self.env_v,
                                                    env_s, env_v)
        jit.promote(self.new_env_s)
        # return eval_arg_then_apply(exp, 1, closure_exp_get, env_s, env_v,
        #                            closure_k(self, ck_prev_env_v, k))

        arg_len = len(exp) - 1
        vals = [None]* arg_len
        return build_prim_eval_cont(exp, 1, closure_exp_get, vals,
                                    0, arg_len-1, env_s, env_v, closure_k(self, ck_prev_env_v, k))

def closure_exp_get(x):
    return x

@jit.unroll_safe
def find_env_in_chain_speculate(target_env_structure, target_env_values, env_structure, env_values):
    jit.promote(target_env_structure)
    jit.promote(env_structure)
    while env_structure is not None:
        if env_structure is target_env_structure:
            if env_values is target_env_values:
                return env_values
        env_values = env_values.prev
        env_structure = env_structure.prev
    return target_env_values

class closure_k(Cont):
    _attrs_ = ['clos', 'k', 'prev_env_v']
    _immutable_fields_ = ['clos', 'k', 'prev_env_v']
    def __init__(self, clos, prev_env_v, k):
        assert isinstance(clos, Closure)
        self.clos = clos
        self.prev_env_v = prev_env_v
        self.k = k
    def plug_reduce(self, v):
        assert isinstance(v, MultiValue)
        return (self.clos.body,
                self.clos.new_env_s,
                EnvironmentValues(v.values, self.prev_env_v),
                self.k)



class CapturedCont(Value):
    def __init__(self, k):
        self.k = k
    def evaluate(self, exp, env_s, env_v, k):
        return (exp[1], env_s, env_v, self.k)

class callcc_k(Cont):
    def __init__(self, env_s, env_v, k):
        self.env_s = env_s
        self.env_v = env_v
        self.k = k

    def plug_reduce(self, v):
        ck = closure_k(v, self.env_v, self.k) #TODO fix closure_k now takes environment structure
        return ck.plug_reduce(CapturedCont(self.k))

class Callcc(Value):
    def evaluate(self, exp, env_s, env_v, k):
        arg = exp[1]
        return (arg, env_s, env_v, callcc_k(env_s, env_v, k))

# class CaptEnv(Value):
#     def evaluate(self, exp, env_s, env_v, k):
#         return k.plug_reduce(env)

# class withenv_k(Cont):
#     def __init__(self, eval_exp, env_s, env_v, k):
#         self.exp = exp
#         self.env = env
#         self.k = k
#     def plug_reduce(self, v):
#         return (self.exp, v, self.k)

# class WithEnv(Value):
#     def evaluate(self, exp, env_s, env_v, k):
#         env_exp = exp[1]
#         eval_exp = exp[2]
#         return (evn_exp, env_s, env_v, withenv_k(eval_exp, env_s, env_v, k))

class let_k(Cont):
    _attrs_ = ['body', 'var', 'env_s', 'env_v', 'k', 'env_struct']
    _immutable_fields_ = ['body', 'var', 'env_s', 'env_v', 'k', 'env_struct']
    def __init__(self, env_struct, body, env_s, env_v, k):
        self.env_struct = env_struct
        self.body = body
        self.env_s = env_s
        self.env_v = env_v
        self.k = k
    def plug_reduce(self, v):
        jit.promote(self.env_struct)
        assert isinstance(v, MultiValue)
        return (self.body,
                self.env_struct,
                EnvironmentValues(v.values, self.env_v),
                self.k)

def let_exp_get(exp):
    return exp[1]

class Let(Value):
    def evaluate(self, exp, env_s, env_v, k):
        var_val_exp = exp[1]
        assert isinstance(var_val_exp, SexpAST)
        vars = [e[0].string_value for e in var_val_exp.children]
        vals = [None]*len(vars)
        env_struct = EnvironmentStructure(vars, env_s)
        body_exp = exp[2]
        return (var_val_exp[0][1], env_s, env_v,
                _prim_n(var_val_exp.children, 1, let_exp_get, vals, 0, len(vars)-1, env_s, env_v,
                        let_k(env_struct, body_exp, env_s, env_v, k)))

class fix_k(Cont):
    def __init__(self, var, body, env_s, env_v, k):
        self.var = var
        self.body = body
        self.env_s = env_s
        self.env_v = env_v
        self.k = k
    def plug_reduce(self, v):
        assert isinstance(v, Closure)
        new_v = Closure(v.body, v.args, v.env_s, v.env_v, self.var.string_value)
        return (self.body, new_v.env_s, new_v.env_v, self.k)

class Fix(Value):
    def evaluate(self, exp, env_s, env_v, k):
        var = exp[1][0][0]
        val_exp = exp[1][0][1]
        body_exp = exp[2]
        return (val_exp, env_s, env_v, fix_k(var, body_exp, env_s, env_v, k))

class If(Value):
    def evaluate(self, exp, env_s, env_v, k):
        return (exp[1], env_s, env_v, if_k(exp, env_s, env_v, k))

class if_k(Cont):
    def __init__(self, exp, env_s, env_v, k):
        self.exp = exp
        self.env_s = env_s
        self.env_v = env_v
        self.k = k
    def plug_reduce(self, v):
        if v == true:
            return (self.exp[2], self.env_s, self.env_v, self.k)
        else:
            return (self.exp[3], self.env_s, self.env_v, self.k)




class ConsCell(Value):
    _attrs_ = ['car', 'cdr']
    _immutable_fields_ = ['car', 'cdr']
    def __init__(self, car, cdr):
        self.car = car
        self.cdr = cdr

class Bool(Value):
    pass

nil = None

true = Bool()
false = Bool()

@prim_two_arg
def begin0(v1, v2):
    return v2

@prim_two_arg
def cons(car, cdr):
    return ConsCell(car, cdr)

@prim_one_arg
def car(ls):
    assert isinstance(ls, ConsCell)
    return ls.car

@prim_one_arg
def cdr(ls):
    assert isinstance(ls, ConsCell)
    return ls.cdr

@prim_two_arg
def add(e1, e2):
    assert isinstance(e1, Number) and isinstance(e2, Number)
    return Number(e1.number_value + e2.number_value)

@prim_two_arg
def sub(e1, e2):
    assert isinstance(e1, Number) and isinstance(e2, Number)
    return Number(e1.number_value - e2.number_value)

@prim_two_arg
def mult(e1, e2):
    assert isinstance(e1, Number) and isinstance(e2, Number)
    return Number(e1.number_value * e2.number_value)

@prim_one_arg
def zero_huh(v):
    assert isinstance(v, Number)
    if v.number_value == 0:
        return true
    else:
        return false

stdin = streamio.fdopen_as_stream(0, "r")
stdout = streamio.fdopen_as_stream(1, "w", buffering=1)

class Read(Value):
    def evaluate(self, exp, env_s, env_v, k):
        val = Number(int(stdin.readline()[:-1]))
        return k.plug_reduce(val)

class time_k(Cont):
    def __init__(self, init_time, env_s, env_v, k):
        self.k = k
        self.init_time = init_time
    def plug_reduce(self, v):
        final_time = time.clock()
        t = str(int((final_time - self.init_time) * 1000))
        stdout.write('cpu time: '+t+' real time: '+t+' gc time: 0\n')
        return self.k.plug_reduce(v)
        
class Time(Value):
    def evaluate(self, exp, env_s, env_v, k):
        init_time = time.clock()
        return exp[1], env_s, env_v, time_k(init_time, env_s, env_v, k)

@prim_one_arg
def display(v):
    assert isinstance(v, Number)
    stdout.write(str(v.number_value)+'\n')
    stdout.flush()
    return v

class halt_k(Cont):
    def __init__(self):
        pass
    def plug_reduce(self, v):
        raise Done(v)

class Done(Exception):
    def __init__(self, val):
        self.value = val


def get_symbol_ast(symbol):
    return global_symbol_table.make_symbol_ast(symbol)

PRIM_NAMES = map(get_string_value,
                 map(get_symbol_ast,
                 ['lambda', 'let', 'if', 'true', 'false',
                  'zero?', '+', '-', 'cons', 'car', 'cdr', '*', 'call/cc',
                  'letrec', 'read', 'display', 'time', 'begin0']))
PRIM_VALUES = [Lambda(), Let(), If(), true, false, zero_huh(), add(),
                       sub(), cons(), car(), cdr(), mult(), Callcc(),
                       Fix(), Read(), display(), Time(), begin0()]

