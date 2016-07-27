#! /usr/bin/env python
# -*- coding: utf-8 -*-
#

from prim import *
from ast import *
from rpython.rlib import streamio, jit
from rpython.rlib.objectmodel import specialize
import time
from ast import global_symbol_table, get_string_value

@jit.elidable
def simple_interpret(exp, env):
    return env_lookup(env, exp.string_value)

class Lambda(Value):
    def evaluate(self, exp, env, k):
        args = exp[1]
        body = exp[2]
        body.should_enter = True
        return k.plug_reduce(Closure(body, args, env))

class Closure(Value):
    _attrs_ = ['body', 'args', 'env_struct', 'env']
    _immutable_fields_ = ['body', 'args', 'env_struct', 'env']
    def __init__(self, body, args, env, fix=0):
        #assert isinstance(args, SexpAST)
        self.body = body
        self.args = args
        if fix == 0:
            self.env = env
        else:
            self.env = env_extend(env, [fix], [self])
        if isinstance(args, SexpAST):
            self.env_struct = EnvironmentStructure([var.string_value for var in self.args.children],
                                                   self.env[0])
        else:
            raise NotImplemented('list argument not implemented')
        # self.env_struct = EnvironmentStructure([var.string_value for var in self.args.children],
        #                                      self.env[0])

    def evaluate(self, exp, env, k):
        arg_len = len(exp) - 1
        vals = [None]* arg_len
        return (exp[1], env, _prim_n(exp, 2, vals, 0, arg_len-1, env, closure_k(self, env, k)))

class closure_k(Cont):
    def __init__(self, clos, env, k):
        assert isinstance(clos, Closure)
        self.clos = clos
        self.k = k
    def plug_reduce(self, v):
        jit.promote(self.clos.env_struct)
        assert isinstance(v, MultiValue)
        return (self.clos.body,
                (self.clos.env_struct, EnvironmentValues(v.values, self.clos.env[1])),
                self.k)

class CapturedCont(Value):
    def __init__(self, k):
        self.k = k
    def evaluate(self, exp, env, k):
        return (exp[1], env, self.k)

class callcc_k(Cont):
    def __init__(self, env, k):
        self.env = env
        self.k = k

    def plug_reduce(self, v):
        ck = closure_k(v, self.env, self.k)
        return ck.plug_reduce(CapturedCont(self.k))

class Callcc(Value):
    def evaluate(self, exp, env, k):
        arg = exp[1]
        return (arg, env, callcc_k(env, k))

class CaptEnv(Value):
    def evaluate(self, exp, env, k):
        return k.plug_reduce(env)

class withenv_k(Cont):
    def __init__(self, eval_exp, env, k):
        self.exp = exp
        self.env = env
        self.k = k
    def plug_reduce(self, v):
        return (self.exp, v, self.k)

class WithEnv(Value):
    def evaluate(self, exp, env, k):
        env_exp = exp[1]
        eval_exp = exp[2]
        return (evn_exp, env, withenv_k(eval_exp, env, k))

class let_k(Cont):
    _attrs_ = ['body', 'var', 'env', 'k', 'env_struct']
    _immutable_fields_ = ['body', 'var', 'env', 'k', 'env_struct']
    def __init__(self, var, body, env, k):
        self.var = var
        self.body = body
        self.env = env
        self.k = k
        self.env_struct = EnvironmentStructure([self.var.string_value], self.env[0])
    def plug_reduce(self, v):
        jit.promote(self.env_struct)
        new_env = (self.env_struct, EnvironmentValues([v], self.env[1]))
        return (self.body, new_env, self.k)

class Let(Value):
    def evaluate(self, exp, env, k):
        var = exp[1][0][0]
        val_exp = exp[1][0][1]
        body_exp = exp[2]
        return (val_exp, env, let_k(var, body_exp, env, k))

class fix_k(Cont):
    def __init__(self, var, body, env, k):
        self.var = var
        self.body = body
        self.env = env
        self.k = k
    def plug_reduce(self, v):
        assert isinstance(v, Closure)
        new_v = Closure(v.body, v.args, v.env, self.var.string_value)
        if self.env == v.env:
            return (self.body, new_v.env, self.k)
        else:
            new_env = env_extend(self.env, [self.var.string_value], [new_v])
            return (self.body, new_env, self.k)

class Fix(Value):
    def evaluate(self, exp, env, k):
        var = exp[1][0][0]
        val_exp = exp[1][0][1]
        body_exp = exp[2]
        return (val_exp, env, fix_k(var, body_exp, env, k))

class If(Value):
    def evaluate(self, exp, env, k):
        return (exp[1], env, if_k(exp, env, k))

class if_k(Cont):
    def __init__(self, exp, env, k):
        self.exp = exp
        self.env = env
        self.k = k
    def plug_reduce(self, v):
        if v == true:
            return (self.exp[2], self.env, self.k)
        else:
            return (self.exp[3], self.env, self.k)

class _prim_n(Cont):
    def __init__(self, exp_array, exp_offset,
                       value_array, current_index, end_index,
                       env, k):
        self.exp_array = exp_array
        self.exp_offset = exp_offset
        self.value_array = value_array
        self.current_index = current_index
        self.end_index = end_index
        self.env = env
        self.k = k

    def plug_reduce(self, v):
        self.value_array[self.current_index] = v
        if self.current_index == self.end_index:
            return self.k.plug_reduce(MultiValue(self.value_array))
        else:
            return (self.exp_array[self.current_index + self.exp_offset],
                    self.env,
                    _prim_n(self.exp_array, self.exp_offset,
                            self.value_array, self.current_index+1, self.end_index,
                            self.env, self.k))

def prim_one_arg(func):
    class prim_k(Cont):
        def __init__(self, env, k):
            self.env = env
            self.k = k
        def plug_reduce(self, v):
            return self.k.plug_reduce(func(v))

    class prim_eval(Value):
        def evaluate(self, exp, env, k):
            return (exp[1], env, prim_k(env, k))
    return prim_eval

def prim_two_arg(func):
    class prim_k1(Cont):
        _attrs_ = ['exp', 'env', 'k']
        _immutable_fields_ = ['exp', 'env', 'k']
        def __init__(self, exp2, env, k):
            self.exp = exp2
            self.env = env
            self.k = k
        def plug_reduce(self, v):
            return (self.exp, self.env, prim_k2(v, self.env, self.k))

    class prim_k2(Cont):
        _attrs_ = ['v1', 'env', 'k']
        _immutable_fields_ = ['v1', 'env', 'k']
        def __init__(self, v1, env, k):
            self.v1 = v1
            self.env = env
            self.k = k
        def plug_reduce(self, v):
            return self.k.plug_reduce(func(self.v1, v))

    class prim_eval(Value):
        def evaluate(self, exp, env, k):
            return (exp[1], env, prim_k1(exp[2], env, k))
    return prim_eval

class Cell(Value):
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
def begin2(v1, v2):
    return v2

@prim_two_arg
def cons(car, cdr):
    return Cell(car, cdr)

@prim_one_arg
def car(ls):
    assert isinstance(ls, Cell)
    return ls.car

@prim_one_arg
def cdr(ls):
    assert isinstance(ls, Cell)
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
    def evaluate(self, exp, env, k):
        val = Number(int(stdin.readline()[:-1]))
        return k.plug_reduce(val)

class time_k(Cont):
    def __init__(self, init_time, env, k):
        self.k = k
        self.init_time = init_time
    def plug_reduce(self, v):
        final_time = time.clock()
        stdout.write('time: ' + str(int((final_time - self.init_time) * 1000))+'ms.\n')
        return self.k.plug_reduce(v)
        
class Time(Value):
    def evaluate(self, exp, env, k):
        init_time = time.clock()
        return exp[1], env, time_k(init_time, env, k)

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
                  'letrec', 'read', 'display', 'time', 'begin2']))
PRIM_VALUES = [Lambda(), Let(), If(), true, false, zero_huh(), add(),
                       sub(), cons(), car(), cdr(), mult(), Callcc(),
                       Fix(), Read(), display(), Time(), begin2()]

