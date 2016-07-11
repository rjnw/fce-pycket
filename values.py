#! /usr/bin/env python
# -*- coding: utf-8 -*-
#
from ast import *
from rpython.rlib import streamio, jit
import time

def trampoline(exp, env, cont):
    return (exp, env, cont)
def tramp_exp(t):
    return t[0]
def tramp_env(t):
    return t[1]
def tramp_cont(t):
    t[2]

class Value(object):
    _immutable_fields_ = ['number_value', 'val_arr[*]', 'var_map[*]', 'prev', 'exp', 'env', 'k']
    def evaluate(self, exp, env, k):
        raise NotImplementedError("Abstract base class")

class Cont(Value):
    def plug_reduce(self, v):
        raise NotImplementedError("Abstract base class")

class Number(Value):
    _immutable_fields_ = ['number_value']
    def __init__(self, num):
        self.number_value = num
    def tostring(self):
        return str(self.number_value)

@jit.elidable_promote('all')
def make_env_map(vars):
    return vars

@jit.elidable_promote('all')
def get_env_index(var_map, var):
    for i, v in enumerate(var_map):
        if v == var:
            return i
    return -1

class Environment(Value):
    _immutable_fields_ = ['val_arr[*]', 'var_map[*]', 'prev']
    def __init__(self, var_map, values, prev=None):
        self.val_arr = values
        self.var_map = var_map
        self.prev = prev

    @jit.elidable_promote('all')
    def lookup(self, key):
        index = get_env_index(self.var_map, key)
        if index == -1:
            return self.prev.lookup(key)
        else:
            return self.val_arr[index]

    def extend(self, key, value):
        evm = make_env_map([key])
        ne = Environment(evm, [value], self)
        return ne

class Lambda(Value):
    _immutable_fields_ = ['exp', 'env', 'k']
    def evaluate(self, exp, env, k):
        arg = exp[1][0]
        body = exp[2]
        body.should_enter = True
        return k.plug_reduce(Closure(body, arg, env))

class CapturedCont(Value):
    _immutable_fields_ = ['k']
    def __init__(self, k):
        self.k = k
    def evaluate(self, exp, env, k):
        return trampoline(exp[1], env, self.k)

class callcc_k(Cont):
    _immutable_fields_ = ['env', 'k']
    def __init__(self, env, k):
        self.env = env
        self.k = k

    def plug_reduce(self, v):
        ck = closure_k(v, self.env, self.k)
        return ck.plug_reduce(CapturedCont(self.k))

class Callcc(Value):
    _immutable_fields_ = ['exp', 'env', 'k']
    def evaluate(self, exp, env, k):
        arg = exp[1]
        return trampoline(arg, env, callcc_k(env, k))

class CaptEnv(Value):
    _immutable_fields_ = ['exp', 'env', 'k']
    def evaluate(self, exp, env, k):
        return k.plug_reduce(env)

class withenv_k(Cont):
    _immutable_fields_ = ['eval_exp', 'env', 'k']
    def __init__(self, eval_exp, env, k):
        self.exp = exp
        self.env = env
        self.k = k
    def plug_reduce(self, v):
        return trampoline(self.exp, v, self.k)

class WithEnv(Value):
    _immutable_fields_ = ['exp', 'env', 'k']
    def evaluate(self, exp, env, k):
        env_exp = exp[1]
        eval_exp = exp[2]
        return trampoline(evn_exp, env, withenv_k(eval_exp, env, k))

class let_k(Cont):
    _immutable_fields_ = ['var', 'body', 'env', 'k']
    def __init__(self, var, body, env, k):
        self.var = var
        self.body = body
        self.env = env
        self.k = k
        self.env_var_map = make_env_map([self.var.string_value])
    def plug_reduce(self, v):
        new_env = Environment(self.env_var_map, [v], self.env)
        return trampoline(self.body, new_env, self.k)

class Let(Value):
    _immutable_fields_ = ['exp', 'env', 'k']
    def evaluate(self, exp, env, k):
        var = exp[1][0][0]
        val_exp = exp[1][0][1]
        body_exp = exp[2]
        return trampoline(val_exp, env, let_k(var, body_exp, env, k))

class fix_k(Cont):
    _immutable_fields_ = ['var', 'body', 'env', 'k']
    def __init__(self, var, body, env, k):
        self.var = var
        self.body = body
        self.env = env
        self.k = k
    def plug_reduce(self, v):
        assert isinstance(v, Closure)
        new_v = Closure(v.body, v.var, v.env, self.var.string_value)
        if self.env == v.env:
            return trampoline(self.body, new_v.env, self.k)
        else:
            new_env = self.env.extend(self.var.string_value, new_v)
            return trampoline(self.body, new_env, self.k)

class Fix(Value):
    def evaluate(self, exp, env, k):
        var = exp[1][0][0]
        val_exp = exp[1][0][1]
        body_exp = exp[2]
        return trampoline(val_exp, env, fix_k(var, body_exp, env, k))

class closure_k(Cont):
    _immutable_fields_ = ['clos', 'k']
    def __init__(self, clos, env, k):
        assert isinstance(clos, Closure)
        self.clos = clos
        self.k = k
    def plug_reduce(self, v):
        return (self.clos.body,
                Environment(self.clos.env_var_map, [v], self.clos.env),
                self.k)

class Closure(Value):
    _immutable_fields_ = ['body', 'var', 'k', 'env', 'env_var_map[*]']
    def __init__(self, body, var, env, fix=None):
        self.body = body
        self.var = var
        self.env_var_map = make_env_map([self.var.string_value])
        if fix is None:
            self.env = env
        else:
            self.env = env.extend(fix, self)
    def evaluate(self, exp, env, k):
        rand = exp[1]
        return (rand, env, closure_k(self, env, k))

class If(Value):
    def evaluate(self, exp, env, k):
        return trampoline(exp[1], env, if_k(exp, env, k))

class if_k(Cont):
    _immutable_fields_ = ['exp', 'env', 'k']
    def __init__(self, exp, env, k):
        self.exp = exp
        self.env = env
        self.k = k
    def plug_reduce(self, v):
        if v == true:
            return trampoline(self.exp[2], self.env, self.k)
        else:
            return trampoline(self.exp[3], self.env, self.k)

class Cell(Value):
    def __init__(self, car, cdr):
        self.car = car
        self.cdr = cdr

class Bool(Value):
    pass

class _prim_n(Cont):
    def __init__(self, exp, current_n, total_n, value_array,  func, env, k):
        self.exp = exp
        self.n = current_n
        self.N = total_n
        self.values = value_array
        self.func = func
        self.env = env
        self.k = k
    def plug_reduce(self, v):
        if self.n == self.N:
            value_array[self.n - 1] = v
            self.k.plug_reduce(func(value_array))
        else:
            exp[current_n+1], env, _prim_n(exp, current_n+1, total_n, value_array, func, env, k)

def prim_one_arg(func):
    class prim_k(Cont):
        _immutable_fields_ = ['env', 'k']
        def __init__(self, env, k):
            self.env = env
            self.k = k
        def plug_reduce(self, v):
            return self.k.plug_reduce(func(v))

    class prim_eval(Value):
        def evaluate(self, exp, env, k):
            return trampoline(exp[1], env, prim_k(env, k))
    return prim_eval

def prim_two_arg(func):
    class prim_k1(Cont):
        _immutable_fields_ = ['exp2', 'env', 'k']
        def __init__(self, exp2, env, k):
            self.exp2 = exp2
            self.env = env
            self.k = k
        def plug_reduce(self, v):
            return trampoline(self.exp2, self.env, prim_k2(v, self.env, self.k))

    class prim_k2(Cont):
        _immutable_fields_ = ['v1', 'env', 'k']
        def __init__(self, v1, env, k):
            self.v1 = v1
            self.env = env
            self.k = k
        def plug_reduce(self, v):
            return self.k.plug_reduce(func(self.v1, v))

    class prim_eval(Value):
        def evaluate(self, exp, env, k):
            return trampoline(exp[1], env, prim_k1(exp[2], env, k))
    return prim_eval

nil = None

true = Bool()
false = Bool()

@prim_two_arg
def cons(car, cdr):
    return Cell(car, cdr)

@prim_one_arg
def car(ls):
    return ls.car

@prim_one_arg
def cdr(ls):
    return ls.cdr

@prim_two_arg
def add(e1, e2):
    return Number(e1.number_value + e2.number_value)

@prim_two_arg
def sub(e1, e2):
    return Number(e1.number_value - e2.number_value)

@prim_two_arg
def mult(e1, e2):
    return Number(e1.number_value * e2.number_value)

@prim_one_arg
def zero_huh(v):
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
    stdout.write(str(v.number_value)+'\n')
    stdout.flush()
    return v

class app_k(Cont):
    _immutable_fields_ = ['exp', 'env', 'k']
    def __init__(self, exp, env, k):
        self.exp = exp
        self.env = env
        self.k = k
    def plug_reduce(self, v):
        return v.evaluate(self.exp, self.env, self.k)

class halt_k(Cont):
    def __init__(self):
        pass
    def plug_reduce(self, v):
        raise Done(v)

class Done(Exception):
    def __init__(self, val):
        self.value = val

class TopLevelEnvironment(Value):
    _immutable_fields_ = ['val_arr[*]', 'var_map']
    def __init__(self, names, values):
        self.var_map = {}
        for i, v in enumerate(names):
            self.var_map[v] = i
        self.val_arr = values

    @jit.elidable_promote('all')
    def lookup(self, key):
        return self.val_arr[self.get_index(key)]

    def get_index(self, key):
        return self.var_map[key]


#TODO fix prev of top level to raise exception if not found
def initial_environment():
    names = ['lambda', 'let', 'if', 'true', 'false',
                   'zero?', '+', '-', 'cons', 'car', 'cdr', '*', 'call/cc',
                   'fix', 'read', 'display', 'time']
    values = [Lambda(), Let(), If(), true, false, zero_huh(), add(),
                       sub(), cons(), car(), cdr(), mult(), Callcc(),
                       Fix(), Read(), display(), Time()]

    env = TopLevelEnvironment(names, values)
    return env
