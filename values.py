#! /usr/bin/env python
# -*- coding: utf-8 -*-
#
from ast import *
from rpython.rlib import streamio as sio

def trampoline(exp, env, cont):
    return (exp, env, cont)
def tramp_exp(t):
    return t[0]
def tramp_env(t):
    return t[1]
def tramp_cont(t):
    t[2]

class Value(object):
    def evaluate(self, exp, env, k):
        raise NotImplementedError("Abstract base class")

class Cont(Value):
    def plug_reduce(self, v):
        raise NotImplementedError("Abstract base class")

class Number(Value):
    def __init__(self, num):
        self.number_value = num
    def tostring(self):
        return str(self.number_value)

class Environment(Value):
    def __init__(self, prev=None):
        self.val_arr = None
        self.var_map = {}
        self.prev = prev
    def apply(self, key):
        index = self.lookup(key)
        if index == -1:
            return self.prev.apply(key)
        else:
            return self.val_arr[index]
    def lookup(self, key):
        return self.var_map.get(key, -1)
    def extend(self, key, value):
        ne = Environment()
        ne.prev = self
        ne.val_arr = [value]
        ne.var_map[key] = 0
        return ne
    def extend_mult(self, keys, values):
        ne = Environment()
        ne.prev = self
        ne.val_arr = values
        for i,k in enumerate(keys):
            ne.var_map[k] = i
        return ne

class Lambda(Value):
    def evaluate(self, exp, env, k):
        arg = exp[1][0]
        body = exp[2]
        return k.plug_reduce(Closure(body, arg, env))

class CapturedCont(Value):
    def __init__(self, k):
        self.k = k
    def evaluate(self, exp, env, k):
        return trampoline(exp[1], env, self.k)

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
        return trampoline(arg, env, callcc_k(env, k))

class CaptEnv(Value):
    def evaluate(self, exp, env, k):
        return k.plug_reduce(env)

class withenv_k(Cont):
    def __init__(self, eval_exp, env, k):
        self.exp = exp
        self.env = env
        self.k = k
    def plug_reduce(self, v):
        return trampoline(self.exp, v, self.k)

class WithEnv(Value):
    def evaluate(self, exp, env, k):
        env_exp = exp[1]
        eval_exp = exp[2]
        return trampoline(evn_exp, env, withenv_k(eval_exp, env, k))

class let_k(Cont):
    def __init__(self, var, body, env, k):
        self.var = var
        self.body = body
        self.env = env
        self.k = k
    def plug_reduce(self, v):
        new_env = self.env.extend(self.var.string_value, v)
        return trampoline(self.body, new_env, self.k)


class Let(Value):
    def evaluate(self, exp, env, k):
        var = exp[1][0]
        val_exp = exp[1][1]
        body_exp = exp[2]
        return trampoline(val_exp, env, let_k(var, body_exp, env, k))

class fix_k(Cont):
    def __init__(self, var, body, env, k):
        self.var = var
        self.body = body
        self.env = env
        self.k = k
    def plug_reduce(self, v):
        new_env = self.env.extend(self.var.string_value, v)
        assert isinstance(v, Closure)
        v.env = new_env
        return trampoline(self.body, new_env, self.k)

class Fix(Value):
    def evaluate(self, exp, env, k):
        var = exp[1][0]
        val_exp = exp[1][1]
        body_exp = exp[2]
        return trampoline(val_exp, env, fix_k(var, body_exp, env, k))

class closure_k(Cont):
    def __init__(self, clos, env, k):
        assert isinstance(clos, Closure)
        self.clos = clos
        self.eval_env = env
        self.k = k
    def plug_reduce(self, v):
        return trampoline(self.clos.body,
                          self.clos.env.extend(self.clos.var.string_value, v),
                          self.k)

class Closure(Value):
    def __init__(self, body, var, env):
        self.body = body
        self.var = var
        self.env = env
    def evaluate(self, exp, env, k):
        rand = exp[1]
        return trampoline(rand, env, closure_k(self, env, k))

class If(Value):
    def evaluate(self, exp, env, k):
        ch = exp[1]
        th = exp[2]
        el = exp[3]
        return trampoline(ch, env, if_k(th, el, env, k))

class if_k(Cont):
    def __init__(self, th, el, env, k):
        self.th = th
        self.el = el
        self.env = env
        self.k = k
    def plug_reduce(self, v):
        if v == true: #TODO
            return trampoline(self.th, self.env, self.k)
        else:
            return trampoline(self.el, self.env, self.k)

class Cell(Value):
    def __init__(self, car, cdr):
        self.car = car
        self.cdr = cdr
class Bool(Value):
    pass

def prim_one_arg(func):
    class prim_k(Cont):
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
        def __init__(self, exp2, env, k):
            self.exp2 = exp2
            self.env = env
            self.k = k
        def plug_reduce(self, v):
            return trampoline(self.exp2, self.env, prim_k2(v, self.env, self.k))

    class prim_k2(Cont):
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

stdin = sio.fdopen_as_stream(0, "r")
stdout = sio.fdopen_as_stream(1, "w", buffering=1)

class Read(Value):
    def evaluate(self, exp, env, k):
        val = Number(int(stdin.readline()[:-1]))
        return k.plug_reduce(val)

@prim_one_arg
def display(v):
    stdout.write(str(v.number_value)+'\n')
    stdout.flush()
    return v

class app_k(Cont):
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

def initial_environment():
    env = Environment()
    env = env.extend_mult(['lambda', 'let', 'if', 'true', 'false',
                           'zero?', '+', '-', 'cons', 'car', 'cdr', '*', 'call/cc',
                           'fix', 'read', 'display'],
                          [Lambda(), Let(), If(), true, false, zero_huh(), add(),
                           sub(), cons(), car(), cdr(), mult(), Callcc(),
                           Fix(), Read(), display()])
    return env
