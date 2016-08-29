#! /usr/bin/env python
# -*- coding: utf-8 -*-
#

from prim import *
from initenv import prim, prim_value
from rpython.rlib import streamio, jit
from rpython.rlib.objectmodel import specialize, compute_unique_id
import time

class StateCont(Cont):
    _immutable_fields_ = ['env_s', 'env_v', 'exp', 'k']
    pass

def prim_one_arg(func):
    class prim_k(StateCont):
        def __init__(self, exp, env_s, env_v, k):
            self.exp = exp[1]
            self.env_s = env_s
            self.env_v = env_v
            self.k = k
        def plug_reduce(self, v):
            return ContState(self, func(v))
            return self.k.plug_reduce(func(v))

    class prim_eval(Value):
        def evaluate(self, exp, env_s, env_v, k):
            if can_simple_eval(exp[1]):
                v = exp[1].simple_eval(env_s, env_v)
                return k.plug_reduce(func(v))
            else:
                return ExpState(exp[1], env_s, env_v, prim_k(exp, env_s, env_v, k))
    return prim_eval

class prim_store(object):
    _immutable_fields_ = ['exp', 'env_s', 'env_v', 'k']
    def __init__(self, exp, env_s, env_v, k):
        self.exp = exp
        self.env_s = env_s
        self.env_v = env_v
        self.k = k

def prim_two_arg(func):
    class prim_k2_2arg(StateCont):
        _immutable_fields_ = ['v1', 'k', 'env_s', 'env_v', 'exp']
        def __init__(self, exp, v1, env_s, env_v, k):
            self.exp = exp
            self.env_s = env_s
            self.env_v = env_v
            self.v1 = v1
            self.k = k
        def plug_reduce(self, v):
            return ContState(self, func(self.v1, v))
            return self.k.plug_reduce(func(self.v1, v))

    class prim_k1_2arg(StateCont):
        _immutable_fields_ = ['exp', 'env_s', 'env_v', 'k', 'plug_reduce']
        def __init__(self, exp, env_s, env_v, k):
            self.exp = exp
            self.env_s = env_s
            self.env_v = env_v
            self.k = k
        def plug_reduce(self, v):
            if can_simple_eval(self.exp):
                v2 = self.exp.simple_eval(self.env_s, self.env_v)
                return self.k.plug_reduce(func(v,v2))
            else:
                return ExpState(self.exp, self.env_s, self.env_v,
                        prim_k2_2arg(self.exp, v, self.env_s, self.env_v, self.k))


    class prim_eval(Value):
        def evaluate(self, exp, env_s, env_v, k):
            if can_simple_eval(exp[1]) and can_simple_eval(exp[2]):
                v1 = exp[1].simple_eval(env_s, env_v)
                v2 = exp[2].simple_eval(env_s, env_v)
                return k.plug_reduce(func(v1, v2))
            elif can_simple_eval(exp[1]):
                v = exp[1].simple_eval(env_s, env_v)
                return ExpState(exp[2], env_s, env_v, prim_k2_2arg(exp[2], v, env_s, env_v, k))
            else:
                return ExpState(exp[1], env_s, env_v, prim_k1_2arg(exp[2], env_s, env_v, k))
    return prim_eval

@prim('define')
class Define(Value):
    def evaluate(self, exp, env_s, env_v, k):
        body = exp[2]
        arg = exp[1]
        assert isinstance(arg, SymbolAST)
        return ExpState(body, env_s, env_v, define_k(arg, env_s, env_v, k))

class define_k(Cont):
    def __init__(self, arg, env_s, env_v, k):
        self.arg = arg
        self.env_s = env_s
        self.env_v = env_v
        self.k = k
    def plug_reduce(self, v):
        if isinstance(v, Closure):
            v = Closure(v.lambda_ast, v.env_s, v.env_v)
        e = env_extend(self.env_s, self.env_v, [self.arg.string_value], [v])
        env = EnvironmentValue(e[0], e[1])
        return self.k.plug_reduce(env)

class ModuleAST(AST):
    _attrs_ = ['children', 'should_enter']
    _immutable_fields_ = ['children', 'should_enter']
    def __init__(self, children):
        self.children = children
        self.should_enter = False
    def eval(self, env_s, env_v, k):
        return ExpState(self.children[0], env_s, env_v,
                module_k(self.children,
                         1, len(self.children), env_s, env_v, k))

class module_k(Cont):
    def __init__(self, children, current_index, end_index, env_s, env_v, k):
        self.children = children
        self.current_index = current_index
        self.end_index = end_index
        self.env_s = env_s
        self.env_v = env_v
        self.k = k
    def plug_reduce(self, v):
        if self.current_index == self.end_index:
            return self.k.plug_reduce(EnvironmentValue(self.env_s, self.env_v))

        elif isinstance(v, EnvironmentValue):
            return ExpState(self.children[self.current_index], v.env_s, v.env_v,
                    module_k(self.children, self.current_index+1,
                             self.end_index, v.env_s, v.env_v, self.k))
        else:
            _prim_display(v)
            return ExpState(self.children[self.current_index], self.env_s, self.env_v,
                    module_k(self.children, self.current_index+1,
                             self.end_index, self.env_s, self.env_v, self.k))

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


class ConsCell(Value):
    _attrs_ = ['car', 'cdr']
    _immutable_fields_ = ['car', 'cdr']
    def __init__(self, car, cdr):
        self.car = car
        self.cdr = cdr

@prim('begin0')
@prim_two_arg
def begin0(v1, v2):
    return v2

@prim('cons')
@prim_two_arg
def cons(car, cdr):
    return ConsCell(car, cdr)

@prim('car')
@prim_one_arg
def car(ls):
    assert isinstance(ls, ConsCell)
    return ls.car

@prim('cdr')
@prim_one_arg
def cdr(ls):
    assert isinstance(ls, ConsCell)
    return ls.cdr

@prim('+')
@prim_two_arg
def add(e1, e2):
    assert isinstance(e1, Number) and isinstance(e2, Number)
    return Number(e1.number_value + e2.number_value)

@prim('-')
@prim_two_arg
def sub(e1, e2):
    assert isinstance(e1, Number) and isinstance(e2, Number)
    return Number(e1.number_value - e2.number_value)

@prim('<')
@prim_two_arg
def less_than(e1, e2):
    assert isinstance(e1, Number) and isinstance(e2, Number)
    if e1.number_value < e2.number_value:
        return true
    else:
        return false

@prim('>')
@prim_two_arg
def greater_than(e1, e2):
    assert isinstance(e1, Number) and isinstance(e2, Number)
    if e1.number_value < e2.number_value:
        return false
    else:
        return true
    
@prim('=')
@prim_two_arg
def equal(e1, e2):
    assert isinstance(e1, Number) and isinstance(e2, Number)
    if e1.number_value == e2.number_value:
        return true
    else:
        return false

@prim('*')
@prim_two_arg
def mult(e1, e2):
    assert isinstance(e1, Number) and isinstance(e2, Number)
    return Number(e1.number_value * e2.number_value)

@prim('zero?')
@prim_one_arg
def zero_huh(v):
    assert isinstance(v, Number)
    if v.number_value == 0:
        return true
    else:
        return false

@prim('not')
@prim_one_arg
def not_bool(e1):
    if isinstance(e1, Bool) and e1 == false:
        return true
    else:
        return false

stdin = streamio.fdopen_as_stream(0, "r")
stdout = streamio.fdopen_as_stream(1, "w", buffering=1)

@prim('read')
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
        
@prim('time')
class Time(Value):
    def evaluate(self, exp, env_s, env_v, k):
        init_time = time.clock()
        return ExpState(exp[1], env_s, env_v, time_k(init_time, env_s, env_v, k))

def _prim_display(v):
    if isinstance(v, Number):
        stdout.write(str(v.number_value))
    elif isinstance(v, Void):
        stdout.write('#<void>')
    elif isinstance(v, Bool):
        if v == true:
            stdout.write('#t')
        else:
            stdout.write('#f')
    elif isinstance(v, Closure):
        stdout.write('#<procedure>')
    elif isinstance(v, ConsCell):
        pass #TODO print cons
    else:
        raise Exception('unknown type to print')
    stdout.flush()


@prim('display')
@prim_one_arg
def display(v):
    _prim_display(v)
    return void

class halt_k(Cont):
    def __init__(self):
        pass
    def plug_reduce(self, v):
        raise Done(v)

class Done(Exception):
    def __init__(self, val):
        self.value = val
