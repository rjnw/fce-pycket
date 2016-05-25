#! /usr/bin/env python
# -*- coding: utf-8 -*-
#

from parse import *#convert_to_ast, SymbolAST, NumberAST
import pdb

class Cont(object):
    def __init__(self, closure_args, env, k, func):
        self.closure_args = closure_args
        self.env = env
        self.k = k
        self.func = func
    def __call__(self, v):
        return self.func(self.closure_args, self.env, self.k, v)

def continuation(func):
    def new_f(cl_args, env, k):
        return Cont(cl_args, env, k, func)
    return new_f

@continuation
def app_k1(cl_args, env, k, v1):
    exp1 = cl_args[0]
    return eval(exp1, env, app_k2([v1], env, k))

@continuation
def app_k2(cl_args, env, k, v2):
    v1 = cl_args[0]
    return apply_closure(v1, v2, k)

@continuation
def halt_k(cl_args, env, k, v):
    raise Done(v)

class Done(Exception):
    def __init__(self, vals):
        self.values = vals

def apply_k(cont, val):
    return cont.__call__(val)

def eval(exp, env, k):
    if type(exp) is NumberAST:
        return apply_k(k, exp)

    elif type(exp) is SymbolAST:
        return apply_env(exp.string_value, env, k)

    elif type(exp[0]) is SymbolAST and exp[0].string_value == "lambda":
        return create_closure(exp[2], exp[1][0].string_value, env, k)

    else:
        return eval(exp[0], env, app_k1([exp[1]], env, k))

def create_closure (exp, var, env, k):
    return apply_k(k, ClosureAST(exp, var, env))

def apply_closure(closure, arg, k):
    body = closure.exp
    var = closure.var
    env = closure.env
    return eval(body, extend_env(var, arg, env), k)

def extend_env (id, arg, env):
    new_env = env.copy()
    new_env[id] = arg
    return new_env

def apply_env(exp, env, k):
    return apply_k(k, env[exp])

def eval_loop(eval_pair):
    func, args = eval_pair
    try:
        func(*args)
    except Done, e:
        return e.values
    return args

def entry_point(argv):
    import os
    try:
        filename = argv[1]
    except IndexError:
        print "You must supply a filename"
        return 1
    fp = os.open(filename, os.O_RDONLY, 0777)
    src = os.read(fp, 4096)
    os.close(fp)

    print 'evaluating: \n',src
    try:
        eval(convert_to_ast(src), {}, halt_k([], None, None))
    except Done, e:
        return e.values.number_value

def target(*args):
    return entry_point, None

if __name__ == "__main__":
    import sys
    print entry_point(sys.argv)

