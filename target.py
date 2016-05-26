#! /usr/bin/env python
# -*- coding: utf-8 -*-
#

from parse import *#convert_to_ast, SymbolAST, NumberAST
import pdb

@continuation
def app_k1(cl_args, env, k, v1):
    exp1 = cl_args[0]
    return (exp1, env, app_k2([v1], env, k))

@continuation
def app_k2(cl_args, env, k, v2):
    v1 = cl_args[0]
    return apply_closure(v1, v2, k)

@continuation
def halt_k(cl_args, env, k, v):
    raise Done(v)

@continuation
def let_k(cl_args, env, k, v):
    id, body = cl_args
    new_env = extend_env(id, v, env)
    return body, new_env, k

@continuation
def callcc_k(cl_args, env, k, v):
    return apply_closure(v, k, k)

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

    elif type(exp[0]) is SymbolAST and exp[0].string_value == 'lambda':
        return create_closure(exp[2], exp[1][0], env, k)

    elif type(exp[0]) is SymbolAST and exp[0].string_value == 'let':
        print 'let:', exp, exp[1][1]
        return exp[1][1], env, let_k([exp[1][0], exp[2]], env, k)

    elif type(exp[0]) is SymbolAST and exp[0].string_value == 'call/cc':
        return exp[1], env, callcc_k([], env, k)

    else:
        return (exp[0], env, app_k1([exp[1]], env, k))

def create_closure(exp, var, env, k):
    return apply_k(k, ClosureAST(exp, var, env))

def apply_closure(closure, arg, k):
    if type(closure) is Cont:
        return apply_k(closure, arg)
    else:
        body = closure.exp
        var = closure.var
        env = closure.env
        return (body, extend_env(var, arg, env), k)


def extend_env (id, arg, env):
    new_env = env.copy()
    new_env[id.string_value] = arg
    return new_env

def apply_env(exp, env, k):
    return apply_k(k, env[exp])

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
    exp, env, k = (convert_to_ast(src), {}, halt_k([], None, None))
    try:
        while True:
            exp, env, k = eval(exp, env, k)
    except Done, e:
        print e.values
        return 0

def target(*args):
    return entry_point, None

if __name__ == "__main__":
    import sys
    entry_point(sys.argv)

