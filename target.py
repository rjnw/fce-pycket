#! /usr/bin/env python
# -*- coding: utf-8 -*-
#

from parse import *
from values import *
import pdb

# @continuation
# def app_k1(cl_args, env, k, v1):
#     exp1 = cl_args[0]
#     return (exp1, env, app_k2([v1], env, k))

# @continuation
# def app_k2(cl_args, env, k, v2):
#     v1 = cl_args[0]
#     return apply_closure(v1, v2, k)


@continuation
def halt_k(cl_args, env, k, v):
    raise Done(v)

# @continuation
# def let_k(cl_args, env, k, v):
#     id, body = cl_args
#     new_env = env.extend(id, v)
#     return body, new_env, k

# @continuation
# def callcc_k(cl_args, env, k, v):
#     return apply_closure(v, k, k)

# @continuation
# def callce_k(cl_args, env, k, ce_env):
#     body = cl_args[0]
#     return body, ce_env, k

class Done(Exception):
    def __init__(self, vals):
        self.values = vals

# def apply_k(cont, val):
#     return cont.__call__(val)

def eval(tramp):
    exp = tramp.exp
    env = tramp.env
    k = tramp.kont
    if type(exp) is NumberAST:
        return k(Number(exp.number_value))

    elif type(exp) is SymbolAST:
        return k(env.apply(exp))

    else:
        return Trampoline(exp[0], env, app_k(exp[1:], env, k))

# def create_closure(exp, var, env, k):
#     return apply_k(k, ClosureAST(exp, var, env))

# def apply_closure(closure, arg, k):
#     if type(closure) is Cont:
#         return apply_k(closure, arg)
#     else:
#         body = closure.exp
#         var = closure.var
#         env = closure.env
#         return (body, env.extend(var, arg), k)

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
    tramp = Trampoline(convert_to_ast(src), initial_environment(), halt_k([], None, None))
    try:
        while True:
            tramp = eval(tramp)
    except Done, e:
        print str(e.values)
        return str(e.values)

def target(*args):
    return entry_point, None

if __name__ == "__main__":
    import sys
    entry_point(sys.argv)

