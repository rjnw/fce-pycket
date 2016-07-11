#! /usr/bin/env python
# -*- coding: utf-8 -*-
#

from parse import *
from values import *
import pdb

from rpython.rlib.jit import JitDriver

def get_printable_location(exp):
    return exp.tostring()

jitdriver = JitDriver(greens=['exp'],
                      reds=['env', 'k'],
                      get_printable_location=get_printable_location)

def eval(t):
    while True:
        exp, env, k = t
        jitdriver.jit_merge_point(exp=exp, env=env, k=k)
        if type(exp) is NumberAST:
            t = k.plug_reduce(Number(exp.number_value))
        elif type(exp) is SymbolAST:
            t = k.plug_reduce(env.lookup(exp.string_value))
        elif type(exp) is SexpAST and type(exp[0]) is SymbolAST:
            ev = env.lookup(exp[0].string_value)
            t = ev.evaluate(exp, env, k)
            exp,env,k = t
        elif type(exp) is SexpAST:
            exp, env, k = exp[0], env, app_k(exp, env, k)
            t = (exp, env, k)
        else:
            raise Exception('Unknown AST')
        exp,env,k = t
        jitdriver.can_enter_jit(exp=exp, env=env, k=k)

def jitpolicy(driver):
    from rpython.jit.codewriter.policy import JitPolicy
    return JitPolicy()

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
    tramp = trampoline(convert_to_ast(src), initial_environment(), halt_k())
    try:
        eval(tramp)
    except Done, e:
        print str(e.value.number_value)
        return 0

def target(*args):
    return entry_point, None

if __name__ == "__main__":
    import sys
    entry_point(sys.argv)

