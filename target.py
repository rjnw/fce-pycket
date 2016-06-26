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

def eval(tramp):
    exp = tramp.exp
    env = tramp.env
    k = tramp.kont
    jitdriver.jit_merge_point(exp=exp, env=env, k=k)
    if type(exp) is NumberAST:
        return k.plug_reduce(Number(exp.number_value))
    elif type(exp) is SymbolAST:
        return k.plug_reduce(env.apply(exp.string_value))
    elif type(exp) is SexpAST:
        ret = Trampoline(exp[0], env, app_k(exp.children[1:], env, k))
        jitdriver.can_enter_jit(exp=ret.exp, env=ret.env, k=ret.kont)
        return ret

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
    tramp = Trampoline(convert_to_ast(src), initial_environment(), halt_k())
    try:
        while True:
            tramp = eval(tramp)
    except Done, e:
        print str(e.value.number_value)
        return 0

def target(*args):
    return entry_point, None

if __name__ == "__main__":
    import sys
    entry_point(sys.argv)

