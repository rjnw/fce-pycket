#! /usr/bin/env python
# -*- coding: utf-8 -*-
#

from parse import *
from prim import *
from values import *
from initenv import global_initial_env
import pdb

from rpython.rlib.jit import JitDriver

def get_printable_location(exp, env_struct):
    return exp.tostring()

jitdriver = JitDriver(greens=['exp', 'env_struct'],
                      reds=['env_values', 'k'],
                      get_printable_location=get_printable_location)

INIT_ENV = create_top_level_env(map(get_string_value, global_initial_env.prim_names),
                                 global_initial_env.prim_values)

def eval(t):
    exp, env_s, env_v, k = t
    while True:
        jitdriver.jit_merge_point(exp=exp, env_struct=env_s, env_values=env_v, k=k)

        exp, env_s, env_v, k = exp.eval(env_s, env_v, k)

        if exp.should_enter == True:
            jitdriver.can_enter_jit(exp=exp, env_struct=env_s, env_values=env_v, k=k)

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
    start_module = parse_module(src)
    if len(start_module) == 1:
        init_ast = start_module[0]
    else:
        init_ast = ModuleAST(list(start_module))
    env_s, env_v = INIT_ENV
    tramp = (init_ast, env_s, env_v, halt_k())
    try:
        eval(tramp)
    except Done, e:
        return 0

def target(*args):
    return entry_point, None

if __name__ == "__main__":
    import sys
    entry_point(sys.argv)

