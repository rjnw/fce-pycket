#! /usr/bin/env python
# -*- coding: utf-8 -*-
#

from parse import *
from prim import *
from values import *
from jit import *
from initenv import global_initial_env
import pdb

from rpython.rlib.jit import JitDriver

def get_printable_location(exp, env_struct):
    return exp.tostring()

INIT_ENV = create_top_level_env(map(get_string_value, global_initial_env.prim_names),
                                 global_initial_env.prim_values)

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
    state = ExpState(init_ast, env_s, env_v, halt_k())
    try:
        eval(state)
    except Done, e:
        return 0

def target(*args):
    return entry_point, None

if __name__ == "__main__":
    import sys
    entry_point(sys.argv)

"""
make syntax a little more fixed
try separate driver for plug reduce
"""
