#! /usr/bin/env python
# -*- coding: utf-8 -*-
#

from parse import *
from ast import *
from values import *
import pdb

from rpython.rlib.jit import JitDriver

def get_printable_location(exp):
    return exp.tostring()

jitdriver = JitDriver(greens=['exp'],
                      reds=['env', 'k'],
                      get_printable_location=get_printable_location)

INIT_ENV = create_top_level_env(PRIM_NAMES, PRIM_VALUES)

def eval(t):
    exp, env, k = t
    while True:
        jitdriver.jit_merge_point(exp=exp, env=env, k=k)
        exp, env, k = exp.eval(env, k)
        if exp.should_enter == True:
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
    tramp = (convert_to_ast(src, INIT_ENV), INIT_ENV, halt_k())
    try:
        eval(tramp)
    except Done, e:
        return 0

def target(*args):
    return entry_point, None

if __name__ == "__main__":
    import sys
    entry_point(sys.argv)

