#! /usr/bin/env python
# -*- coding: utf-8 -*-
#

from rpython.rlib.parsing.ebnfparse import parse_ebnf, make_parse_function
import string
import pdb

def parse_exp(st, curr_ind):
    while st[curr_ind] in string.whitespace:
        curr_ind += 1
    if st[curr_ind] == '(':
        tokens = []
        while st[curr_ind] != ')':
            t, i = parse_exp(st, curr_ind+1)
            curr_ind = i
            tokens.append(t)
        curr_ind += 1
        ast = tokens
    else:
        prev_ind = curr_ind
        while st[curr_ind] not in string.whitespace+')':
            curr_ind += 1
        ast = st[prev_ind:curr_ind]
    return ast, curr_ind

def parse(st):
    t, i = parse_exp(st, 0)
    return t

def eval(exp, env, k):
    if exp == ():
        return k(())

    elif type(exp) == str and exp.isdigit():
        return k(int(exp))

    elif exp == 'true' or exp == 'false':
        return k(exp)
    
    elif type(exp) == str:
        return apply_env(exp, env, k)
    
    elif exp[0] == "zero?":
        def k1(v1):
            if v1 == 0:
                return k("true")
            else:
                return k("false")
        return eval(exp[1], env, k1)

    elif exp[0] == "*":
        def k1(v1):
            def k2(v2):
                return k(v1 * v2)
            return eval(exp[2], env, k2)
        return eval(exp[1], env, k1)

    elif exp[0] == "+":
        def k1(v1):
            def k2(v2):
                return k(v1 + v2)
            return eval(exp[2], env, k2)
        return eval(exp[1], env, k1)

    elif exp[0] == "-":
        def k1(v1):
            def k2(v2):
                return k(v1 - v2)
            return eval(exp[2], env, k2)
        return eval(exp[1], env, k1)

    elif exp[0] == "if":
        def k1(vch):
            if vch == 'true':
                return eval(exp[2], env, k)
            else:
                return eval(exp[3], env, k)
        return eval(exp[1], env, k1)

    elif exp[0] == "cons":
        def k1(v1):
            def k2(v2):
                return k((v1, v2))
            return eval(exp[2], env, k2)
        return eval(exp[1],env, k1)

    elif exp[0] == "car":
        def k1(v1):
            return k(v1[0])
        return eval(exp[1], env, k1)

    elif exp[0] == "cdr":
        def k1(v1):
            return k(v[1:])
        eval(exp[1], env, k1)

    elif exp[0] == "begin2":
        def k1(v1):
            return eval(exp[2], env, k)
        return eval(exp[1], env, k1)

    elif exp[0] == "lambda":
        return closure(exp[2], exp[1][0], env, k)

    elif exp[0] == "let":
        def k1(env1):
            eval(exp[2], env1)
        return extend_env(exp[1][0], exp[1][1], env, k1)

    else:
        def k1(v1):
            def k2(v2):
                apply_closure(v1, v2, k)
            return eval(exp[1], env, k2)
        return eval(exp[0], env, k1)

def closure (exp, var, env, k):
    return k((exp, var, env))

def apply_closure(closure, arg, k):
    body, var, env = closure
    def k1(env1):
        return eval(body, env1, k)
    return extend_env(var, arg, env, k1)

def extend_env (id, arg, env, k):
    new_env = env.copy()
    new_env[id] = arg
    return k(new_env)

def apply_env(exp, env, k):
    return k(env[exp])

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

    #parse(src)
    def halt(v):
        print v
        return v
    return eval(parse(src), {}, halt)

def target(*args):
    return entry_point, None

if __name__ == "__main__":
    import sys
    print entry_point(sys.argv)

