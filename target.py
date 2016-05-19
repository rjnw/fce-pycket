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
        ast = tuple(tokens)
    else:
        prev_ind = curr_ind
        while st[curr_ind] not in string.whitespace+')':
            curr_ind += 1
        ast = st[prev_ind:curr_ind]
    return ast, curr_ind

def convert_to_ast(st):
    t, i = parse_exp(st, 0)
    return t

def zero_k(k, v):
    if v == 0:
        return apply_k, (k, 'true')
    else:
        return apply_k, (k, 'false')

def mult_k1(env, exp_2, k, v1):
    return eval, (exp_2, env, (mult_k2, k, v1))

def mult_k2(k, v1, v2):
    return apply_k, (k, v1*v2)

def add_k1(env, exp_2, k, v1):
    return eval, (exp_2, env, (add_k2, k, v1))

def add_k2(k, v1, v2):
    return apply_k, (k, v1+v2)

def sub_k1(env, exp_2, k, v1):
    return eval, (exp_2, env, (sub_k2, k, v1))

def sub_k2(k, v1, v2):
    return apply_k, (k, v1-v2)

def if_k(exp2, exp3, env, k, v):
    if v == 'true':
        return eval, (exp2, env, k)
    else:
        return eval, (exp3, env, k)

def cons_k1(exp2, env, k, v1):
    return eval, (exp2, env, (cons_k2, v1, k))

def cons_k2(v1, k, v2):
    return apply_k, (k, (v1, v2))

def car_k(k, v):
    return apply_k, (k, v[0])

def cdr_k(k, v):
    return apply_k, (k, v[1])

def begin_k(exp2, env, k, v):
    return eval, (exp2, env, k)

def let_k(exp2, k, env):
    return eval, (exp2, env, k)

def app_k1(exp1, env, k, v1):
    return eval, (exp1, env, (app_k2, v1, k))

def app_k2(v1, k, v2):
    return apply_closure, (v1, v2, k)

def apply_k(closure, val):
    func = closure[0]
    return apply, (func, closure[1:]+(val,))

def eval(exp, env, k):
    if exp == ():
        return apply_k, (k, ())

    elif type(exp) == str and exp.isdigit():
        return apply_k, (k, int(exp))

    elif exp == 'true' or exp == 'false':
        return apply_k, (k, exp)
    
    elif type(exp) == str:
        return apply_env, (exp, env, k)
    
    elif exp[0] == "zero?":
        return eval, (exp[1], env, (zero_k, k))

    elif exp[0] == "*":
        return eval, (exp[1], env, (mult_k1, env, exp[2], k))

    elif exp[0] == "+":
        return eval, (exp[1], env, (add_k1, env, exp[2], k))

    elif exp[0] == "-":
        return eval, (exp[1], env, (sub_k1, env, exp[2], k))

    elif exp[0] == "if":
        return eval, (exp[1], env, (if_k, exp[2], exp[3], env, k))

    elif exp[0] == "cons":
        return eval, (exp[1], env, (cons_k1, exp[2], env, k))

    elif exp[0] == "car":
        return eval, (exp[1], env, (car_k, k))

    elif exp[0] == "cdr":
        return eval, (exp[1], env, (cdr_k, k))

    elif exp[0] == "begin2":
        return eval, (exp[1], env, (begin_k, exp[2], env, k))

    elif exp[0] == "lambda":
        return create_closure, (exp[2], exp[1][0], env, k)

    elif exp[0] == "let":
        return extend_env, (exp[1][0], exp[1][1], env, (let_k, exp[2], k))

    else:
        return eval, (exp[0], env, (app_k1, exp[1], env, k))

def create_closure (exp, var, env, k):
    return apply_k, (k, (exp, var, env))

def closure_k(body, k, env):
    return eval, (body, env, k)

def apply_closure(closure, arg, k):
    body, var, env = closure
    return extend_env, (var, arg, env, (closure_k, body, k))

def extend_env (id, arg, env, k):
    new_env = env.copy()
    new_env[id] = arg
    return apply_k, (k, new_env)

def apply_env(exp, env, k):
    return apply_k, (k, env[exp])

def halt_k(v):
    return 'halt', v

def eval_loop(eval_pair):
    func, args = eval_pair
    while func != 'halt':
        #print 'eval_loop: ', func, args
        #pdb.set_trace()
        func, args = apply(func, args)
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
    return eval_loop((eval, (convert_to_ast(src), {}, (halt_k,))))

def target(*args):
    return entry_point, None

if __name__ == "__main__":
    import sys
    print entry_point(sys.argv)

