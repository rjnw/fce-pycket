#! /usr/bin/env python
# -*- coding: utf-8 -*-
#

from rpython.rlib.parsing.ebnfparse import parse_ebnf, make_parse_function
import pdb

class ASTVisitor(object):
    def visit_expr(self, node):
        if len(node.children) == 3:
            return node.children[1].visit(self)
        elif len(node.children) == 1 and node.children[0].symbol == 'VALUE':
            return node.children[0].additional_info
        else:
            pdb.set_trace()

    def visit__star_symbol0(self, node):
        if len(node.children) == 2 and \
           node.children[0].symbol == 'expr' and \
           node.children[1].symbol == '_star_symbol0':
            return (node.children[0].visit(self),)+node.children[1].visit(self)
        elif len(node.children) == 1:
            return (node.children[0].visit(self),)
        else:
            pdb.set_trace()


def convert_to_ast(st):
    regexs, rules, transformer = parse_ebnf("""
VALUE: "([a-zA-Z0-9\?\+\-\*])+";
IGNORE: "[ \\n\\t]";
expr: "(" expr * ")" | VALUE ;
""")
    parse = make_parse_function(regexs, rules)
    tree = parse(st)
    return tree.visit(ASTVisitor())

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
    return eval(convert_to_ast(src), {}, halt)

def target(*args):
    return entry_point, None

if __name__ == "__main__":
    import sys
    print entry_point(sys.argv)

