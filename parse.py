import string
from prim import *

def skip_whitespace(st, ind):
    while  ind < len(st) and st[ind] in string.whitespace:
        ind+=1
    return ind

def parse_exp(st, curr_ind, init_env, captured_env=False):
    curr_ind = skip_whitespace(st, curr_ind)
    if st[curr_ind] == '(':
        tokens = []
        while st[curr_ind] != ')':
            if captured_env or (len(tokens) > 1 and with_captured_env(tokens[0])):
                t, i = parse_exp(st, curr_ind+1, init_env, True)
            else:
                t, i = parse_exp(st, curr_ind+1, init_env, False)
            curr_ind = i
            tokens.append(t)
        curr_ind += 1
        ast = get_ast(tokens, captured_env, init_env)#SexpAST(list(tokens))
    elif st[curr_ind] == "'":
        t, i = parse_exp(st, curr_ind+1, init_env, captured_env)
        return SexpAST([global_symbol_table.make_symbol_ast('quote')] +[t]), i
    elif st[curr_ind] == ';':
        while st[curr_ind] != '\n':
            curr_ind += 1
        return parse_exp(st, curr_ind+1, init_env, captured_env)
    else:
        prev_ind = curr_ind
        while st[curr_ind] not in string.whitespace+')':
            curr_ind += 1
        ast_st = st[prev_ind:curr_ind]
        if ast_st.isdigit():
            ast = NumberAST(int(ast_st))
        else:
            ast = global_symbol_table.make_symbol_ast(ast_st)
    return ast, curr_ind

def convert_to_ast(st, init_env=None):
    t, i = parse_exp(st, 0, init_env)
    return t

def parse_module(st, init_env=None):
    i = 0
    asts = []
    while i < len(st):
        t, i = parse_exp(st, i, init_env)
        i = skip_whitespace(st, i)
        asts.append(t)
    return asts

def get_ast(tokens, captured_env, init_env):
    return SexpAST(list(tokens))
    if (not captured_env) and \
       isinstance(tokens[0], SymbolAST) and \
       tokens[0].string_value in PRIM_NAMES:
        return PrimSexpAST(list(tokens), init_env)
    else:
        return SexpAST(list(tokens))

def with_captured_env(token):
    return isinstance(token, SymbolAST) and \
        (token.string_value == global_symbol_table.make_symbol_ast('with-captured-environment').string_value)
