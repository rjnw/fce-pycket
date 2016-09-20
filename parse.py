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
    elif st[curr_ind] in ';#':
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

lambda_symbol = global_symbol_table.make_symbol_ast('lambda')
let_symbol = global_symbol_table.make_symbol_ast('let')
letrec_symbol = global_symbol_table.make_symbol_ast('letrec')
if_symbol = global_symbol_table.make_symbol_ast('if')
begin_symbol = global_symbol_table.make_symbol_ast('begin')

def get_ast(tokens, captured_env, init_env):
    rator = tokens[0]
    if not isinstance(rator, SymbolAST):
        return SexpAST(list(tokens))
    elif rator.string_value == lambda_symbol.string_value:
        vrs = tokens[1]
        assert isinstance(vrs, SexpAST)
        return LambdaAST(vrs.children, tokens[2])
    elif rator.string_value == let_symbol.string_value:
        vr_vl = tokens[1]
        assert isinstance(vr_vl, SexpAST)
        vars = [e[0] for e in vr_vl.children]
        var_vals = [e[1] for e in vr_vl.children]
        return LetAST(vars, var_vals, tokens[2])
    elif rator.string_value == letrec_symbol.string_value:
        var = tokens[1][0][0]
        var_val = tokens[1][0][1]
        return LetrecAST(var, var_val, tokens[2])
    elif rator.string_value == if_symbol.string_value:
        return IfAST(tokens[1], tokens[2], tokens[3])
    # elif rator.string_value == begin_symbol.string_value:
    #     return BeginAST(list(tokens[1:]))
    else:
        return SexpAST(list(tokens))

def with_captured_env(token):
    return isinstance(token, SymbolAST) and \
        (token.string_value == global_symbol_table.make_symbol_ast('with-captured-environment').string_value)
