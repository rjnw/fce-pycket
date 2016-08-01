import string
from ast import NumberAST, SexpAST, SymbolAST, PrimSexpAST, global_symbol_table
from values import PRIM_NAMES

def parse_exp(st, curr_ind, init_env, captured_env=False):
    while st[curr_ind] in string.whitespace:
        curr_ind += 1
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
    # elif st[curr_ind] == "'":
    #     t,i = parse_exp(st, curr_ind+1)
    #     return ('quote', t), i
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

def convert_to_ast(st, init_env):
    t, i = parse_exp(st, 0, init_env)
    return t

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
