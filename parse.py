import string
from ast import NumberAST, SexpAST, SymbolAST, PrimSexpAST
from values import PRIM_NAMES
def parse_exp(st, curr_ind, captured_env=False):
    while st[curr_ind] in string.whitespace:
        curr_ind += 1
    if st[curr_ind] == '(':
        tokens = []
        while st[curr_ind] != ')':
            if captured_env or (len(tokens) > 1 and with_captured_env(tokens[0])):
                t, i = parse_exp(st, curr_ind+1, True)
            else:
                t, i = parse_exp(st, curr_ind+1, False)
            curr_ind = i
            tokens.append(t)
        curr_ind += 1
        ast = get_ast(tokens, captured_env)#SexpAST(list(tokens))
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
            ast = SymbolAST(ast_st)
    return ast, curr_ind

def convert_to_ast(st):
    t, i = parse_exp(st, 0)
    return t

def get_ast(tokens, captured_env):
    if (not captured_env) and \
       isinstance(tokens[0], SymbolAST) and \
       tokens[0].string_value in PRIM_NAMES:
        return PrimSexpAST(list(tokens))
    else:
        return SexpAST(list(tokens))

def with_captured_env(token):
    return isinstance(token, SymbolAST) and token.string_value in ('with-captured-environment')
