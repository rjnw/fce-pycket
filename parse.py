import string
from ast import NumberAST, SexpAST, SymbolAST

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
        ast = SexpAST(list(tokens))
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

