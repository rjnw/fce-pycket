import string

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
        ast = SexpAST(tokens)
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


class AST(object):
    pass

class SexpAST(AST):
    def __init__(self, children):
        self.children = children
    def __getitem__(self, key):
        return self.children[key]
    def __str__(self):
        return str([str(ast) for ast in self.children])

class SymbolAST(AST):
    def __init__(self, symbol_str):
        self.string_value = symbol_str
    def __str__(self):
        return self.string_value

class NumberAST(AST):
    def __init__(self, num):
        self.number_value = num
    def __str__(self):
        return str(self.number_value)

class ClosureAST(AST):
    def __init__(self, exp, var, env):
        self.exp = exp
        self.var = var
        self.env = env
    def __str__(self):
        return 'ast: closure'

class Cont(ClosureAST):
    def __init__(self, closure_args, env, k, func):
        self.closure_args = closure_args
        self.env = env
        self.k = k
        self.func = func
    def __call__(self, v):
        return self.func(self.closure_args, self.env, self.k, v)
    def __str__(self):
        return 'ast: continuation'

def continuation(func):
    def new_f(cl_args, env, k):
        return Cont(cl_args, env, k, func)
    return new_f
