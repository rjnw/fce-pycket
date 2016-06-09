class AST(object):
    pass

# class AppAST(AST):
#     def __init__(self, rator, rand):
#         self.rator = rator
#         self.rand = rand

# class LambdaAST(AST):
#     def __init__(self, var, body):
#         self.var = var
#         self.body = body

# class letAST(AST):
#     def __init__(self, var, val, body):
#         self.var = var
#         self.val = val
#         self.body = body

# class callccAST(AST):
#     def __init__(self, expr):
#         self.exp = exp

# class wceAST(AST):
#     def __init__(self, env_exp, exp):
#         self.env_exp = env_exp
#         self.exp = exp

# class ceAST(AST):
#     def __init__(self, exp):
#         self.exp = exp

class SymbolAST(AST):
    def __init__(self, symbol_str):
        self.string_value = symbol_str

class NumberAST(AST):
    def __init__(self, num):
        self.number_value = num

class SexpAST(AST):
    def __init__(self, children):
        self.children = children
    def __getitem__(self, key):
        return self.children[key]
    def __str__(self):
        return str([str(ast) for ast in self.children])
