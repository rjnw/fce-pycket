class AST(object):
    pass

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

class SexpAST(AST):
    def __init__(self, children):
        self.children = children
    def __getitem__(self, key):
        return self.children[key]
    def __str__(self):
        return str([str(ast) for ast in self.children])
