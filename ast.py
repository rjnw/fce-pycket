class AST(object):
    _immutable_fields_ = ['string_value', 'number_value', 'children']
    #TODO fix parse to make the array immutable as well
    pass

class SymbolAST(AST):
    def __init__(self, symbol_str):
        self.string_value = symbol_str
    def tostring(self):
        return self.string_value

class NumberAST(AST):
    def __init__(self, num):
        self.number_value = num
    def tostring(self):
        return str(self.number_value)

class SexpAST(AST):
    def __init__(self, children):
        self.children = children
    def __getitem__(self, key):
        return self.children[key]
    def tostring(self):
        return '(' + ' '.join([ast.tostring() for ast in self.children]) + ')'

