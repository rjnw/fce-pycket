from values import Number, app_k

class AST(object):
    _immutable_fields_ = ['string_value', 'number_value', 'children[*]']
    #TODO fix parse to make the array immutable as well
    pass

class SymbolAST(AST):
    def __init__(self, symbol_str):
        self.string_value = symbol_str
    def tostring(self):
        return self.string_value
    def eval(self, env, k):
        return k.plug_reduce(env.lookup(self.string_value))

class NumberAST(AST):
    def __init__(self, num):
        self.number_value = Number(num)
    def tostring(self):
        return str(self.number_value.number_value)
    def eval(self, env, k):
        return k.plug_reduce(self.number_value)

def simple_sexp_eval(self, env, k):
    ev = env.lookup(self.children[0].string_value)
    return ev.evaluate(self.children, env, k)

def complex_sexp_eval(self, env, k):
    return self.children[0], env, app_k(self.children, env, k)

class SexpAST(AST):
    _immutable_fields_ = ['children[*]', '_eval']
    def __init__(self, children):
        self.children = children
        if type(self.children[0]) is SymbolAST:
            self._eval = simple_sexp_eval
        else:
            self._eval = complex_sexp_eval
    def __getitem__(self, key):
        return self.children[key]
    def tostring(self):
        return '(' + ' '.join([ast.tostring() for ast in self.children]) + ')'
    def eval(self, env, k):
        return self._eval(self, env, k)

