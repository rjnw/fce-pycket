from values import Number, app_k, INIT_ENV

class AST(object):
    _attrs_ = ['should_enter', 'string_value']
    #TODO string_value should not be here but rpython doesn't believe my isinstance checks
    _immutable_fields_ = ['should_enter', 'string_value']
    pass

class SymbolAST(AST):
    _attrs_ = ['string_value', 'should_enter']
    _immutable_fields_ = ['string_value', 'should_enter']
    def __init__(self, symbol_str):
        self.string_value = symbol_str
        self.should_enter = False
    def tostring(self):
        return self.string_value
    def eval(self, env, k):
        return k.plug_reduce(env.lookup(self.string_value))

class NumberAST(AST):
    _attrs_ = ['number_value', 'should_enter']
    _immutable_fields_ = ['number_value', 'should_enter']
    def __init__(self, num):
        self.number_value = Number(num)
        self.should_enter = False
    def tostring(self):
        return str(self.number_value.number_value)
    def eval(self, env, k):
        return k.plug_reduce(self.number_value)

def simple_sexp_eval(self, env, k):
    ev = env.lookup(self.children[0].string_value)
    return ev.evaluate(self.children, env, k)

def complex_sexp_eval(self, env, k):
    return self.children[0], env, app_k(self.children, env, k)

def prim_sexp_eval(self, env, k):
    return self._evaluator.evaluate(self.children, env, k)

class SexpAST(AST):
    _attrs_ = ['children', '_eval', 'should_enter']
    _immutable_fields_ = ['children[*]', '_eval', 'should_enter']

    def __init__(self, children, captured_env=False):
        self.children = children
        self.should_enter = False

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

class PrimSexpAST(AST):
    _attrs_ = ['children', 'should_enter', '_evaluator']
    _immutable_fields_ = ['children[*]', '_evaluator', 'should_enter']
    def __init__(self, children):
        self.children = children
        self.should_enter = False
        self._evaluator = INIT_ENV.lookup(self.children[0].string_value)

    def __getitem__(self, key):
        return self.children[key]

    def tostring(self):
        return '(' + ' '.join([ast.tostring() for ast in self.children]) + ')'

    def eval(self, env, k):
        return self._evaluator.evaluate(self.children, env, k)
