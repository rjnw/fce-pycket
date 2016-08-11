from prim import Number, app_k, env_lookup

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
        return global_symbol_table.num_to_symbol[self.string_value]
    def eval(self, env, k):
        return k.plug_reduce(env_lookup(env, self.string_value))

class SymbolTable(object):
    def __init__(self):
        self.symbol_to_ast = {}
        self.num_to_symbol = {}
        self.count = 1

    def make_symbol_ast(self, symbol):
        if symbol in self.symbol_to_ast:
            return self.symbol_to_ast[symbol]
        else:
            ast = SymbolAST(self.count)
            self.symbol_to_ast[symbol] = ast
            self.num_to_symbol[self.count] = symbol
            self.count += 1
            return ast

global_symbol_table = SymbolTable()

def get_string_value(s_ast):
    return s_ast.string_value

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
    ev = env_lookup(env, self.children[0].string_value)
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

class ModuleAST(AST):
    _attrs_ = ['children', 'should_enter']
    _immutable_fields_ = ['children', 'should_enter']
    def __init__(self, children):
        self.children = children
        self.should_enter = False
    def eval(self, env, k):
        return self.chlidren[0], env, module_k(self.children, 1, len(self.children), env, k)

class PrimSexpAST(AST):
    _attrs_ = ['children', 'should_enter', '_evaluator']
    _immutable_fields_ = ['children[*]', '_evaluator', 'should_enter']
    def __init__(self, children, init_env):
        self.children = children
        self.should_enter = False
        self._evaluator = env_lookup(init_env, self.children[0].string_value)

    def __getitem__(self, key):
        return self.children[key]

    def tostring(self):
        return '(' + ' '.join([ast.tostring() for ast in self.children]) + ')'

    def eval(self, env, k):
        return self._evaluator.evaluate(self.children, env, k)
