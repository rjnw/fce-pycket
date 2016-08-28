
class AST(object):
    def __init__(self, ast_type, continuation_structure):
        self.ast_type = ast_type
        self.continuation_structure = continuation_structure

class AST_TYPE(object):
    pass

class SEXP(AST_TYPE):
    def __init__(self, oprator, oprands):
        """
        operator: AST
        operands: [AST]
        """
        self.oprator = oprator
        self.oprands = oprands

class LAMBDA(AST_TYPE):
    def __init__(self, variables, body):
        """
        variables: [AST]
        body: AST
        """
        self.variables = variables
        self.body = body

class SYMBOL(AST_TYPE):
    def __init__(self, symbol_value):
        """
        symbol_value: int
        """
        self.symbol_value = symbol_value

class INTEGER(AST_TYPE):
    def __init__(self, int_value):
        """
        int_value: value.INT
        """
        self.int_value = int_value
