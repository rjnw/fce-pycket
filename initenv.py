from prim import get_symbol_ast

class InitialEnvironment(object):
    def __init__(self):
        self.prim_names = []
        self.prim_values = []
    def add_prim(self, name, value):
        self.prim_names.append(get_symbol_ast(name))
        self.prim_values.append(value)

global_initial_env = InitialEnvironment()

def prim(*names):
    def decorate(cls):
        c = cls()
        for name in names:
            global_initial_env.add_prim(name, cls())
        return cls
    return decorate

def prim_value(name, value):
    global_initial_env.add_prim(name, value)
