from ast import *
class Trampoline(object):
    def __init__(self, exp, env, kont):
        self.exp = exp
        self.env = env
        self.kont = kont

class Value(object):
    pass

def continuation(func):
    def new_f(cl_args, env, k):
        return Cont(cl_args, env, k, func)
    return new_f

@continuation
def cont_k(cl_args, env, k, v):
    kont = cl_args[0]
    return kont(v)

class Cont(Value):
    def __init__(self, closure_args, env, k, func):
        self.closure_args = closure_args
        self.env = env
        self.k = k
        self.func = func
    def __call__(self, v):
        return self.func(self.closure_args, self.env, self.k, v)
    def evaluate(self, rest_exps):
        rand = rest_exps[0]
        return Trampoline(rand, env, cont_k([self], env, k))
    def __str__(self):
        return 'value: continuation'

class Number(Value):
    def __init__(self, num):
        self.number_value = num
    def __str__(self):
        return str(self.number_value)

class Environment(Value):
    def __init__(self):
        self.var_map = {}
    def apply(self, key):
        return self.var_map[key.string_value]
    def extend(self, key, value):
        ne = Environment()
        ne.var_map = self.var_map.copy()
        ne.var_map[key.string_value] = value
        return ne

class Lambda(Value):
    def evaluate(self, rest_exps, env, k):
        arg = rest_exps[0][0]
        body = rest_exps[1]
        return k(Closure(body, arg, env))
    def __str__(self):
        return 'value: native lambda evaluator'

@continuation
def let_k(cl_args, env, k, v):
    id, body = cl_args
    new_env = env.extend(id, v)
    return Trampoline(body, new_env, k)

class Let(Value):
    def evaluate(self, rest_exps, env, k):
        var = rest_exps[0][0]
        val_exp = rest_exps[0][1]
        body_exp = rest_exps[1]
        return Trampoline(val_exp, env, let_k([var, body_exp], env, k))
    def __str__(self):
        return 'value: native let evaluator'

@continuation
def closure_k(cl_args, env, k, v):
    clos = cl_args[0]
    return Trampoline(clos.body, clos.env.extend(clos.var, v), k)

class Closure(Value):
    def __init__(self, body, var, env):
        self.body = body
        self.var = var
        self.env = env
    def __str__(self):
        return 'value: closure'
    def evaluate(self, rest_exps, env, k):
        rand = rest_exps[0]
        return Trampoline(rand, env, closure_k([self], env, k))


@continuation
def app_k(cl_args, env, k, v):
    return v.evaluate(cl_args, env, k)


def initial_environment():
    env = Environment()
    env = env.extend(SymbolAST('lambda'), Lambda())
    env = env.extend(SymbolAST('let'), Let())
    return env
