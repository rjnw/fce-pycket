from prim import *
from rpython.rlib.jit import JitDriver

def get_printable_location(exp, env_struct):
    return exp.tostring()
def get_printable_location_k(k_exp):
    return k_exp.tostring()

exp_jitdriver = JitDriver(greens=['exp', 'env_struct'],
                      reds=['env_values', 'k'],
                      get_printable_location=get_printable_location)


k_jitdriver = JitDriver(greens=['k_exp'],
                        reds=['k','v'],
                        get_printable_location=get_printable_location_k)

def eval_exp(t):
    exp, env_s, env_v, k = t
    while True:
        exp_jitdriver.jit_merge_point(exp=exp, env_struct=env_s, env_values=env_v, k=k)

        exp, env_s, env_v, k = exp.eval(env_s, env_v, k)

        if exp.should_enter == True:
            exp_jitdriver.can_enter_jit(exp=exp, env_struct=env_s, env_values=env_v, k=k)

def plug_reduce(t):
    k, v = t
    while True:
        k_jitdriver.jit_merge_point(k_exp=k.exp, k=k, v=v)
        k, v = k.plug_reduce(v)
        k_jitdriver.can_enter_jit(k_exp=k.exp, k=k, v=v)

def eval(x):
    while True:
        try:
            if isinstance(x, EvalExp):
                eval_exp(x.t)
            elif isinstance(x, PlugReduce):
                plug_reduce(x.t)
            else:
                raise Exception('unknown state')
        except PlugReduce, pr:
            x = pr
        except EvalExp, ee:
            x = ee
