from prim import *
from rpython.rlib.jit import JitDriver

def get_printable_location(exp):
    return exp.tostring()
def get_printable_location_k(k_exp):
    return k_exp.tostring()

jitdriver = JitDriver(greens=['exp'],
                      reds=['env_struct', 'env_values', 'k', 'state'],
                      get_printable_location=get_printable_location)


def eval(state):
    exp, env_s, env_v, k = state.get_jit_vars()
    while True:
        jitdriver.jit_merge_point(exp=exp, env_struct=env_s, env_values=env_v, k=k, state=state)
        # state.merge_point(jitdriver)
        state = state.step()
        # state.enter_jit(jitdriver)
        exp, env_s, env_v, k = state.get_jit_vars()
        if state.should_enter():
            jitdriver.can_enter_jit(exp=exp, env_struct=env_s, env_values=env_v, k=k, state=state)
