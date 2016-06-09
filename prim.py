nil = ()
true = 'true'
hash_true = true
false = 'false'
hash_false = false
def cons(car, cdr):
    return (car, cdr)
def car(ls):
    return ls[0]
def cdr(ls):
    return ls[1]
def add(e1, e2):
    return e1 + e2
def sub(e1, e2):
    return e1 - e2
def zero_huh(e):
    if e:
        return true
    else:
        return false
