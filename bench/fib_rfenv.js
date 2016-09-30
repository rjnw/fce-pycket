
fib_fun = function (n, env) {
    with(env) {
        var fib = fib_env;
    }
    if (n < 2) {
        return n;
    } else {
        return fib(n - 1, env) + fib(n - 2, env);
    }
}

a = {"fib_env":fib_fun};
for (i = 0; i <= 1000; i++){
    fib_fun(10, a);
}

console.time("cpu time");
for (i = 0; i <= 500000; i++){
    fib_fun(10, a);
}
console.timeEnd("cpu time");
