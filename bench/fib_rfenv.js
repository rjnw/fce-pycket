
var fib = function (n, env) {
    with(env) {
        fib = fib;
    }
    if (n < 2) {
        return n;
    } else {
        return fib(n - 1, env) + fib(n - 2, env);
    }
}

a = {"fib":fib};
for (i = 0; i <= 1000; i++){
    console.log(fib(10, a));
}

console.time("cpu time");
for (i = 0; i <= 500000; i++){
    fib(10, a);
}
console.timeEnd("cpu time");
