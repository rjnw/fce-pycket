
function fib(n) {
    if (n < 2) {
        return n;
    } else {
        return fib(n - 1) + fib(n - 2);
    }
}

for (i = 0; i <= 1000; i++){
    fib(10);
}

console.time("cpu time");

for (i = 0; i <= 500000; i++){
    fib(10);
}
console.timeEnd("cpu time");
