function sum(n){
    var total = 0;
    for (var i = 0; i <= n; i++) {
        total = total + i;
    }
    return total;
}
console.time("cpu time");
for (var i = 1; i < 10000; i++) {
    sum(10000);
}
console.timeEnd("cpu time");
