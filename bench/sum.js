function sum(n){
    var total = 0;
    for (i = 0; i <= n; i++) {
        total = total + i;
    }
    return total;
}
console.time("cpu time");
for (i = 0; i < 10003; i++) {
    sum(10000);
}
console.timeEnd("cpu time");
