
function ack(m, n){
    if (m == 0) {
        return n+1;
    } else if (n == 0) {
        return ack((m - 1), 1);
    } else {
        var new_n = ack(m, (n - 1))
        return ack((m - 1), new_n);
    }
}
for (i = 0; i < 5; i++) {
    ack(3, 9);
}
console.time("cpu time");
for (i = 0; i < 10; i++) {
    ack(3, 9);
}
console.timeEnd("cpu time");
