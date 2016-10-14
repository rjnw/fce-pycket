
sum_fun = function (env, n){
    with(env) {
        var sum = sum_env;
    }
    var total = 0;
    for (var i = 0; i <= n; i++) {
        total = total + i;
    }
    return total;

}
var env = {'sum_env':sum_fun}

console.time("cpu time");
for (var i = 0; i < 10000; i++) {
    sum_fun(env, 10000);
}
console.timeEnd("cpu time");
