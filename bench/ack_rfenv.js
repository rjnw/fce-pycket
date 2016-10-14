

ack_fun = function (env, m, n){
    ack_env = null;
    with(env) {
        ack = ack_env;
    }
    with (env) {
        if (m == 0) {
            return n+1;
        } else if (n == 0) {
            return ack(env, (m - 1), 1);
        } else {
            var new_n = ack(env, m, (n - 1))
            return ack(env, (m - 1), new_n);
        }
    }
}
var env = {'ack_env':ack_fun}
for (i = 0; i < 5; i++) {
    ack_fun(env, 3, 9);
}
console.time("cpu time");
for (i = 0; i < 10; i++) {
    ack_fun(env, 3, 9);
}
console.timeEnd("cpu time");
