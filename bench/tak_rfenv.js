
tak_fun = function (env, x, y, z){
    with (env) {
        tak = tak_env;
    }
    if (y < x) {
        return tak(env,
                   tak(env, x - 1, y, z),
                   tak(env, y - 1, z, x),
                   tak(env, z - 1, x, y));
    } else {
        return z;
    }
}
var env = {'tak_env':tak_fun}

console.time("cpu time");
for (i = 0; i < 500; i++) {
    tak_fun(env, 18, 12, 6);
}
console.timeEnd("cpu time");
