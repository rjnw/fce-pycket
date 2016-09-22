env = {}
function tak(x, y, z){
    with (env) {
        if (y < x) {
            return tak(tak(x - 1, y, z),
                       tak(y - 1, z, x),
                       tak(z - 1, x, y));
        } else {
            return z;
        }
    }
}
console.time("cpu time");
for (i = 0; i < 500; i++) {
    tak(18, 12, 6);
}
console.timeEnd("cpu time");
