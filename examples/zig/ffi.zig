const std = @import("std");

const Point = extern struct {
    x: i64,
    y: i64,
};

export fn zig_add(a: i32, b: i32) i32 {
    return a + b;
}

export fn zig_dot(p: Point) i64 {
    return p.x * p.y;
}

export fn zig_sum_bytes(ptr: [*]const u8, len: usize) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < len) : (i += 1) {
        total += ptr[i];
    }
    return total;
}
