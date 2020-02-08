const std = @import("std");
const testing = std.testing;

pub usingnamespace @import("./cpu.zig");
pub usingnamespace @import("./bus.zig");

export fn add(a: i32, b: i32) i32 {
    return a + b;
}

test "basic add functionality" {
    testing.expect(add(3, 7) == 10);
}
