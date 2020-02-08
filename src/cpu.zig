const std = @import("std");
const mem = std.mem;
const testing = std.testing;

/// An enumeration of all the word sized registers of the Z80 processor.
pub const WordRegister = enum(usize) {
    AF = 0, AF_ = 2,
    BC = 4, BC_ = 6,
    DE = 8, DE_ = 10,
    HL = 12, HL_ = 14,
    IX = 18, IY = 20,
    SP = 22, PC = 24,
};

/// An enumeration of all the byte sized registers of the Z80 processor.
pub const ByteRegister = enum(usize) {
    A = 1, F = 0,
    B = 5, C = 4,
    D = 9, E = 8,
    H = 13, L = 12,
    I = 16, R = 17,
};

/// A data structure containing all the state of a Z80 processor.
pub const CPU = struct {
    const Self = @This();

    fn RegisterForType(comptime T: type) type {
        return switch (T) {
            u8 => ByteRegister,
            i8 => ByteRegister,
            u16 => WordRegister,
            i16 => WordRegister,
            else => @compileError("Invalid data type " ++ @typeName(T) ++ " for a Z80 register"),
        };
    }

    _allocator: *mem.Allocator,
    _registers: [26]u8,

    /// Creates a new CPU with the given allocator.
    pub fn create(allocator: *mem.Allocator) !*Self {
        var self = try allocator.create(Self);
        errdefer allocator.destroy(self);

        self._allocator = allocator;
        mem.set(u8, self._registers[0..], 0);

        return self;
    }

    /// Destroys the given CPU.
    pub fn destroy(self: *Self) void {
        self._allocator.destroy(self);
    }

    /// Reads a value from the given register.
    pub fn read(self: *const Self, comptime T: type, register: RegisterForType(T)) T {
        return switch (T) {
            u8 => self._registers[@enumToInt(register)],
            i8 => @bitCast(i8, self.read(u8, register)),
            u16 => blk: {
                var low_byte = @intCast(u16, self._registers[@enumToInt(register)]);
                var high_byte = @intCast(u16, self._registers[@enumToInt(register) + 1]);
                break :blk (high_byte << 8) | low_byte;
            },
            i16 => @bitCast(i16, self.read(u16, register)),
            else => @compileError("Invalid data type " ++ @typeName(T) ++ " for a Z80 register"),
        };
    }

    /// Writes a value to the given register.
    pub fn write(self: *Self, comptime T: type, register: RegisterForType(T), value: T) void {
        switch (T) {
            u8 => self._registers[@enumToInt(register)] = value,
            i8 => self.write(u8, register, @bitCast(u8, value)),
            u16 => {
                self._registers[@enumToInt(register)] = @intCast(u8, value % 256);
                self._registers[@enumToInt(register) + 1] = @intCast(u8, (value >> 8) % 256);
            },
            i16 => self.write(u16, register, @bitCast(u16, value)),
            else => @compileError("Invalid data type " ++ @typeName(T) ++ " for a Z80 register"),
        }
    }
};

test "cpu is zero initialized" {
    var cpu = try CPU.create(std.heap.page_allocator);
    defer cpu.destroy();

    var expected: [26]u8 = undefined;
    mem.set(u8, expected[0..], 0);
    testing.expectEqualSlices(u8, expected[0..], cpu._registers[0..]);
}

test "cpu registers can be read from" {
    var cpu = try CPU.create(std.heap.page_allocator);
    defer cpu.destroy();

    testing.expectEqual(@intCast(u16, 0), cpu.read(u16, .AF));
    testing.expectEqual(@intCast(i8, 0), cpu.read(i8, .I));
    testing.expectEqual(@intCast(u16, 0), cpu.read(u16, .PC));
}

test "cpu registers can be writen to" {
    var cpu = try CPU.create(std.heap.page_allocator);
    defer cpu.destroy();

    cpu.write(u16, .BC, 0xCAFE);
    testing.expectEqual(@intCast(u16, 0xCAFE), cpu.read(u16, .BC));

    cpu.write(i8, .R, -13);
    testing.expectEqual(@intCast(i8, -13), cpu.read(i8, .R));

    cpu.write(i16, .HL, -300);
    testing.expectEqual(@bitCast(u16, @intCast(i16, -300)), cpu.read(u16, .HL));
}

test "cpu word registers that are modified impact byte registers" {
    var cpu = try CPU.create(std.heap.page_allocator);
    defer cpu.destroy();

    cpu.write(u16, .BC, 0xCAFE);
    testing.expectEqual(@intCast(u8, 0xCA), cpu.read(u8, .B));
    testing.expectEqual(@intCast(u8, 0xFE), cpu.read(u8, .C));

    cpu.write(u16, .AF, 0xDEAD);
    testing.expectEqual(@intCast(u8, 0xDE), cpu.read(u8, .A));
    testing.expectEqual(@intCast(u8, 0xAD), cpu.read(u8, .F));
}

test "cpu byte registers that are modified impact word registers" {
    var cpu = try CPU.create(std.heap.page_allocator);
    defer cpu.destroy();

    cpu.write(u8, .H, 0xBA);
    cpu.write(u8, .L, 0xBE);
    testing.expectEqual(@intCast(u16, 0xBABE), cpu.read(u16, .HL));

    cpu.write(u8, .A, 0x12);
    cpu.write(u8, .F, 0x34);
    testing.expectEqual(@intCast(u16, 0x1234), cpu.read(u16, .AF));
}

test "cpu registers can be interpreted as either signed or unsigned" {
    var cpu = try CPU.create(std.heap.page_allocator);
    defer cpu.destroy();

    cpu.write(u16, .AF, 0x1234);
    testing.expectEqual(@intCast(i16, 0x1234), cpu.read(i16, .AF));

    cpu.write(u16, .AF, 0xFFFF);
    testing.expectEqual(@intCast(i16, -1), cpu.read(i16, .AF));

    cpu.write(u8, .D, 42);
    testing.expectEqual(@intCast(i8, 42), cpu.read(i8, .D));

    cpu.write(u8, .D, 0xFE);
    testing.expectEqual(@intCast(i8, -2), cpu.read(i8, .D));
}
