const std = @import("std");
const mem = std.mem;
const testing = std.testing;
const ArrayList = std.ArrayList;

/// An operation on the bus.
pub const BusOperation = enum {
    /// A request to read a byte from the bus.
    Read,
    /// A request to write a byte to the bus.
    Write,
};

/// An interface for anything that can be present on the bus.
pub const BusComponent = struct {
    /// A function to indicate if this interface should accept read or write
    /// requests from the bus for the given address.
    accept_fn: fn(bus_component: *const BusComponent, address: u16, operation: BusOperation) bool,

    /// A function to read a single byte from the bus given an address.
    read_fn: fn(bus_component: *const BusComponent, address: u16) u8,

    /// A function to write a single byte to the bus given an address.
    write_fn: fn(bus_component: *BusComponent, address: u16, byte: u8) void,

    /// Destroys this components.
    destroy_fn: fn(bus_component: *BusComponent) void,

    /// Indicates if the given operation at the given address is supported for
    /// this instance.
    pub inline fn accept(self: *const BusComponent, address: u16, operation: BusOperation) bool {
        return self.accept_fn(self, address, operation);
    }

    /// Reads the byte value at the given address.
    pub inline fn read(self: *const BusComponent, address: u16) u8 {
        return self.read_fn(self, address);
    }

    /// Writes a byte value at the given address.
    pub inline fn write(self: *BusComponent, address: u16, byte: u8) void {
        self.write_fn(self, address, byte);
    }

    /// Destroys this bus component.
    pub inline fn destroy(self: *BusComponent) void {
        self.destroy_fn(self);
    }
};

/// A bus of components that can receive read and writes requests from a Z80
/// processor.
pub const Bus = struct {
    const Self = @This();

    _allocator: *mem.Allocator,
    components: ArrayList(*BusComponent),

    /// Creates a new bus using the given allocator to expand the list of bus
    /// components dynamically.
    pub fn create(allocator: *mem.Allocator) !*Self {
        var self = try allocator.create(Self);
        errdefer allocator.destroy(self);

        self._allocator = allocator;
        self.components = ArrayList(*BusComponent).init(allocator);

        return self;
    }

    /// Destroys this bus.
    pub fn destroy(self: *Self) void {
        self.components.deinit();
        self._allocator.destroy(self);
    }

    /// Reads a value of the given type at the given address from all the bus
    /// components.
    ///
    /// If multiple components respond to the same address, a bitwise *or* will
    /// be applied to fold their results together.
    pub fn read(self: *const Self, comptime T: type, address: u16 ) T {
        return switch (T) {
            u8 => blk: {
                var byte: u8 = 0;
                for (self.components.toSliceConst()) |component| {
                    if (component.accept(address, .Read)) {
                        byte = byte | component.read(address);
                    }
                }
                break :blk byte;
            },
            i8 => @bitCast(i8, self.read(u8, address)),
            u16 => blk: {
                var low_byte: u8 = 0;
                var high_byte: u8 = 0;
                const next_address = address +% 1;
                for (self.components.toSliceConst()) |component| {
                    if (component.accept(address, .Read)) {
                        low_byte = low_byte | component.read(address);
                    }

                    if (component.accept(next_address, .Write)) {
                        high_byte = high_byte | component.read(next_address);
                    }
                }
                break :blk (@intCast(u16, high_byte) << 8) | @intCast(u16, low_byte);
            },
            i16 => @bitCast(i16, self.read(u16, address)),
            else => @compileError("Invalid data type " ++ @typeName(T) ++ " to be read from the bus."),
        };
    }

    /// Writes a value of the given type at the given address of all the bus
    /// components.
    ///
    /// If multiple component respond to the same address, they will each be
    /// called at that address with the same value.
    pub fn write(self: *Self, comptime T: type, address: u16, value: T) void {
        switch (T) {
            u8 => {
                for (self.components.toSlice()) |component| {
                    if (component.accept(address, .Write)) {
                        component.write(address, value);
                    }
                }
            },
            i8 => self.write(u8, address, @bitCast(u8, value)),
            u16 => {
                const low_byte = @intCast(u8, value % 256);
                const high_byte = @intCast(u8, (value >> 8) % 256);
                const next_address = address +% 1;
                for (self.components.toSlice()) |component| {
                    if (component.accept(address, .Write)) {
                        component.write(address, low_byte);
                    }

                    if (component.accept(next_address, .Write)) {
                        component.write(next_address, high_byte);
                    }
                }
            },
            else => @compileError("Invalid data type " ++ @typeName(T) ++ " to be writen to the bus."),
        }
    }
};

fn memoryAccept(bus_component: *const BusComponent, address: u16, operation: BusOperation) bool {
    var self = @fieldParentPtr(Memory, "_component", bus_component);

    if (self._memory_type == .ReadWrite or (self._memory_type == .Read and operation == .Read) or (self._memory_type == .Write and operation == .Write)) {
        return address >= self._start_address and address < self._end_address;
    } else {
        return false;
    }
}

fn memoryRead(bus_component: *const BusComponent, address: u16) u8 {
    var self = @fieldParentPtr(Memory, "_component", bus_component);

    return self._memory[address - self._start_address];
}

fn memoryWrite(bus_component: *BusComponent, address: u16, byte: u8) void {
    var self = @fieldParentPtr(Memory, "_component", bus_component);

    self._memory[address - self._start_address] = byte;
}

fn memoryDestroy(bus_component: *BusComponent) void {
    var self = @fieldParentPtr(Memory, "_component", bus_component);

    self._allocator.free(self._memory);
    self._allocator.destroy(self);
}

/// A memory bus component that can either be read, write or both at the same time.
pub const Memory = struct {
    const Self = @This();

    /// The type of memory to construct.
    pub const MemoryType = enum {
        /// Read only memory
        Read,
        /// Write only memory
        Write,
        /// Read and write memory
        ReadWrite,
    };

    _allocator: *mem.Allocator,
    _start_address: u16,
    _end_address: u16,
    _memory: []u8,
    _component: BusComponent,
    _memory_type: MemoryType,

    /// Creates a new memory component for addresses in the given range.
    pub fn create(allocator: *mem.Allocator, memory_type: MemoryType, start_address: u16, end_address: u16) !*BusComponent {
        var self = try allocator.create(Self);
        errdefer allocator.destroy(self);

        self._component = .{
            .accept_fn = memoryAccept,
            .read_fn = memoryRead,
            .write_fn = memoryWrite,
            .destroy_fn = memoryDestroy,
        };

        self._allocator = allocator;
        self._start_address = start_address;
        self._end_address = end_address;
        self._memory_type = memory_type;

        self._memory = try allocator.alloc(u8, @intCast(usize, end_address - start_address));
        errdefer allocator.free(self._memory);

        mem.set(u8, self._memory, 0);

        return &self._component;
    }
};

test "bus can read an address to get the combination of all the components' values" {
    var bus = try Bus.create(std.heap.page_allocator);
    defer bus.destroy();

    var m1 = try Memory.create(std.heap.page_allocator, .ReadWrite, 0x100, 0x200);
    defer m1.destroy();

    var m2 = try Memory.create(std.heap.page_allocator, .ReadWrite, 0x100, 0x200);
    defer m2.destroy();

    try bus.components.append(m1);
    try bus.components.append(m2);

    m1.write(0x120, 0b10011001);
    m2.write(0x120, 0b01100110);

    testing.expectEqual(@intCast(u8, 0b11111111), bus.read(u8, 0x120));
}

test "bus can write to an address to set the value in all components" {
    var bus = try Bus.create(std.heap.page_allocator);
    defer bus.destroy();

    var m1 = try Memory.create(std.heap.page_allocator, .ReadWrite, 0x100, 0x200);
    defer m1.destroy();

    var m2 = try Memory.create(std.heap.page_allocator, .ReadWrite, 0x100, 0x200);
    defer m2.destroy();

    try bus.components.append(m1);
    try bus.components.append(m2);

    testing.expectEqual(@intCast(u8, 0), bus.read(u8, 0x150));

    bus.write(u8, 0x150, 42);
    testing.expectEqual(@intCast(u8, 42), m1.read(0x150));
    testing.expectEqual(@intCast(u8, 42), m2.read(0x150));
}

test "bus can split words between multiple components" {
    var bus = try Bus.create(std.heap.page_allocator);
    defer bus.destroy();

    var m1 = try Memory.create(std.heap.page_allocator, .ReadWrite, 0x0, 0x100);
    defer m1.destroy();

    var m2 = try Memory.create(std.heap.page_allocator, .ReadWrite, 0x100, 0x200);
    defer m2.destroy();

    try bus.components.append(m1);
    try bus.components.append(m2);

    bus.write(u16, 0x100 - 1, 0xCAFE);

    testing.expectEqual(@intCast(u8, 0xFE), m1.read(0x100 -1));
    testing.expectEqual(@intCast(u8, 0xCA), m2.read(0x100));
}

test "memory can only be read if declared as so" {
    var memory = try Memory.create(std.heap.page_allocator, .Read, 0x100, 0x200);
    defer memory.destroy();

    testing.expect(memory.accept(0x180, .Read));
    testing.expect(!memory.accept(0x180, .Write));
}

test "memory can only be written if declared as so" {
    var memory = try Memory.create(std.heap.page_allocator, .Write, 0x100, 0x200);
    defer memory.destroy();

    testing.expect(!memory.accept(0x180, .Read));
    testing.expect(memory.accept(0x180, .Write));
}

test "memory can accept any operations if both readable and writable" {
    var memory = try Memory.create(std.heap.page_allocator, .ReadWrite, 0x100, 0x200);
    defer memory.destroy();

    testing.expect(memory.accept(0x180, .Read));
    testing.expect(memory.accept(0x180, .Write));
}

test "memory is initialized to zero" {
    var memory = try Memory.create(std.heap.page_allocator, .Read, 0, 0x100);
    defer memory.destroy();

    var i: u16 = 0;
    while (i < 0x100) : (i += 1) {
        testing.expectEqual(@intCast(u8, 0), memory.read(i));
    }
}

test "memory can be written to" {
    var memory = try Memory.create(std.heap.page_allocator, .Write, 0, 0x100);
    defer memory.destroy();

    var i: u16 = 1;
    while (i <= 5) : (i += 1) {
        memory.write(0x30 + i, @intCast(u8, i));
    }

    testing.expectEqual(@intCast(u8, 1), memory.read(0x31));
    testing.expectEqual(@intCast(u8, 2), memory.read(0x32));
    testing.expectEqual(@intCast(u8, 3), memory.read(0x33));
    testing.expectEqual(@intCast(u8, 4), memory.read(0x34));
    testing.expectEqual(@intCast(u8, 5), memory.read(0x35));
}
