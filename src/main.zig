//! An example of using Arviss from Zig.

const std = @import("std");
const arviss = @import("arviss");

const membase: u32 = 0;
const memsize: u32 = 0x8000;
const romsize: u32 = 0x4000;
const rambase: u32 = membase + romsize;
const ramsize: u32 = 0x4000;

const Memory = struct {
    mem: [memsize]u8,

    const Self = @This();

    const ttystatus: u32 = membase + memsize;
    const ttydata: u32 = ttystatus + 1;

    pub fn read8(self: *Self, addr: u32, bus_code: ?*arviss.BusCode) u8 {
        if (addr >= membase and addr < membase + memsize) {
            return self.mem[addr - membase];
        }
        if (addr == ttystatus) {
            return 0xff;
        }
        if (bus_code) |bc| {
            bc.* = arviss.BusCode.bcLOAD_ACCESS_FAULT;
        }
        return 0;
    }

    pub fn read16(self: *Self, addr: u32, bus_code: ?*arviss.BusCode) u16 {
        if (addr >= membase and addr < membase + memsize - 1) {
            return @intCast(u16, self.mem[addr - membase]) | (@intCast(u16, self.mem[addr - membase + 1]) << 8);
        }
        if (bus_code) |bc| {
            bc.* = arviss.BusCode.bcLOAD_ACCESS_FAULT;
        }
        return 0;
    }

    pub fn read32(self: *Self, addr: u32, bus_code: ?*arviss.BusCode) u32 {
        if (addr >= membase and addr < membase + memsize - 3) {
            return @intCast(u32, self.mem[addr - membase]) |
                (@intCast(u32, self.mem[addr - membase + 1]) << 8) |
                (@intCast(u32, self.mem[addr - membase + 2]) << 16) |
                (@intCast(u32, self.mem[addr - membase + 3]) << 24);
        }
        if (bus_code) |bc| {
            bc.* = arviss.BusCode.bcLOAD_ACCESS_FAULT;
        }
        return 0;
    }

    pub fn write8(self: *Self, addr: u32, value: u8, bus_code: ?*arviss.BusCode) void {
        if (addr >= rambase and addr < rambase + ramsize) {
            self.mem[addr - membase] = value;
            return;
        }
        if (addr == ttydata) {
            std.debug.print("{c}", .{value});
            return;
        }
        if (bus_code) |bc| {
            bc.* = arviss.BusCode.bcSTORE_ACCESS_FAULT;
        }
    }

    pub fn write16(self: *Self, addr: u32, value: u16, bus_code: ?*arviss.BusCode) void {
        if (addr >= rambase and addr < rambase + ramsize - 1) {
            self.mem[addr - membase] = @intCast(u8, value & 0xff);
            self.mem[addr - membase + 1] = @intCast(u8, (value >> 8) & 0xff);
            return;
        }
        if (bus_code) |bc| {
            bc.* = arviss.BusCode.bcSTORE_ACCESS_FAULT;
        }
    }

    pub fn write32(self: *Self, addr: u32, value: u32, bus_code: ?*arviss.BusCode) void {
        if (addr >= rambase and addr < rambase + ramsize - 3) {
            self.mem[addr - membase] = @intCast(u8, value & 0xff);
            self.mem[addr - membase + 1] = @intCast(u8, (value >> 8) & 0xff);
            self.mem[addr - membase + 2] = @intCast(u8, (value >> 16) & 0xff);
            self.mem[addr - membase + 3] = @intCast(u8, (value >> 24) & 0xff);
            return;
        }
        if (bus_code) |bc| {
            bc.* = arviss.BusCode.bcSTORE_ACCESS_FAULT;
        }
    }

    pub fn zeromem(self: *Self, addr: u32, len: u32) void {
        for (self.mem[addr - membase .. addr - membase + len]) |*b| {
            b.* = 0;
        }
    }

    pub fn writev(self: *Self, addr: u32, src: []u8, len: u32) void {
        std.mem.copy(u8, self.mem[addr - membase .. addr - membase + len], src);
    }
};

fn read8(token: arviss.BusToken, addr: u32, bus_code: *allowzero arviss.BusCode) callconv(.C) u8 {
    // std.debug.print("Read8 {} from {}\n", .{ token, addr });
    const mem = @ptrCast(*Memory, token.t);
    return mem.read8(addr, bus_code);
}

fn read16(token: arviss.BusToken, addr: u32, bus_code: *allowzero arviss.BusCode) callconv(.C) u16 {
    // std.debug.print("Read16 from {}\n", .{addr});
    const mem = @ptrCast(*Memory, token.t);
    return mem.read16(addr, bus_code);
}

fn read32(token: arviss.BusToken, addr: u32, bus_code: *allowzero arviss.BusCode) callconv(.C) u32 {
    // std.debug.print("Read32 from {}\n", .{addr});
    const mem = @ptrCast(*Memory, token.t);
    return mem.read32(addr, bus_code);
}

fn write8(token: arviss.BusToken, addr: u32, value: u8, bus_code: *allowzero arviss.BusCode) callconv(.C) void {
    const mem = @ptrCast(*Memory, token.t);
    mem.write8(addr, value, bus_code);
}

fn write16(token: arviss.BusToken, addr: u32, value: u16, bus_code: *allowzero arviss.BusCode) callconv(.C) void {
    const mem = @ptrCast(*Memory, token.t);
    mem.write16(addr, value, bus_code);
}

fn write32(token: arviss.BusToken, addr: u32, value: u32, bus_code: *allowzero arviss.BusCode) callconv(.C) void {
    const mem = @ptrCast(*Memory, token.t);
    mem.write32(addr, value, bus_code);
}

fn ArvissCpu(bus: arviss.Bus) arviss.ArvissCpu {
    var cpu: arviss.ArvissCpu = undefined;
    cpu.bus = bus;
    return cpu;
}

fn zeromem(token: arviss.ElfToken, addr: u32, len: u32) callconv(.C) void {
    const mem = @ptrCast(*Memory, token.t);
    mem.zeromem(addr, len);
}

fn writev(token: arviss.ElfToken, addr: u32, src: ?*c_void, len: u32) callconv(.C) void {
    const mem = @ptrCast(*Memory, token.t);
    const src_as_slice = @ptrCast([*]u8, src)[0..len];
    mem.writev(addr, src_as_slice, len);
}

pub fn main() !void {
    const filename = "hello";

    var mem: Memory = .{ .mem = undefined };
    const config: arviss.ElfLoaderConfig = .{
        .token = .{ .t = &mem.mem }, // Give the loader a token to pass back to us when it wants to access VM memory.
        .zeroMemFn = zeromem, // Tell the loader how to zero VM memory.
        .writeMemFn = writev, // Tell the loader how to write a vector of bytes to VM memory.
        // Tell the loader about the memory segments.
        .targetSegments = &[_]arviss.ElfSegmentDescriptor{
            .{ .start = membase, .size = romsize }, // ROM.
            .{ .start = rambase, .size = ramsize }, // RAM.
        },
        .numSegments = 2, // This is a little error prone.
    };
    const loader_result = arviss.LoadElf(filename, &config);
    if (loader_result != arviss.ElfResult.ER_OK) {
        return;
    }

    var c = ArvissCpu(.{
        .token = .{ .t = &mem },
        .Read8 = read8,
        .Read16 = read16,
        .Read32 = read32,
        .Write8 = write8,
        .Write16 = write16,
        .Write32 = write32,
    });

    arviss.ArvissReset(&c);
    var result = arviss.ArvissRun(&c, 1000000);
    while (result.type == arviss.ArvissResultType.rtOK) {
        result = arviss.ArvissRun(&c, 1000000);
    }

    // I know this isn't magic, but it's awesome how this prints the fields of a C struct and enums.
    std.debug.print("Result: {any}", .{result});
}
