const std = @import("std");
const arviss = @import("arviss");
const Opcode = arviss.ArvissOpcode;

const ArvissExecute = arviss.ArvissExecute;
const testing = std.testing;

const membase: u32 = 0;
const memsize: u32 = 0x8000;
const romsize: u32 = 0x4000;
const rambase: u32 = membase + romsize;
const ramsize: u32 = 0x4000;

const Memory = struct {
    mem: [memsize]u8,

    const Self = @This();

    fn read8(self: *Self, addr: u32, bus_code: ?*arviss.BusCode) u8 {
        if (addr >= membase and addr < membase + memsize) {
            return self.mem[addr - membase];
        }
        if (bus_code) |bc| {
            bc.* = arviss.BusCode.bcLOAD_ACCESS_FAULT;
        }
        return 0;
    }

    fn read16(self: *Self, addr: u32, bus_code: ?*arviss.BusCode) u16 {
        if (addr >= membase and addr < membase + memsize - 1) {
            return @intCast(u16, self.mem[addr - membase]) | (@intCast(u16, self.mem[addr - membase + 1]) << 8);
        }
        if (bus_code) |bc| {
            bc.* = arviss.BusCode.bcLOAD_ACCESS_FAULT;
        }
        return 0;
    }

    fn read32(self: *Self, addr: u32, bus_code: ?*arviss.BusCode) u32 {
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

    fn write8(self: *Self, addr: u32, value: u8, bus_code: ?*arviss.BusCode) void {
        if (addr >= rambase and addr < rambase + ramsize) {
            self.mem[addr - membase] = value;
            return;
        }
        if (bus_code) |bc| {
            bc.* = arviss.BusCode.bcSTORE_ACCESS_FAULT;
        }
    }

    fn write16(self: *Self, addr: u32, value: u16, bus_code: ?*arviss.BusCode) void {
        if (addr >= rambase and addr < rambase + ramsize - 1) {
            self.mem[addr - membase] = @intCast(u8, value & 0xff);
            self.mem[addr - membase + 1] = @intCast(u8, (value >> 8) & 0xff);
            return;
        }
        if (bus_code) |bc| {
            bc.* = arviss.BusCode.bcSTORE_ACCESS_FAULT;
        }
    }

    fn write32(self: *Self, addr: u32, value: u32, bus_code: ?*arviss.BusCode) void {
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

inline fn encodeRs1(n: u32) u32 {
    return n << 15;
}

inline fn encodeRs2(n: u32) u32 {
    return n << 20;
}

inline fn encodeRd(n: u32) u32 {
    return n << 7;
}

inline fn encodeJ(n: anytype) u32 {
    switch (@TypeOf(n)) {
        u32 => {
            return ((n & 0x100000) << 11) // imm[20]    -> j[31]
            | ((n & 0x7fe) << 20) // imm[10:1]  -> j[30:21]
            | ((n & 0x800) << 9) // imm[11]    -> j[20]
            | (n & 0x000ff000) // imm[19:12] -> j[19:12]
            ;
        },
        else => {
            return encodeJ(asU32(n));
        },
    }
}

inline fn encodeB(n: u32) u32 {
    return ((n & 0x1000) << 19) // imm[12]   -> b[31]
    | ((n & 0x7e0) << 20) // imm[10:5] -> b[30:25]
    | ((n & 0x1e) << 7) // imm[4:1]  -> b[11:4]
    | ((n & 0x800) >> 4) // imm[11]   -> b[7]
    ;
}

inline fn encodeS(n: anytype) u32 {
    switch (@TypeOf(n)) {
        u32 => {
            return ((n & 0xfe0) << 20) // imm[11:5] -> s[31:25]
            | ((n & 0x1f) << 7) // imm[4:0]  -> s[11:7]
            ;
        },
        else => {
            return encodeS(asU32(n));
        },
    }
}

inline fn encodeI(n: anytype) u32 {
    switch (@TypeOf(n)) {
        u32 => {
            return (n & 0xfff) << 20; // imm[11:0] -> s[31:20]
        },
        else => {
            return encodeI(asU32(n));
        },
    }
}

inline fn opcodeAsU32(comptime opcode: Opcode) u32 {
    return @enumToInt(opcode);
}

inline fn asU32(n: anytype) u32 {
    switch (@TypeOf(n)) {
        comptime_int => {
            return @intCast(u32, n);
        },
        u32 => {
            return n;
        },
        i32 => {
            return @bitCast(u32, n);
        },
        u64 => {
            return @intCast(u32, n);
        },
        else => {
            @compileError("Unable to convert " ++ @typeName(@TypeOf(n)) ++ " to u32");
        },
    }
}

inline fn asI32(n: anytype) i32 {
    switch (@TypeOf(n)) {
        comptime_int => {
            return @intCast(u32, n);
        },
        i32 => {
            return n;
        },
        u32 => {
            return @bitCast(i32, n);
        },
        i64 => {
            return @intCast(i32, n);
        },
        else => {
            @compileError("Unable to convert " ++ @typeName(@TypeOf(n)) ++ " to i32");
        },
    }
}

inline fn asU8(n: anytype) u8 {
    switch (@TypeOf(n)) {
        comptime_int => {
            return @intCast(u8, n);
        },
        u32 => {
            return @intCast(u8, n);
        },
        else => {
            @compileError("Unable to convert " ++ @typeName(@TypeOf(n)) ++ " to u8");
        },
    }
}

inline fn asU16(n: anytype) u16 {
    switch (@TypeOf(n)) {
        comptime_int => {
            return @intCast(u16, n);
        },
        u32 => {
            return @intCast(u16, n);
        },
        else => {
            @compileError("Unable to convert" ++ @typeName(@TypeOf(n)) ++ " to u16");
        },
    }
}

var memory: Memory = .{ .mem = undefined };

fn ArvissCpu(mem: *Memory) arviss.ArvissCpu {
    var cpu: arviss.ArvissCpu = undefined;
    arviss.ArvissReset(&cpu);
    cpu.pc = rambase;
    cpu.xreg[2] = rambase + ramsize; // Set the stack pointer.
    cpu.bus = .{
        .token = .{ .t = mem },
        .Read8 = read8,
        .Read16 = read16,
        .Read32 = read32,
        .Write8 = write8,
        .Write16 = write16,
        .Write32 = write32,
    };
    return cpu;
}

fn Cpu() arviss.ArvissCpu {
    return ArvissCpu(&memory);
}

test "lui" {
    var cpu = Cpu();

    // rd <- imm_u, pc <- pc + 4
    const values = .{ 0, 1, -1, 1234, -1234, -(1 << 19), (1 << 19) - 1 };
    inline for (values) |value| {
        const imm_u: i32 = value;
        const rd: u32 = 3;
        const pc: u32 = cpu.pc;

        _ = ArvissExecute(&cpu, asU32(imm_u << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opLUI));

        // rd <- imm_u
        try testing.expectEqual(imm_u, asI32(cpu.xreg[rd]) >> 12);

        // pc <- pc + 4
        try testing.expectEqual(pc + 4, cpu.pc);
    }
}

test "lui leaves x0 as zero" {
    var cpu = Cpu();

    const imm_u: i32 = 123;
    const rd: u32 = 0;
    const pc: u32 = cpu.pc;

    _ = ArvissExecute(&cpu, asU32(imm_u << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opLUI));

    // x0 <- 0
    try testing.expectEqual(asU32(0), cpu.xreg[0]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);
}

test "auipc" {
    var cpu = Cpu();

    // rd <- pc + imm_u, pc <- pc + 4
    const values = .{ 0, 1, -1, 1234, -1234, -(1 << 19), (1 << 19) - 1 };
    inline for (values) |value| {
        const imm_u: i32 = value;
        const rd: u32 = 9;
        const pc: u32 = cpu.pc;

        _ = ArvissExecute(&cpu, asU32(imm_u << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opAUIPC));

        // rd <- pc + imm_u
        try testing.expectEqual(pc +% asU32(imm_u << 12), cpu.xreg[rd]);

        // pc <- pc + 4
        try testing.expectEqual(pc + 4, cpu.pc);
    }
}

test "auipc leaves x0 as zero" {
    var cpu = Cpu();

    const imm_u: i32 = 123;
    const rd: u32 = 0;
    const pc: u32 = cpu.pc;

    _ = ArvissExecute(&cpu, asU32(imm_u << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opAUIPC));

    // x0 <- 0
    try testing.expectEqual(asU32(0), cpu.xreg[0]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);
}

test "jal" {
    var cpu = Cpu();

    // rd <- pc + 4, pc <- pc + imm_j
    const values = .{ 0, -2, 2, -(1 << 20), (1 << 20) - 2 };
    inline for (values) |value| {
        const imm_j: i32 = value;
        const rd: u32 = 3;
        const pc: u32 = cpu.pc;

        _ = ArvissExecute(&cpu, encodeJ(imm_j) | encodeRd(rd) | opcodeAsU32(Opcode.opJAL));

        // rd <- pc + 4
        try testing.expectEqual(pc + 4, cpu.xreg[rd]);

        // pc <- pc + imm_j
        try testing.expectEqual(asU32(asI32(pc) + value), cpu.pc);
    }
}

test "jal leaves x0 as zero" {
    var cpu = Cpu();

    const value: i32 = 4;
    const imm_j: i32 = value;
    const rd: u32 = 0;
    const pc: u32 = cpu.pc;

    _ = ArvissExecute(&cpu, encodeJ(imm_j) | encodeRd(rd) | opcodeAsU32(Opcode.opJAL));

    // x0 <- 0
    try testing.expectEqual(asU32(0), cpu.xreg[0]);

    // pc <- pc + imm_j
    try testing.expectEqual(asU32(asI32(pc) + value), cpu.pc);
}

test "jalr" {
    var cpu = Cpu();

    // rd <- pc + 4, pc <- (rs1 + imm_i) & ~1
    const values = .{ 0, -1, 1, -(1 << 1), (1 << 11) - 1 };
    inline for (values) |value| {
        cpu.pc = 0x1000;

        const pc: u32 = cpu.pc;
        const imm_i: i32 = value;
        const rs1: u32 = 10;
        const rd: u32 = 10;
        const rs1_before = 12345;
        cpu.xreg[rs1] = rs1_before;

        _ = ArvissExecute(&cpu, encodeI(imm_i) | encodeRs1(rs1) | (0b000 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opJALR));

        // rd <- pc + 4
        try testing.expectEqual(pc + 4, cpu.xreg[rd]);

        // pc <- (rs1 + imm_i) & ~1
        try testing.expectEqual(asU32(rs1_before + imm_i) & 0xfffffffe, cpu.pc);
    }
}

test "jalr leaves x0 as zero" {
    var cpu = Cpu();

    cpu.pc = 0x1000;

    const value: i32 = (1 << 3);
    const pc: u32 = cpu.pc;
    const imm_i: i32 = value;
    const rs1: u32 = 10;
    const rd: u32 = 0;
    const rs1_before = 12345;
    cpu.xreg[rs1] = rs1_before;

    _ = ArvissExecute(&cpu, encodeI(imm_i) | encodeRs1(rs1) | (0b000 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opJALR));

    // x0 <- 0
    try testing.expectEqual(asU32(0), cpu.xreg[0]);

    // pc <- (rs1 + imm_i) & ~1
    try testing.expectEqual(asU32(rs1_before + imm_i) & 0xfffffffe, cpu.pc);
}

test "beq" {
    var cpu = Cpu();

    // pc <- pc + ((rs1 == rs2) ? imm_b : 4)
    var pc: u32 = cpu.pc;
    const imm_b: i32 = 1234;
    const rs1: u32 = 19;
    const rs2: u32 = 27;

    // Branch taken.
    cpu.xreg[rs1] = 5678;
    cpu.xreg[rs2] = cpu.xreg[rs1];

    _ = ArvissExecute(&cpu, encodeB(imm_b) | encodeRs2(rs2) | encodeRs1(rs1) | (0b000 << 12) | opcodeAsU32(Opcode.opBRANCH));

    // pc <- pc + imm_b
    try testing.expectEqual(pc + imm_b, cpu.pc);

    // Branch not taken.
    pc = cpu.pc;
    cpu.xreg[rs1] = 5678;
    cpu.xreg[rs2] = 8765;

    _ = ArvissExecute(&cpu, encodeB(imm_b) | encodeRs2(rs2) | encodeRs1(rs1) | (0b000 << 12) | opcodeAsU32(Opcode.opBRANCH));

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);
}

test "bne" {
    var cpu = Cpu();

    // pc <- pc + ((rs1 != rs2) ? imm_b : 4)
    var pc: u32 = cpu.pc;
    const imm_b: i32 = 1234;
    const rs1: u32 = 19;
    const rs2: u32 = 27;

    // Branch taken.
    cpu.xreg[rs1] = 5678;
    cpu.xreg[rs2] = 8765;

    _ = ArvissExecute(&cpu, encodeB(imm_b) | encodeRs2(rs2) | encodeRs1(rs1) | (0b001 << 12) | opcodeAsU32(Opcode.opBRANCH));

    // pc <- pc + imm_b
    try testing.expectEqual(pc + imm_b, cpu.pc);

    // Branch not taken.
    pc = cpu.pc;
    cpu.xreg[rs1] = 5678;
    cpu.xreg[rs2] = cpu.xreg[rs1];

    _ = ArvissExecute(&cpu, encodeB(imm_b) | encodeRs2(rs2) | encodeRs1(rs1) | (0b001 << 12) | opcodeAsU32(Opcode.opBRANCH));

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);
}

test "blt" {
    var cpu = Cpu();

    // pc <- pc + ((rs1 < rs2) ? imm_b : 4)
    var pc: u32 = cpu.pc;
    const imm_b: i32 = 1234;
    const rs1: u32 = 19;
    const rs2: u32 = 27;

    // Branch taken.
    cpu.xreg[rs1] = 0xffffffff; // -1
    cpu.xreg[rs2] = 0;

    _ = ArvissExecute(&cpu, encodeB(imm_b) | encodeRs2(rs2) | encodeRs1(rs1) | (0b100 << 12) | opcodeAsU32(Opcode.opBRANCH));

    // pc <- pc + imm_b
    try testing.expectEqual(pc + imm_b, cpu.pc);

    // Branch not taken.
    pc = cpu.pc;
    cpu.xreg[rs1] = 456;
    cpu.xreg[rs2] = 123;

    _ = ArvissExecute(&cpu, encodeB(imm_b) | encodeRs2(rs2) | encodeRs1(rs1) | (0b100 << 12) | opcodeAsU32(Opcode.opBRANCH));

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);
}

test "bge" {
    var cpu = Cpu();

    // pc <- pc + ((rs1 >= rs2) ? imm_b : 4)
    var pc: u32 = cpu.pc;
    const imm_b: i32 = 1234;
    const rs1: u32 = 19;
    const rs2: u32 = 27;

    // Branch taken (greater)
    cpu.xreg[rs1] = 0;
    cpu.xreg[rs2] = 0xffffffff; // -1

    _ = ArvissExecute(&cpu, encodeB(imm_b) | encodeRs2(rs2) | encodeRs1(rs1) | (0b101 << 12) | opcodeAsU32(Opcode.opBRANCH));

    // pc <- pc + imm_b
    try testing.expectEqual(pc + imm_b, cpu.pc);

    // Branch taken (equal)
    pc = cpu.pc;
    cpu.xreg[rs1] = 0xffffffff; // -1
    cpu.xreg[rs2] = 0xffffffff; // -1

    _ = ArvissExecute(&cpu, encodeB(imm_b) | encodeRs2(rs2) | encodeRs1(rs1) | (0b101 << 12) | opcodeAsU32(Opcode.opBRANCH));

    // pc <- pc + imm_b
    try testing.expectEqual(pc + imm_b, cpu.pc);

    // Branch not taken.
    pc = cpu.pc;
    cpu.xreg[rs1] = 0xffffffff; // -1
    cpu.xreg[rs2] = 0;

    _ = ArvissExecute(&cpu, encodeB(imm_b) | encodeRs2(rs2) | encodeRs1(rs1) | (0b101 << 12) | opcodeAsU32(Opcode.opBRANCH));

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);
}

test "bltu" {
    var cpu = Cpu();

    // pc <- pc + ((rs1 < rs2) ? imm_b : 4)
    var pc: u32 = cpu.pc;
    const imm_b: i32 = 1234;
    const rs1: u32 = 19;
    const rs2: u32 = 27;

    // Branch taken.
    cpu.xreg[rs1] = 0;
    cpu.xreg[rs2] = 0xffffffff;

    _ = ArvissExecute(&cpu, encodeB(imm_b) | encodeRs2(rs2) | encodeRs1(rs1) | (0b110 << 12) | opcodeAsU32(Opcode.opBRANCH));

    // pc <- pc + imm_b
    try testing.expectEqual(pc + imm_b, cpu.pc);

    // Branch not taken.
    pc = cpu.pc;
    cpu.xreg[rs1] = 0xffffffff;
    cpu.xreg[rs2] = 0;

    _ = ArvissExecute(&cpu, encodeB(imm_b) | encodeRs2(rs2) | encodeRs1(rs1) | (0b110 << 12) | opcodeAsU32(Opcode.opBRANCH));

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);
}

test "bgeu" {
    var cpu = Cpu();

    // pc <- pc + ((rs1 >= rs2) ? imm_b : 4)
    var pc: u32 = cpu.pc;
    const imm_b: i32 = 1234;
    const rs1: u32 = 19;
    const rs2: u32 = 27;

    // Branch taken (greater)
    cpu.xreg[rs1] = 0xffffffff;
    cpu.xreg[rs2] = 0;

    _ = ArvissExecute(&cpu, encodeB(imm_b) | encodeRs2(rs2) | encodeRs1(rs1) | (0b111 << 12) | opcodeAsU32(Opcode.opBRANCH));

    // pc <- pc + imm_b
    try testing.expectEqual(pc + imm_b, cpu.pc);

    // Branch taken (equal)
    pc = cpu.pc;
    cpu.xreg[rs1] = 1;
    cpu.xreg[rs2] = 1;

    _ = ArvissExecute(&cpu, encodeB(imm_b) | encodeRs2(rs2) | encodeRs1(rs1) | (0b111 << 12) | opcodeAsU32(Opcode.opBRANCH));

    // pc <- pc + imm_b
    try testing.expectEqual(pc + imm_b, cpu.pc);

    // Branch not taken.
    pc = cpu.pc;
    cpu.xreg[rs1] = 0;
    cpu.xreg[rs2] = 0xffffffff;

    _ = ArvissExecute(&cpu, encodeB(imm_b) | encodeRs2(rs2) | encodeRs1(rs1) | (0b111 << 12) | opcodeAsU32(Opcode.opBRANCH));

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);
}

test "lb (load byte)" {
    var cpu = Cpu();

    // rd <- sx(m8(rs1 + imm_i)), pc += 4
    cpu.pc = 0x1000;
    var pc: u32 = cpu.pc;
    const imm_i: i32 = 23;
    const rd: u32 = 31;
    const rs1: u32 = 13;
    cpu.xreg[rs1] = rambase;

    // Sign extend when bit 7 is zero.
    memory.write8(cpu.xreg[rs1] + imm_i, 123, &cpu.busCode);

    _ = ArvissExecute(&cpu, encodeI(imm_i) | encodeRs1(rs1) | (0b000 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opLOAD));

    // rd <- sx(m8(rs1 + imm_i))
    try testing.expectEqual(asU32(123), cpu.xreg[rd]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);

    // Sign extend when bit 7 is one.
    pc = cpu.pc;
    memory.write8(cpu.xreg[rs1] + imm_i, 0xff, &cpu.busCode);

    _ = ArvissExecute(&cpu, encodeI(imm_i) | encodeRs1(rs1) | (0b000 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opLOAD));

    // rd <- sx(m8(rs1 + imm_i))
    try testing.expectEqual(asU32(0xffffffff), cpu.xreg[rd]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);
}

test "lh (load halfword)" {
    var cpu = Cpu();

    // rd <- sx(m16(rs1 + imm_i)), pc += 4
    cpu.pc = 0x1000;
    var pc: u32 = cpu.pc;
    const imm_i: i32 = 2000;
    const rd: u32 = 31;
    const rs1: u32 = 6;
    cpu.xreg[rs1] = rambase;

    // Sign extend when bit 15 is zero.
    memory.write16(cpu.xreg[rs1] + imm_i, 0x7fff, &cpu.busCode);

    _ = ArvissExecute(&cpu, encodeI(imm_i) | encodeRs1(rs1) | (0b001 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opLOAD));

    // rd <- sx(m16(rs1 + imm_i))
    try testing.expectEqual(asU32(0x7fff), cpu.xreg[rd]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);

    // Sign extend when bit 15 is one.
    pc = cpu.pc;
    memory.write16(cpu.xreg[rs1] + imm_i, 0xffff, &cpu.busCode);

    _ = ArvissExecute(&cpu, encodeI(imm_i) | encodeRs1(rs1) | (0b001 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opLOAD));

    // rd <- sx(m16(rs1 + imm_i))
    try testing.expectEqual(asU32(0xffffffff), cpu.xreg[rd]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);
}

test "lw (load word)" {
    var cpu = Cpu();

    // rd <- sx(m32(rs1 + imm_i)), pc += 4
    var pc: u32 = cpu.pc;
    const imm_i: i32 = 274;
    const rd: u32 = 14;
    const rs1: u32 = 15;
    cpu.xreg[rs1] = rambase;

    // Sign extend when bit 31 is zero.
    memory.write32(cpu.xreg[rs1] + imm_i, 0x7fffffff, &cpu.busCode);

    _ = ArvissExecute(&cpu, encodeI(imm_i) | encodeRs1(rs1) | (0b010 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opLOAD));

    // rd <- sx(m32(rs1 + imm_i))
    try testing.expectEqual(asU32(0x7fffffff), cpu.xreg[rd]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);

    // Sign extend when bit 31 is one.
    pc = cpu.pc;
    memory.write32(cpu.xreg[rs1] + imm_i, 0xffffffff, &cpu.busCode);

    _ = ArvissExecute(&cpu, encodeI(imm_i) | encodeRs1(rs1) | (0b010 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opLOAD));

    // rd <- sx(m32(rs1 + imm_i))
    try testing.expectEqual(asU32(0xffffffff), cpu.xreg[rd]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);
}

test "lbu (load byte unsigned)" {
    var cpu = Cpu();

    // rd <- zx(m8(rs1 + imm_i)), pc += 4
    cpu.pc = 0x1000;
    var pc: u32 = cpu.pc;
    const imm_i: i32 = -5;
    const rd: u32 = 23;
    const rs1: u32 = 18;
    cpu.xreg[rs1] = rambase + ramsize / 2;

    // Zero extend when bit 7 is zero.
    memory.write8(asU32(asI32(cpu.xreg[rs1]) + imm_i), 123, &cpu.busCode);

    _ = ArvissExecute(&cpu, encodeI(imm_i) | encodeRs1(rs1) | (0b100 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opLOAD));

    // rd <- zx(m8(rs1 + imm_i))
    try testing.expectEqual(asU32(123), cpu.xreg[rd]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);

    // Zero extend when bit 7 is one.
    pc = cpu.pc;
    memory.write8(asU32(asI32(cpu.xreg[rs1]) + imm_i), 0xff, &cpu.busCode);

    _ = ArvissExecute(&cpu, encodeI(imm_i) | encodeRs1(rs1) | (0b100 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opLOAD));

    // rd <- zx(m8(rs1 + imm_i))
    try testing.expectEqual(asU32(0xff), cpu.xreg[rd]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);
}

test "lhu (load halfword unsigned)" {
    var cpu = Cpu();

    // rd <- zx(m16(rs1 + imm_i)), pc += 4
    var pc: u32 = cpu.pc;
    const imm_i: i32 = -1024;
    const rd: u32 = 13;
    const rs1: u32 = 16;
    cpu.xreg[rs1] = rambase + ramsize / 2;

    // Zero extend when bit 15 is zero.
    memory.write16(asU32(asI32(cpu.xreg[rs1]) + imm_i), 0x7fff, &cpu.busCode);

    _ = ArvissExecute(&cpu, encodeI(imm_i) | encodeRs1(rs1) | (0b101 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opLOAD));

    // rd <- zx(m16(rs1 + imm_i))
    try testing.expectEqual(asU32(0x7fff), cpu.xreg[rd]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);

    // Zero extend when bit 15 is one.
    pc = cpu.pc;
    memory.write16(asU32(asI32(cpu.xreg[rs1]) + imm_i), 0xffff, &cpu.busCode);

    _ = ArvissExecute(&cpu, encodeI(imm_i) | encodeRs1(rs1) | (0b101 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opLOAD));

    // rd <- zx(m16(rs1 + imm_i))
    try testing.expectEqual(asU32(0xffff), cpu.xreg[rd]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);
}

test "loads don't affect x0" {
    var cpu = Cpu();

    memory.write32(rambase, 0x12345678, &cpu.busCode);

    // lb
    const rs1: u32 = 13;
    cpu.xreg[rs1] = rambase;
    memory.write8(rambase, 0xff, &cpu.busCode);
    _ = ArvissExecute(&cpu, encodeI(0) | encodeRs1(rs1) | (0b000 << 12) | encodeRd(0) | opcodeAsU32(Opcode.opLOAD));

    // x0 <- 0
    try testing.expectEqual(asU32(0), cpu.xreg[0]);

    // lh
    _ = ArvissExecute(&cpu, encodeI(0) | encodeRs1(rs1) | (0b001 << 12) | encodeRd(0) | opcodeAsU32(Opcode.opLOAD));

    // x0 <- 0
    try testing.expectEqual(asU32(0), cpu.xreg[0]);

    // lw
    _ = ArvissExecute(&cpu, encodeI(0) | encodeRs1(rs1) | (0b010 << 12) | encodeRd(0) | opcodeAsU32(Opcode.opLOAD));

    // x0 <- 0
    try testing.expectEqual(asU32(0), cpu.xreg[0]);

    // lbu
    _ = ArvissExecute(&cpu, encodeI(0) | encodeRs1(rs1) | (0b100 << 12) | encodeRd(0) | opcodeAsU32(Opcode.opLOAD));

    // x0 <- 0
    try testing.expectEqual(asU32(0), cpu.xreg[0]);

    // lhu
    _ = ArvissExecute(&cpu, encodeI(0) | encodeRs1(rs1) | (0b101 << 12) | encodeRd(0) | opcodeAsU32(Opcode.opLOAD));

    // x0 <- 0
    try testing.expectEqual(asU32(0), cpu.xreg[0]);
}

test "sb (store byte)" {
    var cpu = Cpu();

    // m8(rs1 + imm_s) <- rs2[7:0], pc += 4
    var pc: u32 = cpu.pc;
    const imm_s: i32 = -123;
    const rs1: u32 = 12;
    const rs2: u32 = 3;
    cpu.xreg[rs1] = rambase + ramsize / 2;

    _ = ArvissExecute(&cpu, encodeS(imm_s) | encodeRs2(rs2) | encodeRs1(rs1) | (0b000 << 12) | opcodeAsU32(Opcode.opSTORE));

    // m8(rs1 + imm_s) <- rs2[7:0]
    const byte_result = memory.read8(asU32(asI32(cpu.xreg[rs1]) + imm_s), &cpu.busCode);
    try testing.expectEqual(arviss.BusCode.bcOK, cpu.busCode);
    try testing.expectEqual(byte_result, asU8(cpu.xreg[rs2] & 0xff));

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);
}

test "sh (store halfword)" {
    var cpu = Cpu();

    // m16(rs1 + imm_s) <- rs2[15:0], pc += 4
    var pc: u32 = cpu.pc;
    const imm_s: i32 = 222;
    const rs1: u32 = 2;
    const rs2: u32 = 29;
    cpu.xreg[rs1] = rambase + ramsize / 2;

    _ = ArvissExecute(&cpu, encodeS(imm_s) | encodeRs2(rs2) | encodeRs1(rs1) | (0b001 << 12) | opcodeAsU32(Opcode.opSTORE));

    // m16(rs1 + imm_s) <- rs2[15:0]
    const halfword_result = memory.read16(asU32(asI32(cpu.xreg[rs1]) + imm_s), &cpu.busCode);
    try testing.expectEqual(arviss.BusCode.bcOK, cpu.busCode);
    try testing.expectEqual(halfword_result, asU16(cpu.xreg[rs2] & 0xffff));

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);
}

test "sw (store word)" {
    var cpu = Cpu();

    // m32(rs1 + imm_s) <- rs2[31:0], pc += 4
    var pc: u32 = cpu.pc;
    const imm_s: i32 = 222;
    const rs1: u32 = 2;
    const rs2: u32 = 29;
    cpu.xreg[rs1] = rambase + ramsize / 2;

    _ = ArvissExecute(&cpu, encodeS(imm_s) | encodeRs2(rs2) | encodeRs1(rs1) | (0b010 << 12) | opcodeAsU32(Opcode.opSTORE));

    // m32(rs1 + imm_s) <- rs2[31:0]
    const word_result = memory.read32(asU32(asI32(cpu.xreg[rs1]) + imm_s), &cpu.busCode);
    try testing.expectEqual(arviss.BusCode.bcOK, cpu.busCode);
    try testing.expectEqual(word_result, cpu.xreg[rs2]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);
}

test "addi (add immediate)" {
    var cpu = Cpu();

    // rd <- rs1 + imm_i, pc += 4
    var pc: u32 = cpu.pc;
    const rd: u32 = 15;
    const rs1: u32 = 12;
    var imm_i: i32 = 64;
    cpu.xreg[rs1] = 128;

    // Add immediate.
    _ = ArvissExecute(&cpu, encodeI(imm_i) | encodeRs1(rs1) | (0b000 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opOPIMM));

    // rd <- rs1 + imm_i
    try testing.expectEqual(asU32(asI32(cpu.xreg[rs1]) + imm_i), cpu.xreg[rd]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);

    // Add negative number.
    pc = cpu.pc;
    imm_i = -123;
    _ = ArvissExecute(&cpu, encodeI(imm_i) | encodeRs1(rs1) | (0b000 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opOPIMM));

    // rd <- rs1 + imm_i
    try testing.expectEqual(asU32(asI32(cpu.xreg[rs1]) + imm_i), cpu.xreg[rd]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);
}

test "slti (set less than immediate)" {
    var cpu = Cpu();

    // rd <- (rs1 < imm_i) ? 1 : 0, pc += 4
    var pc: u32 = cpu.pc;

    const imm_i: i32 = 0;
    const rd: u32 = 19;
    const rs1: u32 = 27;

    // Condition true.
    cpu.xreg[rs1] = 0xffffffff; // (-1)
    _ = ArvissExecute(&cpu, encodeI(imm_i) | encodeRs1(rs1) | (0b010 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opOPIMM));

    // rd <- (rs1 < imm_i) ? 1 : 0
    try testing.expectEqual(asU32(1), cpu.xreg[rd]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);

    // Condition false.
    pc = cpu.pc;
    cpu.xreg[rs1] = 123;
    _ = ArvissExecute(&cpu, encodeI(imm_i) | encodeRs1(rs1) | (0b010 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opOPIMM));

    // rd <- (rs1 < imm_i) ? 1 : 0
    try testing.expectEqual(asU32(0), cpu.xreg[rd]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);
}

test "sltiu (set less than immediate unsigned)" {
    var cpu = Cpu();

    // rd <- (rs1 < imm_i) ? 1 : 0, pc += 4
    var pc: u32 = cpu.pc;

    const imm_i: i32 = 0xffff;
    const rd: u32 = 9;
    const rs1: u32 = 11;

    // Condition true.
    cpu.xreg[rs1] = 0;
    _ = ArvissExecute(&cpu, encodeI(imm_i) | encodeRs1(rs1) | (0b011 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opOPIMM));

    // rd <- (rs1 < imm_i) ? 1 : 0
    try testing.expectEqual(asU32(1), cpu.xreg[rd]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);

    // Condition false.
    pc = cpu.pc;
    cpu.xreg[rs1] = 0xffffffff;
    _ = ArvissExecute(&cpu, encodeI(imm_i) | encodeRs1(rs1) | (0b010 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opOPIMM));

    // rd <- (rs1 < imm_i) ? 1 : 0
    try testing.expectEqual(asU32(0), cpu.xreg[rd]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);
}

test "xori (xor immediate)" {
    var cpu = Cpu();

    // rd <- rs1 ^ imm_i, pc += 4
    var pc: u32 = cpu.pc;
    const rd: u32 = 5;
    const rs1: u32 = 3;
    var imm_i: i32 = -1;
    cpu.xreg[rs1] = 123456;

    _ = ArvissExecute(&cpu, encodeI(imm_i) | encodeRs1(rs1) | (0b100 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opOPIMM));

    // rd <- rs1 ^ imm_i
    try testing.expectEqual(cpu.xreg[rs1] ^ asU32(imm_i), cpu.xreg[rd]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);
}

test "ori (or immediate)" {
    var cpu = Cpu();

    // rd <- rs1 | imm_i, pc += 4
    var pc: u32 = cpu.pc;
    const rd: u32 = 25;
    const rs1: u32 = 13;
    var imm_i: i32 = 0x00ff;
    cpu.xreg[rs1] = 0xff00;

    _ = ArvissExecute(&cpu, encodeI(imm_i) | encodeRs1(rs1) | (0b110 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opOPIMM));

    // rd <- rs1 | imm_i
    try testing.expectEqual(cpu.xreg[rs1] | asU32(imm_i), cpu.xreg[rd]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);
}

test "andi (and immediate)" {
    var cpu = Cpu();

    // rd <- rs1 & imm_i, pc += 4
    var pc: u32 = cpu.pc;
    const rd: u32 = 5;
    const rs1: u32 = 3;
    var imm_i: i32 = 0xfff0;
    cpu.xreg[rs1] = 0xffff;

    _ = ArvissExecute(&cpu, encodeI(imm_i) | encodeRs1(rs1) | (0b111 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opOPIMM));

    // rd <- rs1 & imm_i
    try testing.expectEqual(cpu.xreg[rs1] & asU32(imm_i), cpu.xreg[rd]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);
}

test "slli (shift left logical immediate)" {
    var cpu = Cpu();

    // rd <- rs1 << shamt_i, pc += 4
    var pc: u32 = cpu.pc;
    const rd: u32 = 5;
    const rs1: u32 = 3;
    const shamt: u32 = 4;
    cpu.xreg[rs1] = 0x0010;

    _ = ArvissExecute(&cpu, encodeI(shamt) | encodeRs1(rs1) | (0b001 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opOPIMM));

    // rd <- rs1 << shamt_i
    try testing.expectEqual(cpu.xreg[rs1] << shamt, cpu.xreg[rd]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);
}

test "srli (shift right logical immediate)" {
    var cpu = Cpu();

    // rd <- rs1 >> shamt_i, pc += 4
    var pc: u32 = cpu.pc;
    const rd: u32 = 15;
    const rs1: u32 = 23;
    const shamt: u32 = 4;
    cpu.xreg[rs1] = 0x1000;

    _ = ArvissExecute(&cpu, encodeI(shamt) | encodeRs1(rs1) | (0b101 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opOPIMM));

    // rd <- rs1 >> shamt_i
    try testing.expectEqual(cpu.xreg[rs1] >> shamt, cpu.xreg[rd]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);
}

test "srai (shift right arithmetic immediate)" {
    var cpu = Cpu();

    // rd <- sx(rs1) >> shamt_i, pc += 4
    var pc: u32 = cpu.pc;
    const rd: u32 = 10;
    const rs1: u32 = 11;
    const shamt: u32 = 3;
    cpu.xreg[rs1] = 0x80000000;

    _ = ArvissExecute(&cpu, (1 << 30) | encodeI(shamt) | encodeRs1(rs1) | (0b101 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opOPIMM));

    // rd <- sx(rs1) >> shamt_i
    try testing.expectEqual(asU32(asI32(cpu.xreg[rs1]) >> shamt), cpu.xreg[rd]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);
}

test "immediate ops don't affect x0" {
    var cpu = Cpu();

    // addi
    _ = ArvissExecute(&cpu, encodeI(123) | encodeRs1(1) | (0b000 << 12) | encodeRd(0) | opcodeAsU32(Opcode.opOPIMM));

    // x0 <- 0
    try testing.expectEqual(asU32(0), cpu.xreg[0]);

    // slti
    _ = ArvissExecute(&cpu, encodeI(123) | encodeRs1(1) | (0b010 << 12) | encodeRd(0) | opcodeAsU32(Opcode.opOPIMM));

    // x0 <- 0
    try testing.expectEqual(asU32(0), cpu.xreg[0]);

    // sltiu
    _ = ArvissExecute(&cpu, encodeI(123) | encodeRs1(1) | (0b011 << 12) | encodeRd(0) | opcodeAsU32(Opcode.opOPIMM));

    // x0 <- 0
    try testing.expectEqual(asU32(0), cpu.xreg[0]);

    // xori
    _ = ArvissExecute(&cpu, encodeI(123) | encodeRs1(1) | (0b100 << 12) | encodeRd(0) | opcodeAsU32(Opcode.opOPIMM));

    // x0 <- 0
    try testing.expectEqual(asU32(0), cpu.xreg[0]);

    // ori
    _ = ArvissExecute(&cpu, encodeI(123) | encodeRs1(1) | (0b110 << 12) | encodeRd(0) | opcodeAsU32(Opcode.opOPIMM));

    // x0 <- 0
    try testing.expectEqual(asU32(0), cpu.xreg[0]);

    // andi
    _ = ArvissExecute(&cpu, encodeI(123) | encodeRs1(1) | (0b111 << 12) | encodeRd(0) | opcodeAsU32(Opcode.opOPIMM));

    // x0 <- 0
    try testing.expectEqual(asU32(0), cpu.xreg[0]);

    // slli
    cpu.xreg[1] = 0xffffffff;
    _ = ArvissExecute(&cpu, encodeI(0xff) | encodeRs1(1) | (0b001 << 12) | encodeRd(0) | opcodeAsU32(Opcode.opOPIMM));

    // x0 <- 0
    try testing.expectEqual(asU32(0), cpu.xreg[0]);

    // srli
    cpu.xreg[1] = 0xffffffff;
    _ = ArvissExecute(&cpu, encodeI(3) | encodeRs1(1) | (0b101 << 12) | encodeRd(0) | opcodeAsU32(Opcode.opOPIMM));

    // x0 <- 0
    try testing.expectEqual(asU32(0), cpu.xreg[0]);

    // srai
    cpu.xreg[1] = 0xffffffff;
    _ = ArvissExecute(&cpu, (1 << 30) | encodeI(3) | encodeRs1(1) | (0b101 << 12) | encodeRd(0) | opcodeAsU32(Opcode.opOPIMM));

    // x0 <- 0
    try testing.expectEqual(asU32(0), cpu.xreg[0]);
}

test "add" {
    var cpu = Cpu();

    // rd <- rs1 + rs2, pc += 4
    var pc: u32 = cpu.pc;
    const rd: u32 = 1;
    const rs1: u32 = 2;
    const rs2: u32 = 3;
    cpu.xreg[rs1] = 128;
    cpu.xreg[rs2] = 64;

    // Add.
    _ = ArvissExecute(&cpu, (0 << 25) | encodeRs2(rs2) | encodeRs1(rs1) | (0b000 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opOP));

    // rd <- rs1 + rs2
    try testing.expectEqual(cpu.xreg[rs1] +% cpu.xreg[rs2], cpu.xreg[rd]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);

    // x0 <- 0
    _ = ArvissExecute(&cpu, (0 << 25) | encodeRs2(rs2) | encodeRs1(rs1) | (0b000 << 12) | encodeRd(0) | opcodeAsU32(Opcode.opOP));
    try testing.expectEqual(asU32(0), cpu.xreg[0]);
}

test "sub" {
    var cpu = Cpu();

    // rd <- rs1 - rs2, pc += 4
    var pc: u32 = cpu.pc;
    const rd: u32 = 1;
    const rs1: u32 = 2;
    const rs2: u32 = 3;
    cpu.xreg[rs1] = 192;
    cpu.xreg[rs2] = 64;

    _ = ArvissExecute(&cpu, (0b0100000 << 25) | encodeRs2(rs2) | encodeRs1(rs1) | (0b000 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opOP));

    // rd <- rs1 - rs2
    try testing.expectEqual(cpu.xreg[rs1] -% cpu.xreg[rs2], cpu.xreg[rd]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);

    // x0 <- 0
    _ = ArvissExecute(&cpu, (0b0100000 << 25) | encodeRs2(rs2) | encodeRs1(rs1) | (0b000 << 12) | encodeRd(0) | opcodeAsU32(Opcode.opOP));
    try testing.expectEqual(asU32(0), cpu.xreg[0]);
}

test "mul" { // 'M' extension.
    // MUL performs a 32-bit x 32-bit multiplication of rs1 by rs2 and places the lower 32 bits in the destination register.
    var cpu = Cpu();

    // rd <- lower32(rs1 * rs2), pc += 4
    var pc: u32 = cpu.pc;
    const rd: u32 = 5;
    const rs1: u32 = 13;
    const rs2: u32 = 14;
    cpu.xreg[rs1] = 333;
    cpu.xreg[rs2] = 3;
    const expected: u32 = 999;

    _ = ArvissExecute(&cpu, (0b0000001 << 25) | encodeRs2(rs2) | encodeRs1(rs1) | (0b000 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opOP));

    // rd <- lower32(rs1 * rs2)
    try testing.expectEqual(expected, cpu.xreg[rd]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);

    // x0 <- 0
    _ = ArvissExecute(&cpu, (0b0000001 << 25) | encodeRs2(rs2) | encodeRs1(rs1) | (0b000 << 12) | encodeRd(0) | opcodeAsU32(Opcode.opOP));
    try testing.expectEqual(asU32(0), cpu.xreg[0]);
}

test "sll (shift left logical)" {
    var cpu = Cpu();

    // rd <- rs1 << (rs2 % XLEN), pc += 4
    var pc: u32 = cpu.pc;
    const rd: u32 = 1;
    const rs1: u32 = 2;
    const rs2: u32 = 3;
    cpu.xreg[rs1] = 1;
    cpu.xreg[rs2] = 10;

    _ = ArvissExecute(&cpu, (0 << 25) | encodeRs2(rs2) | encodeRs1(rs1) | (0b001 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opOP));

    // rd <- rs1 << (rs2 % XLEN)
    try testing.expectEqual(cpu.xreg[rs1] << @intCast(u5, (cpu.xreg[rs2] % 32)), cpu.xreg[rd]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);

    // x0 <- 0
    _ = ArvissExecute(&cpu, (0 << 25) | encodeRs2(rs2) | encodeRs1(rs1) | (0b001 << 12) | encodeRd(0) | opcodeAsU32(Opcode.opOP));
    try testing.expectEqual(asU32(0), cpu.xreg[0]);
}

test "mulh" { // 'M' extension.
    // MULH performs a 32-bit x 32-bit (signed x signed) multiplication of rs1 by rs2 and places the upper 32 bits of the 64 bit
    // product in the destination register.
    var cpu = Cpu();

    // rd <- upper32(rs1 * rs2), pc += 4
    var pc: u32 = cpu.pc;
    const rd: u32 = 5;
    const rs1: u32 = 13;
    const rs2: u32 = 14;
    cpu.xreg[rs1] = 16777216; // 2 ** 24
    cpu.xreg[rs2] = 0xf5ffec40; // -(2 ** 24)

    const product: i64 = @intCast(i64, asI32(cpu.xreg[rs1])) * @intCast(i64, asI32(cpu.xreg[rs2]));
    const expected: i32 = asI32(product >> 32);

    _ = ArvissExecute(&cpu, (0b0000001 << 25) | encodeRs2(rs2) | encodeRs1(rs1) | (0b001 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opOP));

    // rd <- upper32(rs1 * rs2)
    try testing.expectEqual(asU32(expected), cpu.xreg[rd]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);

    // x0 <- 0
    _ = ArvissExecute(&cpu, (0b0000001 << 25) | encodeRs2(rs2) | encodeRs1(rs1) | (0b001 << 12) | encodeRd(0) | opcodeAsU32(Opcode.opOP));
    try testing.expectEqual(asU32(0), cpu.xreg[0]);
}

test "slt (set less than)" {
    var cpu = Cpu();

    // rd <- (rs1 < rs2) ? 1 : 0, pc += 4
    var pc: u32 = cpu.pc;
    const rd: u32 = 19;
    const rs1: u32 = 7;
    const rs2: u32 = 4;
    cpu.xreg[rs2] = 0;

    // Condition true.
    cpu.xreg[rs1] = 0xffffffff; // -1
    _ = ArvissExecute(&cpu, (0 << 25) | encodeRs2(rs2) | encodeRs1(rs1) | (0b010 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opOP));

    // rd <- (rs1 < imm_i) ? 1 : 0
    try testing.expectEqual(asU32(1), cpu.xreg[rd]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);

    // Condition false.
    pc = cpu.pc;
    cpu.xreg[rs1] = 123;
    _ = ArvissExecute(&cpu, (0 << 25) | encodeRs2(rs2) | encodeRs1(rs1) | (0b010 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opOP));

    // rd <- (rs1 < imm_i) ? 1 : 0
    try testing.expectEqual(asU32(0), cpu.xreg[rd]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);

    // x0 <- 0
    _ = ArvissExecute(&cpu, (0 << 25) | encodeRs2(rs2) | encodeRs1(rs1) | (0b010 << 12) | encodeRd(0) | opcodeAsU32(Opcode.opOP));
    try testing.expectEqual(asU32(0), cpu.xreg[0]);
}

test "mulhsu" { // 'M' extension.
    // MULHSU performs a 32-bit x 32-bit (signed x unsigned) multiplication of rs1 by rs2 and places the upper 32 bits of the 64 bit
    // product in the destination register.
    var cpu = Cpu();

    // rd <- upper32(rs1 * rs2), pc += 4
    var pc: u32 = cpu.pc;
    const rd: u32 = 5;
    const rs1: u32 = 13;
    const rs2: u32 = 14;
    cpu.xreg[rs1] = 16777216; // 2 ** 24
    cpu.xreg[rs2] = 0xffffc000; // -16384 signed, 4294950912 unsigned

    const product: i64 = @bitCast(i64, @intCast(u64, asI32(cpu.xreg[rs1])) * @intCast(u64, cpu.xreg[rs2]));
    const expected: i32 = asI32(product >> 32);

    _ = ArvissExecute(&cpu, (0b0000001 << 25) | encodeRs2(rs2) | encodeRs1(rs1) | (0b010 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opOP));

    // rd <- upper32(rs1 * rs2)
    try testing.expectEqual(asU32(expected), cpu.xreg[rd]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);

    // x0 <- 0
    _ = ArvissExecute(&cpu, (0b0000001 << 25) | encodeRs2(rs2) | encodeRs1(rs1) | (0b010 << 12) | encodeRd(0) | opcodeAsU32(Opcode.opOP));
    try testing.expectEqual(asU32(0), cpu.xreg[0]);
}

test "sltu (set less than unsigned)" {
    var cpu = Cpu();

    // rd <- (rs1 < rs2) ? 1 : 0, pc += 4
    var pc: u32 = cpu.pc;
    const rd: u32 = 19;
    const rs1: u32 = 7;
    const rs2: u32 = 4;
    cpu.xreg[rs2] = 0xffffffff;

    // Condition true.
    cpu.xreg[rs1] = 0;
    _ = ArvissExecute(&cpu, (0 << 25) | encodeRs2(rs2) | encodeRs1(rs1) | (0b011 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opOP));

    // rd <- (rs1 < imm_i) ? 1 : 0
    try testing.expectEqual(asU32(1), cpu.xreg[rd]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);

    // Condition false.
    pc = cpu.pc;
    cpu.xreg[rs1] = 0xffffffff;
    _ = ArvissExecute(&cpu, (0 << 25) | encodeRs2(rs2) | encodeRs1(rs1) | (0b011 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opOP));

    // rd <- (rs1 < imm_i) ? 1 : 0
    try testing.expectEqual(asU32(0), cpu.xreg[rd]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);

    // x0 <- 0
    _ = ArvissExecute(&cpu, (0 << 25) | encodeRs2(rs2) | encodeRs1(rs1) | (0b011 << 12) | encodeRd(0) | opcodeAsU32(Opcode.opOP));
    try testing.expectEqual(asU32(0), cpu.xreg[0]);
}

test "mulhu" { // 'M' extension.
    // MULHSU performs a 32-bit x 32-bit (unsigned x unsigned) multiplication of rs1 by rs2 and places the upper 32 bits of the 64 bit
    // product in the destination register.
    var cpu = Cpu();

    // rd <- upper32(rs1 * rs2), pc += 4
    var pc: u32 = cpu.pc;
    const rd: u32 = 5;
    const rs1: u32 = 13;
    const rs2: u32 = 14;
    cpu.xreg[rs1] = 0xffffc000; // 4294950912 unsigned
    cpu.xreg[rs2] = 0xffffc000; // 4294950912 unsigned

    const product: u64 = @intCast(u64, cpu.xreg[rs1]) * @intCast(u64, cpu.xreg[rs2]);
    const expected: u32 = asU32(product >> 32);

    _ = ArvissExecute(&cpu, (0b0000001 << 25) | encodeRs2(rs2) | encodeRs1(rs1) | (0b011 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opOP));

    // rd <- upper32(rs1 * rs2)
    try testing.expectEqual(asU32(expected), cpu.xreg[rd]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);

    // x0 <- 0
    _ = ArvissExecute(&cpu, (0b0000001 << 25) | encodeRs2(rs2) | encodeRs1(rs1) | (0b011 << 12) | encodeRd(0) | opcodeAsU32(Opcode.opOP));
    try testing.expectEqual(asU32(0), cpu.xreg[0]);
}

test "xor" {
    var cpu = Cpu();

    // rd <- rs1 ^ rs2, pc += 4
    var pc: u32 = cpu.pc;
    const rd: u32 = 9;
    const rs1: u32 = 10;
    const rs2: u32 = 11;
    cpu.xreg[rs1] = 0xff;
    cpu.xreg[rs2] = 0xfe;

    _ = ArvissExecute(&cpu, (0 << 25) | encodeRs2(rs2) | encodeRs1(rs1) | (0b100 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opOP));

    // rd <- rs1 ^ rs2
    try testing.expectEqual(cpu.xreg[rs1] ^ cpu.xreg[rs2], cpu.xreg[rd]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);

    // x0 <- 0
    _ = ArvissExecute(&cpu, (0 << 25) | encodeRs2(rs2) | encodeRs1(rs1) | (0b100 << 12) | encodeRd(0) | opcodeAsU32(Opcode.opOP));
    try testing.expectEqual(asU32(0), cpu.xreg[0]);
}

test "div" { // 'M' extension.
    // DIV performs a 32-bit x 32-bit (signed / signed) integer division of rs1 by rs2, rounding towards zero.
    var cpu = Cpu();

    // rd <- rs1 / rs2, pc += 4
    var pc: u32 = cpu.pc;
    const rd: u32 = 5;
    const rs1: u32 = 13;
    const rs2: u32 = 14;
    cpu.xreg[rs1] = 262144;
    cpu.xreg[rs2] = 0xfffffc00; // -1024

    const expected = @divTrunc(asI32(cpu.xreg[rs1]), asI32(cpu.xreg[rs2]));

    _ = ArvissExecute(&cpu, (0b0000001 << 25) | encodeRs2(rs2) | encodeRs1(rs1) | (0b100 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opOP));

    // rd <- rs1 / rs2
    try testing.expectEqual(expected, asI32(cpu.xreg[rd]));

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);

    // Division by zero sets the result to -1.
    cpu.xreg[rs2] = 0;
    _ = ArvissExecute(&cpu, (0b0000001 << 25) | encodeRs2(rs2) | encodeRs1(rs1) | (0b100 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opOP));
    try testing.expectEqual(@intCast(i32, -1), asI32(cpu.xreg[rd]));

    // Division of the most negative integer by -1 results in overflow.
    cpu.xreg[rs1] = 0x80000000; // -2**31
    cpu.xreg[rs2] = 0xffffffff; // -1
    _ = ArvissExecute(&cpu, (0b0000001 << 25) | encodeRs2(rs2) | encodeRs1(rs1) | (0b100 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opOP));
    try testing.expectEqual(asU32(0x80000000), cpu.xreg[rd]);

    // x0 <- 0
    _ = ArvissExecute(&cpu, (0b0000001 << 25) | encodeRs2(rs2) | encodeRs1(rs1) | (0b100 << 12) | encodeRd(0) | opcodeAsU32(Opcode.opOP));
    try testing.expectEqual(asU32(0), cpu.xreg[0]);
}

test "srl (shift right logical)" {
    var cpu = Cpu();

    // rd <- rs1 >> (rs2 % XLEN), pc += 4
    var pc: u32 = cpu.pc;
    const rd: u32 = 9;
    const rs1: u32 = 10;
    const rs2: u32 = 11;
    cpu.xreg[rs1] = 0x80000000;
    cpu.xreg[rs2] = 4;

    _ = ArvissExecute(&cpu, (0 << 25) | encodeRs2(rs2) | encodeRs1(rs1) | (0b101 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opOP));

    // rd <- rs1 >> (rs2 % XLEN)
    try testing.expectEqual(cpu.xreg[rs1] >> @intCast(u5, (cpu.xreg[rs2] % 32)), cpu.xreg[rd]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);

    // x0 <- 0
    _ = ArvissExecute(&cpu, (0 << 25) | encodeRs2(rs2) | encodeRs1(rs1) | (0b101 << 12) | encodeRd(0) | opcodeAsU32(Opcode.opOP));
    try testing.expectEqual(asU32(0), cpu.xreg[0]);
}

test "sra (shift right arithmetic)" {
    var cpu = Cpu();

    // rd <- sx(rs1) >> (rs2 % XLEN), pc += 4
    var pc: u32 = cpu.pc;
    const rd: u32 = 9;
    const rs1: u32 = 10;
    const rs2: u32 = 11;
    cpu.xreg[rs1] = 0x80000000;
    cpu.xreg[rs2] = 4;

    _ = ArvissExecute(&cpu, (0b0100000 << 25) | encodeRs2(rs2) | encodeRs1(rs1) | (0b101 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opOP));

    // rd <- sx(rs1) >> (rs2 % XLEN)
    try testing.expectEqual(asU32(asI32(cpu.xreg[rs1]) >> @intCast(u5, (cpu.xreg[rs2] % 32))), cpu.xreg[rd]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);

    // x0 <- 0
    _ = ArvissExecute(&cpu, (0b0100000 << 25) | encodeRs2(rs2) | encodeRs1(rs1) | (0b101 << 12) | encodeRd(0) | opcodeAsU32(Opcode.opOP));
    try testing.expectEqual(asU32(0), cpu.xreg[0]);
}

test "divu" { // 'M' extension.
    // DIVU performs a 32-bit x 32-bit (unsigned / unsigned) integer division of rs1 by rs2, rounding towards zero.
    var cpu = Cpu();

    // rd <- rs1 / rs2, pc += 4
    var pc: u32 = cpu.pc;
    const rd: u32 = 5;
    const rs1: u32 = 13;
    const rs2: u32 = 14;
    cpu.xreg[rs1] = 262144;
    cpu.xreg[rs2] = 1024;

    const expected = @divTrunc(cpu.xreg[rs1], cpu.xreg[rs2]);

    _ = ArvissExecute(&cpu, (0b0000001 << 25) | encodeRs2(rs2) | encodeRs1(rs1) | (0b101 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opOP));

    // rd <- rs1 / rs2
    try testing.expectEqual(expected, cpu.xreg[rd]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);

    // Division by zero sets the result to 0xffffffff.
    cpu.xreg[rs2] = 0;
    _ = ArvissExecute(&cpu, (0b0000001 << 25) | encodeRs2(rs2) | encodeRs1(rs1) | (0b101 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opOP));
    try testing.expectEqual(asU32(0xffffffff), cpu.xreg[rd]);

    // x0 <- 0
    _ = ArvissExecute(&cpu, (0b0000001 << 25) | encodeRs2(rs2) | encodeRs1(rs1) | (0b101 << 12) | encodeRd(0) | opcodeAsU32(Opcode.opOP));
    try testing.expectEqual(asU32(0), cpu.xreg[0]);
}

test "or" {
    var cpu = Cpu();

    // rd <- rs1 | rs2, pc += 4
    var pc: u32 = cpu.pc;
    const rd: u32 = 9;
    const rs1: u32 = 10;
    const rs2: u32 = 11;
    cpu.xreg[rs1] = 0x00ff00ff;
    cpu.xreg[rs2] = 0xff00ffff;

    _ = ArvissExecute(&cpu, (0 << 25) | encodeRs2(rs2) | encodeRs1(rs1) | (0b110 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opOP));

    // rd <- rs1 | rs2
    try testing.expectEqual(cpu.xreg[rs1] | cpu.xreg[rs2], cpu.xreg[rd]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);

    // x0 <- 0
    _ = ArvissExecute(&cpu, (0 << 25) | encodeRs2(rs2) | encodeRs1(rs1) | (0b110 << 12) | encodeRd(0) | opcodeAsU32(Opcode.opOP));
    try testing.expectEqual(asU32(0), cpu.xreg[0]);
}

test "rem" { // 'M' extension.
    // REM performs a 32-bit x 32-bit (signed / signed) integer division of rs1 by rs2, rounding towards zero, and returns the
    // remainder. The sign of the result is the sign of the dividend.
    var cpu = Cpu();

    // rd <- rs % rs2, pc += 4
    var pc: u32 = cpu.pc;
    const rd: u32 = 5;
    const rs1: u32 = 13;
    const rs2: u32 = 14;
    const dividend: i32 = -65535;
    cpu.xreg[rs1] = asU32(dividend);
    cpu.xreg[rs2] = 4096;

    const expected = @rem(asI32(cpu.xreg[rs1]), asI32(cpu.xreg[rs2]));

    _ = ArvissExecute(&cpu, (0b0000001 << 25) | encodeRs2(rs2) | encodeRs1(rs1) | (0b110 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opOP));

    // rd <- rs1 % rs2
    try testing.expectEqual(expected, asI32(cpu.xreg[rd]));

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);

    // Division by zero sets the result to the dividend.
    cpu.xreg[rs2] = 0;
    _ = ArvissExecute(&cpu, (0b0000001 << 25) | encodeRs2(rs2) | encodeRs1(rs1) | (0b110 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opOP));
    try testing.expectEqual(dividend, asI32(cpu.xreg[rd]));

    // Division of the most negative number by -1 results in overflow which sets the result to zero.
    cpu.xreg[rs1] = 0x80000000; // -2**31
    cpu.xreg[rs2] = 0xffffffff; // -1
    _ = ArvissExecute(&cpu, (0b0000001 << 25) | encodeRs2(rs2) | encodeRs1(rs1) | (0b110 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opOP));
    try testing.expectEqual(asI32(0), asI32(cpu.xreg[rd]));

    // x0 <- 0
    _ = ArvissExecute(&cpu, (0b0000001 << 25) | encodeRs2(rs2) | encodeRs1(rs1) | (0b110 << 12) | encodeRd(0) | opcodeAsU32(Opcode.opOP));
    try testing.expectEqual(asU32(0), cpu.xreg[0]);
}

test "and" {
    var cpu = Cpu();

    // rd <- rs1 & rs2, pc += 4
    var pc: u32 = cpu.pc;
    const rd: u32 = 12;
    const rs1: u32 = 13;
    const rs2: u32 = 14;
    cpu.xreg[rs1] = 0xff00ff00;
    cpu.xreg[rs2] = 0xff00ffff;

    _ = ArvissExecute(&cpu, (0 << 25) | encodeRs2(rs2) | encodeRs1(rs1) | (0b111 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opOP));

    // rd <- rs1 & rs2
    try testing.expectEqual(cpu.xreg[rs1] & cpu.xreg[rs2], cpu.xreg[rd]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);

    // x0 <- 0
    _ = ArvissExecute(&cpu, (0 << 25) | encodeRs2(rs2) | encodeRs1(rs1) | (0b111 << 12) | encodeRd(0) | opcodeAsU32(Opcode.opOP));
    try testing.expectEqual(asU32(0), cpu.xreg[0]);
}

test "remu" { // 'M' extension.
    // REMU performs a 32-bit x 32-bit (unsigned / unsigned) integer division of rs1 by rs2, rounding towards zero, and returns the
    // remainder.
    var cpu = Cpu();

    // rd <- rs % rs2, pc += 4
    var pc: u32 = cpu.pc;
    const rd: u32 = 5;
    const rs1: u32 = 13;
    const rs2: u32 = 14;
    const dividend: u32 = 65535;
    cpu.xreg[rs1] = dividend;
    cpu.xreg[rs2] = 16384;

    const expected = @rem(cpu.xreg[rs1], cpu.xreg[rs2]);

    _ = ArvissExecute(&cpu, (0b0000001 << 25) | encodeRs2(rs2) | encodeRs1(rs1) | (0b111 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opOP));

    // rd <- rs1 % rs2
    try testing.expectEqual(expected, cpu.xreg[rd]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);

    // Division by zero sets the result to the dividend.
    cpu.xreg[rs2] = 0;
    _ = ArvissExecute(&cpu, (0b0000001 << 25) | encodeRs2(rs2) | encodeRs1(rs1) | (0b111 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opOP));
    try testing.expectEqual(dividend, cpu.xreg[rd]);

    // x0 <- 0
    _ = ArvissExecute(&cpu, (0b0000001 << 25) | encodeRs2(rs2) | encodeRs1(rs1) | (0b111 << 12) | encodeRd(0) | opcodeAsU32(Opcode.opOP));
    try testing.expectEqual(asU32(0), cpu.xreg[0]);
}

test "mret" {
    var cpu = Cpu();

    // pc <- mepc + 4
    cpu.mepc = 0x4000;
    cpu.pc = 0x8080;

    _ = ArvissExecute(&cpu, (0b001100000010 << 20) | opcodeAsU32(Opcode.opSYSTEM));

    // pc <- mepc + 4
    try testing.expectEqual(cpu.mepc + 4, cpu.pc);
}

test "traps set mepc" {
    var cpu = Cpu();

    // mepc <- pc
    cpu.pc = 0x8086;
    cpu.mepc = 0;
    const saved_pc: u32 = cpu.pc;

    // Take a breakpoint.
    _ = ArvissExecute(&cpu, (0b000000000001 << 20) | opcodeAsU32(Opcode.opSYSTEM));

    // mepc <- pc
    try testing.expectEqual(saved_pc, cpu.mepc);
}

test "traps set mcause" {
    var cpu = Cpu();

    // mepc <- pc
    cpu.pc = 0x8086;
    cpu.mepc = 0;

    // Take a breakpoint.
    _ = ArvissExecute(&cpu, (0b000000000001 << 20) | opcodeAsU32(Opcode.opSYSTEM));

    // mepc <- pc
    try testing.expectEqual(@enumToInt(arviss.ArvissTrapType.trBREAKPOINT), cpu.mcause);
}

test "traps set mtval" {
    var cpu = Cpu();

    // mepc <- pc
    const address: u32 = 0x80000000;
    cpu.mepc = 0;

    // Attempt to read from invalid memory.
    const pc: u32 = cpu.pc;
    const imm_i: i32 = 0;
    const rd: u32 = 14;
    const rs1: u32 = 15;
    cpu.xreg[rs1] = address;

    _ = ArvissExecute(&cpu, encodeI(imm_i) | encodeRs1(rs1) | (0b000 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opLOAD));

    // mtval <- exception specific information
    try testing.expectEqual(address, cpu.mtval);
}

test "flw" { // 'F' extension.
    var cpu = Cpu();

    // rd <- f32(rs1 + imm_i), pc += 4
    var pc: u32 = cpu.pc;
    const imm_i: i32 = 274;
    const rd: u32 = 14;
    const rs1: u32 = 15;
    cpu.xreg[rs1] = rambase;

    // Write a float.
    const expected: f32 = -1234e-6;
    const expected_as_u32: u32 = @bitCast(u32, expected);
    memory.write32(asU32(asI32(cpu.xreg[rs1]) + imm_i), expected_as_u32, &cpu.busCode);

    _ = ArvissExecute(&cpu, encodeI(imm_i) | encodeRs1(rs1) | (0b010 << 12) | encodeRd(rd) | opcodeAsU32(Opcode.opLOADFP));

    // rd <= f32(rs1 + imm_i)
    try testing.expectEqual(expected, cpu.freg[rd]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);
}


test "fsw" { // 'F' extension.
    var cpu = Cpu();

    // f32(rs1 + imm_s) = rs2, pc += 4
    var pc: u32 = cpu.pc;
    const imm_s: i32 = 222;
    const rs1: u32 = 2;
    const rs2: u32 = 29;
    cpu.xreg[rs1] = rambase + ramsize / 2;
    const expected: f32 = 123456.99;
    cpu.freg[rs2] = expected;

    _ = ArvissExecute(&cpu, encodeS(imm_s) | encodeRs2(rs2) | encodeRs1(rs1) | (0b010 << 12) | opcodeAsU32(Opcode.opSTOREFP));

    // m32(rs1 + imm_s) <- rs2
    const word_result: u32 = memory.read32(asU32(asI32(cpu.xreg[rs1]) + imm_s), &cpu.busCode);

    try testing.expectEqual(expected, @bitCast(f32, word_result));

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);
}
