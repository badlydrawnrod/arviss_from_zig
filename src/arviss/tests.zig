const std = @import("std");
const arviss = @import("arviss");

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

inline fn encodeJ(n: u32) u32 {
    return ((n & 0x100000) << 11) // imm[20]    -> j[31]
    | ((n & 0x7fe) << 20) // imm[10:1]  -> j[30:21]
    | ((n & 0x800) << 9) // imm[11]    -> j[20]
    | (n & 0x000ff000) // imm[19:12] -> j[19:12]
    ;
}

inline fn encodeB(n: u32) u32 {
    return ((n & 0x1000) << 19) // imm[12]   -> b[31]
    | ((n & 0x7e0) << 20) // imm[10:5] -> b[30:25]
    | ((n & 0x1e) << 7) // imm[4:1]  -> b[11:4]
    | ((n & 0x800) >> 4) // imm[11]   -> b[7]
    ;
}

inline fn encodeS(n: u32) u32 {
    return ((n & 0xfe0) << 20)  // imm[11:5] -> s[31:25]
            | ((n & 0x1f) << 7) // imm[4:0]  -> s[11:7]
            ;
}

inline fn encodeI(n: u32) u32 {
    return (n & 0xfff) << 20; // imm[11:0] -> s[31:20]
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

        _ = ArvissExecute(&cpu, @bitCast(u32, (imm_u << 12)) | encodeRd(rd) | @enumToInt(arviss.ArvissOpcode.opLUI));

        // rd <- imm_u
        try testing.expectEqual(imm_u, @bitCast(i32, cpu.xreg[rd]) >> 12);

        // pc <- pc + 4
        try testing.expectEqual(pc + 4, cpu.pc);
    }
}

test "lui leaves x0 as zero" {
    var cpu = Cpu();

    const imm_u: i32 = 123;
    const rd: u32 = 0;
    const pc: u32 = cpu.pc;

    _ = ArvissExecute(&cpu, @bitCast(u32, (imm_u << 12)) | encodeRd(rd) | @enumToInt(arviss.ArvissOpcode.opLUI));

    // x0 <- 0
    try testing.expectEqual(@intCast(u32, 0), cpu.xreg[0]);

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

        _ = ArvissExecute(&cpu, @bitCast(u32, (imm_u << 12)) | encodeRd(rd) | @enumToInt(arviss.ArvissOpcode.opAUIPC));

        // rd <- pc + imm_u
        try testing.expectEqual(pc +% @bitCast(u32, imm_u << 12), cpu.xreg[rd]);

        // pc <- pc + 4
        try testing.expectEqual(pc + 4, cpu.pc);
    }
}

test "auipc leaves x0 as zero" {
    var cpu = Cpu();

    const imm_u: i32 = 123;
    const rd: u32 = 0;
    const pc: u32 = cpu.pc;

    _ = ArvissExecute(&cpu, @bitCast(u32, (imm_u << 12)) | encodeRd(rd) | @enumToInt(arviss.ArvissOpcode.opAUIPC));

    // x0 <- 0
    try testing.expectEqual(@intCast(u32, 0), cpu.xreg[0]);

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

        _ = ArvissExecute(&cpu, encodeJ(@bitCast(u32, imm_j)) | encodeRd(rd) | @enumToInt(arviss.ArvissOpcode.opJAL));

        // rd <- pc + 4
        try testing.expectEqual(pc + 4, cpu.xreg[rd]);

        // pc <- pc + imm_j
        try testing.expectEqual(@bitCast(u32, @bitCast(i32, pc) + value), cpu.pc);
    }
}

test "jal leaves x0 as zero" {
    var cpu = Cpu();

    const value: i32 = 4;
    const imm_j: i32 = value;
    const rd: u32 = 0;
    const pc: u32 = cpu.pc;

    _ = ArvissExecute(&cpu, encodeJ(@bitCast(u32, imm_j)) | encodeRd(rd) | @enumToInt(arviss.ArvissOpcode.opJAL));

    // x0 <- 0
    try testing.expectEqual(@intCast(u32, 0), cpu.xreg[0]);

    // pc <- pc + imm_j
    try testing.expectEqual(@bitCast(u32, @bitCast(i32, pc) + value), cpu.pc);
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

        _ = ArvissExecute(&cpu, encodeI(@bitCast(u32, imm_i)) | encodeRs1(rs1) | (0b000 << 12) | encodeRd(rd) | @enumToInt(arviss.ArvissOpcode.opJALR));

        // rd <- pc + 4
        try testing.expectEqual(pc + 4, cpu.xreg[rd]);

        // pc <- (rs1 + imm_i) & ~1
        try testing.expectEqual(@bitCast(u32, (rs1_before + imm_i)) & 0xfffffffe, cpu.pc);
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

    _ = ArvissExecute(&cpu, encodeI(@bitCast(u32, imm_i)) | encodeRs1(rs1) | (0b000 << 12) | encodeRd(rd) | @enumToInt(arviss.ArvissOpcode.opJALR));

    // x0 <- 0
    try testing.expectEqual(@intCast(u32, 0), cpu.xreg[0]);

    // pc <- (rs1 + imm_i) & ~1
    try testing.expectEqual(@bitCast(u32, (rs1_before + imm_i)) & 0xfffffffe, cpu.pc);
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

    _ = ArvissExecute(&cpu, encodeB(imm_b) | encodeRs2(rs2) | encodeRs1(rs1) | (0b000 << 12) | @enumToInt(arviss.ArvissOpcode.opBRANCH));

    // pc <- pc + imm_b
    try testing.expectEqual(pc + imm_b, cpu.pc);

    // Branch not taken.
    pc = cpu.pc;
    cpu.xreg[rs1] = 5678;
    cpu.xreg[rs2] = 8765;

    _ = ArvissExecute(&cpu, encodeB(imm_b) | encodeRs2(rs2) | encodeRs1(rs1) | (0b000 << 12) | @enumToInt(arviss.ArvissOpcode.opBRANCH));

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

    _ = ArvissExecute(&cpu, encodeB(imm_b) | encodeRs2(rs2) | encodeRs1(rs1) | (0b001 << 12) | @enumToInt(arviss.ArvissOpcode.opBRANCH));

    // pc <- pc + imm_b
    try testing.expectEqual(pc + imm_b, cpu.pc);

    // Branch not taken.
    pc = cpu.pc;
    cpu.xreg[rs1] = 5678;
    cpu.xreg[rs2] = cpu.xreg[rs1];

    _ = ArvissExecute(&cpu, encodeB(imm_b) | encodeRs2(rs2) | encodeRs1(rs1) | (0b001 << 12) | @enumToInt(arviss.ArvissOpcode.opBRANCH));

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

    _ = ArvissExecute(&cpu, encodeB(imm_b) | encodeRs2(rs2) | encodeRs1(rs1) | (0b100 << 12) | @enumToInt(arviss.ArvissOpcode.opBRANCH));

    // pc <- pc + imm_b
    try testing.expectEqual(pc + imm_b, cpu.pc);

    // Branch not taken.
    pc = cpu.pc;
    cpu.xreg[rs1] = 456;
    cpu.xreg[rs2] = 123;

    _ = ArvissExecute(&cpu, encodeB(imm_b) | encodeRs2(rs2) | encodeRs1(rs1) | (0b100 << 12) | @enumToInt(arviss.ArvissOpcode.opBRANCH));

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

    _ = ArvissExecute(&cpu, encodeB(imm_b) | encodeRs2(rs2) | encodeRs1(rs1) | (0b101 << 12) | @enumToInt(arviss.ArvissOpcode.opBRANCH));

    // pc <- pc + imm_b
    try testing.expectEqual(pc + imm_b, cpu.pc);

    // Branch taken (equal)
    pc = cpu.pc;
    cpu.xreg[rs1] = 0xffffffff; // -1
    cpu.xreg[rs2] = 0xffffffff; // -1

    _ = ArvissExecute(&cpu, encodeB(imm_b) | encodeRs2(rs2) | encodeRs1(rs1) | (0b101 << 12) | @enumToInt(arviss.ArvissOpcode.opBRANCH));

    // pc <- pc + imm_b
    try testing.expectEqual(pc + imm_b, cpu.pc);

    // Branch not taken.
    pc = cpu.pc;
    cpu.xreg[rs1] = 0xffffffff; // -1
    cpu.xreg[rs2] = 0;

    _ = ArvissExecute(&cpu, encodeB(imm_b) | encodeRs2(rs2) | encodeRs1(rs1) | (0b101 << 12) | @enumToInt(arviss.ArvissOpcode.opBRANCH));

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

    _ = ArvissExecute(&cpu, encodeB(imm_b) | encodeRs2(rs2) | encodeRs1(rs1) | (0b110 << 12) | @enumToInt(arviss.ArvissOpcode.opBRANCH));

    // pc <- pc + imm_b
    try testing.expectEqual(pc + imm_b, cpu.pc);

    // Branch not taken.
    pc = cpu.pc;
    cpu.xreg[rs1] = 0xffffffff;
    cpu.xreg[rs2] = 0;

    _ = ArvissExecute(&cpu, encodeB(imm_b) | encodeRs2(rs2) | encodeRs1(rs1) | (0b110 << 12) | @enumToInt(arviss.ArvissOpcode.opBRANCH));

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

    _ = ArvissExecute(&cpu, encodeB(imm_b) | encodeRs2(rs2) | encodeRs1(rs1) | (0b111 << 12) | @enumToInt(arviss.ArvissOpcode.opBRANCH));

    // pc <- pc + imm_b
    try testing.expectEqual(pc + imm_b, cpu.pc);

    // Branch taken (equal)
    pc = cpu.pc;
    cpu.xreg[rs1] = 1;
    cpu.xreg[rs2] = 1;

    _ = ArvissExecute(&cpu, encodeB(imm_b) | encodeRs2(rs2) | encodeRs1(rs1) | (0b111 << 12) | @enumToInt(arviss.ArvissOpcode.opBRANCH));

    // pc <- pc + imm_b
    try testing.expectEqual(pc + imm_b, cpu.pc);

    // Branch not taken.
    pc = cpu.pc;
    cpu.xreg[rs1] = 0;
    cpu.xreg[rs2] = 0xffffffff;

    _ = ArvissExecute(&cpu, encodeB(imm_b) | encodeRs2(rs2) | encodeRs1(rs1) | (0b111 << 12) | @enumToInt(arviss.ArvissOpcode.opBRANCH));

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

    _ = ArvissExecute(&cpu, encodeI(imm_i) | encodeRs1(rs1) | (0b000 << 12) | encodeRd(rd) | @enumToInt(arviss.ArvissOpcode.opLOAD));

    // rd <- sx(m8(rs1 + imm_i))
    try testing.expectEqual(@intCast(u32, 123), cpu.xreg[rd]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);

    // Sign extend when bit 7 is one.
    pc = cpu.pc;
    memory.write8(cpu.xreg[rs1] + imm_i, 0xff, &cpu.busCode);

    _ = ArvissExecute(&cpu, encodeI(imm_i) | encodeRs1(rs1) | (0b000 << 12) | encodeRd(rd) | @enumToInt(arviss.ArvissOpcode.opLOAD));

    // rd <- sx(m8(rs1 + imm_i))
    try testing.expectEqual(@intCast(u32, 0xffffffff), cpu.xreg[rd]);

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

    _ = ArvissExecute(&cpu, encodeI(imm_i) | encodeRs1(rs1) | (0b001 << 12) | encodeRd(rd) | @enumToInt(arviss.ArvissOpcode.opLOAD));

    // rd <- sx(m16(rs1 + imm_i))
    try testing.expectEqual(@intCast(u32, 0x7fff), cpu.xreg[rd]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);

    // Sign extend when bit 15 is one.
    pc = cpu.pc;
    memory.write16(cpu.xreg[rs1] + imm_i, 0xffff, &cpu.busCode);

    _ = ArvissExecute(&cpu, encodeI(imm_i) | encodeRs1(rs1) | (0b001 << 12) | encodeRd(rd) | @enumToInt(arviss.ArvissOpcode.opLOAD));

    // rd <- sx(m16(rs1 + imm_i))
    try testing.expectEqual(@intCast(u32, 0xffffffff), cpu.xreg[rd]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);
}

test "lw (load word)" {
    var cpu = Cpu();

    // rd <- sx(m32(rs1 + imm_i)), pc += 4
    cpu.pc = 0x1000;
    var pc: u32 = cpu.pc;
    const imm_i: i32 = 274;
    const rd: u32 = 14;
    const rs1: u32 = 15;
    cpu.xreg[rs1] = rambase;

    // Sign extend when bit 31 is zero.
    memory.write32(cpu.xreg[rs1] + imm_i, 0x7fffffff, &cpu.busCode);

    _ = ArvissExecute(&cpu, encodeI(imm_i) | encodeRs1(rs1) | (0b010 << 12) | encodeRd(rd) | @enumToInt(arviss.ArvissOpcode.opLOAD));

    // rd <- sx(m32(rs1 + imm_i))
    try testing.expectEqual(@intCast(u32, 0x7fffffff), cpu.xreg[rd]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);

    // Sign extend when bit 31 is one.
    pc = cpu.pc;
    memory.write32(cpu.xreg[rs1] + imm_i, 0xffffffff, &cpu.busCode);

    _ = ArvissExecute(&cpu, encodeI(imm_i) | encodeRs1(rs1) | (0b010 << 12) | encodeRd(rd) | @enumToInt(arviss.ArvissOpcode.opLOAD));

    // rd <- sx(m32(rs1 + imm_i))
    try testing.expectEqual(@intCast(u32, 0xffffffff), cpu.xreg[rd]);

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
    memory.write8(@intCast(u32, @intCast(i32, cpu.xreg[rs1]) + imm_i), 123, &cpu.busCode);

    _ = ArvissExecute(&cpu, encodeI(@bitCast(u32, imm_i)) | encodeRs1(rs1) | (0b100 << 12) | encodeRd(rd) | @enumToInt(arviss.ArvissOpcode.opLOAD));

    // rd <- zx(m8(rs1 + imm_i))
    try testing.expectEqual(@intCast(u32, 123), cpu.xreg[rd]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);

    // Zero extend when bit 7 is one.
    pc = cpu.pc;
    memory.write8(@intCast(u32, @intCast(i32, cpu.xreg[rs1]) + imm_i), 0xff, &cpu.busCode);

    _ = ArvissExecute(&cpu, encodeI(@bitCast(u32, imm_i)) | encodeRs1(rs1) | (0b100 << 12) | encodeRd(rd) | @enumToInt(arviss.ArvissOpcode.opLOAD));

    // rd <- zx(m8(rs1 + imm_i))
    try testing.expectEqual(@intCast(u32, 0xff), cpu.xreg[rd]);

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
    memory.write16(@intCast(u32, @intCast(i32, cpu.xreg[rs1]) + imm_i), 0x7fff, &cpu.busCode);

    _ = ArvissExecute(&cpu, encodeI(@bitCast(u32, imm_i)) | encodeRs1(rs1) | (0b101 << 12) | encodeRd(rd) | @enumToInt(arviss.ArvissOpcode.opLOAD));

    // rd <- zx(m16(rs1 + imm_i))
    try testing.expectEqual(@intCast(u32, 0x7fff), cpu.xreg[rd]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);

    // Zero extend when bit 15 is one.
    pc = cpu.pc;
    memory.write16(@intCast(u32, @intCast(i32, cpu.xreg[rs1]) + imm_i), 0xffff, &cpu.busCode);

    _ = ArvissExecute(&cpu, encodeI(@bitCast(u32, imm_i)) | encodeRs1(rs1) | (0b101 << 12) | encodeRd(rd) | @enumToInt(arviss.ArvissOpcode.opLOAD));

    // rd <- zx(m16(rs1 + imm_i))
    try testing.expectEqual(@intCast(u32, 0xffff), cpu.xreg[rd]);

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
    _ = ArvissExecute(&cpu, encodeI(0) | encodeRs1(rs1) | (0b000 << 12) | encodeRd(0) | @enumToInt(arviss.ArvissOpcode.opLOAD));

    // x0 <- 0
    try testing.expectEqual(@intCast(u32, 0), cpu.xreg[0]);

    // lh
    _ = ArvissExecute(&cpu, encodeI(0) | encodeRs1(rs1) | (0b001 << 12) | encodeRd(0) | @enumToInt(arviss.ArvissOpcode.opLOAD));

    // x0 <- 0
    try testing.expectEqual(@intCast(u32, 0), cpu.xreg[0]);

    // lw
    _ = ArvissExecute(&cpu, encodeI(0) | encodeRs1(rs1) | (0b010 << 12) | encodeRd(0) | @enumToInt(arviss.ArvissOpcode.opLOAD));

    // x0 <- 0
    try testing.expectEqual(@intCast(u32, 0), cpu.xreg[0]);

    // lbu
    _ = ArvissExecute(&cpu, encodeI(0) | encodeRs1(rs1) | (0b100 << 12) | encodeRd(0) | @enumToInt(arviss.ArvissOpcode.opLOAD));

    // x0 <- 0
    try testing.expectEqual(@intCast(u32, 0), cpu.xreg[0]);

    // lhu
    _ = ArvissExecute(&cpu, encodeI(0) | encodeRs1(rs1) | (0b101 << 12) | encodeRd(0) | @enumToInt(arviss.ArvissOpcode.opLOAD));

    // x0 <- 0
    try testing.expectEqual(@intCast(u32, 0), cpu.xreg[0]);
}

test "sb (store byte)" {
    var cpu = Cpu();

    // m8(rs1 + imm_s) <- rs2[7:0], pc += 4
    var pc: u32 = cpu.pc;
    const imm_s: i32 = -123;
    const rs1: u32 = 12;
    const rs2: u32 = 3;
    cpu.xreg[rs1] = rambase + ramsize / 2;

    _ = ArvissExecute(&cpu, encodeS(@bitCast(u32, imm_s)) | encodeRs2(rs2) | encodeRs1(rs1) | (0b000 << 12) | @enumToInt(arviss.ArvissOpcode.opSTORE));

    // m8(rs1 + imm_s) <- rs2[7:0]
    const byte_result = memory.read8(@bitCast(u32, @bitCast(i32, cpu.xreg[rs1]) + imm_s), &cpu.busCode);
    try testing.expectEqual(arviss.BusCode.bcOK, cpu.busCode);
    try testing.expectEqual(byte_result, @intCast(u8, cpu.xreg[rs2] & 0xff));

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

    _ = ArvissExecute(&cpu, encodeS(@bitCast(u32, imm_s)) | encodeRs2(rs2) | encodeRs1(rs1) | (0b001 << 12) | @enumToInt(arviss.ArvissOpcode.opSTORE));

    // m16(rs1 + imm_s) <- rs2[15:0]
    const halfword_result = memory.read16(@bitCast(u32, @bitCast(i32, cpu.xreg[rs1]) + imm_s), &cpu.busCode);
    try testing.expectEqual(arviss.BusCode.bcOK, cpu.busCode);
    try testing.expectEqual(halfword_result, @intCast(u16, cpu.xreg[rs2] & 0xffff));

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

    _ = ArvissExecute(&cpu, encodeS(@bitCast(u32, imm_s)) | encodeRs2(rs2) | encodeRs1(rs1) | (0b010 << 12) | @enumToInt(arviss.ArvissOpcode.opSTORE));

    // m32(rs1 + imm_s) <- rs2[31:0]
    const word_result = memory.read32(@bitCast(u32, @bitCast(i32, cpu.xreg[rs1]) + imm_s), &cpu.busCode);
    try testing.expectEqual(arviss.BusCode.bcOK, cpu.busCode);
    try testing.expectEqual(word_result, cpu.xreg[rs2]);

    // pc <- pc + 4
    try testing.expectEqual(pc + 4, cpu.pc);
}
