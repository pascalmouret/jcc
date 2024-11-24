const std = @import("std");

const tacky = @import("../tacky.zig").tacky;

pub fn program_to_x86(allocator: std.mem.Allocator, program: tacky.Program) !Progam {
    return try Progam.from_program(allocator, program);
}

const Progam = struct {
    allocator: std.mem.Allocator,
    function_definition: FunctionDefinition,
    pub fn from_program(allocator: std.mem.Allocator, program: tacky.Program) !Progam {
        return Progam{
            .allocator = allocator,
            .function_definition = try FunctionDefinition.from_function_definition(allocator, program.function_definition),
        };
    }
    pub fn write(self: Progam, writer: std.fs.File.Writer) !void {
        try self.function_definition.write(writer);
    }
    pub fn deinit(self: Progam) void {
        self.function_definition.deinit(self.allocator);
    }
};

const FunctionDefinition = struct {
    name: []u8,
    instructions: []Instruction,
    pub fn from_function_definition(allocator: std.mem.Allocator, function_definition: tacky.FunctionDefinition) !FunctionDefinition {
        var list = std.ArrayList(Instruction).init(allocator);
        defer list.deinit();

        for (function_definition.instructions) |instruction| {
            try Instruction.append_from_instruction(&list, instruction);
        }

        const initial = try list.toOwnedSlice();
        defer allocator.free(initial);

        const stacked = try FunctionDefinition.replace_pseudo(allocator, initial);
        defer allocator.free(stacked);

        const final = try FunctionDefinition.fix_memory_mov(allocator, stacked);

        return FunctionDefinition{ .name = function_definition.name, .instructions = final };
    }
    pub fn replace_pseudo(allocator: std.mem.Allocator, instructions: []const Instruction) ![]Instruction {
        var offset_map = std.StringHashMap(usize).init(allocator);
        defer offset_map.deinit();

        var list = std.ArrayList(Instruction).init(allocator);
        defer list.deinit();

        for (instructions) |instruction| {
            switch (instruction) {
                .mov => |mov| {
                    try list.append(Instruction.mov(try mov.src.stack_if_pseudo(&offset_map), try mov.dst.stack_if_pseudo(&offset_map)));
                },
                .unary => |unary| {
                    try list.append(Instruction.unary(unary.operator, try unary.operand.stack_if_pseudo(&offset_map)));
                },
                else => try list.append(instruction),
            }
        }

        try list.insert(0, Instruction.allocate_stack(offset_map.count() * 4));

        return try list.toOwnedSlice();
    }
    pub fn fix_memory_mov(allocator: std.mem.Allocator, instructions: []const Instruction) ![]Instruction {
        var list = std.ArrayList(Instruction).init(allocator);
        defer list.deinit();

        for (instructions) |instruction| {
            switch (instruction) {
                .mov => |mov| {
                    if (mov.dst == .stack and mov.src == .stack) {
                        try list.append(Instruction.mov(mov.src, Operand.register(.r10d)));
                        try list.append(Instruction.mov(Operand.register(.r10d), mov.dst));
                    } else {
                        try list.append(instruction);
                    }
                },
                else => try list.append(instruction),
            }
        }

        return list.toOwnedSlice();
    }
    pub fn write(self: FunctionDefinition, writer: std.fs.File.Writer) !void {
        try writer.print("   .globl _{s}\n_{s}:\n    pushq %rbp\n    movq %rsp, %rbp\n", .{ self.name, self.name });
        for (self.instructions) |instruction| {
            try instruction.write(writer);
        }
    }
    pub fn deinit(self: FunctionDefinition, allocator: std.mem.Allocator) void {
        allocator.free(self.instructions);
    }
};

const Instruction = union(enum) {
    mov: Mov,
    ret: Ret,
    unary: Unary,
    allocate_stack: AllocateStack,
    pub fn append_from_instruction(list: *std.ArrayList(Instruction), instruction: tacky.Instruction) !void {
        switch (instruction) {
            .ret => |r| {
                try list.append(Instruction.mov(Operand.from_val(r.val), Operand.register(Register.eax)));
                try list.append(Instruction.ret());
            },
            .unary => |u| {
                try list.append(Instruction.mov(Operand.from_val(u.src), Operand.pseudo(u.dst.name)));
                try list.append(Instruction.unary(UnaryOperator.from_tacky(u), Operand.pseudo(u.dst.name)));
            },
        }
    }
    pub fn write(self: Instruction, writer: std.fs.File.Writer) !void {
        switch (self) {
            .mov => |i| try i.write(writer),
            .ret => try Ret.write(writer),
            .unary => |u| try u.write(writer),
            .allocate_stack => |as| try as.write(writer),
        }
    }
    pub fn mov(src: Operand, dst: Operand) Instruction {
        return Instruction{ .mov = Mov{ .src = src, .dst = dst } };
    }
    pub fn ret() Instruction {
        return Instruction{ .ret = Ret{} };
    }
    pub fn unary(operator: UnaryOperator, operand: Operand) Instruction {
        return Instruction{ .unary = Unary{ .operator = operator, .operand = operand } };
    }
    pub fn allocate_stack(size: usize) Instruction {
        return Instruction{ .allocate_stack = AllocateStack{ .size = size } };
    }
};

const Ret = struct {
    pub fn write(writer: std.fs.File.Writer) !void {
        try writer.writeAll("    movq %rbp, %rsp\n    popq %rbp\n    ret\n");
    }
};

const Mov = struct {
    src: Operand,
    dst: Operand,
    pub fn write(self: Mov, writer: std.fs.File.Writer) !void {
        try writer.writeAll("    movl ");
        try self.src.write(writer);
        try writer.writeAll(", ");
        try self.dst.write(writer);
        try writer.writeByte('\n');
    }
};

const UnaryOperator = enum {
    neg,
    not,
    pub fn from_tacky(unary: tacky.Unary) UnaryOperator {
        switch (unary.operator) {
            .complement => return .not,
            .negate => return .neg,
        }
    }
    pub fn write(self: UnaryOperator, writer: std.fs.File.Writer) !void {
        switch (self) {
            .neg => try writer.writeAll("negl"),
            .not => try writer.writeAll("notl"),
        }
    }
};

const Unary = struct {
    operator: UnaryOperator,
    operand: Operand,
    pub fn write(self: Unary, writer: std.fs.File.Writer) !void {
        try writer.writeAll("    ");
        try self.operator.write(writer);
        try writer.writeByte(' ');
        try self.operand.write(writer);
        try writer.writeByte('\n');
    }
};

const AllocateStack = struct {
    size: usize,
    pub fn write(self: AllocateStack, writer: std.fs.File.Writer) !void {
        try writer.print("    subq ${d}, %rsp\n", .{self.size});
    }
};

const Operand = union(enum) {
    register: Register,
    immediate: Immediate,
    pseudo: Pseudo,
    stack: Stack,
    pub fn write(self: Operand, writer: std.fs.File.Writer) !void {
        switch (self) {
            .register => |r| try r.write(writer),
            .immediate => |i| try i.write(writer),
            .stack => |s| try s.write(writer),
            .pseudo => unreachable,
        }
    }
    pub fn from_val(val: tacky.Val) Operand {
        switch (val) {
            .constant => |c| return Operand.immediate(c.value),
            .tmp => |t| return Operand.pseudo(t.name),
        }
    }
    pub fn stack_if_pseudo(self: Operand, map: *std.StringHashMap(usize)) !Operand {
        switch (self) {
            .pseudo => |p| {
                if (map.get(p.name)) |offset| {
                    return Operand.stack(offset);
                } else {
                    try map.put(p.name, (map.count() + 1) * 4);
                    return self.stack_if_pseudo(map);
                }
            },
            else => return self,
        }
    }
    pub fn immediate(int: isize) Operand {
        return Operand{ .immediate = Immediate{ .int = int } };
    }
    pub fn register(reg: Register) Operand {
        return Operand{ .register = reg };
    }
    pub fn pseudo(name: []u8) Operand {
        return Operand{ .pseudo = Pseudo{ .name = name } };
    }
    pub fn stack(offset: usize) Operand {
        return Operand{ .stack = Stack{ .offset = offset } };
    }
};

const Immediate = union(enum) {
    int: isize,
    pub fn write(self: Immediate, writer: std.fs.File.Writer) !void {
        switch (self) {
            .int => |int| try writer.print("${d}", .{int}),
        }
    }
};

const Register = enum {
    eax,
    r10d,
    pub fn write(self: Register, writer: std.fs.File.Writer) !void {
        try writer.print("%{s}", .{@tagName(self)});
    }
};

const Pseudo = struct {
    name: []u8,
};

const Stack = struct {
    offset: usize,
    pub fn write(self: Stack, writer: std.fs.File.Writer) !void {
        try writer.print("-{d}(%rbp)", .{self.offset});
    }
};
