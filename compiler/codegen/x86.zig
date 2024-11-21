const std = @import("std");

const ast = @import("../parser.zig").ast;

pub fn program_to_x86(allocator: std.mem.Allocator, program: ast.Program) !Progam {
    return try Progam.from_program(allocator, program);
}

const Progam = struct {
    allocator: std.mem.Allocator,
    function_definition: FunctionDefinition,
    pub fn from_program(allocator: std.mem.Allocator, program: ast.Program) !Progam {
        return Progam{
            .allocator = allocator,
            .function_definition = try FunctionDefinition.from_function(allocator, program.function),
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
    pub fn from_function(allocator: std.mem.Allocator, function: ast.Function) !FunctionDefinition {
        return FunctionDefinition{ .name = function.name.name, .instructions = try Instruction.from_statement(allocator, function.body) };
    }
    pub fn write(self: FunctionDefinition, writer: std.fs.File.Writer) !void {
        try writer.print("   .globl _{s}\n_{s}:\n", .{ self.name, self.name });
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
    pub fn from_statement(allocator: std.mem.Allocator, statement: ast.Statement) ![]Instruction {
        switch (statement) {
            .ret => |r| {
                var result = try allocator.alloc(Instruction, 2);
                result[0] = Instruction.mov(
                    Operand.immediate(r.expression.constant.value),
                    Operand.register(Register.eax),
                );
                result[1] = Instruction.ret();
                return result;
            },
        }
    }
    pub fn write(self: Instruction, writer: std.fs.File.Writer) !void {
        _ = try writer.write("    ");

        switch (self) {
            .mov => |i| try i.write(writer),
            .ret => try Ret.write(writer),
        }
        _ = try writer.write("\n");
    }
    pub fn mov(src: Operand, dst: Operand) Instruction {
        return Instruction{ .mov = Mov{ .src = src, .dst = dst } };
    }
    pub fn ret() Instruction {
        return Instruction{ .ret = Ret{} };
    }
};

const Ret = struct {
    pub fn write(writer: std.fs.File.Writer) !void {
        _ = try writer.write("ret");
    }
};

const Mov = struct {
    src: Operand,
    dst: Operand,
    pub fn write(self: Mov, writer: std.fs.File.Writer) !void {
        _ = try writer.write("movl ");
        try self.src.write(writer);
        _ = try writer.write(", ");
        try self.dst.write(writer);
    }
};

const Operand = union(enum) {
    register: Register,
    immediate: Immediate,
    pub fn write(self: Operand, writer: std.fs.File.Writer) !void {
        switch (self) {
            .register => |r| try r.write(writer),
            .immediate => |i| {
                try i.write(writer);
            },
        }
    }
    pub fn immediate(int: isize) Operand {
        return Operand{ .immediate = Immediate{ .int = int } };
    }
    pub fn register(reg: Register) Operand {
        return Operand{ .register = reg };
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
    pub fn write(self: Register, writer: std.fs.File.Writer) !void {
        try writer.print("%{s}", .{@tagName(self)});
    }
};
