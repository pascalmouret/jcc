const std = @import("std");
const Allocator = std.mem.Allocator;

const ast = @import("../parser/ast.zig");

pub fn program_to_tacky(allocator: Allocator, program: ast.Program) !Program {
    return Program.from_program(allocator, program);
}

pub const Program = struct {
    allocator: Allocator,
    function_definition: FunctionDefinition,
    pub fn from_program(allocator: Allocator, program: ast.Program) !Program {
        return Program{ .allocator = allocator, .function_definition = try FunctionDefinition.from_function(allocator, program.function) };
    }
    pub fn deinit(self: Program) void {
        self.function_definition.deinit(self.allocator);
    }
};

pub const FunctionDefinition = struct {
    name: []u8,
    instructions: []Instruction,
    pub fn from_function(allocator: Allocator, function: ast.Function) !FunctionDefinition {
        return FunctionDefinition{ .name = function.name.name, .instructions = try Instruction.from_statement(allocator, function.body) };
    }
    pub fn deinit(self: FunctionDefinition, allocator: Allocator) void {
        for (self.instructions) |i| i.deinit(allocator);
        allocator.free(self.instructions);
    }
};

pub const Instruction = union(enum) {
    unary: Unary,
    ret: Ret,
    pub fn from_statement(allocator: Allocator, statement: ast.Statement) ![]Instruction {
        var list = std.ArrayList(Instruction).init(allocator);
        defer list.deinit();

        switch (statement) {
            .ret => |s| {
                const dst = try resolve_expression(&list, s.expression);
                try list.append(Instruction{ .ret = Ret{ .val = dst } });
            },
        }

        return list.toOwnedSlice();
    }
    fn resolve_expression(list: *std.ArrayList(Instruction), expression: *ast.Expression) error{OutOfMemory}!Val {
        switch (expression.*) {
            .constant => |exp| return Val{ .constant = Constant{ .value = exp.value } },
            .unary => |exp| {
                const src = try Instruction.resolve_expression(list, exp.expression);
                const dst = try Tmp.make(list.allocator);
                switch (exp.operator) {
                    .complement => try list.append(Instruction{ .unary = Unary{ .operator = .complement, .src = src, .dst = dst } }),
                    .negate => try list.append(Instruction{ .unary = Unary{ .operator = .negate, .src = src, .dst = dst } }),
                }
                return Val{ .tmp = dst };
            },
        }
    }
    pub fn deinit(self: Instruction, allocator: Allocator) void {
        switch (self) {
            .unary => |i| i.deinit(allocator),
            else => {},
        }
    }
};

const Ret = struct {
    val: Val,
    pub fn deinit(self: Ret, allocator: Allocator) void {
        self.val.deinit(allocator);
    }
};

pub const Unary = struct {
    operator: enum { complement, negate },
    src: Val,
    dst: Tmp,
    pub fn deinit(self: Unary, alloator: Allocator) void {
        self.dst.deinit(alloator);
    }
};

pub const Val = union(enum) {
    constant: Constant,
    tmp: Tmp,
    pub fn deinit(self: Val, allocator: Allocator) void {
        switch (self) {
            .tmp => self.tmp.deinit(allocator),
            else => {},
        }
    }
};

const Constant = struct {
    value: u32,
};

const Tmp = struct {
    var seed: usize = 0;

    name: []u8,

    pub fn make(allocator: Allocator) !Tmp {
        defer seed += 1;
        return Tmp{ .name = try std.fmt.allocPrint(allocator, "tmp_${d}", .{seed}) };
    }
    pub fn deinit(self: Tmp, allocator: Allocator) void {
        allocator.free(self.name);
    }
};
