const std = @import("std");
const Allocator = std.mem.Allocator;

const ast = @import("../parser/ast.zig");

pub fn programToTacky(allocator: Allocator, program: ast.Program) !Program {
    return Program.fromProgram(allocator, program);
}

pub const Program = struct {
    allocator: Allocator,
    function_definition: FunctionDefinition,
    pub fn fromProgram(allocator: Allocator, program: ast.Program) !Program {
        return Program{ .allocator = allocator, .function_definition = try FunctionDefinition.fromFunction(allocator, program.function) };
    }
    pub fn deinit(self: Program) void {
        self.function_definition.deinit(self.allocator);
    }
};

const FunctionContext = struct {
    allocator: Allocator,
    managed_strings: std.ArrayList([]u8),
    pub fn init(allocator: Allocator) FunctionContext {
        return FunctionContext{ .allocator = allocator, .managed_strings = std.ArrayList([]u8).init(allocator) };
    }
    pub fn allocateFormatedString(self: *FunctionContext, comptime fmt: []const u8, args: anytype) ![]u8 {
        const string = try std.fmt.allocPrint(self.allocator, fmt, args);
        errdefer self.allocator.free(string);
        try self.managed_strings.append(string);
        return string;
    }
    pub fn deinit(self: FunctionContext) void {
        for (self.managed_strings.items) |string| self.allocator.free(string);
        self.managed_strings.deinit();
    }
};

pub const FunctionDefinition = struct {
    name: []u8,
    context: FunctionContext,
    instructions: []Instruction,
    pub fn fromFunction(allocator: Allocator, function: ast.Function) !FunctionDefinition {
        var context = FunctionContext.init(allocator);
        return FunctionDefinition{
            .name = try allocator.dupe(u8, function.name.name),
            .instructions = try Instruction.fromBlockItem(&context, function.body[0]),
            .context = context,
        };
    }
    pub fn deinit(self: FunctionDefinition, allocator: Allocator) void {
        self.context.deinit();
        allocator.free(self.name);
        allocator.free(self.instructions);
    }
};

pub const Instruction = union(enum) {
    unary: Unary,
    ret: Ret,
    binary: Binary,
    copy: Copy,
    jump: Jump,
    label: Label,
    pub fn fromBlockItem(context: *FunctionContext, item: ast.BlockItem) ![]Instruction {
        switch (item) {
            .statement => return Instruction.fromStatement(context, item.statement),
            else => unreachable,
        }
    }
    pub fn fromStatement(context: *FunctionContext, statement: ast.Statement) ![]Instruction {
        var list = std.ArrayList(Instruction).init(context.allocator);
        defer list.deinit();

        switch (statement) {
            .ret => |s| {
                const dst = try resolveExpression(context, &list, s.expression);
                try list.append(Instruction{ .ret = Ret{ .val = dst } });
            },
            else => unreachable,
        }

        return list.toOwnedSlice();
    }
    fn resolveExpression(context: *FunctionContext, list: *std.ArrayList(Instruction), expression: *ast.Expression) error{OutOfMemory}!Val {
        switch (expression.*) {
            .factor => |factor| return try resolveFactor(context, list, factor),
            .binary => |binary| {
                if (binary.canShortCircuit()) {
                    const short_circuit_label = try Label.make(
                        context,
                        "{s}_{s}",
                        .{ @tagName(binary.operator), if (binary.operator == .logical_and) "false" else "true" },
                    );
                    const end_label = try Label.make(context, "{s}_end", .{@tagName(binary.operator)});

                    const dst = try Tmp.make(context);
                    const src1 = try Instruction.resolveExpression(context, list, binary.left);

                    switch (binary.operator) {
                        .logical_and => {
                            try list.append(Instruction{ .jump = Jump{ .condition = .{ .on = .zero, .val = src1 }, .label = short_circuit_label.name } });
                        },
                        .logical_or => {
                            try list.append(Instruction{ .jump = Jump{ .condition = .{ .on = .not_zero, .val = src1 }, .label = short_circuit_label.name } });
                        },
                        else => unreachable,
                    }

                    const src2 = try Instruction.resolveExpression(context, list, binary.right);

                    switch (binary.operator) {
                        .logical_and => {
                            try list.append(Instruction{ .jump = Jump{ .condition = .{ .on = .zero, .val = src2 }, .label = short_circuit_label.name } });
                        },
                        .logical_or => {
                            try list.append(Instruction{ .jump = Jump{ .condition = .{ .on = .not_zero, .val = src2 }, .label = short_circuit_label.name } });
                        },
                        else => unreachable,
                    }

                    try list.append(Instruction{ .copy = Copy{ .src = Val{ .constant = Constant{ .value = if (binary.operator == .logical_and) 1 else 0 } }, .dst = dst } });
                    try list.append(Instruction{ .jump = Jump{ .label = end_label.name } });
                    try list.append(Instruction{ .label = short_circuit_label });
                    try list.append(Instruction{ .copy = Copy{ .src = Val{ .constant = Constant{ .value = if (binary.operator == .logical_and) 0 else 1 } }, .dst = dst } });
                    try list.append(Instruction{ .label = end_label });

                    return Val{ .tmp = dst };
                } else {
                    const src1 = try Instruction.resolveExpression(context, list, binary.left);
                    const src2 = try Instruction.resolveExpression(context, list, binary.right);
                    const dst = try Tmp.make(context);
                    try list.append(Instruction{ .binary = Binary{ .operator = binary.operator, .src1 = src1, .src2 = src2, .dst = dst } });
                    return Val{ .tmp = dst };
                }
            },
            else => unreachable,
        }
    }
    fn resolveFactor(context: *FunctionContext, list: *std.ArrayList(Instruction), factor: *ast.Factor) error{OutOfMemory}!Val {
        switch (factor.*) {
            .constant => |constant| return Val{ .constant = Constant{ .value = constant.value } },
            .unary => |unary| {
                const src = try Instruction.resolveFactor(context, list, unary.factor);
                const dst = try Tmp.make(context);
                try list.append(Instruction{ .unary = Unary{ .operator = unary.operator, .src = src, .dst = dst } });
                return Val{ .tmp = dst };
            },
            .expression => |exp| {
                return try resolveExpression(context, list, exp);
            },
            else => unreachable,
        }
    }
};

pub const Ret = struct {
    val: Val,
};

pub const Unary = struct {
    operator: ast.UnaryOperator,
    src: Val,
    dst: Tmp,
};

pub const Binary = struct {
    operator: ast.BinaryOperator,
    src1: Val,
    src2: Val,
    dst: Tmp,
};

pub const Jump = struct {
    label: []u8,
    condition: ?struct {
        on: enum { zero, not_zero },
        val: Val,
    } = null,
};

pub const Copy = struct {
    src: Val,
    dst: Tmp,
};

pub const Label = struct {
    var seed: usize = 0;

    name: []u8,

    pub fn make(context: *FunctionContext, comptime prefix_fmt: []const u8, args: anytype) !Label {
        defer seed += 1;
        const label = try context.allocateFormatedString(prefix_fmt ++ "_{d}", args ++ .{seed});
        return Label{ .name = label };
    }
};

pub const Val = union(enum) {
    constant: Constant,
    tmp: Tmp,
};

const Constant = struct {
    value: i32,
};

pub const Tmp = struct {
    var seed: usize = 0;

    name: []u8,

    pub fn make(context: *FunctionContext) !Tmp {
        defer seed += 1;
        const name = try context.allocateFormatedString("tmp_${d}", .{seed});
        return Tmp{ .name = name };
    }
};
