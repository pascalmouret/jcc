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
    pub fn dupeString(self: *FunctionContext, string: []const u8) ![]u8 {
        const dupe = try self.allocator.dupe(u8, string);
        errdefer self.allocator.free(dupe);
        try self.managed_strings.append(dupe);
        return dupe;
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
        errdefer context.deinit();

        var list = std.ArrayList(Instruction).init(allocator);
        defer list.deinit();

        for (function.body) |item| {
            try Instruction.resolveBlockItem(&context, &list, item);
        }

        // make sure we return 0 if no return happened before
        try list.append(Instruction{ .@"return" = Return{ .val = Val{ .constant = Constant{ .value = 0 } } } });

        return FunctionDefinition{
            .name = try allocator.dupe(u8, function.name.name),
            .instructions = try list.toOwnedSlice(),
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
    @"return": Return,
    binary: Binary,
    copy: Copy,
    jump: Jump,
    label: Label,
    pub fn resolveBlockItem(context: *FunctionContext, list: *std.ArrayList(Instruction), item: ast.BlockItem) !void {
        switch (item) {
            .statement => return Instruction.resolveStatement(context, list, item.statement),
            .declaration => return Instruction.resolveDeclaration(context, list, item.declaration),
        }
    }
    fn resolveStatement(context: *FunctionContext, list: *std.ArrayList(Instruction), statement: ast.Statement) !void {
        switch (statement) {
            .@"return" => |s| {
                const dst = try resolveExpression(context, list, s.expression);
                try list.append(Instruction{ .@"return" = Return{ .val = dst } });
            },
            .expression => |exp| {
                // we get a destination but we don't need it for standalone expressions
                _ = try resolveExpression(context, list, exp);
            },
            .null => {},
        }
    }
    fn resolveDeclaration(context: *FunctionContext, list: *std.ArrayList(Instruction), declaration: ast.Declaration) !void {
        if (declaration.expression) |exp| {
            const dst = try resolveExpression(context, list, exp);
            try list.append(Instruction{ .copy = Copy{ .src = dst, .dst = try Tmp.fromIdentifier(context, declaration) } });
        }
    }
    fn resolveExpression(context: *FunctionContext, list: *std.ArrayList(Instruction), expression: *ast.Expression) error{OutOfMemory}!Val {
        switch (expression.*) {
            .factor => |factor| return try resolveFactor(context, list, factor),
            .binary => |binary| {
                if (binary.canShortCircuit()) {
                    const short_circuit_label = try Label.make(
                        context,
                        "{s}_{s}",
                        .{ @tagName(binary.operator.operator), if (binary.operator.operator == .logical_and) "false" else "true" },
                    );
                    const end_label = try Label.make(context, "{s}_end", .{@tagName(binary.operator.operator)});

                    const dst = try Tmp.make(context);
                    const src1 = try Instruction.resolveExpression(context, list, binary.left);

                    switch (binary.operator.operator) {
                        .logical_and => {
                            try list.append(Instruction{ .jump = Jump{ .condition = .{ .on = .zero, .val = src1 }, .label = short_circuit_label.name } });
                        },
                        .logical_or => {
                            try list.append(Instruction{ .jump = Jump{ .condition = .{ .on = .not_zero, .val = src1 }, .label = short_circuit_label.name } });
                        },
                        else => unreachable,
                    }

                    const src2 = try Instruction.resolveExpression(context, list, binary.right);

                    switch (binary.operator.operator) {
                        .logical_and => {
                            try list.append(Instruction{ .jump = Jump{ .condition = .{ .on = .zero, .val = src2 }, .label = short_circuit_label.name } });
                        },
                        .logical_or => {
                            try list.append(Instruction{ .jump = Jump{ .condition = .{ .on = .not_zero, .val = src2 }, .label = short_circuit_label.name } });
                        },
                        else => unreachable,
                    }

                    try list.append(Instruction{ .copy = Copy{ .src = Val{ .constant = Constant{ .value = if (binary.operator.operator == .logical_and) 1 else 0 } }, .dst = dst } });
                    try list.append(Instruction{ .jump = Jump{ .label = end_label.name } });
                    try list.append(Instruction{ .label = short_circuit_label });
                    try list.append(Instruction{ .copy = Copy{ .src = Val{ .constant = Constant{ .value = if (binary.operator.operator == .logical_and) 0 else 1 } }, .dst = dst } });
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
            .assignment => |assignment| {
                // This always has to be a variable as we validated this before
                const destination = try Instruction.resolveExpression(context, list, assignment.left);
                const value = try Instruction.resolveExpression(context, list, assignment.right);
                try list.append(Instruction{ .copy = Copy{ .src = value, .dst = destination.tmp } });
                return destination;
            },
        }
    }
    fn resolveFactor(context: *FunctionContext, list: *std.ArrayList(Instruction), factor: *ast.Factor) error{OutOfMemory}!Val {
        switch (factor.*) {
            .constant => |constant| return Val{ .constant = Constant{ .value = constant.value } },
            .unary => |unary| {
                const src = try Instruction.resolveFactor(context, list, unary.factor);
                const dst = try Tmp.make(context);

                if (unary.operator.operator == .increment or unary.operator.operator == .decrement) {
                    const operator = if (unary.operator.operator == .increment) ast.Operator.add else ast.Operator.subtract;

                    var postfix_copy: ?Tmp = null;
                    if (unary.is_postfix) {
                        postfix_copy = try Tmp.make(context);
                        try list.append(Instruction{ .copy = Copy{ .src = src, .dst = postfix_copy.? } });
                    }

                    try list.append(Instruction{ .binary = Binary{ .operator = operator, .src1 = src, .src2 = Val{ .constant = Constant{ .value = 1 } }, .dst = dst } });
                    try list.append(Instruction{ .copy = Copy{ .src = Val{ .tmp = dst }, .dst = (try Instruction.resolveFactor(context, list, unary.factor)).tmp } });

                    if (unary.is_postfix) {
                        return Val{ .tmp = postfix_copy.? };
                    } else {
                        return Val{ .tmp = dst };
                    }
                } else {
                    try list.append(Instruction{ .unary = Unary{ .operator = unary.operator, .src = src, .dst = dst } });
                    return Val{ .tmp = dst };
                }
            },
            .expression => |exp| {
                return try resolveExpression(context, list, exp);
            },
            .variable => |variable| {
                return Val{ .tmp = try Tmp.fromIdentifier(context, variable) };
            },
        }
    }
};

pub const Return = struct {
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
        const name = try context.allocateFormatedString("tmp.${d}", .{seed});
        return Tmp{ .name = name };
    }

    pub fn fromIdentifier(context: *FunctionContext, variableOrDeclaration: anytype) !Tmp {
        const name = try context.dupeString(variableOrDeclaration.identifier.name);
        return Tmp{ .name = name };
    }
};
