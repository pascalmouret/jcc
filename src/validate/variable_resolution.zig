const std = @import("std");

const ast = @import("../parser.zig").ast;

const ValidationError = error{
    AlreadyDeclared,
    NotDeclared,
    InvalidLValue,
};

const Context = struct {
    allocator: std.mem.Allocator,
    var_id: usize,
    pub fn init(allocator: std.mem.Allocator) Context {
        return Context{
            .allocator = allocator,
            .var_id = 0,
        };
    }
    pub fn generateUniqueIdentifier(self: Context, identifier: ast.Identifier) !ast.Identifier {
        return ast.Identifier{
            .name = try std.fmt.allocPrint(self.allocator, "ident.{d}.{s}", .{ self.var_id, identifier.name }),
            .position = identifier.position,
        };
    }
};

pub fn resolveVariables(program: ast.Program) !ast.Program {
    defer program.deinit();

    var context = Context.init(program.allocator);

    const result = ast.Program{
        .allocator = program.allocator,
        .function = try processFunction(&context, program.function),
    };

    return result;
}

fn processFunction(context: *Context, function: ast.Function) !ast.Function {
    var list = std.ArrayList(ast.BlockItem).init(context.allocator);
    defer list.deinit();
    errdefer for (list.items) |item| item.deinit(context.allocator);

    var map = std.StringHashMap([]const u8).init(context.allocator);
    defer map.deinit();

    for (function.body) |item| try list.append(try processBlockItem(context, &map, item));

    return ast.Function{
        .name = ast.Identifier{ .name = try context.allocator.dupe(u8, function.name.name), .position = function.name.position },
        .position = function.position,
        .body = try list.toOwnedSlice(),
    };
}

fn processBlockItem(context: *Context, map: *std.StringHashMap([]const u8), block_item: ast.BlockItem) !ast.BlockItem {
    switch (block_item) {
        .declaration => |declaration| {
            if (map.get(declaration.name.name)) |_| {
                return validationError(
                    ValidationError.AlreadyDeclared,
                    declaration.position,
                    "Variable '{s}' has already been decalred.",
                    .{declaration.name.name},
                );
            } else {
                const unique_identifier = try context.generateUniqueIdentifier(declaration.name);

                try map.put(declaration.name.name, unique_identifier.name);

                const result = ast.Declaration{
                    .name = unique_identifier,
                    .position = declaration.position,
                    .expression = if (declaration.expression) |exp| try processExpression(context, map, exp) else null,
                };

                return ast.BlockItem{ .declaration = result };
            }
        },
        .statement => |statement| return ast.BlockItem{ .statement = try processStatement(context, map, statement) },
    }
}

fn processStatement(context: *Context, map: *std.StringHashMap([]const u8), statement: ast.Statement) !ast.Statement {
    switch (statement) {
        .null => return ast.Statement{ .null = .{} },
        .ret => |ret| return ast.Statement{
            .ret = ast.Ret{
                .expression = try processExpression(context, map, ret.expression),
                .position = ret.position,
            },
        },
        .expression => |expression| return ast.Statement{
            .expression = try processExpression(context, map, expression),
        },
    }
}

fn processExpression(context: *Context, map: *std.StringHashMap([]const u8), expression: *ast.Expression) (ValidationError || error{OutOfMemory})!*ast.Expression {
    switch (expression.*) {
        .assignment => |assignment| {
            if (assignment.left.* == .factor and assignment.left.factor.* == .variable) {
                return try ast.Expression.assignment(
                    context.allocator,
                    try processExpression(context, map, assignment.left),
                    try processExpression(context, map, assignment.right),
                    assignment.position,
                );
            }

            return validationError(
                ValidationError.InvalidLValue,
                assignment.position,
                "Left hand side of assignment must be a variable.",
                .{},
            );
        },
        .binary => |binary| {
            return try ast.Expression.binary(
                context.allocator,
                binary.operator,
                try processExpression(context, map, binary.left),
                try processExpression(context, map, binary.right),
                binary.position,
            );
        },
        .factor => |factor| return ast.Expression.factor(context.allocator, try processFactor(context, map, factor)),
    }
}

fn processFactor(context: *Context, map: *std.StringHashMap([]const u8), factor: *ast.Factor) (ValidationError || error{OutOfMemory})!*ast.Factor {
    switch (factor.*) {
        .variable => |variable| {
            if (map.get(variable.name.name)) |unique| {
                return ast.Factor.variable(
                    context.allocator,
                    ast.Identifier{
                        .name = try context.allocator.dupe(u8, unique),
                        .position = variable.name.position,
                    },
                    variable.position,
                );
            } else {
                return validationError(ValidationError.NotDeclared, variable.position, "Variable '{s}' has not been declared.", .{variable.name.name});
            }
        },
        .constant => |constant| return try ast.Factor.constant(context.allocator, constant.value, constant.position),
        .unary => |unary| return try ast.Factor.unary(context.allocator, unary.operator, try processFactor(context, map, unary.factor), unary.position),
        .expression => |expression| return try ast.Factor.expression(context.allocator, try processExpression(context, map, expression)),
    }
}

fn validationError(
    err: ValidationError,
    position: ast.Position,
    comptime format: []const u8,
    args: anytype,
) ValidationError {
    std.log.err("{s} [{d}:{d}]: " ++ format, .{ @errorName(err), position.line, position.character } ++ args);
    return err;
}
