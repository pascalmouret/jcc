const std = @import("std");

const ast = @import("./ast.zig");

const PrettyPrinter = struct {
    writer: std.fs.File.Writer,
    current_indent: usize,
    pub fn init(writer: std.fs.File.Writer) PrettyPrinter {
        return PrettyPrinter{ .writer = writer, .current_indent = 0 };
    }
    pub fn indent(self: *PrettyPrinter) void {
        self.current_indent += 2;
    }
    pub fn undent(self: *PrettyPrinter) void {
        if (self.current_indent < 2) {
            self.current_indent = 0;
        } else {
            self.current_indent -= 2;
        }
    }
    pub fn printLine(
        self: PrettyPrinter,
        comptime fmt: []const u8,
        args: anytype,
    ) !void {
        for (0..self.current_indent) |_| try self.writer.writeByte(' ');
        try self.writer.print(fmt, args);
        try self.writer.writeByte('\n');
    }
};

pub fn printProgram(program: ast.Program, writer: std.fs.File.Writer) !void {
    var printer = PrettyPrinter.init(writer);

    try printer.printLine("Program(", .{});
    printer.indent();
    try printFunction(program.function, &printer);
    printer.undent();
    try printer.printLine(")", .{});
}

fn printFunction(function: ast.Function, printer: *PrettyPrinter) !void {
    try printer.printLine("Function(", .{});
    printer.indent();
    try printer.printLine("name = {s}", .{function.name.name});
    for (function.body) |block_item| {
        try printBlockItem(block_item, printer);
    }
    printer.undent();
    try printer.printLine(")", .{});
}

fn printBlockItem(item: ast.BlockItem, printer: *PrettyPrinter) !void {
    switch (item) {
        .statement => try printStatement(item.statement, printer),
        .declaration => try printDeclaration(item.declaration, printer),
    }
}

fn printDeclaration(declaration: ast.Declaration, printer: *PrettyPrinter) !void {
    try printer.printLine("Declaration(", .{});
    printer.indent();
    try printer.printLine("name = {s}", .{declaration.identifier.name});
    if (declaration.expression) |expression| {
        try printExpression(expression, printer);
    }
    printer.undent();
    try printer.printLine(")", .{});
}

fn printStatement(statement: ast.Statement, printer: *PrettyPrinter) !void {
    try printer.printLine("{s}(", .{@tagName(statement)});
    printer.indent();

    switch (statement) {
        .@"return" => |ret| {
            try printExpression(ret.expression, printer);
        },
        .expression => |exp| try printExpression(exp, printer),
        .null => {},
    }

    printer.undent();
    try printer.printLine(")", .{});
}

fn printExpression(expression: *ast.Expression, printer: *PrettyPrinter) std.fs.File.WriteError!void {
    switch (expression.*) {
        .factor => |factor| try printFactor(factor, printer),
        .binary => |binary| {
            try printer.printLine("Binary(", .{});
            printer.indent();
            try printer.printLine("{s}", .{@tagName(binary.operator.operator)});
            try printExpression(binary.left, printer);
            try printExpression(binary.right, printer);
            printer.undent();
            try printer.printLine(")", .{});
        },
        .assignment => |assignment| {
            try printer.printLine("Assignment(", .{});
            printer.indent();
            try printExpression(assignment.left, printer);
            try printExpression(assignment.right, printer);
            printer.undent();
            try printer.printLine(")", .{});
        },
    }
}

fn printFactor(factor: *ast.Factor, printer: *PrettyPrinter) !void {
    switch (factor.*) {
        .unary => |u| {
            try printer.printLine("{s}(", .{@tagName(u.operator.operator)});
            printer.indent();
            try printFactor(u.factor, printer);
            printer.undent();
            try printer.printLine(")", .{});
        },
        .expression => |e| try printExpression(e, printer),
        .constant => try printer.printLine("Constant({d})", .{factor.constant.value}),
        .variable => |variable| {
            try printer.printLine("Variable({s})", .{variable.identifier.name});
        },
    }
}
