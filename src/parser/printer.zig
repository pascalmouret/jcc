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
    pub fn print_line(
        self: PrettyPrinter,
        comptime fmt: []const u8,
        args: anytype,
    ) !void {
        for (0..self.current_indent) |_| try self.writer.writeByte(' ');
        try self.writer.print(fmt, args);
        try self.writer.writeByte('\n');
    }
};

pub fn print_program(program: ast.Program, writer: std.fs.File.Writer) !void {
    var printer = PrettyPrinter.init(writer);

    try printer.print_line("Program(", .{});
    printer.indent();
    try print_function(program.function, &printer);
    printer.undent();
    try printer.print_line(")", .{});
}

fn print_function(function: ast.Function, printer: *PrettyPrinter) !void {
    try printer.print_line("Function(", .{});
    printer.indent();
    try printer.print_line("name = {s}", .{function.name.name});
    try print_statement(function.body, printer);
    printer.undent();
    try printer.print_line(")", .{});
}

fn print_statement(statement: ast.Statement, printer: *PrettyPrinter) !void {
    try printer.print_line("{s}(", .{@tagName(statement)});
    printer.indent();

    switch (statement) {
        .ret => |ret| {
            try print_expression(ret.expression, printer);
        },
    }

    printer.undent();
    try printer.print_line(")", .{});
}

fn print_expression(expression: *ast.Expression, printer: *PrettyPrinter) std.fs.File.WriteError!void {
    switch (expression.*) {
        .factor => |factor| try print_factor(factor, printer),
        .binary => |binary| {
            try printer.print_line("Binary(", .{});
            printer.indent();
            try printer.print_line("{s}", .{@tagName(binary.operator)});
            try print_expression(binary.left, printer);
            try print_expression(binary.right, printer);
            printer.undent();
            try printer.print_line(")", .{});
        },
    }
}

fn print_factor(factor: *ast.Factor, printer: *PrettyPrinter) !void {
    switch (factor.*) {
        .unary => |u| {
            try printer.print_line("{s}(", .{@tagName(u.operator)});
            printer.indent();
            try print_factor(u.factor, printer);
            printer.undent();
            try printer.print_line(")", .{});
        },
        .expression => |e| try print_expression(e, printer),
        .constant => try printer.print_line("Constant({d})", .{factor.constant.value}),
    }
}
