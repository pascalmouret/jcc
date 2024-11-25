const std = @import("std");

const tacky = @import("./tacky.zig");

pub fn print_program(program: tacky.Program, writer: std.fs.File.Writer) !void {
    try print_function_definition(program.function_definition, writer);
}

pub fn print_function_definition(function_definition: tacky.FunctionDefinition, writer: std.fs.File.Writer) !void {
    try writer.print("{s}\n", .{function_definition.name});
    for (function_definition.instructions) |i| try print_instruction(i, writer);
}

pub fn print_instruction(instruction: tacky.Instruction, writer: std.fs.File.Writer) !void {
    switch (instruction) {
        .ret => |ret| {
            try writer.writeAll("    return ");
            try print_val(ret.val, writer);
            try writer.writeByte('\n');
        },
        .unary => |op| {
            try writer.print("    {s} = {s} ", .{ op.dst.name, @tagName(op.operator) });
            try print_val(op.src, writer);
            try writer.writeByte('\n');
        },
        .binary => |op| {
            try writer.print("    {s} = {s} ", .{ op.dst.name, @tagName(op.operator) });
            try print_val(op.src1, writer);
            try writer.writeByte(' ');
            try print_val(op.src2, writer);
            try writer.writeByte('\n');
        },
    }
}

pub fn print_val(val: tacky.Val, writer: std.fs.File.Writer) !void {
    switch (val) {
        .constant => |c| try writer.print("{d}", .{c.value}),
        .tmp => |t| try writer.print("{s}", .{t.name}),
    }
}
