const std = @import("std");

const tacky = @import("./tacky.zig");

pub fn printProgram(program: tacky.Program, writer: std.fs.File.Writer) !void {
    try printFunctionDefinition(program.function_definition, writer);
}

pub fn printFunctionDefinition(function_definition: tacky.FunctionDefinition, writer: std.fs.File.Writer) !void {
    try writer.print("{s}\n", .{function_definition.name});
    for (function_definition.instructions) |i| try printInstruction(i, writer);
}

pub fn printInstruction(instruction: tacky.Instruction, writer: std.fs.File.Writer) !void {
    switch (instruction) {
        .ret => |ret| {
            try writer.writeAll("    return ");
            try printVal(ret.val, writer);
        },
        .unary => |op| {
            try writer.print("    {s} = {s} ", .{ op.dst.name, @tagName(op.operator) });
            try printVal(op.src, writer);
        },
        .binary => |op| {
            try writer.print("    {s} = {s} ", .{ op.dst.name, @tagName(op.operator) });
            try printVal(op.src1, writer);
            try writer.writeByte(' ');
            try printVal(op.src2, writer);
        },
        .jump => |op| {
            if (op.condition) |condition| {
                try writer.print("    jump {s} ", .{@tagName(condition.on)});
                try printVal(condition.val, writer);
                try writer.writeAll(": ");
            } else {
                try writer.writeAll("    jump: ");
            }
            try writer.writeAll(op.label);
        },
        .copy => |op| {
            try writer.print("    {s} = ", .{op.dst.name});
            try printVal(op.src, writer);
        },
        .label => |op| {
            try writer.print("{s}:", .{op.name});
        },
    }
    try writer.writeByte('\n');
}

pub fn printVal(val: tacky.Val, writer: std.fs.File.Writer) !void {
    switch (val) {
        .constant => |c| try writer.print("{d}", .{c.value}),
        .tmp => |t| try writer.print("{s}", .{t.name}),
    }
}
