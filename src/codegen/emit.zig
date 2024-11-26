const std = @import("std");

const x86 = @import("./x86.zig");

const PrettyEmitter = struct {
    writer: std.fs.File.Writer,
    pub fn init(writer: std.fs.File.Writer) PrettyEmitter {
        return PrettyEmitter{ .writer = writer };
    }
    pub fn emit_comment(self: PrettyEmitter, comptime fmt: []const u8, args: anytype) !void {
        self.writer.print("# " ++ fmt, args);
    }
    fn print_instruction(self: PrettyEmitter, instruction: []const u8, operands: anytype) !void {
        try self.writer.print("    {s} ", .{instruction});

        switch (operands.len) {
            0 => {},
            1 => {
                try self.print_operand(operands[0]);
            },
            2 => {
                try self.print_operand(operands[0]);
                try self.writer.writeAll(", ");
                try self.print_operand(operands[1]);
            },
            else => unreachable,
        }

        try self.writer.writeByte('\n');
    }
    fn print_operand(self: PrettyEmitter, operand: x86.Operand) !void {
        switch (operand) {
            .stack => |stack| {
                try self.writer.print("-{d}(%rbp)", .{stack.offset});
            },
            .register => |register| {
                switch (register) {
                    .ax => try self.writer.writeAll("%eax"),
                    .dx => try self.writer.writeAll("%edx"),
                    .cl => try self.writer.writeAll("%cl"),
                    .r10 => try self.writer.writeAll("%r10d"),
                    .r11 => try self.writer.writeAll("%r11d"),
                    .rsp => try self.writer.writeAll("%rsp"),
                    .rbp => try self.writer.writeAll("%rbp"),
                }
            },
            .immediate => |immediate| {
                try self.writer.print("${d}", .{immediate.int});
            },
            .pseudo => unreachable,
        }
    }
    pub fn emit_program(self: PrettyEmitter, program: x86.Program) !void {
        try self.emit_function_definition(program.function_definition);
    }
    fn emit_function_definition(self: PrettyEmitter, function_definition: x86.FunctionDefinition) !void {
        try self.writer.print(
            "   .globl _{s}\n_{s}:\n",
            .{ function_definition.name, function_definition.name },
        );
        try self.print_instruction("pushq", .{x86.Operand.register(.rbp)});
        try self.print_instruction("movq", .{ x86.Operand.register(.rsp), x86.Operand.register(.rbp) });
        for (function_definition.instructions) |instruction| {
            try self.emit_instruction(instruction);
        }
    }
    fn emit_instruction(self: PrettyEmitter, instruction: x86.Instruction) !void {
        switch (instruction) {
            .mov => |mov| {
                if (mov.dst == .register and mov.dst.register == .cl) {
                    try self.print_instruction("movb", .{ mov.src, mov.dst });
                } else {
                    try self.print_instruction("movl", .{ mov.src, mov.dst });
                }
            },
            .ret => {
                try self.print_instruction("movq", .{ x86.Operand.register(.rbp), x86.Operand.register(.rsp) });
                try self.print_instruction("popq", .{x86.Operand.register(.rbp)});
                try self.print_instruction("ret", .{});
            },
            .unary => |unary| {
                switch (unary.operator) {
                    .complement => try self.print_instruction("notl", .{unary.operand}),
                    .negate => try self.print_instruction("negl", .{unary.operand}),
                }
            },
            .binary => |binary| {
                switch (binary.operator) {
                    .add => try self.print_instruction("addl", .{ binary.operand1, binary.operand2 }),
                    .subtract => try self.print_instruction("subl", .{ binary.operand1, binary.operand2 }),
                    .multiply => try self.print_instruction("imull", .{ binary.operand1, binary.operand2 }),
                    .@"and" => try self.print_instruction("andl", .{ binary.operand1, binary.operand2 }),
                    .@"or" => try self.print_instruction("orl", .{ binary.operand1, binary.operand2 }),
                    .xor => try self.print_instruction("xorl", .{ binary.operand1, binary.operand2 }),
                    .shift_left => try self.print_instruction("sall", .{ binary.operand1, binary.operand2 }),
                    .shift_right => try self.print_instruction("sarl", .{ binary.operand1, binary.operand2 }),
                    else => unreachable,
                }
            },
            .idiv => |idiv| try self.print_instruction("idivl", .{idiv.operand}),
            .cdq => try self.print_instruction("cdq", .{}),
            .allocate_stack => |allocate| try self.print_instruction("subq", .{ allocate.operand, x86.Operand.register(.rsp) }),
        }
    }
};

pub fn emit_x86_program(program: x86.Program, writer: std.fs.File.Writer) !void {
    try PrettyEmitter.init(writer).emit_program(program);
}
