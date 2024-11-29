const std = @import("std");

const Program = @import("./program.zig").Program;
const FunctionDefinition = @import("./program.zig").FunctionDefinition;
const Operand = @import("./instruction.zig").Operand;
const Instruction = @import("instruction.zig").Instruction;

pub fn emitProgram(program: Program, writer: std.fs.File.Writer) !void {
    try PrettyEmitter.init(writer).emitProgram(program);
}

const PrettyEmitter = struct {
    writer: std.fs.File.Writer,
    pub fn init(writer: std.fs.File.Writer) PrettyEmitter {
        return PrettyEmitter{ .writer = writer };
    }
    fn printInstruction(self: PrettyEmitter, comptime instruction: []const u8, operands: anytype) !void {
        try self.printFormattedInstruction(instruction, .{}, operands);
    }
    fn printFormattedInstruction(self: PrettyEmitter, comptime instruction: []const u8, args: anytype, operands: anytype) !void {
        try self.writer.print("    " ++ instruction ++ " ", args);

        switch (operands.len) {
            0 => {},
            1 => {
                try self.printOperand(operands[0]);
            },
            2 => {
                try self.printOperand(operands[0]);
                try self.writer.writeAll(", ");
                try self.printOperand(operands[1]);
            },
            else => unreachable,
        }

        try self.writer.writeByte('\n');
    }
    fn printOperand(self: PrettyEmitter, operand: Operand) !void {
        switch (operand) {
            .stack => |stack| {
                try self.writer.print("-{d}(%rbp)", .{stack.offset});
            },
            .register => |register| {
                switch (register.name) {
                    .ax, .dx, .cx => {
                        switch (register.size) {
                            1 => try self.writer.print("%{c}l", .{@tagName(register.name)[0]}),
                            4 => try self.writer.print("%e{s}", .{@tagName(register.name)}),
                            else => unreachable,
                        }
                    },
                    .r10, .r11 => {
                        const suffix: u8 = switch (register.size) {
                            1 => 'b',
                            4 => 'd',
                            else => unreachable,
                        };
                        try self.writer.print("%{s}{c}", .{ @tagName(register.name), suffix });
                    },
                    .rsp, .rbp => try self.writer.print("%{s}", .{@tagName(register.name)}),
                }
            },
            .immediate => |immediate| {
                try self.writer.print("${d}", .{immediate.int});
            },
            .pseudo => unreachable,
        }
    }
    pub fn emitProgram(self: PrettyEmitter, program: Program) !void {
        try self.emitFunctionDefinition(program.function_definition);
    }
    fn emitFunctionDefinition(self: PrettyEmitter, function_definition: FunctionDefinition) !void {
        try self.writer.print(
            "   .globl _{s}\n_{s}:\n",
            .{ function_definition.name, function_definition.name },
        );
        try self.printInstruction("pushq", .{Operand.register(.rbp)});
        try self.printInstruction("movq", .{ Operand.register(.rsp), Operand.register(.rbp) });
        for (function_definition.instructions) |instruction| {
            try self.emitInstruction(instruction);
        }
    }
    fn emitInstruction(self: PrettyEmitter, instruction: Instruction) !void {
        switch (instruction) {
            .mov => |mov| {
                //  TODO: do this cleanly
                if (mov.dst == .register and mov.dst.register.size == 1) {
                    try self.printInstruction("movb", .{ mov.src, mov.dst });
                } else {
                    try self.printInstruction("movl", .{ mov.src, mov.dst });
                }
            },
            .ret => {
                try self.printInstruction("movq", .{ Operand.register(.rbp), Operand.register(.rsp) });
                try self.printInstruction("popq", .{Operand.register(.rbp)});
                try self.printInstruction("ret", .{});
            },
            .unary => |unary| {
                switch (unary.operator) {
                    .complement => try self.printInstruction("notl", .{unary.operand}),
                    .negate => try self.printInstruction("negl", .{unary.operand}),
                    else => unreachable,
                }
            },
            .binary => |binary| {
                switch (binary.operator) {
                    .add => try self.printInstruction("addl", .{ binary.src, binary.dst }),
                    .subtract => try self.printInstruction("subl", .{ binary.src, binary.dst }),
                    .multiply => try self.printInstruction("imull", .{ binary.src, binary.dst }),
                    .bitwise_and => try self.printInstruction("andl", .{ binary.src, binary.dst }),
                    .bitwise_or => try self.printInstruction("orl", .{ binary.src, binary.dst }),
                    .xor => try self.printInstruction("xorl", .{ binary.src, binary.dst }),
                    .shift_left => try self.printInstruction("sall", .{ binary.src, binary.dst }),
                    .shift_right => try self.printInstruction("sarl", .{ binary.src, binary.dst }),
                    else => unreachable,
                }
            },
            .idiv => |idiv| try self.printInstruction("idivl", .{idiv.operand}),
            .cdq => try self.printInstruction("cdq", .{}),
            .jmp => |jmp| {
                if (jmp.on) |on| {
                    try self.printFormattedInstruction("j{s} L{s}", .{ @tagName(on), jmp.label }, .{});
                } else {
                    try self.printFormattedInstruction("jmp L{s}", .{jmp.label}, .{});
                }
            },
            .set_cc => |set_cc| try self.printFormattedInstruction("set{s}", .{@tagName(set_cc.code)}, .{set_cc.dst}),
            .label => |label| try self.writer.print("    L{s}:\n", .{label.name}),
            .cmp => |cmp| try self.printInstruction("cmpl", .{ cmp.src, cmp.dst }),
            .allocate_stack => |allocate| try self.printInstruction("subq", .{ allocate.operand, Operand.register(.rsp) }),
        }
    }
};
