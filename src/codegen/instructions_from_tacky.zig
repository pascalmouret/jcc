const std = @import("std");

const tacky = @import("../tacky.zig").tacky;

const Instruction = @import("./instruction.zig").Instruction;
const Operand = @import("./instruction.zig").Operand;
const ConditionCode = @import("instruction.zig").ConditionCode;

const InstructionList = std.ArrayList(Instruction);

pub fn fromTacky(allocator: std.mem.Allocator, instructions: []tacky.Instruction) ![]Instruction {
    var list = InstructionList.init(allocator);
    defer list.deinit();

    for (instructions) |instruction| {
        try appendInstruction(&list, instruction);
    }

    return try list.toOwnedSlice();
}

fn appendInstruction(list: *InstructionList, instruction: tacky.Instruction) !void {
    switch (instruction) {
        .ret => |r| try appendRet(list, r),
        .unary => |u| try appendUnary(list, u),
        .binary => |b| try appendBinary(list, b),
        .jump => |j| try appendJump(list, j),
        .label => |l| try appendLabel(list, l),
        .copy => |c| try appendCopy(list, c),
    }
}

fn appendCopy(list: *InstructionList, copy: tacky.Copy) !void {
    try list.append(Instruction.mov(Operand.fromVal(copy.src), Operand.pseudo(copy.dst.name)));
}

fn appendLabel(list: *InstructionList, label: tacky.Label) !void {
    try list.append(Instruction.label(label.name));
}

fn appendRet(list: *InstructionList, ret: tacky.Ret) !void {
    try list.append(Instruction.mov(Operand.fromVal(ret.val), Operand.register(.ax)));
    try list.append(Instruction.ret());
}

fn appendJump(list: *InstructionList, jump: tacky.Jump) !void {
    if (jump.condition) |condition| {
        const on: ConditionCode = switch (condition.on) {
            .zero => .e,
            .not_zero => .ne,
        };
        try list.append(Instruction.cmp(Operand.immediate(0), Operand.fromVal(condition.val)));
        try list.append(Instruction.jmp(jump.label, on));
    } else {
        try list.append(Instruction.jmp(jump.label, null));
    }
}

fn appendUnary(list: *InstructionList, unary: tacky.Unary) !void {
    if (unary.operator == .logical_not) {
        try list.append(Instruction.cmp(Operand.immediate(0), Operand.fromVal(unary.src)));
        try list.append(Instruction.mov(Operand.immediate(0), Operand.pseudo(unary.dst.name)));
        try list.append(Instruction.setCC(.e, Operand.pseudo(unary.dst.name)));
    } else {
        try list.append(Instruction.mov(Operand.fromVal(unary.src), Operand.pseudo(unary.dst.name)));
        try list.append(Instruction.unary(unary.operator, Operand.pseudo(unary.dst.name)));
    }
}

fn appendBinary(list: *InstructionList, binary: tacky.Binary) !void {
    switch (binary.operator) {
        .less_than, .less_than_equal, .greater_than, .greater_than_equal, .equal, .not_equal => {
            const condition: ConditionCode = switch (binary.operator) {
                .less_than => .l,
                .less_than_equal => .le,
                .greater_than => .g,
                .greater_than_equal => .ge,
                .equal => .e,
                .not_equal => .ne,
                else => unreachable,
            };
            try list.append(Instruction.cmp(Operand.fromVal(binary.src2), Operand.fromVal(binary.src1)));
            try list.append(Instruction.mov(Operand.immediate(0), Operand.pseudo(binary.dst.name)));
            try list.append(Instruction.setCC(condition, Operand.pseudo(binary.dst.name)));
        },
        .divide, .modulo => {
            try list.append(Instruction.mov(Operand.fromVal(binary.src1), Operand.register(.ax)));
            try list.append(Instruction.cdq());
            try list.append(Instruction.idiv(Operand.fromVal(binary.src2)));

            if (binary.operator == .divide) {
                try list.append(Instruction.mov(Operand.register(.ax), Operand.pseudo(binary.dst.name)));
            } else {
                try list.append(Instruction.mov(Operand.register(.dx), Operand.pseudo(binary.dst.name)));
            }
        },
        else => {
            try list.append(Instruction.mov(Operand.fromVal(binary.src1), Operand.pseudo(binary.dst.name)));
            try list.append(Instruction.binary(binary.operator, Operand.fromVal(binary.src2), Operand.pseudo(binary.dst.name)));
        },
    }
}
