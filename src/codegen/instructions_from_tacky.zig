const std = @import("std");

const tacky = @import("../tacky.zig").tacky;

const Instruction = @import("./instruction.zig").Instruction;
const Operand = @import("./instruction.zig").Operand;
const ConditionCode = @import("instruction.zig").ConditionCode;
const ProgramContext = @import("program.zig").Context;
const Identifier = @import("program.zig").Identifier;

const InstructionList = std.ArrayList(Instruction);

const FunctionContext = struct {
    program_context: *ProgramContext,
    list: *InstructionList,
    pub fn operandFromVal(self: FunctionContext, val: tacky.Val) !Operand {
        switch (val) {
            .constant => |c| return Operand.immediate(c.value),
            .tmp => |t| return self.operandFromTmp(t),
        }
    }
    pub fn operandFromTmp(self: FunctionContext, tmp: tacky.Tmp) !Operand {
        return Operand.pseudo(try self.program_context.getIdentifier(tmp.name));
    }
};

pub fn fromTacky(program_context: *ProgramContext, instructions: []tacky.Instruction) ![]Instruction {
    var list = InstructionList.init(program_context.allocator);
    defer list.deinit();

    var context = FunctionContext{
        .program_context = program_context,
        .list = &list,
    };

    for (instructions) |instruction| {
        try appendInstruction(&context, instruction);
    }

    return try list.toOwnedSlice();
}

fn appendInstruction(context: *FunctionContext, instruction: tacky.Instruction) !void {
    switch (instruction) {
        .ret => |r| try appendRet(context, r),
        .unary => |u| try appendUnary(context, u),
        .binary => |b| try appendBinary(context, b),
        .jump => |j| try appendJump(context, j),
        .label => |l| try appendLabel(context, l),
        .copy => |c| try appendCopy(context, c),
    }
}

fn appendCopy(context: *FunctionContext, copy: tacky.Copy) !void {
    try context.list.append(Instruction.mov(try context.operandFromVal(copy.src), try context.operandFromTmp(copy.dst)));
}

fn appendLabel(context: *FunctionContext, label: tacky.Label) !void {
    try context.list.append(Instruction.label(try context.program_context.getIdentifier(label.name)));
}

fn appendRet(context: *FunctionContext, ret: tacky.Ret) !void {
    try context.list.append(Instruction.mov(try context.operandFromVal(ret.val), Operand.register(.ax)));
    try context.list.append(Instruction.ret());
}

fn appendJump(context: *FunctionContext, jump: tacky.Jump) !void {
    if (jump.condition) |condition| {
        const on: ConditionCode = switch (condition.on) {
            .zero => .e,
            .not_zero => .ne,
        };
        try context.list.append(Instruction.cmp(Operand.immediate(0), try context.operandFromVal(condition.val)));
        try context.list.append(Instruction.jmp(try context.program_context.getIdentifier(jump.label), on));
    } else {
        try context.list.append(Instruction.jmp(try context.program_context.getIdentifier(jump.label), null));
    }
}

fn appendUnary(context: *FunctionContext, unary: tacky.Unary) !void {
    if (unary.operator == .logical_not) {
        try context.list.append(Instruction.cmp(Operand.immediate(0), try context.operandFromVal(unary.src)));
        try context.list.append(Instruction.mov(Operand.immediate(0), try context.operandFromTmp(unary.dst)));
        try context.list.append(Instruction.setCC(.e, try context.operandFromTmp(unary.dst)));
    } else {
        try context.list.append(Instruction.mov(try context.operandFromVal(unary.src), try context.operandFromTmp(unary.dst)));
        try context.list.append(Instruction.unary(unary.operator, try context.operandFromTmp(unary.dst)));
    }
}

fn appendBinary(context: *FunctionContext, binary: tacky.Binary) !void {
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
            try context.list.append(Instruction.cmp(try context.operandFromVal(binary.src2), try context.operandFromVal(binary.src1)));
            try context.list.append(Instruction.mov(Operand.immediate(0), try context.operandFromTmp(binary.dst)));
            try context.list.append(Instruction.setCC(condition, try context.operandFromTmp(binary.dst)));
        },
        .divide, .modulo => {
            try context.list.append(Instruction.mov(try context.operandFromVal(binary.src1), Operand.register(.ax)));
            try context.list.append(Instruction.cdq());
            try context.list.append(Instruction.idiv(try context.operandFromVal(binary.src2)));

            if (binary.operator == .divide) {
                try context.list.append(Instruction.mov(Operand.register(.ax), try context.operandFromTmp(binary.dst)));
            } else {
                try context.list.append(Instruction.mov(Operand.register(.dx), try context.operandFromTmp(binary.dst)));
            }
        },
        else => {
            try context.list.append(Instruction.mov(try context.operandFromVal(binary.src1), try context.operandFromTmp(binary.dst)));
            try context.list.append(Instruction.binary(binary.operator, try context.operandFromVal(binary.src2), try context.operandFromTmp(binary.dst)));
        },
    }
}
