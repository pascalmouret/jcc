const std = @import("std");

const ast = @import("../parser.zig").ast;
const tacky = @import("../tacky.zig").tacky;

//  TODO: operand file
const Identifier = @import("./program.zig").Identifier;

pub const fromTacky = @import("./instructions_from_tacky.zig").fromTacky;

pub const Instruction = union(enum) {
    mov: Mov,
    ret: Ret,
    unary: Unary,
    binary: Binary,
    idiv: Idiv,
    cdq: Cdq,
    cmp: Cmp,
    jmp: Jmp,
    set_cc: SetCC,
    label: Label,
    allocate_stack: AllocateStack,
    pub fn mov(src: Operand, dst: Operand) Instruction {
        return Instruction{ .mov = Mov{ .src = src, .dst = dst } };
    }
    pub fn ret() Instruction {
        return Instruction{ .ret = Ret{} };
    }
    pub fn unary(operator: ast.UnaryOperator, operand: Operand) Instruction {
        return Instruction{ .unary = Unary{ .operator = operator, .operand = operand } };
    }
    pub fn binary(operator: ast.BinaryOperator, src: Operand, dst: Operand) Instruction {
        return Instruction{ .binary = Binary{ .operator = operator, .src = src, .dst = dst } };
    }
    pub fn idiv(operand: Operand) Instruction {
        return Instruction{ .idiv = Idiv{ .operand = operand } };
    }
    pub fn cdq() Instruction {
        return Instruction{ .cdq = Cdq{} };
    }
    pub fn cmp(src: Operand, dst: Operand) Instruction {
        return Instruction{ .cmp = Cmp{ .src = src, .dst = dst } };
    }
    pub fn jmp(dst: Identifier, on: ?ConditionCode) Instruction {
        return Instruction{ .jmp = Jmp{ .label = dst, .on = on } };
    }
    pub fn setCC(code: ConditionCode, dst: Operand) Instruction {
        return Instruction{ .set_cc = SetCC{ .code = code, .dst = dst.sized(1) } };
    }
    pub fn label(identifier: Identifier) Instruction {
        return Instruction{ .label = Label{ .identifier = identifier } };
    }
    pub fn allocateStack(size: i32) Instruction {
        return Instruction{ .allocate_stack = AllocateStack{ .operand = Operand.immediate(size) } };
    }
};

const Ret = struct {};

const Mov = struct {
    src: Operand,
    dst: Operand,
};

const Unary = struct {
    operator: ast.UnaryOperator,
    operand: Operand,
};

const Binary = struct {
    operator: ast.BinaryOperator,
    src: Operand,
    dst: Operand,
};

const Idiv = struct {
    operand: Operand,
};

const Cdq = struct {};

const Cmp = struct {
    src: Operand,
    dst: Operand,
};

pub const ConditionCode = enum {
    e,
    ne,
    g,
    ge,
    l,
    le,
};

const Jmp = struct {
    label: Identifier,
    on: ?ConditionCode = null,
};

const SetCC = struct {
    code: ConditionCode,
    dst: Operand,
};

const Label = struct {
    identifier: Identifier,
};

const AllocateStack = struct {
    operand: Operand,
};

pub const Operand = union(enum) {
    register: Register,
    immediate: Immediate,
    pseudo: Pseudo,
    stack: Stack,

    pub fn stackIfPseudo(self: Operand, map: *std.StringHashMap(usize)) !Operand {
        switch (self) {
            .pseudo => |p| {
                if (map.get(p.identifier.name)) |offset| {
                    return Operand.stack(offset);
                } else {
                    try map.put(p.identifier.name, (map.count() + 1) * 4);
                    return self.stackIfPseudo(map);
                }
            },
            else => return self,
        }
    }
    pub fn sized(self: Operand, size: usize) Operand {
        if (self == .register) {
            return Operand{ .register = Register{ .name = self.register.name, .size = size } };
        }
        return self;
    }
    pub fn immediate(int: i32) Operand {
        return Operand{ .immediate = Immediate{ .int = int } };
    }
    pub fn register(reg: RegisterName) Operand {
        return Operand{ .register = Register{ .name = reg } };
    }
    pub fn sizedRegister(reg: RegisterName, size: usize) Operand {
        return Operand{ .register = Register{ .name = reg, .size = size } };
    }
    pub fn pseudo(identifier: Identifier) Operand {
        return Operand{ .pseudo = Pseudo{ .identifier = identifier } };
    }
    pub fn stack(offset: usize) Operand {
        return Operand{ .stack = Stack{ .offset = offset } };
    }
};

const Immediate = union(enum) {
    int: i32,
};

const RegisterName = enum {
    ax,
    dx,
    cx,
    r10,
    r11,
    rsp,
    rbp,
};

const Register = struct {
    name: RegisterName,
    size: usize = 4,
};

const Pseudo = struct {
    identifier: Identifier,
    size: usize = 4,
};

const Stack = struct {
    offset: usize,
    size: usize = 4,
};
