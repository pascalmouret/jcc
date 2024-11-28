const std = @import("std");

const tacky = @import("../tacky.zig").tacky;
const ast = @import("../parser.zig").ast;

pub fn program_to_x86(allocator: std.mem.Allocator, program: tacky.Program) !Program {
    return try Program.from_program(allocator, program);
}

pub const Program = struct {
    allocator: std.mem.Allocator,
    function_definition: FunctionDefinition,
    pub fn from_program(allocator: std.mem.Allocator, program: tacky.Program) !Program {
        return Program{
            .allocator = allocator,
            .function_definition = try FunctionDefinition.from_function_definition(allocator, program.function_definition),
        };
    }
    pub fn deinit(self: Program) void {
        self.function_definition.deinit(self.allocator);
    }
};

pub const FunctionDefinition = struct {
    name: []u8,
    instructions: []Instruction,
    pub fn from_function_definition(allocator: std.mem.Allocator, function_definition: tacky.FunctionDefinition) !FunctionDefinition {
        var list = std.ArrayList(Instruction).init(allocator);
        defer list.deinit();

        for (function_definition.instructions) |instruction| {
            try Instruction.append_from_instruction(&list, instruction);
        }

        const initial = try list.toOwnedSlice();
        defer allocator.free(initial);

        const stacked = try FunctionDefinition.replace_pseudo(allocator, initial);
        defer allocator.free(stacked);

        const final = try FunctionDefinition.fix_instructions(allocator, stacked);

        return FunctionDefinition{ .name = function_definition.name, .instructions = final };
    }
    pub fn replace_pseudo(allocator: std.mem.Allocator, instructions: []const Instruction) ![]Instruction {
        var offset_map = std.StringHashMap(usize).init(allocator);
        defer offset_map.deinit();

        var list = std.ArrayList(Instruction).init(allocator);
        defer list.deinit();

        for (instructions) |instruction| {
            switch (instruction) {
                .mov => |mov| {
                    try list.append(Instruction.mov(try mov.src.stack_if_pseudo(&offset_map), try mov.dst.stack_if_pseudo(&offset_map)));
                },
                .unary => |unary| {
                    try list.append(Instruction.unary(unary.operator, try unary.operand.stack_if_pseudo(&offset_map)));
                },
                .binary => |binary| {
                    try list.append(Instruction.binary(binary.operator, try binary.src1.stack_if_pseudo(&offset_map), try binary.src2.stack_if_pseudo(&offset_map)));
                },
                .idiv => |idiv| {
                    try list.append(Instruction.idiv(try idiv.operand.stack_if_pseudo(&offset_map)));
                },
                .set_cc => |set_cc| {
                    try list.append(Instruction.set_cc(set_cc.code, try set_cc.dst.stack_if_pseudo(&offset_map)));
                },
                .cmp => |cmp| {
                    try list.append(Instruction.cmp(try cmp.src1.stack_if_pseudo(&offset_map), try cmp.src2.stack_if_pseudo(&offset_map)));
                },
                else => try list.append(instruction),
            }
        }

        // TODO: account for sized
        try list.insert(0, Instruction.allocate_stack(offset_map.count() * 4));

        return try list.toOwnedSlice();
    }
    pub fn fix_instructions(allocator: std.mem.Allocator, instructions: []const Instruction) ![]Instruction {
        var list = std.ArrayList(Instruction).init(allocator);
        defer list.deinit();

        for (instructions) |instruction| {
            switch (instruction) {
                .mov => |mov| {
                    if (mov.dst == .stack and mov.src == .stack) {
                        try list.append(Instruction.mov(mov.src, Operand.register(.r10)));
                        try list.append(Instruction.mov(Operand.register(.r10), mov.dst));
                        continue;
                    }
                },
                .idiv => |idiv| {
                    if (idiv.operand == .immediate) {
                        try list.append(Instruction.mov(idiv.operand, Operand.register(.r10)));
                        try list.append(Instruction.idiv(Operand.register(.r10)));
                        continue;
                    }
                },
                .binary => |binary| {
                    if ((binary.operator == .add or binary.operator == .subtract or binary.operator == .bitwise_and or binary.operator == .bitwise_or or binary.operator == .xor) and (binary.src1 == .stack and binary.src2 == .stack)) {
                        try list.append(Instruction.mov(binary.src1, Operand.register(.r10)));
                        try list.append(Instruction.binary(binary.operator, Operand.register(.r10), binary.src2));
                        continue;
                    }

                    if ((binary.operator == .shift_left or binary.operator == .shift_right) and binary.src1 != .immediate) {
                        try list.append(Instruction.mov(binary.src1, Operand.sized_register(.cx, 1)));
                        try list.append(Instruction.binary(binary.operator, Operand.sized_register(.cx, 1), binary.src2));
                        continue;
                    }

                    if (binary.operator == .multiply) {
                        try list.append(Instruction.mov(binary.src2, Operand.register(.r11)));
                        try list.append(Instruction.binary(binary.operator, binary.src1, Operand.register(.r11)));
                        try list.append(Instruction.mov(Operand.register(.r11), binary.src2));
                        continue;
                    }
                },
                .cmp => |cmp| {
                    if (cmp.src1 == .stack and cmp.src2 == .stack) {
                        try list.append(Instruction.mov(cmp.src1, Operand.register(.r10)));
                        try list.append(Instruction.cmp(Operand.register(.r10), cmp.src2));
                        continue;
                    }

                    if (cmp.src2 == .immediate) {
                        try list.append(Instruction.mov(cmp.src2, Operand.register(.r11)));
                        try list.append(Instruction.cmp(cmp.src1, Operand.register(.r11)));
                        continue;
                    }
                },
                else => {},
            }

            try list.append(instruction);
        }

        return list.toOwnedSlice();
    }
    pub fn deinit(self: FunctionDefinition, allocator: std.mem.Allocator) void {
        allocator.free(self.instructions);
    }
};

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
    pub fn append_from_instruction(list: *std.ArrayList(Instruction), instruction: tacky.Instruction) !void {
        switch (instruction) {
            .ret => |r| {
                try list.append(Instruction.mov(Operand.from_val(r.val), Operand.register(.ax)));
                try list.append(Instruction.ret());
            },
            .unary => |u| {
                if (u.operator == .logical_not) {
                    try list.append(Instruction.cmp(Operand.immediate(0), Operand.from_val(u.src)));
                    try list.append(Instruction.mov(Operand.immediate(0), Operand.pseudo(u.dst.name)));
                    try list.append(Instruction.set_cc(.e, Operand.pseudo(u.dst.name)));
                } else {
                    try list.append(Instruction.mov(Operand.from_val(u.src), Operand.pseudo(u.dst.name)));
                    try list.append(Instruction.unary(u.operator, Operand.pseudo(u.dst.name)));
                }
            },
            .binary => |b| {
                switch (b.operator) {
                    .less_than, .less_than_equal, .greater_than, .greater_than_equal, .equal, .not_equal => {
                        const condition: ConditionCode = switch (b.operator) {
                            .less_than => .l,
                            .less_than_equal => .le,
                            .greater_than => .g,
                            .greater_than_equal => .ge,
                            .equal => .e,
                            .not_equal => .ne,
                            else => unreachable,
                        };
                        try list.append(Instruction.cmp(Operand.from_val(b.src2), Operand.from_val(b.src1)));
                        try list.append(Instruction.mov(Operand.immediate(0), Operand.pseudo(b.dst.name)));
                        try list.append(Instruction.set_cc(condition, Operand.pseudo(b.dst.name)));
                    },
                    .divide, .modulo => {
                        try list.append(Instruction.mov(Operand.from_val(b.src1), Operand.register(.ax)));
                        try list.append(Instruction.cdq());
                        try list.append(Instruction.idiv(Operand.from_val(b.src2)));

                        if (b.operator == .divide) {
                            try list.append(Instruction.mov(Operand.register(.ax), Operand.pseudo(b.dst.name)));
                        } else {
                            try list.append(Instruction.mov(Operand.register(.dx), Operand.pseudo(b.dst.name)));
                        }
                    },
                    else => {
                        try list.append(Instruction.mov(Operand.from_val(b.src1), Operand.pseudo(b.dst.name)));
                        try list.append(Instruction.binary(b.operator, Operand.from_val(b.src2), Operand.pseudo(b.dst.name)));
                    },
                }
            },
            .jump => |j| {
                if (j.condition) |condition| {
                    const on: ConditionCode = switch (condition.on) {
                        .zero => .e,
                        .not_zero => .ne,
                    };
                    try list.append(Instruction.cmp(Operand.immediate(0), Operand.from_val(condition.val)));
                    try list.append(Instruction.jmp(j.label, on));
                } else {
                    try list.append(Instruction.jmp(j.label, null));
                }
            },
            .label => |l| {
                try list.append(Instruction.label(l.name));
            },
            .copy => |c| {
                try list.append(Instruction.mov(Operand.from_val(c.src), Operand.pseudo(c.dst.name)));
            },
        }
    }
    pub fn mov(src: Operand, dst: Operand) Instruction {
        return Instruction{ .mov = Mov{ .src = src, .dst = dst } };
    }
    pub fn ret() Instruction {
        return Instruction{ .ret = Ret{} };
    }
    pub fn unary(operator: ast.UnaryOperator, operand: Operand) Instruction {
        return Instruction{ .unary = Unary{ .operator = operator, .operand = operand } };
    }
    pub fn binary(operator: ast.BinaryOperator, operand1: Operand, operand2: Operand) Instruction {
        return Instruction{ .binary = Binary{ .operator = operator, .src1 = operand1, .src2 = operand2 } };
    }
    pub fn idiv(operand: Operand) Instruction {
        return Instruction{ .idiv = Idiv{ .operand = operand } };
    }
    pub fn cdq() Instruction {
        return Instruction{ .cdq = Cdq{} };
    }
    pub fn cmp(src1: Operand, src2: Operand) Instruction {
        return Instruction{ .cmp = Cmp{ .src1 = src1, .src2 = src2 } };
    }
    pub fn jmp(dst_label: []u8, on: ?ConditionCode) Instruction {
        return Instruction{ .jmp = Jmp{ .label = dst_label, .on = on } };
    }
    pub fn set_cc(code: ConditionCode, dst: Operand) Instruction {
        return Instruction{ .set_cc = SetCC{ .code = code, .dst = dst.sized(1) } };
    }
    pub fn label(name: []u8) Instruction {
        return Instruction{ .label = Label{ .name = name } };
    }
    pub fn allocate_stack(size: isize) Instruction {
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
    src1: Operand,
    src2: Operand,
};

const Idiv = struct {
    operand: Operand,
};

const Cdq = struct {};

const Cmp = struct {
    src1: Operand,
    src2: Operand,
};

const ConditionCode = enum {
    e,
    ne,
    g,
    ge,
    l,
    le,
};

const Jmp = struct {
    label: []u8,
    on: ?ConditionCode = null,
};

const SetCC = struct {
    code: ConditionCode,
    dst: Operand,
};

const Label = struct {
    name: []u8,
};

const AllocateStack = struct {
    operand: Operand,
};

pub const Operand = union(enum) {
    register: Register,
    immediate: Immediate,
    pseudo: Pseudo,
    stack: Stack,
    pub fn from_val(val: tacky.Val) Operand {
        switch (val) {
            .constant => |c| return Operand.immediate(c.value),
            .tmp => |t| return Operand.pseudo(t.name),
        }
    }
    pub fn stack_if_pseudo(self: Operand, map: *std.StringHashMap(usize)) !Operand {
        switch (self) {
            .pseudo => |p| {
                if (map.get(p.name)) |offset| {
                    return Operand.stack(offset);
                } else {
                    try map.put(p.name, (map.count() + 1) * 4);
                    return self.stack_if_pseudo(map);
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
    pub fn immediate(int: isize) Operand {
        return Operand{ .immediate = Immediate{ .int = int } };
    }
    pub fn register(reg: RegisterName) Operand {
        return Operand{ .register = Register{ .name = reg } };
    }
    pub fn sized_register(reg: RegisterName, size: usize) Operand {
        return Operand{ .register = Register{ .name = reg, .size = size } };
    }
    pub fn pseudo(name: []u8) Operand {
        return Operand{ .pseudo = Pseudo{ .name = name } };
    }
    pub fn stack(offset: usize) Operand {
        return Operand{ .stack = Stack{ .offset = offset } };
    }
};

const Immediate = union(enum) {
    int: isize,
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
    name: []u8,
    size: usize = 4,
};

const Stack = struct {
    offset: usize,
    size: usize = 4,
};
