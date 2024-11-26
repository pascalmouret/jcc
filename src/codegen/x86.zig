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
                    try list.append(Instruction.binary(binary.operator, try binary.operand1.stack_if_pseudo(&offset_map), try binary.operand2.stack_if_pseudo(&offset_map)));
                },
                .idiv => |idiv| {
                    try list.append(Instruction.idiv(try idiv.operand.stack_if_pseudo(&offset_map)));
                },
                else => try list.append(instruction),
            }
        }

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
                    if ((binary.operator == .add or binary.operator == .subtract or binary.operator == .bitwise_and or binary.operator == .bitwise_or or binary.operator == .xor) and (binary.operand1 == .stack and binary.operand2 == .stack)) {
                        try list.append(Instruction.mov(binary.operand1, Operand.register(.r10)));
                        try list.append(Instruction.binary(binary.operator, Operand.register(.r10), binary.operand2));
                        continue;
                    }

                    if ((binary.operator == .shift_left or binary.operator == .shift_right) and binary.operand2 != .immediate) {
                        try list.append(Instruction.mov(binary.operand2, Operand.register(.cl)));
                        try list.append(Instruction.binary(binary.operator, binary.operand1, Operand.register(.cl)));
                        continue;
                    }

                    if (binary.operator == .multiply) {
                        try list.append(Instruction.mov(binary.operand2, Operand.register(.r11)));
                        try list.append(Instruction.binary(binary.operator, binary.operand1, Operand.register(.r11)));
                        try list.append(Instruction.mov(Operand.register(.r11), binary.operand2));
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
    allocate_stack: AllocateStack,
    pub fn append_from_instruction(list: *std.ArrayList(Instruction), instruction: tacky.Instruction) !void {
        switch (instruction) {
            .ret => |r| {
                try list.append(Instruction.mov(Operand.from_val(r.val), Operand.register(.ax)));
                try list.append(Instruction.ret());
            },
            .unary => |u| {
                try list.append(Instruction.mov(Operand.from_val(u.src), Operand.pseudo(u.dst.name)));
                try list.append(Instruction.unary(u.operator, Operand.pseudo(u.dst.name)));
            },
            .binary => |b| {
                switch (b.operator) {
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
        return Instruction{ .binary = Binary{ .operator = operator, .operand1 = operand1, .operand2 = operand2 } };
    }
    pub fn idiv(operand: Operand) Instruction {
        return Instruction{ .idiv = Idiv{ .operand = operand } };
    }
    pub fn cdq() Instruction {
        return Instruction{ .cdq = Cdq{} };
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
    operand1: Operand,
    operand2: Operand,
};

const Idiv = struct {
    operand: Operand,
};

const Cdq = struct {};

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
    pub fn immediate(int: isize) Operand {
        return Operand{ .immediate = Immediate{ .int = int } };
    }
    pub fn register(reg: Register) Operand {
        return Operand{ .register = reg };
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

const Register = enum {
    ax,
    dx,
    cl,
    r10,
    r11,
    rsp,
    rbp,
};

const Pseudo = struct {
    name: []u8,
};

const Stack = struct {
    offset: usize,
};
