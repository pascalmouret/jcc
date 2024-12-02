const std = @import("std");

const tacky = @import("../tacky.zig").tacky;
const ast = @import("../parser.zig").ast;

const Instruction = @import("./instruction.zig").Instruction;
const instructionsFromTacky = @import("instruction.zig").fromTacky;
const Operand = @import("./instruction.zig").Operand;

pub fn fromTacky(allocator: std.mem.Allocator, program: tacky.Program) !Program {
    return try Program.fromProgram(allocator, program);
}

pub const Context = struct {
    allocator: std.mem.Allocator,
    identifiers: std.ArrayList(Identifier),
    pub fn init(allocator: std.mem.Allocator) Context {
        return Context{ .allocator = allocator, .identifiers = std.ArrayList(Identifier).init(allocator) };
    }
    pub fn getIdentifier(self: *Context, name: []u8) !Identifier {
        for (self.identifiers.items) |item| {
            if (std.mem.eql(u8, item.name, name)) {
                return item;
            }
        }

        const identifier = try Identifier.fromExisting(self.allocator, name);
        errdefer identifier.deinit(self.allocator);
        try self.identifiers.append(identifier);
        return identifier;
    }
    pub fn deinit(self: Context) void {
        for (self.identifiers.items) |item| item.deinit(self.allocator);
        self.identifiers.deinit();
    }
};

pub const Program = struct {
    context: Context,
    function_definition: FunctionDefinition,
    pub fn fromProgram(allocator: std.mem.Allocator, program: tacky.Program) !Program {
        var result = Program{ .context = Context.init(allocator), .function_definition = undefined };
        result.function_definition = try FunctionDefinition.fromFunctionDefinition(&result.context, program.function_definition);
        return result;
    }
    pub fn deinit(self: Program) void {
        self.function_definition.deinit(self.context.allocator);
        self.context.deinit();
    }
};

pub const FunctionDefinition = struct {
    identifier: Identifier,
    instructions: []Instruction,
    pub fn fromFunctionDefinition(context: *Context, function_definition: tacky.FunctionDefinition) !FunctionDefinition {
        const initial = try instructionsFromTacky(context, function_definition.instructions);
        defer context.allocator.free(initial);

        const stacked = try FunctionDefinition.replacePseudo(context.allocator, initial);
        defer context.allocator.free(stacked);

        const final = try FunctionDefinition.fixInstructions(context.allocator, stacked);

        return FunctionDefinition{ .identifier = try context.getIdentifier(function_definition.name), .instructions = final };
    }
    pub fn replacePseudo(allocator: std.mem.Allocator, instructions: []const Instruction) ![]Instruction {
        var offset_map = std.StringHashMap(usize).init(allocator);
        defer offset_map.deinit();

        var list = std.ArrayList(Instruction).init(allocator);
        defer list.deinit();

        for (instructions) |instruction| {
            switch (instruction) {
                .mov => |mov| {
                    try list.append(Instruction.mov(try mov.src.stackIfPseudo(&offset_map), try mov.dst.stackIfPseudo(&offset_map)));
                },
                .unary => |unary| {
                    try list.append(Instruction.unary(unary.operator, try unary.operand.stackIfPseudo(&offset_map)));
                },
                .binary => |binary| {
                    try list.append(Instruction.binary(binary.operator, try binary.src.stackIfPseudo(&offset_map), try binary.dst.stackIfPseudo(&offset_map)));
                },
                .idiv => |idiv| {
                    try list.append(Instruction.idiv(try idiv.operand.stackIfPseudo(&offset_map)));
                },
                .set_cc => |set_cc| {
                    try list.append(Instruction.setCC(set_cc.code, try set_cc.dst.stackIfPseudo(&offset_map)));
                },
                .cmp => |cmp| {
                    try list.append(Instruction.cmp(try cmp.src.stackIfPseudo(&offset_map), try cmp.dst.stackIfPseudo(&offset_map)));
                },
                else => try list.append(instruction),
            }
        }

        // TODO: account for sized
        try list.insert(0, Instruction.allocateStack(@intCast(offset_map.count() * 4)));

        return try list.toOwnedSlice();
    }
    pub fn fixInstructions(allocator: std.mem.Allocator, instructions: []const Instruction) ![]Instruction {
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
                    if ((binary.operator == .add or binary.operator == .subtract or binary.operator == .bitwise_and or binary.operator == .bitwise_or or binary.operator == .xor) and (binary.src == .stack and binary.dst == .stack)) {
                        try list.append(Instruction.mov(binary.src, Operand.register(.r10)));
                        try list.append(Instruction.binary(binary.operator, Operand.register(.r10), binary.dst));
                        continue;
                    }

                    if ((binary.operator == .shift_left or binary.operator == .shift_right) and binary.src != .immediate) {
                        try list.append(Instruction.mov(binary.src, Operand.sizedRegister(.cx, 1)));
                        try list.append(Instruction.binary(binary.operator, Operand.sizedRegister(.cx, 1), binary.dst));
                        continue;
                    }

                    if (binary.operator == .multiply) {
                        try list.append(Instruction.mov(binary.dst, Operand.register(.r11)));
                        try list.append(Instruction.binary(binary.operator, binary.src, Operand.register(.r11)));
                        try list.append(Instruction.mov(Operand.register(.r11), binary.dst));
                        continue;
                    }
                },
                .cmp => |cmp| {
                    if (cmp.src == .stack and cmp.dst == .stack) {
                        try list.append(Instruction.mov(cmp.src, Operand.register(.r10)));
                        try list.append(Instruction.cmp(Operand.register(.r10), cmp.dst));
                        continue;
                    }

                    if (cmp.dst == .immediate) {
                        try list.append(Instruction.mov(cmp.dst, Operand.register(.r11)));
                        try list.append(Instruction.cmp(cmp.src, Operand.register(.r11)));
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

pub const Identifier = struct {
    name: []u8,
    pub fn fromExisting(allocator: std.mem.Allocator, existing: []const u8) !Identifier {
        const name = try allocator.dupe(u8, existing);
        return Identifier{ .name = name };
    }
    pub fn deinit(self: Identifier, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
    }
};
