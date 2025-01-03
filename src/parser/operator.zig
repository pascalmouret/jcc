const std = @import("std");

const ParserContext = @import("./ast.zig").ParserContext;
const parserError = @import("./ast.zig").parserError;

pub const Operator = union(enum) {
    unary: UnaryOperator,
    binary: BinaryOperator,
    assignment: AssignmentOperator,

    pub const negate = UnaryOperator{ .operator = .negate };
    pub const complement = UnaryOperator{ .operator = .complement };
    pub const logical_not = UnaryOperator{ .operator = .logical_not };
    pub const increment = UnaryOperator{ .operator = .increment };
    pub const decrement = UnaryOperator{ .operator = .decrement };

    pub const divide = BinaryOperator{ .operator = .divide, .precedence = 55 };
    pub const multiply = BinaryOperator{ .operator = .multiply, .precedence = 55 };
    pub const modulo = BinaryOperator{ .operator = .modulo, .precedence = 55 };
    pub const add = BinaryOperator{ .operator = .add, .precedence = 50 };
    pub const subtract = BinaryOperator{ .operator = .subtract, .precedence = 50 };
    pub const shift_left = BinaryOperator{ .operator = .shift_left, .precedence = 45 };
    pub const shift_right = BinaryOperator{ .operator = .shift_right, .precedence = 45 };
    pub const less_than = BinaryOperator{ .operator = .less_than, .precedence = 40 };
    pub const less_than_equal = BinaryOperator{ .operator = .less_than_equal, .precedence = 40 };
    pub const greater_than = BinaryOperator{ .operator = .greater_than, .precedence = 40 };
    pub const greater_than_equal = BinaryOperator{ .operator = .greater_than_equal, .precedence = 40 };
    pub const equal = BinaryOperator{ .operator = .equal, .precedence = 35 };
    pub const not_equal = BinaryOperator{ .operator = .not_equal, .precedence = 35 };
    pub const bitwise_and = BinaryOperator{ .operator = .bitwise_and, .precedence = 30 };
    pub const xor = BinaryOperator{ .operator = .xor, .precedence = 25 };
    pub const bitwise_or = BinaryOperator{ .operator = .bitwise_or, .precedence = 20 };
    pub const logical_and = BinaryOperator{ .operator = .logical_and, .precedence = 15 };
    pub const logical_or = BinaryOperator{ .operator = .logical_or, .precedence = 15 };
    pub const conditional = BinaryOperator{ .operator = .conditional, .precedence = 10 };

    pub const assign = AssignmentOperator{ .operator = .assign, .binary_operator = null };
    pub const assign_add = AssignmentOperator{ .operator = .assign_add, .binary_operator = Operator.add };
    pub const assign_subtract = AssignmentOperator{ .operator = .assign_add, .binary_operator = Operator.subtract };
    pub const assign_multiply = AssignmentOperator{ .operator = .assign_add, .binary_operator = Operator.multiply };
    pub const assign_divide = AssignmentOperator{ .operator = .assign_add, .binary_operator = Operator.divide };
    pub const assign_modulo = AssignmentOperator{ .operator = .assign_add, .binary_operator = Operator.modulo };
    pub const assign_bitwise_and = AssignmentOperator{ .operator = .assign_add, .binary_operator = Operator.bitwise_and };
    pub const assign_bitwise_or = AssignmentOperator{ .operator = .assign_add, .binary_operator = Operator.bitwise_or };
    pub const assign_xor = AssignmentOperator{ .operator = .assign_add, .binary_operator = Operator.xor };
    pub const assign_shift_left = AssignmentOperator{ .operator = .assign_add, .binary_operator = Operator.shift_left };
    pub const assign_shift_right = AssignmentOperator{ .operator = .assign_add, .binary_operator = Operator.shift_right };

    pub fn parse(context: *ParserContext) !Operator {
        const result = try Operator.peekParse(context);

        if (result == null) {
            const peek = try context.peek();
            return parserError(
                error.UnexpectedToken,
                peek.line,
                peek.character,
                "Expected operator, found '{s}'.",
                .{peek.bytes},
            );
        } else {
            return result;
        }
    }

    pub fn peekParse(context: *ParserContext) !?Operator {
        if (try UnaryOperator.peekParse(context)) |op| return Operator{ .unary = op };
        if (try BinaryOperator.peekParse(context)) |op| return Operator{ .binary = op };
        if (try AssignmentOperator.peekParse(context)) |op| return Operator{ .assignment = op };

        return null;
    }

    pub fn peekParseWithoutUnary(context: *ParserContext) !?Operator {
        if (try BinaryOperator.peekParse(context)) |op| return Operator{ .binary = op };
        if (try AssignmentOperator.peekParse(context)) |op| return Operator{ .assignment = op };

        return null;
    }

    pub fn precedence(self: Operator) usize {
        return switch (self) {
            .binary => self.binary.precedence,
            .assignment => self.assignment.precedence,
            .unary => unreachable,
        };
    }
};

pub const UnaryOperator = struct {
    operator: enum {
        negate,
        complement,
        logical_not,
        increment,
        decrement,
    },

    pub fn parse(context: *ParserContext) !UnaryOperator {
        const result = try UnaryOperator.peekParse(context);

        if (result == null) {
            const peek = try context.peek();
            return parserError(
                error.UnexpectedToken,
                peek.line,
                peek.character,
                "Expected unary operator, found '{s}'.",
                .{peek.bytes},
            );
        } else {
            try context.consumeNext();
            return result.?;
        }
    }

    pub fn peekParse(context: *ParserContext) !?UnaryOperator {
        return switch (try context.peekKind()) {
            .hyphen => Operator.negate,
            .tilde => Operator.complement,
            .exclamation_point => Operator.logical_not,
            .increment => Operator.increment,
            .decrement => Operator.decrement,
            else => null,
        };
    }
};

pub const BinaryOperator = struct {
    can_short_circuit: bool = false,
    precedence: usize,
    operator: enum {
        add,
        subtract,
        divide,
        multiply,
        modulo,
        bitwise_and,
        bitwise_or,
        xor,
        shift_left,
        shift_right,
        less_than,
        less_than_equal,
        greater_than,
        greater_than_equal,
        equal,
        not_equal,
        logical_and,
        logical_or,
        conditional,
    },

    pub fn parse(context: *ParserContext) !BinaryOperator {
        const result = try BinaryOperator.peekParse(context);

        if (result == null) {
            const peek = try context.peek();
            return parserError(
                error.UnexpectedToken,
                peek.line,
                peek.character,
                "Expected binary operator, found '{s}'.",
                .{peek.bytes},
            );
        } else {
            try context.consumeNext();
            return result.?;
        }
    }

    pub fn peekParse(context: *ParserContext) !?BinaryOperator {
        return switch (try context.peekKind()) {
            .plus => Operator.add,
            .hyphen => Operator.subtract,
            .slash => Operator.divide,
            .asterisk => Operator.multiply,
            .percent => Operator.modulo,
            .ampersand => Operator.bitwise_and,
            .pipe => Operator.bitwise_or,
            .caret => Operator.xor,
            .shift_left => Operator.shift_left,
            .shift_right => Operator.shift_right,
            .less => Operator.less_than,
            .less_equal => Operator.less_than_equal,
            .greater => Operator.greater_than,
            .greater_equal => Operator.greater_than_equal,
            .equal => Operator.equal,
            .not_equal => Operator.not_equal,
            .logical_and => Operator.logical_and,
            .logical_or => Operator.logical_or,
            .question_mark => Operator.conditional,
            else => null,
        };
    }
};

pub const AssignmentOperator = struct {
    precedence: usize = 5,
    binary_operator: ?BinaryOperator,
    operator: enum {
        assign,
        assign_add,
        assign_subtract,
        assign_multiply,
        assign_divide,
        assign_modulo,
        assign_bitwise_and,
        assign_bitwise_or,
        assign_bitwise_xor,
        assign_shift_left,
        assign_shift_right,
    },

    pub fn parse(context: *ParserContext) !AssignmentOperator {
        const result = try AssignmentOperator.peekParse(context);

        if (result == null) {
            const peek = try context.peek();
            return parserError(
                error.UnexpectedToken,
                peek.line,
                peek.character,
                "Expected assignment operator, found '{s}'.",
                .{peek.bytes},
            );
        } else {
            try context.consumeNext();
            return result.?;
        }
    }

    pub fn peekParse(context: *ParserContext) !?AssignmentOperator {
        return switch (try context.peekKind()) {
            .equal_sign => Operator.assign,
            .compound_plus => Operator.assign_add,
            .compound_hyphen => Operator.assign_subtract,
            .compound_asterisk => Operator.assign_multiply,
            .compound_slash => Operator.assign_divide,
            .compound_percent => Operator.assign_modulo,
            .compound_ampersand => Operator.assign_bitwise_and,
            .compound_pipe => Operator.assign_bitwise_or,
            .compound_caret => Operator.assign_xor,
            .compound_shift_left => Operator.assign_shift_left,
            .compound_shift_right => Operator.assign_shift_right,
            else => null,
        };
    }
};
