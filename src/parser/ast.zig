const std = @import("std");

const lexer = @import("../lexer/lexer.zig");
const Token = lexer.Token;
const TokenKind = lexer.TokenKind;

const ParserError = error{
    UnexpectedToken,
    UnexpectedEOF,
    ExpectedExpression,
    ExpectedStatement,
    InvalidConstant,
};

const ParserContext = struct {
    allocator: std.mem.Allocator,
    tokens: []Token,
    index: usize,

    pub fn init(allocator: std.mem.Allocator, tokens: []Token) ParserContext {
        return ParserContext{
            .tokens = tokens,
            .index = 0,
            .allocator = allocator,
        };
    }

    pub fn next(self: *ParserContext) !Token {
        if (self.tokens.len >= self.index + 1) {
            defer self.index += 1;
            return self.tokens[self.index];
        } else {
            return error.UnexpectedEOF;
        }
    }

    pub fn peek(self: ParserContext) !Token {
        if (self.tokens.len >= self.index + 1) {
            return self.tokens[self.index];
        } else {
            return error.UnexpectedEOF;
        }
    }

    pub fn peekKind(self: ParserContext) !TokenKind {
        return (try self.peek()).kind;
    }

    pub fn getA(self: *ParserContext, comptime kind: TokenKind) !Token {
        const next_token = try self.next();
        if (next_token.kind != kind) {
            return unexpectedToken(next_token, &[1]TokenKind{kind});
        } else {
            return next_token;
        }
    }

    pub fn getOneOf(self: *ParserContext, comptime kinds: []const TokenKind) !Token {
        const next_token = try self.next();

        for (kinds) |kind| {
            if (next_token.kind == kind) {
                return next_token;
            }
        }

        return unexpectedToken(next_token, kinds);
    }

    pub fn consumeNext(self: *ParserContext) !void {
        _ = try self.next();
    }

    pub fn consumeA(self: *ParserContext, comptime kind: TokenKind) !void {
        _ = try self.getA(kind);
    }

    pub fn consumeOneOf(self: *ParserContext, comptime kinds: []const TokenKind) !TokenKind {
        const token = try getOneOf(self, kinds);
        return token.kind;
    }

    pub fn nextIsOneOf(self: ParserContext, comptime kinds: []const TokenKind) bool {
        const next_kind = self.peekKind() catch return false;

        for (kinds) |kind| {
            if (kind == next_kind) return true;
        }

        return false;
    }
};

pub fn tokensToProgram(allocator: std.mem.Allocator, tokens: []Token) !Program {
    var context = ParserContext.init(allocator, tokens);
    return try Program.parse(&context);
}

pub const Program = struct {
    allocator: std.mem.Allocator,
    function: Function,
    pub fn parse(context: *ParserContext) !Program {
        const program = Program{ .allocator = context.allocator, .function = try Function.parse(context) };
        _ = context.peek() catch return program; // no additional tokens!
        return error.UnexpectedToken;
    }
    pub fn deinit(self: Program) void {
        self.function.deinit(self.allocator);
    }
};

pub const Function = struct {
    name: Identifier,
    body: []BlockItem,
    pub fn parse(context: *ParserContext) !Function {
        try context.consumeA(.int);
        const identifier = try Identifier.parse(context);
        try context.consumeA(.open_parenthesis);
        try context.consumeA(.void);
        try context.consumeA(.close_parenthesis);
        try context.consumeA(.open_brace);

        var list = std.ArrayList(BlockItem).init(context.allocator);
        defer list.deinit();

        var next = try context.peekKind();
        while (next != .close_brace) {
            try list.append(try BlockItem.parse(context));
            next = try context.peekKind();
        }
        try context.consumeA(.close_brace);

        return Function{ .name = identifier, .body = try list.toOwnedSlice() };
    }
    pub fn deinit(self: Function, allocator: std.mem.Allocator) void {
        self.name.deinit(allocator);
        for (self.body) |block| block.deinit(allocator);
        allocator.free(self.body);
    }
};

const Identifier = struct {
    name: []u8,
    pub fn parse(context: *ParserContext) !Identifier {
        const name = try context.allocator.dupe(u8, (try context.getA(.identifier)).bytes);
        return Identifier{ .name = name };
    }
    pub fn deinit(self: Identifier, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
    }
};

pub const BlockItem = union(enum) {
    statement: Statement,
    declaration: Declaration,
    pub fn parse(context: *ParserContext) !BlockItem {
        switch (try context.peekKind()) {
            .int => return BlockItem{ .declaration = try Declaration.parse(context) },
            else => return BlockItem{ .statement = try Statement.parse(context) },
        }
    }
    pub fn deinit(self: BlockItem, allocator: std.mem.Allocator) void {
        switch (self) {
            .statement => self.statement.deinit(allocator),
            .declaration => self.declaration.deinit(allocator),
        }
    }
};

pub const Statement = union(enum) {
    ret: Ret,
    expression: *Expression,
    null: Null,
    pub fn parse(context: *ParserContext) !Statement {
        switch (try context.peekKind()) {
            .ret => return Statement{ .ret = try Ret.parse(context) },
            .semicolon => {
                try context.consumeA(.semicolon);
                return Statement{ .null = .{} };
            },
            else => {
                const expression = Statement{ .expression = try Expression.parse(context, 0) };
                try context.consumeA(.semicolon);
                return expression;
            },
        }
    }
    pub fn deinit(self: Statement, allocator: std.mem.Allocator) void {
        switch (self) {
            .ret => self.ret.deinit(allocator),
            .expression => self.expression.deinit(allocator),
            .null => {},
        }
    }
};

const Null = struct {};

const Ret = struct {
    expression: *Expression,
    pub fn parse(context: *ParserContext) !Ret {
        try context.consumeA(.ret);
        const expression = try Expression.parse(context, 0);
        try context.consumeA(.semicolon);
        return Ret{ .expression = expression };
    }
    pub fn deinit(self: Ret, allocator: std.mem.Allocator) void {
        self.expression.deinit(allocator);
    }
};

pub const Expression = union(enum) {
    binary: Binary,
    assignment: Assignment,
    factor: *Factor,
    pub fn parse(context: *ParserContext, min_precedence: usize) (ParserError || error{OutOfMemory})!*Expression {
        var left = try (Expression{ .factor = try Factor.parse(context) }).toOwned(context.allocator);
        errdefer left.deinit(context.allocator);

        while (context.nextIsOneOf(&BinaryOperator.token_kinds)) {
            const operator = try BinaryOperator.peekParse(context);

            if (operator.precedence() < min_precedence) break;
            try context.consumeNext();

            switch (operator) {
                // right associative
                .assign => {
                    const right = try Expression.parse(context, operator.precedence());
                    const old_left = left;
                    left = try (Expression{ .assignment = Assignment{ .left = old_left, .right = right } }).toOwned(context.allocator);
                },
                else => {
                    const right = try Expression.parse(context, operator.precedence() + 1);
                    const old_left = left;
                    left = try (Expression{ .binary = Binary{ .operator = operator, .left = old_left, .right = right } }).toOwned(context.allocator);
                },
            }
        }

        return left;
    }
    pub fn toOwned(self: Expression, allocator: std.mem.Allocator) !*Expression {
        const result = try allocator.create(Expression);
        result.* = self;
        return result;
    }
    pub fn deinit(self: *Expression, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .binary => self.binary.deinit(allocator),
            .factor => self.factor.deinit(allocator),
            .assignment => self.assignment.deinit(allocator),
        }
        allocator.destroy(self);
    }
};

pub const Factor = union(enum) {
    constant: Constant,
    unary: Unary,
    variable: Variable,
    expression: *Expression,
    pub fn parse(context: *ParserContext) (ParserError || error{OutOfMemory})!*Factor {
        switch (try context.peekKind()) {
            .constant => {
                return (Factor{ .constant = try Constant.parse(context) }).toOwned(context.allocator);
            },
            .hyphen, .tilde, .exclamation_point => {
                return (Factor{ .unary = try Unary.parse(context) }).toOwned(context.allocator);
            },
            .open_parenthesis => {
                try context.consumeA(.open_parenthesis);
                const expression = try Expression.parse(context, 0);
                try context.consumeA(.close_parenthesis);
                return (Factor{ .expression = expression }).toOwned(context.allocator);
            },
            .identifier => {
                const identifier = try Identifier.parse(context);
                return (Factor{ .variable = Variable{ .name = identifier } }).toOwned(context.allocator);
            },
            else => {
                const token = try context.next();
                return parserError(
                    ParserError.ExpectedExpression,
                    token.line,
                    token.character,
                    "Expected expression, found '{s}'",
                    .{token.bytes},
                );
            },
        }
    }
    pub fn toOwned(self: Factor, allocator: std.mem.Allocator) !*Factor {
        const result = try allocator.create(Factor);
        result.* = self;
        return result;
    }
    pub fn deinit(self: *Factor, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .unary => self.unary.deinit(allocator),
            .expression => self.expression.deinit(allocator),
            .variable => self.variable.deinit(allocator),
            else => {},
        }
        allocator.destroy(self);
    }
};

const Variable = struct {
    name: Identifier,
    pub fn parse(context: *ParserContext) !Variable {
        const identifier = try Identifier.parse(context);
        return Variable{ .name = identifier };
    }
    pub fn deinit(self: Variable, allocator: std.mem.Allocator) void {
        self.name.deinit(allocator);
    }
};

const Constant = struct {
    value: i32,
    pub fn parse(context: *ParserContext) !Constant {
        const constant = try context.getA(.constant);
        const as_int = std.fmt.parseInt(i32, constant.bytes, 10) catch {
            return parserError(ParserError.InvalidConstant, constant.line, constant.character, "'{s}' is not a valid constant", .{constant.bytes});
        };
        return Constant{ .value = as_int };
    }
};

pub const UnaryOperator = enum {
    negate,
    complement,
    logical_not,
};

const Unary = struct {
    operator: UnaryOperator,
    factor: *Factor,
    pub fn parse(context: *ParserContext) !Unary {
        switch (try context.consumeOneOf(&.{ .hyphen, .tilde, .exclamation_point })) {
            .hyphen => return Unary{ .operator = .negate, .factor = try Factor.parse(context) },
            .tilde => return Unary{ .operator = .complement, .factor = try Factor.parse(context) },
            .exclamation_point => return Unary{ .operator = .logical_not, .factor = try Factor.parse(context) },
            else => unreachable,
        }
    }
    pub fn deinit(self: Unary, allocator: std.mem.Allocator) void {
        self.factor.deinit(allocator);
    }
};

// TODO: less boilerplate?
pub const BinaryOperator = enum {
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
    assign,

    pub const token_kinds: [19]TokenKind = .{
        .plus,
        .hyphen,
        .slash,
        .asterisk,
        .percent,
        .ampersand,
        .pipe,
        .caret,
        .shift_left,
        .shift_right,
        .less,
        .less_equal,
        .greater,
        .greater_equal,
        .equal,
        .not_equal,
        .logical_and,
        .logical_or,
        .equal_sign,
    };

    pub fn peekParse(context: *ParserContext) !BinaryOperator {
        const next_kind = try context.peekKind();
        return switch (next_kind) {
            .plus => .add,
            .hyphen => .subtract,
            .slash => .divide,
            .asterisk => .multiply,
            .percent => .modulo,
            .ampersand => .bitwise_and,
            .pipe => .bitwise_or,
            .caret => .xor,
            .shift_left => .shift_left,
            .shift_right => .shift_right,
            .less => .less_than,
            .less_equal => .less_than_equal,
            .greater => .greater_than,
            .greater_equal => .greater_than_equal,
            .equal => .equal,
            .not_equal => .not_equal,
            .logical_and => .logical_and,
            .logical_or => .logical_or,
            .equal_sign => .assign,
            else => unreachable,
        };
    }
    pub fn precedence(self: BinaryOperator) usize {
        return switch (self) {
            .modulo => 50,
            .multiply => 50,
            .divide => 50,
            .add => 45,
            .subtract => 45,
            .shift_left => 40,
            .shift_right => 40,
            .less_than => 35,
            .less_than_equal => 35,
            .greater_than => 35,
            .greater_than_equal => 35,
            .equal => 30,
            .not_equal => 30,
            .bitwise_and => 25,
            .xor => 20,
            .bitwise_or => 15,
            .logical_and => 10,
            .logical_or => 10,
            .assign => 5,
        };
    }
};

const Binary = struct {
    operator: BinaryOperator,
    left: *Expression,
    right: *Expression,
    pub fn canShortCircuit(self: Binary) bool {
        return self.operator == .logical_or or self.operator == .logical_and;
    }
    pub fn deinit(self: Binary, allocator: std.mem.Allocator) void {
        self.left.deinit(allocator);
        self.right.deinit(allocator);
    }
};

pub const Declaration = struct {
    name: Identifier,
    expression: ?*Expression,
    pub fn parse(context: *ParserContext) !Declaration {
        try context.consumeA(.int);
        const identifier = try Identifier.parse(context);
        const next = try context.getOneOf(&.{ .equal_sign, .semicolon });
        switch (next.kind) {
            .equal_sign => {
                const expression = try Expression.parse(context, 0);
                try context.consumeA(.semicolon);
                return Declaration{ .name = identifier, .expression = expression };
            },
            .semicolon => {
                return Declaration{ .name = identifier, .expression = null };
            },
            else => unreachable,
        }
    }
    pub fn deinit(self: Declaration, allocator: std.mem.Allocator) void {
        self.name.deinit(allocator);
        if (self.expression) |exp| exp.deinit(allocator);
    }
};

const Assignment = struct {
    left: *Expression,
    right: *Expression,
    pub fn deinit(self: Assignment, allocator: std.mem.Allocator) void {
        self.left.deinit(allocator);
        self.right.deinit(allocator);
    }
};

fn parserError(
    err: ParserError,
    line: usize,
    character: usize,
    comptime format: []const u8,
    args: anytype,
) ParserError {
    std.log.err("{s} [{d}:{d}]: " ++ format, .{ @errorName(err), line, character } ++ args);
    return err;
}

// TODO: fix broken error output
fn unexpectedToken(
    actual: Token,
    comptime expected: []const TokenKind,
) ParserError {
    if (expected.len == 1) {
        return parserError(ParserError.UnexpectedToken, actual.line, actual.character, "Expected a '{s}', found '{s}'", .{ @tagName(expected[0]), actual.bytes });
    } else {
        return parserError(ParserError.UnexpectedToken, actual.line, actual.character, "Expected one of [{s}], found '{s}'", .{ tagList(expected), actual.bytes });
    }
}

fn tagList(
    comptime kinds: []const TokenKind,
) [kindsListLength(kinds)]u8 {
    var bfr: [kindsListLength(kinds)]u8 = undefined;
    var index: usize = 0;
    for (kinds) |kind| {
        if (index != 0) {
            @memcpy(bfr[index .. index + 2], ", ");
            index += 2;
        }
        const tag = @tagName(kind);
        @memcpy(bfr[index .. index + tag.len], tag);
        index += tag.len;
    }
    return bfr;
}

fn kindsListLength(
    comptime kinds: []const TokenKind,
) comptime_int {
    var len = (kinds.len - 1) * 2;
    for (kinds) |kind| {
        len += @tagName(kind).len;
    }
    return len;
}
