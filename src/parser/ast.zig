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
            return unexpected_token(next_token, &[1]TokenKind{kind});
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

        return unexpected_token(next_token, kinds);
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

pub fn tokens_to_program(allocator: std.mem.Allocator, tokens: []Token) !Program {
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
    body: Statement,
    pub fn parse(context: *ParserContext) !Function {
        try context.consumeA(.int);
        const identifier = try Identifier.parse(context);
        try context.consumeA(.open_parenthesis);
        try context.consumeA(.void);
        try context.consumeA(.close_parenthesis);
        try context.consumeA(.open_brace);
        const statement = try Statement.parse(context);
        try context.consumeA(.close_brace);

        return Function{ .name = identifier, .body = statement };
    }
    pub fn deinit(self: Function, allocator: std.mem.Allocator) void {
        self.body.deinit(allocator);
    }
};

const Identifier = struct {
    name: []u8,
    pub fn parse(context: *ParserContext) !Identifier {
        return Identifier{
            .name = (try context.getA(.identifier)).bytes,
        };
    }
};

pub const Statement = union(enum) {
    ret: Ret,
    pub fn parse(context: *ParserContext) !Statement {
        switch (try context.peekKind()) {
            .ret => return Statement{ .ret = try Ret.parse(context) },
            else => {
                const token = try context.next();
                return parser_error(
                    ParserError.ExpectedStatement,
                    token.line,
                    token.character,
                    "Expected statement, found '{s}'",
                    .{token.bytes},
                );
            },
        }
    }
    pub fn deinit(self: Statement, allocator: std.mem.Allocator) void {
        self.ret.deinit(allocator);
    }
};

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
    factor: *Factor,
    pub fn parse(context: *ParserContext, min_precedence: usize) (ParserError || error{OutOfMemory})!*Expression {
        var left = try (Expression{ .factor = try Factor.parse(context) }).to_owned(context.allocator);
        errdefer left.deinit(context.allocator);

        while (context.nextIsOneOf(&.{ .plus, .hyphen, .slash, .asterisk, .percent })) {
            const operator = try BinaryOperator.peek_parse(context);

            if (operator.precedence() < min_precedence) break;
            try context.consumeNext();

            const right = try Expression.parse(context, operator.precedence() + 1);
            const old_left = left;
            left = try (Expression{ .binary = Binary{ .operator = operator, .left = old_left, .right = right } }).to_owned(context.allocator);
        }

        return left;
    }
    pub fn to_owned(self: Expression, allocator: std.mem.Allocator) !*Expression {
        const result = try allocator.create(Expression);
        result.* = self;
        return result;
    }
    pub fn deinit(self: *Expression, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .binary => self.binary.deinit(allocator),
            .factor => self.factor.deinit(allocator),
        }
        allocator.destroy(self);
    }
};

pub const Factor = union(enum) {
    constant: Constant,
    unary: Unary,
    expression: *Expression,
    pub fn parse(context: *ParserContext) (ParserError || error{OutOfMemory})!*Factor {
        switch (try context.peekKind()) {
            .constant => {
                return (Factor{ .constant = try Constant.parse(context) }).to_owned(context.allocator);
            },
            .hyphen, .tilde => {
                return (Factor{ .unary = try Unary.parse(context) }).to_owned(context.allocator);
            },
            .open_parenthesis => {
                try context.consumeA(.open_parenthesis);
                const expression = try Expression.parse(context, 0);
                try context.consumeA(.close_parenthesis);
                return (Factor{ .expression = expression }).to_owned(context.allocator);
            },
            else => {
                const token = try context.next();
                return parser_error(
                    ParserError.ExpectedExpression,
                    token.line,
                    token.character,
                    "Expected expression, found '{s}'",
                    .{token.bytes},
                );
            },
        }
    }
    pub fn to_owned(self: Factor, allocator: std.mem.Allocator) !*Factor {
        const result = try allocator.create(Factor);
        result.* = self;
        return result;
    }
    pub fn deinit(self: *Factor, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .unary => self.unary.deinit(allocator),
            .expression => self.expression.deinit(allocator),
            else => {},
        }
        allocator.destroy(self);
    }
};

const Constant = struct {
    value: u32,
    pub fn parse(context: *ParserContext) !Constant {
        const constant = try context.getA(.constant);
        const as_int = std.fmt.parseInt(u32, constant.bytes, 10) catch {
            return parser_error(ParserError.InvalidConstant, constant.line, constant.character, "'{s}' is not a valid constant", .{constant.bytes});
        };
        return Constant{ .value = as_int };
    }
};

pub const UnaryOperator = enum {
    negate,
    complement,
};

const Unary = struct {
    operator: UnaryOperator,
    expression: *Expression,
    pub fn parse(context: *ParserContext) !Unary {
        switch (try context.consumeOneOf(&.{ .hyphen, .tilde })) {
            .hyphen => return Unary{ .operator = .negate, .expression = try Expression.parse(context, 0) },
            .tilde => return Unary{ .operator = .complement, .expression = try Expression.parse(context, 0) },
            else => unreachable,
        }
    }
    pub fn deinit(self: Unary, allocator: std.mem.Allocator) void {
        self.expression.deinit(allocator);
    }
};

pub const BinaryOperator = enum {
    add,
    subtract,
    divide,
    multiply,
    modulo,
    pub fn peek_parse(context: *ParserContext) !BinaryOperator {
        const next_kind = try context.peekKind();
        return switch (next_kind) {
            .plus => .add,
            .hyphen => .subtract,
            .slash => .divide,
            .asterisk => .multiply,
            .percent => .modulo,
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
        };
    }
};

const Binary = struct {
    operator: BinaryOperator,
    left: *Expression,
    right: *Expression,
    pub fn deinit(self: Binary, allocator: std.mem.Allocator) void {
        self.left.deinit(allocator);
        self.right.deinit(allocator);
    }
};

fn parser_error(
    err: ParserError,
    line: usize,
    character: usize,
    comptime format: []const u8,
    args: anytype,
) ParserError {
    std.log.err("{s} [{d}:{d}]: " ++ format, .{ @errorName(err), line, character } ++ args);
    return err;
}

fn unexpected_token(
    actual: Token,
    comptime expected: []const TokenKind,
) ParserError {
    if (expected.len == 1) {
        return parser_error(ParserError.UnexpectedToken, actual.line, actual.character, "Expected a '{s}', found '{s}'", .{ @tagName(expected[0]), actual.bytes });
    } else {
        return parser_error(ParserError.UnexpectedToken, actual.line, actual.character, "Expected one of '{s}', found '{s}'", .{ tag_list(expected), actual.bytes });
    }
}

fn tag_list(
    comptime kinds: []const TokenKind,
) [kinds_list_length(kinds)]u8 {
    var bfr: [kinds_list_length(kinds)]u8 = undefined;
    var index: usize = 0;
    for (kinds) |kind| {
        if (index != 0) {
            @memcpy(bfr[index .. index + 2], ", ");
            index += 2;
        }
        const tag = @tagName(kind);
        @memcpy(bfr[index..tag.len], tag);
    }
    return bfr;
}

fn kinds_list_length(
    comptime kinds: []const TokenKind,
) comptime_int {
    var len = (kinds.len - 1) * 2;
    for (kinds) |kind| {
        len += @tagName(kind).len;
    }
    return len;
}
