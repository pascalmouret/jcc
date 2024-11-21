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

const TokenIterator = struct {
    tokens: []Token,
    index: usize,

    pub fn init(tokens: []Token) TokenIterator {
        return TokenIterator{
            .tokens = tokens,
            .index = 0,
        };
    }

    pub fn next(self: *TokenIterator) !Token {
        if (self.tokens.len >= self.index + 1) {
            defer self.index += 1;
            return self.tokens[self.index];
        } else {
            return error.UnexpectedEOF;
        }
    }

    pub fn peek(self: TokenIterator) !Token {
        if (self.tokens.len >= self.index + 1) {
            return self.tokens[self.index];
        } else {
            return error.UnexpectedEOF;
        }
    }

    pub fn peekKind(self: TokenIterator) !TokenKind {
        return (try self.peek()).kind;
    }

    pub fn getA(self: *TokenIterator, comptime kind: TokenKind) !Token {
        const next_token = try self.next();
        if (next_token.kind != kind) {
            return unexpected_token(next_token, &[1]TokenKind{kind});
        } else {
            return next_token;
        }
    }

    pub fn getOneOf(self: *TokenIterator, comptime kinds: []const TokenKind) !Token {
        const next_token = try self.next();

        for (kinds) |kind| {
            if (next_token.kind == kind) {
                return next_token;
            }
        }

        return unexpected_token(next_token, kinds);
    }

    pub fn consumeA(self: *TokenIterator, comptime kind: TokenKind) !void {
        _ = try self.getA(kind);
    }

    pub fn consumeOneOf(self: *TokenIterator, comptime kinds: []const TokenKind) !TokenKind {
        const token = try getOneOf(self, kinds);
        return token.kind;
    }
};

pub fn tokens_to_program(allocator: std.mem.Allocator, tokens: []Token) !Program {
    var iterator = TokenIterator.init(tokens);
    return try Program.parse(allocator, &iterator);
}

pub const Program = struct {
    allocator: std.mem.Allocator,
    function: Function,
    pub fn parse(allocator: std.mem.Allocator, tokens: *TokenIterator) !Program {
        const program = Program{ .allocator = allocator, .function = try Function.parse(allocator, tokens) };
        _ = tokens.peek() catch return program; // no additional tokens!
        return error.UnexpectedToken;
    }
    pub fn deinit(self: Program) void {
        self.function.deinit(self.allocator);
    }
};

pub const Function = struct {
    name: Identifier,
    body: Statement,
    pub fn parse(allocator: std.mem.Allocator, tokens: *TokenIterator) !Function {
        try tokens.consumeA(.int);
        const identifier = try Identifier.parse(tokens);
        try tokens.consumeA(.open_parenthesis);
        try tokens.consumeA(.void);
        try tokens.consumeA(.close_parenthesis);
        try tokens.consumeA(.open_brace);
        const statement = try Statement.parse(allocator, tokens);
        try tokens.consumeA(.close_brace);

        return Function{ .name = identifier, .body = statement };
    }
    pub fn deinit(self: Function, allocator: std.mem.Allocator) void {
        self.body.deinit(allocator);
    }
};

const Identifier = struct {
    name: []u8,
    pub fn parse(tokens: *TokenIterator) !Identifier {
        return Identifier{
            .name = (try tokens.getA(.identifier)).bytes,
        };
    }
};

pub const Statement = union(enum) {
    ret: Ret,
    pub fn parse(allocator: std.mem.Allocator, tokens: *TokenIterator) !Statement {
        switch (try tokens.peekKind()) {
            .ret => return Statement{ .ret = try Ret.parse(allocator, tokens) },
            else => {
                const token = try tokens.next();
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
    pub fn parse(allocator: std.mem.Allocator, tokens: *TokenIterator) !Ret {
        try tokens.consumeA(.ret);
        const expression = try Expression.parse(allocator, tokens);
        try tokens.consumeA(.semicolon);
        return Ret{ .expression = expression };
    }
    pub fn deinit(self: Ret, allocator: std.mem.Allocator) void {
        self.expression.deinit(allocator);
    }
};

pub const Expression = union(enum) {
    constant: Constant,
    unary: Unary,
    pub fn parse(allocator: std.mem.Allocator, tokens: *TokenIterator) (ParserError || error{OutOfMemory})!*Expression {
        switch (try tokens.peekKind()) {
            .constant => {
                const result = try allocator.create(Expression);
                errdefer allocator.destroy(result);
                result.* = .{ .constant = try Constant.parse(tokens) };
                return result;
            },
            .hyphen, .tilde => {
                const result = try allocator.create(Expression);
                errdefer allocator.destroy(result);
                result.* = .{ .unary = try Unary.parse(allocator, tokens) };
                return result;
            },
            .open_parenthesis => {
                try tokens.consumeA(.open_parenthesis);
                const expression = try Expression.parse(allocator, tokens);
                try tokens.consumeA(.close_parenthesis);
                return expression;
            },
            else => {
                const token = try tokens.next();
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
    pub fn deinit(self: *Expression, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .unary => self.unary.expression.deinit(allocator),
            else => {},
        }
        allocator.destroy(self);
    }
};

const Constant = struct {
    value: u32,
    pub fn parse(tokens: *TokenIterator) !Constant {
        const constant = try tokens.getA(.constant);
        const as_int = std.fmt.parseInt(u32, constant.bytes, 10) catch {
            return parser_error(ParserError.InvalidConstant, constant.line, constant.character, "'{s}' is not a valid constant", .{constant.bytes});
        };
        return Constant{ .value = as_int };
    }
};

const Unary = struct {
    operator: enum { negate, complement },
    expression: *Expression,
    pub fn parse(allocator: std.mem.Allocator, tokens: *TokenIterator) !Unary {
        switch (try tokens.consumeOneOf(&.{ .hyphen, .tilde })) {
            .hyphen => return Unary{ .operator = .negate, .expression = try Expression.parse(allocator, tokens) },
            .tilde => return Unary{ .operator = .complement, .expression = try Expression.parse(allocator, tokens) },
            else => unreachable,
        }
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
