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

    pub fn getA(self: *TokenIterator, kind: TokenKind) !Token {
        const next_token = try self.next();
        if (next_token.kind != kind) {
            return unexpected_token(next_token, kind);
        } else {
            return next_token;
        }
    }

    pub fn consumeA(self: *TokenIterator, kind: TokenKind) !void {
        _ = try self.getA(kind);
    }
};

pub fn tokens_to_program(tokens: []Token) !Program {
    var iterator = TokenIterator.init(tokens);
    return try Program.parse(&iterator);
}

pub const Program = struct {
    function: Function,
    pub fn parse(tokens: *TokenIterator) !Program {
        const program = Program{ .function = try Function.parse(tokens) };
        _ = tokens.peek() catch return program; // no additional tokens!
        return error.UnexpectedToken;
    }
};

pub const Function = struct {
    name: Identifier,
    body: Statement,
    pub fn parse(tokens: *TokenIterator) !Function {
        try tokens.consumeA(.int);
        const identifier = try Identifier.parse(tokens);
        try tokens.consumeA(.open_parenthesis);
        try tokens.consumeA(.void);
        try tokens.consumeA(.close_parenthesis);
        try tokens.consumeA(.open_brace);
        const statement = try Statement.parse(tokens);
        try tokens.consumeA(.close_brace);

        return Function{ .name = identifier, .body = statement };
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
    pub fn parse(tokens: *TokenIterator) !Statement {
        switch (try tokens.peekKind()) {
            .ret => return Statement{ .ret = try Ret.parse(tokens) },
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
};

const Ret = struct {
    expression: Expression,
    pub fn parse(tokens: *TokenIterator) !Ret {
        try tokens.consumeA(.ret);
        const expression = try Expression.parse(tokens);
        try tokens.consumeA(.semicolon);
        return Ret{ .expression = expression };
    }
};

const Expression = union(enum) {
    constant: Constant,
    pub fn parse(tokens: *TokenIterator) !Expression {
        switch (try tokens.peekKind()) {
            .constant => return Expression{ .constant = try Constant.parse(tokens) },
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
};

const Constant = struct {
    value: isize,
    pub fn parse(tokens: *TokenIterator) !Constant {
        const constant = try tokens.getA(.constant);
        const as_int = std.fmt.parseInt(isize, constant.bytes, 10) catch {
            return parser_error(ParserError.InvalidConstant, constant.line, constant.character, "'{s}' is not a valid constant", .{constant.bytes});
        };
        return Constant{ .value = as_int };
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
    expected: TokenKind,
) ParserError {
    return parser_error(ParserError.UnexpectedToken, actual.line, actual.character, "Expected a '{s}', found '{s}'", .{ @tagName(expected), actual.bytes });
}
