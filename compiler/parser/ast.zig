const std = @import("std");

const lexer = @import("../lexer/lexer.zig");
const Token = lexer.Token;
const TokenType = lexer.TokenType;

const AstNode = enum {
    program,
    function,
    ret,
    constant,
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

    pub fn consumeA(self: *TokenIterator, kind: TokenType) !void {
        if (try self.next() != kind) {
            return error.UnexpectedToken;
        }
    }
};

pub fn tokens_to_program(tokens: []Token) !Program {
    var iterator = TokenIterator.init(tokens);
    return try Program.parse(&iterator);
}

const Program = struct {
    function: Function,
    pub fn parse(tokens: *TokenIterator) !Program {
        const program = Program{ .function = try Function.parse(tokens) };
        _ = tokens.peek() catch return program; // no additional tokens!
        return error.UnexpectedToken;
    }
};

const Function = struct {
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
        switch (try tokens.next()) {
            .identifier => |id| return Identifier{ .name = id.identifier },
            else => return error.ExpectedIdentifier,
        }
    }
};

const Statement = union(enum) {
    ret: Ret,
    pub fn parse(tokens: *TokenIterator) !Statement {
        switch (try tokens.peek()) {
            .ret => return Statement{ .ret = try Ret.parse(tokens) },
            else => return error.ExpectedStatement,
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
        switch (try tokens.peek()) {
            .constant => return Expression{ .constant = try Constant.parse(tokens) },
            else => return error.ExpectedExpression,
        }
    }
};

const Constant = struct {
    value: isize,
    pub fn parse(tokens: *TokenIterator) !Constant {
        switch (try tokens.next()) {
            .constant => |constant| {
                return Constant{ .value = std.fmt.parseInt(isize, constant.constant, 10) catch return error.InvalidInt };
            },
            else => return error.UnexpectedToken,
        }
    }
};
