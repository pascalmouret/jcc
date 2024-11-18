const std = @import("std");

pub const TokenType = enum {
    identifier,
    constant,
    open_parenthesis,
    close_parenthesis,
    open_brace,
    close_brace,
    semicolon,
    int,
    void,
    ret,
};

const IdentifierToken = struct {
    identifier: []u8,
};

const ConstantToken = struct {
    constant: []u8,
};

pub const Token = union(TokenType) {
    identifier: IdentifierToken,
    constant: ConstantToken,
    open_parenthesis: TokenType,
    close_parenthesis: TokenType,
    open_brace: TokenType,
    close_brace: TokenType,
    semicolon: TokenType,
    int: TokenType,
    void: TokenType,
    ret: TokenType,
};

const LexerResult = struct {
    allocator: std.mem.Allocator,
    tokens: []Token,
    pub fn create(allocator: std.mem.Allocator, tokens: []Token) LexerResult {
        return LexerResult{ .allocator = allocator, .tokens = tokens };
    }
    pub fn deinit(self: LexerResult) void {
        for (self.tokens) |token| {
            switch (token) {
                .identifier => |identifier| self.allocator.free(identifier.identifier),
                .constant => |constant| self.allocator.free(constant.constant),
                else => continue,
            }
        }
        defer self.allocator.free(self.tokens);
    }
};

pub fn bytes_to_tokens(allocator: std.mem.Allocator, bytes: []u8) !LexerResult {
    var tokens = std.ArrayList(Token).init(allocator);
    defer tokens.deinit();

    var current = std.ArrayList(u8).init(allocator);
    defer current.deinit();
    for (bytes) |byte| {
        switch (byte) {
            '(' => {
                const slice = try current.toOwnedSlice();
                try parse_multibyte_token(allocator, slice, &tokens);
                try tokens.append(Token{ .open_parenthesis = TokenType.open_parenthesis });
            },
            ')' => {
                const slice = try current.toOwnedSlice();
                try parse_multibyte_token(allocator, slice, &tokens);
                try tokens.append(Token{ .close_parenthesis = TokenType.close_parenthesis });
            },
            '{' => {
                const slice = try current.toOwnedSlice();
                try parse_multibyte_token(allocator, slice, &tokens);
                try tokens.append(Token{ .open_brace = TokenType.open_brace });
            },
            '}' => {
                const slice = try current.toOwnedSlice();
                try parse_multibyte_token(allocator, slice, &tokens);
                try tokens.append(Token{ .close_brace = TokenType.close_brace });
            },
            ';' => {
                const slice = try current.toOwnedSlice();
                try parse_multibyte_token(allocator, slice, &tokens);
                try tokens.append(Token{ .semicolon = TokenType.semicolon });
            },
            '\n', ' ', '\t' => {
                const slice = try current.toOwnedSlice();
                try parse_multibyte_token(allocator, slice, &tokens);
            },
            'a'...'z', 'A'...'Z', '0'...'9', '_' => {
                try current.append(byte);
            },
            else => return error.UnsupportedCharacter,
        }
    }

    try parse_multibyte_token(allocator, try current.toOwnedSlice(), &tokens);
    return LexerResult.create(allocator, try tokens.toOwnedSlice());
}

fn parse_multibyte_token(allocator: std.mem.Allocator, token: []u8, list: *std.ArrayList(Token)) !void {
    defer allocator.free(token);

    if (token.len == 0) {
        return;
    }

    switch (token[0]) {
        'a'...'z', 'A'...'Z', '_' => {
            for (token) |char| {
                if (!is_char(char) and !is_digit(char)) {
                    return error.InvalidIdentifier;
                }
            }

            if (std.mem.eql(u8, token, "int")) {
                try list.append(Token{ .int = TokenType.int });
            } else if (std.mem.eql(u8, token, "void")) {
                try list.append(Token{ .void = TokenType.void });
            } else if (std.mem.eql(u8, token, "return")) {
                try list.append(Token{ .ret = TokenType.ret });
            } else {
                try list.append(Token{ .identifier = IdentifierToken{ .identifier = try allocator.dupe(u8, token) } });
            }
        },
        '0'...'9' => {
            for (token) |byte| {
                if (!is_digit(byte)) {
                    return error.InvalidConstant;
                }
            }
            try list.append(Token{ .constant = ConstantToken{ .constant = try allocator.dupe(u8, token) } });
        },
        else => unreachable,
    }
}

fn is_char(byte: u8) bool {
    return (byte >= 'a' and byte <= 'z') or (byte >= 'A' and byte <= 'Z') or byte == '_';
}

fn is_digit(byte: u8) bool {
    return byte >= '0' and byte <= '9';
}

test bytes_to_tokens {
    const allocator = std.testing.allocator;

    const tokens = try allocator.dupe(u8, "main(){}return int;123");
    defer allocator.free(tokens);
    const result = try bytes_to_tokens(allocator, tokens);
    defer result.deinit();

    const expected = LexerResult.create(allocator, try allocator.dupe(Token, &[_]Token{
        Token{ .identifier = IdentifierToken{ .identifier = try allocator.dupe(u8, "main") } },
        Token{ .open_parenthesis = TokenType.open_parenthesis },
        Token{ .close_parenthesis = TokenType.close_parenthesis },
        Token{ .open_brace = TokenType.open_brace },
        Token{ .close_brace = TokenType.close_brace },
        Token{ .ret = TokenType.ret },
        Token{ .int = TokenType.int },
        Token{ .semicolon = TokenType.semicolon },
        Token{ .constant = ConstantToken{ .constant = try allocator.dupe(u8, "123") } },
    }));
    defer expected.deinit();

    try std.testing.expectEqualDeep(
        expected,
        result,
    );
}
