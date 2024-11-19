const std = @import("std");

pub const TokenKind = enum {
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

pub const Token = struct {
    kind: TokenKind,
    bytes: []u8,
    line: usize,
    character: usize,
};

const LexerError = error{
    InvalidConstant,
    InvalidIdentifier,
    InvalidCharacter,
};

const LexerResult = struct {
    allocator: std.mem.Allocator,
    tokens: []Token,
    pub fn create(allocator: std.mem.Allocator, tokens: []Token) LexerResult {
        return LexerResult{ .allocator = allocator, .tokens = tokens };
    }
    pub fn deinit(self: LexerResult) void {
        defer self.allocator.free(self.tokens);
    }
};

pub fn bytes_to_tokens(allocator: std.mem.Allocator, bytes: []u8) !LexerResult {
    var tokens = std.ArrayList(Token).init(allocator);
    defer tokens.deinit();

    var line: usize = 1;
    var character: usize = 1;

    var token_start: usize = 0;

    for (bytes, 0..) |byte, index| {
        switch (byte) {
            '(' => {
                try parse_multibyte_token(bytes[token_start..index], &tokens, line, character - (index - token_start));
                try tokens.append(create_token(.open_parenthesis, bytes[index .. index + 1], line, character));
                token_start = index + 1;
            },
            ')' => {
                try parse_multibyte_token(bytes[token_start..index], &tokens, line, character - (index - token_start));
                try tokens.append(create_token(.close_parenthesis, bytes[index .. index + 1], line, character));
                token_start = index + 1;
            },
            '{' => {
                try parse_multibyte_token(bytes[token_start..index], &tokens, line, character - (index - token_start));
                try tokens.append(create_token(.open_brace, bytes[index .. index + 1], line, character));
                token_start = index + 1;
            },
            '}' => {
                try parse_multibyte_token(bytes[token_start..index], &tokens, line, character - (index - token_start));
                try tokens.append(create_token(.close_brace, bytes[index .. index + 1], line, character));
                token_start = index + 1;
            },
            ';' => {
                try parse_multibyte_token(bytes[token_start..index], &tokens, line, character - (index - token_start));
                try tokens.append(create_token(.semicolon, bytes[index .. index + 1], line, character));
                token_start = index + 1;
            },
            '\n' => {
                try parse_multibyte_token(bytes[token_start..index], &tokens, line, character - (index - token_start));
                line += 1;
                character = 0; // will be incremented at the end of the loop
                token_start = index + 1;
            },
            ' ', '\t' => {
                try parse_multibyte_token(bytes[token_start..index], &tokens, line, character - (index - token_start));
                token_start = index + 1;
            },
            'a'...'z', 'A'...'Z', '0'...'9', '_' => {},
            else => return lexer_error(LexerError.InvalidCharacter, line, character, "'{c}' is not a valid character", .{byte}),
        }
        character += 1;
    }

    try parse_multibyte_token(bytes[token_start..], &tokens, line, character - (bytes.len - token_start));
    return LexerResult.create(allocator, try tokens.toOwnedSlice());
}

fn parse_multibyte_token(
    token: []u8,
    list: *std.ArrayList(Token),
    line: usize,
    character: usize,
) !void {
    if (token.len == 0) {
        return;
    }

    switch (token[0]) {
        'a'...'z', 'A'...'Z', '_' => {
            for (token) |char| {
                if (!is_char(char) and !is_digit(char)) {
                    return lexer_error(LexerError.InvalidConstant, line, character, "'{s}' is not a valid identifier", .{token});
                }
            }

            if (std.mem.eql(u8, token, "int")) {
                try list.append(create_token(.int, token, line, character));
            } else if (std.mem.eql(u8, token, "void")) {
                try list.append(create_token(.void, token, line, character));
            } else if (std.mem.eql(u8, token, "return")) {
                try list.append(create_token(.ret, token, line, character));
            } else {
                try list.append(create_token(.identifier, token, line, character));
            }
        },
        '0'...'9' => {
            for (token) |byte| {
                if (!is_digit(byte)) {
                    return lexer_error(LexerError.InvalidConstant, line, character, "'{s}' is not a valid constant", .{token});
                }
            }
            try list.append(create_token(.constant, token, line, character));
        },
        else => unreachable,
    }
}

fn create_token(
    kind: TokenKind,
    bytes: []u8,
    line: usize,
    character: usize,
) Token {
    return Token{
        .kind = kind,
        .bytes = bytes,
        .line = line,
        .character = character,
    };
}

fn is_char(byte: u8) bool {
    return (byte >= 'a' and byte <= 'z') or (byte >= 'A' and byte <= 'Z') or byte == '_';
}

fn is_digit(byte: u8) bool {
    return byte >= '0' and byte <= '9';
}

fn lexer_error(
    err: LexerError,
    line: usize,
    character: usize,
    comptime format: []const u8,
    args: anytype,
) LexerError {
    std.log.err("{s} [{d}:{d}]: " ++ format, .{ @errorName(err), line, character } ++ args);
    return err;
}
