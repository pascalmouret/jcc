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
    tilde,
    hyphen,
    decrement,
    plus,
    increment,
    asterisk,
    slash,
    percent,
    ampersand,
    pipe,
    caret,
    shift_left,
    shift_right,
    exclamation_point,
    logical_and,
    logical_or,
    equal,
    not_equal,
    less,
    greater,
    less_equal,
    greater_equal,
};

const keyword_map = std.StaticStringMap(TokenKind).initComptime(.{
    .{ "return", .ret },
    .{ "int", .int },
    .{ "void", .void },
});

const symbol_map = std.StaticStringMap(TokenKind).initComptime(.{
    .{ "(", .open_parenthesis },
    .{ ")", .close_parenthesis },
    .{ "{", .open_brace },
    .{ "}", .close_brace },
    .{ ";", .semicolon },
    .{ "~", .tilde },
    .{ "*", .asterisk },
    .{ "/", .slash },
    .{ "%", .percent },
    .{ "|", .pipe },
    .{ "&", .ampersand },
    .{ "^", .caret },
    .{ "+", .plus },
    .{ "-", .hyphen },
    .{ "<", .less },
    .{ ">", .greater },
    .{ "!", .exclamation_point },
    .{ "--", .decrement },
    .{ "++", .increment },
    .{ ">>", .shift_right },
    .{ "<<", .shift_left },
    .{ "&&", .logical_and },
    .{ "||", .logical_or },
    .{ "==", .equal },
    .{ "!=", .not_equal },
    .{ "<=", .less_equal },
    .{ ">=", .greater_equal },
});

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
    InvalidToken,
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

    var index: usize = 0;
    var token_start: usize = 0;
    var is_symbol_token: bool = false;

    while (index < bytes.len) : (index += 1) {
        switch (bytes[index]) {
            '\n' => {
                try parse_token(bytes[token_start..index], &tokens, line, character - (index - token_start));
                line += 1;
                character = 0; // will be incremented at the end of the loop
                token_start = index + 1;
            },
            ' ', '\t' => {
                try parse_token(bytes[token_start..index], &tokens, line, character - (index - token_start));
                token_start = index + 1;
            },
            '-', '+', '>', '<', '&', '|', '!', '=', '(', ')', '{', '}', '~', '*', '/', '%', ';', '^' => {
                if (!is_symbol_token) {
                    try parse_token(bytes[token_start..index], &tokens, line, character - (index - token_start));
                    token_start = index;
                    is_symbol_token = true;
                }
            },
            'a'...'z', 'A'...'Z', '0'...'9', '_' => {
                if (is_symbol_token) {
                    try parse_token(bytes[token_start..index], &tokens, line, character - (index - token_start));
                    token_start = index;
                    is_symbol_token = false;
                }
            },
            else => return lexer_error(LexerError.InvalidCharacter, line, character, "'{c}' is not a valid character", .{bytes[index]}),
        }

        character += 1;
    }

    try parse_token(bytes[token_start..], &tokens, line, character - (bytes.len - token_start));
    return LexerResult.create(allocator, try tokens.toOwnedSlice());
}

fn parse_token(
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
            if (keyword_map.get(token)) |kind| {
                try list.append(create_token(kind, token, line, character));
            } else {
                for (token) |char| {
                    if (!is_char(char) and !is_digit(char)) {
                        return lexer_error(LexerError.InvalidConstant, line, character, "'{s}' is not a valid identifier", .{token});
                    }
                }
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
        else => {
            if (symbol_map.get(token)) |kind| {
                try list.append(create_token(kind, token, line, character));
            } else {
                for (0..token.len) |iter| {
                    const index = token.len - iter;
                    parse_token(token[0 .. index - 1], list, line, character - iter) catch continue;
                    parse_token(token[index - 1 .. token.len], list, line, character - index) catch continue;
                    return;
                }
                return lexer_error(LexerError.InvalidToken, line, character, "'{s}' is not a valid token", .{token});
            }
        },
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
