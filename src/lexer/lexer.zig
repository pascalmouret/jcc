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
    @"if",
    @"else",
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
    equal_sign,
    compound_plus,
    compound_hyphen,
    compound_asterisk,
    compound_slash,
    compound_percent,
    compound_ampersand,
    compound_pipe,
    compound_caret,
    compound_shift_left,
    compound_shift_right,
    question_mark,
    colon,
};

const keyword_map = std.StaticStringMap(TokenKind).initComptime(.{
    .{ "return", .ret },
    .{ "int", .int },
    .{ "void", .void },
    .{ "if", .@"if" },
    .{ "else", .@"else" },
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
    .{ "?", .question_mark },
    .{ ":", .colon },
    .{ "=", .equal_sign },
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
    .{ "+=", .compound_plus },
    .{ "*=", .compound_asterisk },
    .{ "-=", .compound_hyphen },
    .{ "/=", .compound_slash },
    .{ "%=", .compound_percent },
    .{ "&=", .compound_ampersand },
    .{ "|=", .compound_pipe },
    .{ "^=", .compound_caret },
    .{ "<<=", .compound_shift_left },
    .{ ">>=", .compound_shift_right },
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

pub const LexerResult = struct {
    allocator: std.mem.Allocator,
    tokens: []Token,
    bytes: []u8,
    pub fn create(allocator: std.mem.Allocator, bytes: []u8, tokens: []Token) LexerResult {
        return LexerResult{ .allocator = allocator, .bytes = bytes, .tokens = tokens };
    }
    pub fn deinit(self: LexerResult) void {
        self.allocator.free(self.bytes);
        self.allocator.free(self.tokens);
    }
};

pub fn bytesToTokens(allocator: std.mem.Allocator, bytes: []u8) !LexerResult {
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
                try parseToken(bytes[token_start..index], &tokens, line, character - (index - token_start));
                line += 1;
                character = 0; // will be incremented at the end of the loop
                token_start = index + 1;
            },
            ' ', '\t' => {
                try parseToken(bytes[token_start..index], &tokens, line, character - (index - token_start));
                token_start = index + 1;
            },
            '-', '+', '>', '<', '&', '|', '!', '=', '(', ')', '{', '}', '~', '*', '/', '%', ';', '^', '?', ':' => {
                if (!is_symbol_token) {
                    try parseToken(bytes[token_start..index], &tokens, line, character - (index - token_start));
                    token_start = index;
                    is_symbol_token = true;
                }
            },
            'a'...'z', 'A'...'Z', '0'...'9', '_' => {
                if (is_symbol_token) {
                    try parseToken(bytes[token_start..index], &tokens, line, character - (index - token_start));
                    token_start = index;
                    is_symbol_token = false;
                }
            },
            else => return lexerError(LexerError.InvalidCharacter, line, character, "'{c}' is not a valid character", .{bytes[index]}),
        }

        character += 1;
    }

    try parseToken(bytes[token_start..], &tokens, line, character - (bytes.len - token_start));
    return LexerResult.create(allocator, bytes, try tokens.toOwnedSlice());
}

fn parseToken(
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
                try list.append(createToken(kind, token, line, character));
            } else {
                for (token) |char| {
                    if (!isChar(char) and !isDigit(char)) {
                        return lexerError(LexerError.InvalidConstant, line, character, "'{s}' is not a valid identifier", .{token});
                    }
                }
                try list.append(createToken(.identifier, token, line, character));
            }
        },
        '0'...'9' => {
            for (token) |byte| {
                if (!isDigit(byte)) {
                    return lexerError(LexerError.InvalidConstant, line, character, "'{s}' is not a valid constant", .{token});
                }
            }
            try list.append(createToken(.constant, token, line, character));
        },
        else => {
            for (0..token.len) |iter| {
                const index = token.len - iter;
                if (symbol_map.get(token[0..index])) |kind| {
                    try list.append(createToken(kind, token[0..index], line, character));
                    try parseToken(token[index..token.len], list, line, character - index);
                    return;
                } else {
                    continue;
                }
            }
            return lexerError(LexerError.InvalidToken, line, character, "'{s}' is not a valid token", .{token});
        },
    }
}

fn createToken(
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

fn isChar(byte: u8) bool {
    return (byte >= 'a' and byte <= 'z') or (byte >= 'A' and byte <= 'Z') or byte == '_';
}

fn isDigit(byte: u8) bool {
    return byte >= '0' and byte <= '9';
}

fn lexerError(
    err: LexerError,
    line: usize,
    character: usize,
    comptime format: []const u8,
    args: anytype,
) LexerError {
    std.log.err("{s} [{d}:{d}]: " ++ format, .{ @errorName(err), line, character } ++ args);
    return err;
}
