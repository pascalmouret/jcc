const std = @import("std");

const lexer = @import("../lexer/lexer.zig");
const Token = lexer.Token;
const TokenKind = lexer.TokenKind;

pub const Operator = @import("./operator.zig").Operator;
pub const UnaryOperator = @import("./operator.zig").UnaryOperator;
pub const BinaryOperator = @import("./operator.zig").BinaryOperator;
pub const AssignmentOperator = @import("./operator.zig").AssignmentOperator;

const ParserError = error{
    UnexpectedToken,
    UnexpectedEOF,
    ExpectedExpression,
    ExpectedStatement,
    InvalidConstant,
};

pub const ParserContext = struct {
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

pub const Position = struct {
    character: usize,
    line: usize,
    pub fn extract(from: anytype) Position {
        switch (@TypeOf(from)) {
            *Expression, *Factor => {
                switch (from.*) {
                    inline else => |c| return Position.extract(c),
                }
            },
            Token => {
                return Position{ .character = from.character, .line = from.line };
            },
            else => {
                return from.position;
            },
        }
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
    position: Position,
    pub fn parse(context: *ParserContext) !Function {
        const int = try context.getA(.int);
        const identifier = try Identifier.parse(context);
        errdefer identifier.deinit(context.allocator);
        try context.consumeA(.open_parenthesis);
        try context.consumeA(.void);
        try context.consumeA(.close_parenthesis);
        try context.consumeA(.open_brace);

        var list = std.ArrayList(BlockItem).init(context.allocator);
        defer list.deinit();
        errdefer for (list.items) |item| item.deinit(context.allocator);

        var next = try context.peekKind();
        while (next != .close_brace) {
            try list.append(try BlockItem.parse(context));
            next = try context.peekKind();
        }
        try context.consumeA(.close_brace);

        return Function{
            .name = identifier,
            .body = try list.toOwnedSlice(),
            .position = Position.extract(int),
        };
    }
    pub fn deinit(self: Function, allocator: std.mem.Allocator) void {
        self.name.deinit(allocator);
        for (self.body) |block| block.deinit(allocator);
        allocator.free(self.body);
    }
};

pub const Identifier = struct {
    name: []const u8,
    position: Position,
    pub fn parse(context: *ParserContext) !Identifier {
        const token = try context.getA(.identifier);
        const name = try context.allocator.dupe(u8, token.bytes);
        return Identifier{
            .name = name,
            .position = Position.extract(token),
        };
    }
    pub fn copy(self: Identifier, alloator: std.mem.Allocator) !Identifier {
        const dupe = try alloator.dupe(u8, self.name);
        return Identifier{ .name = dupe, .position = self.position };
    }
    pub fn deinit(self: Identifier, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
    }
};

pub const BlockItem = union(enum) {
    statement: *Statement,
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
    @"if": If,
    @"return": Return,
    expression: *Expression,
    null: Null,
    pub fn parse(context: *ParserContext) (ParserError || error{OutOfMemory})!*Statement {
        switch (try context.peekKind()) {
            .@"if" => return try If.parse(context),
            .@"return" => return try Return.parse(context),
            .semicolon => {
                try context.consumeA(.semicolon);
                return Statement.null(context.allocator);
            },
            else => {
                const exp = try Expression.parse(context, 0);
                try context.consumeA(.semicolon);
                return Statement.expression(context.allocator, exp);
            },
        }
    }
    pub fn @"if"(
        allocator: std.mem.Allocator,
        condition: *Expression,
        then: *Statement,
        @"else": ?*Statement,
        position: Position,
    ) !*Statement {
        const result = try allocator.create(Statement);
        result.* = Statement{
            .@"if" = If{
                .condition = condition,
                .then = then,
                .@"else" = @"else",
                .position = position,
            },
        };
        return result;
    }
    pub fn @"return"(
        allocator: std.mem.Allocator,
        exp: *Expression,
        position: Position,
    ) !*Statement {
        const result = try allocator.create(Statement);
        result.* = Statement{
            .@"return" = Return{
                .expression = exp,
                .position = position,
            },
        };
        return result;
    }
    pub fn expression(
        allocator: std.mem.Allocator,
        inner: *Expression,
    ) !*Statement {
        const result = try allocator.create(Statement);
        result.* = Statement{ .expression = inner };
        return result;
    }
    pub fn @"null"(allocator: std.mem.Allocator) !*Statement {
        const result = try allocator.create(Statement);
        result.* = Statement{ .null = Null{} };
        return result;
    }
    pub fn deinit(self: *Statement, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .@"if" => self.@"if".deinit(allocator),
            .@"return" => self.@"return".deinit(allocator),
            .expression => self.expression.deinit(allocator),
            .null => {},
        }
        allocator.destroy(self);
    }
};

const Null = struct {};

pub const Return = struct {
    expression: *Expression,
    position: Position,
    pub fn parse(context: *ParserContext) !*Statement {
        const ret = try context.getA(.@"return");
        const expression = try Expression.parse(context, 0);
        try context.consumeA(.semicolon);
        return Statement.@"return"(context.allocator, expression, Position.extract(ret));
    }
    pub fn deinit(self: Return, allocator: std.mem.Allocator) void {
        self.expression.deinit(allocator);
    }
};

pub const If = struct {
    condition: *Expression,
    then: *Statement,
    @"else": ?*Statement,
    position: Position,

    pub fn parse(context: *ParserContext) !*Statement {
        const start = try context.getA(.@"if");
        try context.consumeA(.open_parenthesis);

        const condition = try Expression.parse(context, 0);
        errdefer condition.deinit(context.allocator);

        try context.consumeA(.close_parenthesis);

        const then = try Statement.parse(context);
        errdefer then.deinit(context.allocator);

        if (try context.peekKind() == .@"else") {
            try context.consumeA(.@"else");
            const els = try Statement.parse(context);
            return Statement.@"if"(context.allocator, condition, then, els, Position.extract(start));
        } else {
            return Statement.@"if"(context.allocator, condition, then, null, Position.extract(start));
        }
    }
    pub fn deinit(self: If, allocator: std.mem.Allocator) void {
        self.condition.deinit(allocator);
        self.then.deinit(allocator);
        if (self.@"else") |els| {
            els.deinit(allocator);
        }
    }
};

pub const Expression = union(enum) {
    binary: Binary,
    conditional: Conditional,
    assignment: Assignment,
    factor: *Factor,
    pub fn parse(context: *ParserContext, min_precedence: usize) (ParserError || error{OutOfMemory})!*Expression {
        var left = try Expression.factor(context.allocator, try Factor.parse(context));
        errdefer left.deinit(context.allocator);

        var operator = try Operator.peekParseWithoutUnary(context);
        while (operator != null) : (operator = try Operator.peekParseWithoutUnary(context)) {
            if (operator.?.precedence() < min_precedence) break;
            try context.consumeNext();

            switch (operator.?) {
                .assignment => |op| {
                    var right = try Expression.parse(context, op.precedence);

                    if (op.binary_operator) |binary_op| {
                        right = try Expression.binary(context.allocator, binary_op, try left.copy(context.allocator), right, Position.extract(left));
                    }

                    left = try Expression.assignment(context.allocator, left, right, Position.extract(left));
                },
                .binary => |op| {
                    if (op.operator == .conditional) {
                        const then = try Expression.parse(context, 0);
                        try context.consumeA(.colon);
                        const els = try Expression.parse(context, op.precedence);
                        left = try Expression.conditional(context.allocator, left, then, els, Position.extract(left));
                    } else {
                        const right = try Expression.parse(context, op.precedence + 1);
                        left = try Expression.binary(context.allocator, op, left, right, Position.extract(left));
                    }
                },
                .unary => unreachable,
            }
        }

        return left;
    }
    pub fn binary(
        allocator: std.mem.Allocator,
        operator: BinaryOperator,
        left: *Expression,
        right: *Expression,
        position: Position,
    ) !*Expression {
        const result = try allocator.create(Expression);
        result.* = Expression{
            .binary = Binary{
                .operator = operator,
                .left = left,
                .right = right,
                .position = position,
            },
        };
        return result;
    }
    pub fn conditional(
        alloocator: std.mem.Allocator,
        condition: *Expression,
        then: *Expression,
        @"else": *Expression,
        position: Position,
    ) !*Expression {
        const result = try alloocator.create(Expression);
        result.* = Expression{
            .conditional = Conditional{
                .condition = condition,
                .then = then,
                .@"else" = @"else",
                .position = position,
            },
        };
        return result;
    }
    pub fn assignment(
        allocator: std.mem.Allocator,
        left: *Expression,
        right: *Expression,
        position: Position,
    ) !*Expression {
        const result = try allocator.create(Expression);
        result.* = Expression{
            .assignment = Assignment{
                .left = left,
                .right = right,
                .position = position,
            },
        };
        return result;
    }
    pub fn factor(allocator: std.mem.Allocator, inner: *Factor) !*Expression {
        const result = try allocator.create(Expression);
        result.* = Expression{ .factor = inner };
        return result;
    }
    pub fn copy(self: *Expression, allocator: std.mem.Allocator) error{OutOfMemory}!*Expression {
        switch (self.*) {
            .binary => return Expression.binary(
                allocator,
                self.binary.operator,
                try self.binary.left.copy(allocator),
                try self.binary.right.copy(allocator),
                self.binary.position,
            ),
            .conditional => return Expression.conditional(
                allocator,
                try self.conditional.condition.copy(allocator),
                try self.conditional.then.copy(allocator),
                try self.conditional.@"else".copy(allocator),
                self.conditional.position,
            ),
            .factor => return Expression.factor(
                allocator,
                try self.factor.copy(allocator),
            ),
            .assignment => return Expression.assignment(
                allocator,
                try self.assignment.left.copy(allocator),
                try self.assignment.right.copy(allocator),
                self.assignment.position,
            ),
        }
    }
    pub fn deinit(self: *Expression, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .binary => self.binary.deinit(allocator),
            .conditional => self.conditional.deinit(allocator),
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
        var result = switch (try context.peekKind()) {
            .constant => try (Factor{ .constant = try Constant.parse(context) }).toOwned(context.allocator),
            .hyphen, .tilde, .exclamation_point, .increment, .decrement => try (Factor{ .unary = try Unary.parse(context) }).toOwned(context.allocator),
            .open_parenthesis => blk: {
                try context.consumeA(.open_parenthesis);
                const inner = try Expression.parse(context, 0);
                errdefer inner.deinit(context.allocator);
                try context.consumeA(.close_parenthesis);
                break :blk try Factor.expression(context.allocator, inner);
            },
            .identifier => blk: {
                const identifier = try Identifier.parse(context);
                errdefer identifier.deinit(context.allocator);
                break :blk try Factor.variable(context.allocator, identifier, identifier.position);
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
        };
        errdefer result.deinit(context.allocator);

        while (context.nextIsOneOf(&.{ .increment, .decrement })) {
            const operator = try UnaryOperator.parse(context);
            result = try Factor.unary(
                context.allocator,
                operator,
                true,
                result,
                Position.extract(result),
            );
        }

        return result;
    }
    pub fn constant(allocator: std.mem.Allocator, value: i32, position: Position) !*Factor {
        const result = try allocator.create(Factor);
        result.* = Factor{ .constant = Constant{ .value = value, .position = position } };
        return result;
    }
    pub fn unary(
        allocator: std.mem.Allocator,
        operator: UnaryOperator,
        is_postfix: bool,
        inner: *Factor,
        position: Position,
    ) !*Factor {
        const result = try allocator.create(Factor);
        result.* = Factor{
            .unary = Unary{
                .operator = operator,
                .factor = inner,
                .is_postfix = is_postfix,
                .position = position,
            },
        };
        return result;
    }
    pub fn variable(allocator: std.mem.Allocator, name: Identifier, position: Position) !*Factor {
        const result = try allocator.create(Factor);
        result.* = Factor{
            .variable = Variable{
                .identifier = name,
                .position = position,
            },
        };
        return result;
    }
    pub fn expression(allocator: std.mem.Allocator, inner: *Expression) !*Factor {
        const result = try allocator.create(Factor);
        result.* = Factor{ .expression = inner };
        return result;
    }
    pub fn toOwned(self: Factor, allocator: std.mem.Allocator) !*Factor {
        const result = try allocator.create(Factor);
        result.* = self;
        return result;
    }
    pub fn copy(self: *Factor, allocator: std.mem.Allocator) error{OutOfMemory}!*Factor {
        switch (self.*) {
            .unary => return Factor.unary(
                allocator,
                self.unary.operator,
                self.unary.is_postfix,
                try self.unary.factor.copy(allocator),
                self.unary.position,
            ),
            .expression => return Factor.expression(
                allocator,
                try self.expression.copy(allocator),
            ),
            .variable => return Factor.variable(
                allocator,
                try self.variable.identifier.copy(allocator),
                self.variable.position,
            ),
            .constant => return Factor.constant(
                allocator,
                self.constant.value,
                self.constant.position,
            ),
        }
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

const Conditional = struct {
    condition: *Expression,
    then: *Expression,
    @"else": *Expression,
    position: Position,
    pub fn deinit(self: Conditional, allocator: std.mem.Allocator) void {
        self.condition.deinit(allocator);
        self.then.deinit(allocator);
        self.@"else".deinit(allocator);
    }
};

const Variable = struct {
    identifier: Identifier,
    position: Position,
    pub fn parse(context: *ParserContext) !Variable {
        const identifier = try Identifier.parse(context);
        return Variable{
            .identifier = identifier,
            .position = identifier.position,
        };
    }
    pub fn deinit(self: Variable, allocator: std.mem.Allocator) void {
        self.identifier.deinit(allocator);
    }
    pub fn copy(self: Variable, allocator: std.mem.Allocator) !Variable {
        return Variable{ .identifier = try self.identifier.copy(allocator) };
    }
};

const Constant = struct {
    value: i32,
    position: Position,
    pub fn parse(context: *ParserContext) !Constant {
        const constant = try context.getA(.constant);
        const as_int = std.fmt.parseInt(i32, constant.bytes, 10) catch {
            return parserError(ParserError.InvalidConstant, constant.line, constant.character, "'{s}' is not a valid constant", .{constant.bytes});
        };
        return Constant{
            .value = as_int,
            .position = Position.extract(constant),
        };
    }
};

const Unary = struct {
    is_postfix: bool,
    operator: UnaryOperator,
    factor: *Factor,
    position: Position,
    pub fn parse(context: *ParserContext) !Unary {
        const position = Position.extract(try context.peek());
        const operator = try UnaryOperator.parse(context);
        return Unary{
            .operator = operator,
            .factor = try Factor.parse(context),
            .is_postfix = false,
            .position = position,
        };
    }
    pub fn deinit(self: Unary, allocator: std.mem.Allocator) void {
        self.factor.deinit(allocator);
    }
};

const Binary = struct {
    operator: BinaryOperator,
    left: *Expression,
    right: *Expression,
    position: Position,
    pub fn canShortCircuit(self: Binary) bool {
        return self.operator.operator == .logical_or or self.operator.operator == .logical_and;
    }
    pub fn deinit(self: Binary, allocator: std.mem.Allocator) void {
        self.left.deinit(allocator);
        self.right.deinit(allocator);
    }
};

pub const Declaration = struct {
    identifier: Identifier,
    expression: ?*Expression,
    position: Position,
    pub fn parse(context: *ParserContext) !Declaration {
        const int = try context.getA(.int);
        const identifier = try Identifier.parse(context);
        errdefer identifier.deinit(context.allocator);
        const next = try context.getOneOf(&.{ .equal_sign, .semicolon });
        switch (next.kind) {
            .equal_sign => {
                const expression = try Expression.parse(context, 0);
                errdefer expression.deinit(context.allocator);
                try context.consumeA(.semicolon);
                return Declaration{ .identifier = identifier, .expression = expression, .position = Position.extract(int) };
            },
            .semicolon => {
                return Declaration{ .identifier = identifier, .expression = null, .position = Position.extract(int) };
            },
            else => unreachable,
        }
    }
    pub fn deinit(self: Declaration, allocator: std.mem.Allocator) void {
        self.identifier.deinit(allocator);
        if (self.expression) |exp| exp.deinit(allocator);
    }
};

const Assignment = struct {
    left: *Expression,
    right: *Expression,
    position: Position,
    pub fn deinit(self: Assignment, allocator: std.mem.Allocator) void {
        self.left.deinit(allocator);
        self.right.deinit(allocator);
    }
};

pub fn parserError(
    err: ParserError,
    line: usize,
    character: usize,
    comptime format: []const u8,
    args: anytype,
) ParserError {
    std.log.err("{s} [{d}:{d}]: " ++ format, .{ @errorName(err), line, character } ++ args);
    return err;
}

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
