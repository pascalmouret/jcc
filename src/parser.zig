pub const ast = @import("./parser/ast.zig");
pub const printProgram = @import("./parser/printer.zig").printProgram;

test "ast.tokens_to_program" {
    const std = @import("std");
    const lexer = @import("lexer.zig").lexer;

    const allocator = std.testing.allocator;
    const code = try allocator.dupe(u8, "int main(void) {\n\treturn 2;\n}");
    defer allocator.free(code);
    const result = try lexer.bytesToTokens(allocator, code);
    defer result.deinit();
    _ = try ast.tokensToProgram(result.tokens);
}
