pub const ast = @import("./parser/ast.zig");
pub const print_program = @import("./parser/printer.zig").print_program;

test "ast.tokens_to_program" {
    const std = @import("std");
    const lexer = @import("lexer.zig").lexer;

    const allocator = std.testing.allocator;
    const code = try allocator.dupe(u8, "int main(void) {\n\treturn 2;\n}");
    defer allocator.free(code);
    const result = try lexer.bytes_to_tokens(allocator, code);
    defer result.deinit();
    _ = try ast.tokens_to_program(result.tokens);
}
