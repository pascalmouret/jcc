const std = @import("std");

const parse_options = @import("./cli/options.zig").parse_options;

const lex = @import("./lexer.zig").lexer;
const ast = @import("./parser.zig").ast;
const ast_print = @import("./parser.zig").print_program;
const x86 = @import("./codegen.zig").x86;
const emit_x86_program = @import("codegen.zig").emit_x86_program;
const tacky = @import("./tacky.zig").tacky;
const tacky_print = @import("./tacky.zig").print_program;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    try run_compiler(allocator);

    if (gpa.detectLeaks()) {
        return error.LeakyCode;
    }
}

pub fn run_compiler(allocator: std.mem.Allocator) !void {
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    const options = try parse_options(args);

    const file = try std.fs.cwd().openFile(options.input_file, .{});
    defer file.close();

    const bytes = try file.readToEndAlloc(allocator, std.math.maxInt(u32));
    defer allocator.free(bytes);

    const lex_result = try lex.bytes_to_tokens(allocator, bytes);
    defer lex_result.deinit();

    if (options.stage == .lex) return;

    const ast_program = try ast.tokens_to_program(allocator, lex_result.tokens);
    defer ast_program.deinit();

    if (options.print_ast) {
        try ast_print(ast_program, std.io.getStdOut().writer());
    }

    if (options.stage == .parse) return;

    const tacky_program = try tacky.program_to_tacky(allocator, ast_program);
    defer tacky_program.deinit();

    if (options.print_tacky) {
        try tacky_print(tacky_program, std.io.getStdOut().writer());
    }

    if (options.stage == .tacky) return;

    const x86_program = try x86.program_to_x86(allocator, tacky_program);
    defer x86_program.deinit();

    if (options.stage == .codegen) return;

    const output_file = try std.fs.cwd().createFile(options.output_file.?, .{});
    defer output_file.close();

    try emit_x86_program(x86_program, output_file.writer());
}
