const std = @import("std");

const parseOptions = @import("./cli/options.zig").parseOptions;

const lex = @import("./lexer.zig").lexer;
const ast = @import("./parser.zig").ast;
const astPrint = @import("./parser.zig").printProgram;
const x86 = @import("./codegen.zig").x86;
const emit_x86_program = @import("codegen.zig").emitX86Program;
const tacky = @import("./tacky.zig").tacky;
const tackyPrint = @import("./tacky.zig").printProgram;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    try runCompiler(allocator);

    if (gpa.detectLeaks()) {
        return error.LeakyCode;
    }
}

pub fn runCompiler(allocator: std.mem.Allocator) !void {
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    const options = try parseOptions(args);

    const file = try std.fs.cwd().openFile(options.input_file, .{});
    defer file.close();

    const bytes = try file.readToEndAlloc(allocator, std.math.maxInt(u32));
    defer allocator.free(bytes);

    const lex_result = try lex.bytesToTokens(allocator, bytes);
    defer lex_result.deinit();

    if (options.stage == .lex) return;

    const ast_program = try ast.tokensToProgram(allocator, lex_result.tokens);
    defer ast_program.deinit();

    if (options.print_ast) {
        try astPrint(ast_program, std.io.getStdOut().writer());
    }

    if (options.stage == .parse) return;

    const tacky_program = try tacky.programToTacky(allocator, ast_program);
    defer tacky_program.deinit();

    if (options.print_tacky) {
        try tackyPrint(tacky_program, std.io.getStdOut().writer());
    }

    if (options.stage == .tacky) return;

    const x86_program = try x86.programToX86(allocator, tacky_program);
    defer x86_program.deinit();

    if (options.stage == .codegen) return;

    const output_file = try std.fs.cwd().createFile(options.output_file.?, .{});
    defer output_file.close();

    try emit_x86_program(x86_program, output_file.writer());
}
