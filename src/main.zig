const std = @import("std");

const parseOptions = @import("./cli/options.zig").parseOptions;

const lex = @import("./lexer.zig").lexer;
const ast = @import("./parser.zig").ast;
const astPrint = @import("./parser.zig").printProgram;
const codegen = @import("./codegen.zig");
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
    errdefer allocator.free(bytes);

    const lex_result = try lex.bytesToTokens(allocator, bytes);

    if (options.stage == .lex) {
        lex_result.deinit();
        return;
    }

    const ast_program = try ast.tokensToProgram(allocator, lex_result.tokens);

    lex_result.deinit();

    if (options.print_ast) {
        try astPrint(ast_program, std.io.getStdOut().writer());
    }

    if (options.stage == .parse) {
        ast_program.deinit();
        return;
    }

    const tacky_program = try tacky.programToTacky(allocator, ast_program);

    ast_program.deinit();

    if (options.print_tacky) {
        try tackyPrint(tacky_program, std.io.getStdOut().writer());
    }

    if (options.stage == .tacky) {
        tacky_program.deinit();
        return;
    }

    const assembly_program = try codegen.program.fromTacky(allocator, tacky_program);
    defer assembly_program.deinit();

    tacky_program.deinit();

    if (options.stage == .codegen) return;

    const output_file = try std.fs.cwd().createFile(options.output_file.?, .{});
    defer output_file.close();

    try codegen.emitProgram(assembly_program, output_file.writer());
}
