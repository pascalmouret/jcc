const std = @import("std");

const parseOptions = @import("./cli/options.zig").parseOptions;

const lex = @import("./lexer.zig").lexer;
const ast = @import("./parser.zig").ast;
const astPrint = @import("./parser.zig").printProgram;
const codegen = @import("./codegen.zig");
const tacky = @import("./tacky.zig").tacky;
const tackyPrint = @import("./tacky.zig").printProgram;
// TODO: organise the code
const validate = @import("./validate/variable_resolution.zig").resolveVariables;

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

    const lex_result = try fileToTokens(allocator, options.input_file);

    if (options.stage == .lex) {
        lex_result.deinit();
        return;
    }

    const ast_program = try tokensToAst(allocator, lex_result, options.print_ast);

    if (options.stage == .parse) {
        ast_program.deinit();
        return;
    }

    const validated_ast = try validate(ast_program);

    if (options.stage == .validate) {
        validated_ast.deinit();
        return;
    }

    const tacky_program = try astToTacky(allocator, validated_ast, options.print_tacky);

    if (options.stage == .tacky) {
        tacky_program.deinit();
        return;
    }

    const assembly_program = try tackyToAssembly(allocator, tacky_program);
    defer assembly_program.deinit();

    if (options.stage == .codegen) return;

    const output_file = try std.fs.cwd().createFile(options.output_file.?, .{});
    defer output_file.close();

    try codegen.emitProgram(assembly_program, output_file.writer());
}

fn fileToTokens(allocator: std.mem.Allocator, path: []const u8) !lex.LexerResult {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    const bytes = try file.readToEndAlloc(allocator, std.math.maxInt(u32));
    errdefer allocator.free(bytes);

    return try lex.bytesToTokens(allocator, bytes);
}

fn tokensToAst(allocator: std.mem.Allocator, lex_result: lex.LexerResult, print_ast: bool) !ast.Program {
    defer lex_result.deinit();

    const result = try ast.tokensToProgram(allocator, lex_result.tokens);
    errdefer result.deinit();

    if (print_ast) {
        try astPrint(result, std.io.getStdOut().writer());
    }

    return result;
}

fn astToTacky(allocator: std.mem.Allocator, program: ast.Program, print_tacky: bool) !tacky.Program {
    defer program.deinit();

    const result = try tacky.programToTacky(allocator, program);
    errdefer result.deinit();

    if (print_tacky) {
        try tackyPrint(result, std.io.getStdOut().writer());
    }

    return result;
}

fn tackyToAssembly(allocator: std.mem.Allocator, program: tacky.Program) !codegen.program.Program {
    defer program.deinit();

    const result = try codegen.program.fromTacky(allocator, program);
    errdefer result.deinit();

    return result;
}
