const std = @import("std");
const lex = @import("./lexer.zig").lexer;
const ast = @import("./parser.zig").ast;
const x86 = @import("./codegen.zig").x86;
const tacky = @import("./tacky.zig").tacky;

const Options = struct {
    input_file: []u8,
    output_file: []u8,
    stage: Stage,
};

const Stage = enum {
    lex,
    parse,
    tacky,
    codegen,
    full,
};

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

    if (options.stage == .parse) return;

    const tacky_program = try tacky.program_to_tacky(allocator, ast_program);
    defer tacky_program.deinit();

    if (options.stage == .tacky) return;

    // const x86_ast = try x86.program_to_x86(allocator, program);
    // defer x86_ast.deinit();
    //
    // if (options.stage == .codegen) return;
    //
    // const output_file = try std.fs.cwd().createFile(options.output_file, .{});
    // defer output_file.close();
    //
    // try x86_ast.write(output_file.writer());
}

fn parse_options(opts: []const [:0]u8) !Options {
    var file: ?[:0]const u8 = null;
    var stage: Stage = .full;
    var output_file: ?[:0]const u8 = null;

    var index: usize = 1;
    while (index < opts.len) : (index += 1) {
        const opt = opts[index];

        if (opt.len == 2 and std.mem.eql(u8, opt[0..2], "-o")) {
            index += 1;
            output_file = opts[index];
        } else if (opt.len >= 2 and std.mem.eql(u8, opt[0..2], "--")) {
            stage = std.meta.stringToEnum(Stage, opt[2..]) orelse return error.UnknownArgument;
        } else {
            file = opt;
        }
    }

    if (file) |path| {
        return Options{
            .input_file = @constCast(path),
            .output_file = @constCast(output_file orelse return error.MissingOutputPath),
            .stage = stage,
        };
    } else {
        return error.MissingFilePath;
    }
}
