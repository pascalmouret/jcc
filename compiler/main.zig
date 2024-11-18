const std = @import("std");
const lex = @import("./lexer.zig").lexer;
const ast = @import("./parser.zig").ast;

const Options = struct {
    file: []u8,
    stage: Stage,
};

const Stage = enum {
    lex,
    parse,
    full,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    const options = try parse_options(args);

    const file = try std.fs.cwd().openFile(options.file, .{});
    defer file.close();

    const bytes = try file.readToEndAlloc(allocator, std.math.maxInt(u32));
    defer allocator.free(bytes);

    const lex_result = try lex.bytes_to_tokens(allocator, bytes);
    defer lex_result.deinit();

    if (options.stage == .lex) return;

    _ = try ast.tokens_to_program(lex_result.tokens);

    if (options.stage == .parse) return;
}

fn parse_options(opts: []const [:0]u8) !Options {
    var file: ?[:0]const u8 = null;
    var stage: Stage = .full;

    for (opts[1..]) |opt| {
        if (opt.len >= 2 and std.mem.eql(u8, opt[0..2], "--")) {
            stage = std.meta.stringToEnum(Stage, opt[2..]) orelse return error.UnknownArgument;
        } else {
            file = opt;
        }
    }

    if (file) |path| {
        return Options{ .file = @constCast(path), .stage = stage };
    } else {
        return error.MissingFilePath;
    }
}
