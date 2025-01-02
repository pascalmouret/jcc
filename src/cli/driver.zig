const std = @import("std");
const parseOptions = @import("./options.zig").parseOptions;
const Stage = @import("./options.zig").Stage;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    const options = try parseOptions(args);

    const i_file = try replaceEnding(allocator, options.input_file, "i");
    defer allocator.free(i_file);
    try runPreprocessor(allocator, options.input_file, i_file);

    const s_file = try replaceEnding(allocator, i_file, "s");
    defer allocator.free(s_file);
    try runCompiler(allocator, i_file, s_file, options.stage, options.print_tokens, options.print_ast, options.print_tacky);

    try std.fs.cwd().deleteFile(i_file);

    if (options.stage == .full) {
        const b_file = options.output_file orelse try removeEnding(s_file);
        try runLinker(allocator, s_file, @constCast(b_file));
        if (!options.preserve_asm) {
            try std.fs.cwd().deleteFile(s_file);
        }
    }
}

fn runPreprocessor(
    allocator: std.mem.Allocator,
    input_path: []const u8,
    output_path: []const u8,
) !void {
    const command = try std.fmt.allocPrintZ(allocator, "gcc -E -P {s} -o {s}", .{ input_path, output_path });
    defer allocator.free(command);

    runCommand(allocator, command) catch return error.PreprocessingFailed;
}

// TODO: clean this mess up
fn runCompiler(
    allocator: std.mem.Allocator,
    input_path: []const u8,
    output_path: []const u8,
    stage: Stage,
    print_tokens: bool,
    print_ast: bool,
    print_tacky: bool,
) !void {
    var flags = std.ArrayList(u8).init(allocator);
    defer flags.deinit();

    if (print_ast) {
        try flags.appendSlice("--print-ast");
    }

    if (print_tacky) {
        if (flags.items.len > 0) {
            try flags.appendSlice(" ");
        }
        try flags.appendSlice("--print-tacky");
    }

    if (print_tokens) {
        if (flags.items.len > 0) {
            try flags.appendSlice(" ");
        }
        try flags.appendSlice("--print-tokens");
    }

    const flags_bfr = try flags.toOwnedSlice();
    defer allocator.free(flags_bfr);

    const command = try std.fmt.allocPrintZ(allocator, "./zig-out/bin/jcc {s} --{s} -o {s} {s}", .{ input_path, @tagName(stage), output_path, flags_bfr });
    defer allocator.free(command);

    runCommand(allocator, command) catch return error.CompilationFailed;
}

fn runLinker(
    allocator: std.mem.Allocator,
    input_path: []const u8,
    output_path: []const u8,
) !void {
    const command = try std.fmt.allocPrintZ(allocator, "gcc {s} -o {s}", .{ input_path, output_path });
    defer allocator.free(command);

    runCommand(allocator, command) catch return error.LinkingFailed;
}

fn runCommand(allocator: std.mem.Allocator, command: []u8) !void {
    var args = std.ArrayList([:0]u8).init(allocator);
    defer args.deinit();

    var iter = std.mem.split(u8, command, " ");
    while (iter.next()) |arg| {
        try args.append(try allocator.dupeZ(u8, arg));
    }
    const args_z = try args.toOwnedSlice();

    defer allocator.free(args_z);
    defer for (args_z) |arg| allocator.free(arg);

    const result = try std.process.Child.run(.{ .allocator = allocator, .argv = args_z });
    defer allocator.free(result.stderr);
    defer allocator.free(result.stdout);

    try std.io.getStdOut().writeAll(result.stdout);
    try std.io.getStdErr().writeAll(result.stderr);

    switch (result.term) {
        .Exited => |code| {
            if (code != 0) {
                return error.NonZeroExit;
            }
        },
        else => {
            std.debug.print("{?}\n", .{result.term});
            return error.NotExited;
        },
    }
}

fn replaceEnding(allocator: std.mem.Allocator, path: []const u8, ending: []const u8) ![]u8 {
    var position = path.len - 1;
    while (path[position] != '.') {
        position -= 1;

        if (path[position] == '/' or position == 0) {
            return error.NoFileEnding;
        }
    }

    const result_size = (path.len - (path.len - position)) + ending.len + 1;
    const result = try allocator.alloc(u8, result_size);

    @memcpy(result[0 .. position + 1], path[0 .. position + 1]);
    @memcpy(result[position + 1 ..], ending);

    return result;
}

fn removeEnding(path: []const u8) ![]const u8 {
    var position = path.len - 1;
    while (path[position] != '.') {
        position -= 1;

        if (path[position] == '/' or position == 0) {
            return error.NoFileEnding;
        }
    }

    return path[0..position];
}

test replaceEnding {
    const allocator = std.testing.allocator;

    const result1 = try replaceEnding(allocator, "test.c", "i");
    defer allocator.free(result1);
    try std.testing.expectEqualDeep(
        "test.i",
        result1,
    );

    const result2 = try replaceEnding(allocator, "test.c", "asd");
    defer allocator.free(result2);
    try std.testing.expectEqualDeep(
        "test.asd",
        result2,
    );

    const result3 = try replaceEnding(allocator, "./../../test.kbd", "c");
    defer allocator.free(result3);
    try std.testing.expectEqualDeep(
        "./../../test.c",
        result3,
    );

    const result4 = replaceEnding(allocator, "./../program", "iso");
    try std.testing.expectError(
        error.NoFileEnding,
        result4,
    );

    const result5 = replaceEnding(allocator, "program", "iso");
    try std.testing.expectError(
        error.NoFileEnding,
        result5,
    );
}
