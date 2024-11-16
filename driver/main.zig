const std = @import("std");

const PP_COMMAND = "gcc -E -P {} -o {}";
const ASSEMBLY_COMMAND = "gcc {} -o {}";

const Stage = enum {
    lex,
    parse,
    codgen,
    full,
};

const Options = struct {
    file: []u8,
    stage: Stage,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    const options = try parse_options(args);

    const i_file = try replace_ending(allocator, options.file, "i");
    defer allocator.free(i_file);
    try run_preprocessor(allocator, options.file, i_file);

    const s_file = try replace_ending(allocator, i_file, "s");
    defer allocator.free(s_file);
    try run_compiler(allocator, i_file, s_file, options.stage);

    try std.fs.cwd().deleteFile(i_file);

    if (options.stage == .full) {
        const b_file = try remove_ending(s_file);
        try run_linker(allocator, s_file, @constCast(b_file));
    }

    try std.fs.cwd().deleteFile(s_file);
}

fn run_preprocessor(
    allocator: std.mem.Allocator,
    input_path: []u8,
    output_path: []u8,
) !void {
    const args = [_][:0]u8{
        try allocator.dupeZ(u8, "gcc"),
        try allocator.dupeZ(u8, "-E"),
        try allocator.dupeZ(u8, "-P"),
        try allocator.dupeZ(u8, input_path),
        try allocator.dupeZ(u8, "-o"),
        try allocator.dupeZ(u8, output_path),
    };
    defer for (args) |arg| allocator.free(arg);
    _ = std.process.Child.run(.{ .allocator = allocator, .argv = &args }) catch return error.PreprocessingFailed;
}

fn run_compiler(
    allocator: std.mem.Allocator,
    input_path: []u8,
    output_path: []u8,
    stage: Stage,
) !void {
    std.debug.print("{?}", .{stage});
    const args = [_][:0]u8{
        try allocator.dupeZ(u8, "gcc"),
        try allocator.dupeZ(u8, "-S"),
        try allocator.dupeZ(u8, input_path),
        try allocator.dupeZ(u8, "-o"),
        try allocator.dupeZ(u8, output_path),
    };
    defer for (args) |arg| allocator.free(arg);
    _ = std.process.Child.run(.{ .allocator = allocator, .argv = &args }) catch return error.CompilationFailed;
}

fn run_linker(
    allocator: std.mem.Allocator,
    input_path: []u8,
    output_path: []u8,
) !void {
    const args = [_][:0]u8{
        try allocator.dupeZ(u8, "gcc"),
        try allocator.dupeZ(u8, input_path),
        try allocator.dupeZ(u8, "-o"),
        try allocator.dupeZ(u8, output_path),
    };
    defer for (args) |arg| allocator.free(arg);

    _ = std.process.Child.run(.{ .allocator = allocator, .argv = &args }) catch return error.LinkingFailed;
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

fn replace_ending(allocator: std.mem.Allocator, path: []const u8, ending: []const u8) ![]u8 {
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

fn remove_ending(path: []const u8) ![]const u8 {
    var position = path.len - 1;
    while (path[position] != '.') {
        position -= 1;

        if (path[position] == '/' or position == 0) {
            return error.NoFileEnding;
        }
    }

    return path[0..position];
}

test replace_ending {
    const allocator = std.testing.allocator;

    const result1 = try replace_ending(allocator, "test.c", "i");
    defer allocator.free(result1);
    try std.testing.expectEqualDeep(
        "test.i",
        result1,
    );

    const result2 = try replace_ending(allocator, "test.c", "asd");
    defer allocator.free(result2);
    try std.testing.expectEqualDeep(
        "test.asd",
        result2,
    );

    const result3 = try replace_ending(allocator, "./../../test.kbd", "c");
    defer allocator.free(result3);
    try std.testing.expectEqualDeep(
        "./../../test.c",
        result3,
    );

    const result4 = replace_ending(allocator, "./../program", "iso");
    try std.testing.expectError(
        error.NoFileEnding,
        result4,
    );

    const result5 = replace_ending(allocator, "program", "iso");
    try std.testing.expectError(
        error.NoFileEnding,
        result5,
    );
}

test parse_options {
    const allocator = std.testing.allocator;

    const opts1 = [_][:0]u8{
        try allocator.dupeZ(u8, "./driver"),
        try allocator.dupeZ(u8, "../test.c"),
        try allocator.dupeZ(u8, "--lex"),
    };
    defer for (opts1) |opt| allocator.free(opt);
    try std.testing.expectEqualDeep(
        Options{ .file = "../test.c", .stage = .lex },
        try parse_options(&opts1),
    );

    const opts2 = [_][:0]u8{
        try allocator.dupeZ(u8, "./driver"),
        try allocator.dupeZ(u8, "../test.c"),
    };
    defer for (opts2) |opt| allocator.free(opt);
    try std.testing.expectEqualDeep(
        Options{ .file = "../test.c", .stage = .full },
        try parse_options(&opts2),
    );

    const opts3 = [_][:0]u8{
        try allocator.dupeZ(u8, "./driver"),
        try allocator.dupeZ(u8, "--parse"),
    };
    defer for (opts3) |opt| allocator.free(opt);
    try std.testing.expectError(
        error.MissingFilePath,
        parse_options(&opts3),
    );
}
