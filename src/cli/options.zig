const std = @import("std");

pub const Options = struct {
    input_file: []const u8,
    output_file: ?[]const u8,
    stage: Stage,
    print_ast: bool,
    print_tacky: bool,
    preserve_asm: bool,
};

pub const Stage = enum {
    lex,
    parse,
    tacky,
    codegen,
    full,
};

pub fn parseOptions(opts: []const [:0]u8) !Options {
    var file: ?[:0]const u8 = null;
    var stage: Stage = .full;
    var output_file: ?[:0]const u8 = null;
    var print_ast = false;
    var print_tacky = false;
    var preserve_asm = false;

    var index: usize = 1;
    while (index < opts.len) : (index += 1) {
        const opt = opts[index];

        // filter out noise
        if (opt.len == 0) {
            continue;
        }

        if (opt.len == 2) {
            if (std.mem.eql(u8, opt, "-o")) {
                index += 1;
                output_file = opts[index];
            } else if (std.mem.eql(u8, opt, "-S")) {
                preserve_asm = true;
            } else {
                return error.UnknownArgument;
            }
        } else if (opt.len >= 2 and std.mem.eql(u8, opt[0..2], "--")) {
            if (std.mem.eql(u8, opt, "--print-ast")) {
                print_ast = true;
            } else if (std.mem.eql(u8, opt, "--print-tacky")) {
                print_tacky = true;
            } else {
                stage = std.meta.stringToEnum(Stage, opt[2..]) orelse return error.UnknownArgument;
            }
        } else {
            file = opt;
        }
    }

    if (file) |path| {
        return Options{
            .input_file = path,
            .output_file = output_file,
            .stage = stage,
            .print_ast = print_ast,
            .print_tacky = print_tacky,
            .preserve_asm = preserve_asm,
        };
    } else {
        return error.MissingFilePath;
    }
}
