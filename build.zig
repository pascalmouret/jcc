const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const compiler = b.addExecutable(.{
        .name = "jcc",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    b.installArtifact(compiler);

    const driver = b.addExecutable(.{
        .name = "jcc-driver",
        .root_source_file = b.path("src/cli/driver.zig"),
        .target = target,
        .optimize = optimize,
    });

    b.installArtifact(driver);
}
