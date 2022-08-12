const std = @import("std");
const builtin = @import("builtin");
const Builder = std.build.Builder;

pub fn build(b: *Builder) !void {
    const target = b.standardTargetOptions(.{});
    const mode = b.standardReleaseOptions();

    const exe = b.addExecutable("zig_truetype", "src/main.zig");
    exe.setTarget(target);
    exe.setBuildMode(mode);
    exe.install();

    if (builtin.os.tag != .windows) {
        exe.addIncludeDir("/usr/local/include");
    }

    exe.linkSystemLibrary("SDL2");
    exe.addPackage(.{
        .name = "gl",
        .path = std.build.FileSource.relative("lib/gl_4v0.zig")
    });

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}

