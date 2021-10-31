const std = @import("std");

const arviss_path = "src/arviss/";

pub fn build(b: *std.build.Builder) void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const mode = b.standardReleaseOptions();

    const arviss = buildArviss(b, target, mode);

    const exe = b.addExecutable("arviss", "src/main.zig");
    exe.addPackagePath("arviss", arviss_path ++ "arviss.zig");
    exe.addIncludeDir(arviss_path);
    exe.linkLibrary(arviss);
    exe.setTarget(target);
    exe.setBuildMode(mode);
    exe.install();

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}

fn buildArviss(b: *std.build.Builder, target: std.zig.CrossTarget, mode: std.builtin.Mode) *std.build.LibExeObjStep {
    const lib = b.addStaticLibrary("arviss", null);
    lib.setTarget(target);
    lib.setBuildMode(mode);

    lib.addCSourceFile(arviss_path ++ "arviss.c", &[_][]const u8{});
    lib.addCSourceFile(arviss_path ++ "loadelf.c", &[_][]const u8{});
    lib.linkLibC();

    return lib;
}
