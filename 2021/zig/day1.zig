const std = @import("std");
const fs = std.fs;
const process = std.process;
const ArrayList = std.ArrayList;
const Timer = std.time.Timer;

var a: *std.mem.Allocator = undefined;

fn part1(input: *ArrayList(u32)) !u32 {
    var descent: u32 = 0;
    var prev_depth = input.items[0];

    for (input.items[1..]) |depth| {
        if (depth > prev_depth) descent += 1;
        prev_depth = depth;
    }

    return descent;
}

fn part2(input: *ArrayList(u32)) !u32 {
    var descent: u32 = 0;
    var prev_depths: [3]u32 = undefined;

    for (input.items[0..3]) |depth, index| {
        prev_depths[index] = depth;
    }

    for (input.items[3..]) |depth| {
        const shared: u32 = prev_depths[2] + prev_depths[1];
        const prev_group: u32 = shared + prev_depths[0];
        const group: u32 = shared + depth;
        if (group > prev_group) descent += 1;
        prev_depths[0] = prev_depths[1];
        prev_depths[1] = prev_depths[2];
        prev_depths[2] = depth;
    }

    return descent;
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    a = &arena.allocator;

    var args = process.args();
    _ = args.skip(); // Skip executable name

    const input_path_rel = try (args.next(a) orelse {
        std.debug.print("USAGE: day1 INPUT\n", .{});
        return error.InvalidArgs;
    });
    const input_path = try fs.path.resolve(a, &[_][]const u8{input_path_rel});

    const file = try fs.openFileAbsolute(input_path, .{});
    defer file.close();
    const reader = file.reader();

    var buf: [1024]u8 = undefined;
    var input = ArrayList(u32).init(a);
    defer input.deinit();
    while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        try input.append(try std.fmt.parseInt(u32, line, 10));
    }

    var timer = try Timer.start();
    const answer1 = try part1(&input);
    const part1_time = timer.lap();
    const answer2 = try part2(&input);
    const part2_time = timer.lap();

    std.debug.print("Part 1:\n", .{});
    std.debug.print("  Result   = {d}\n", .{answer1});
    std.debug.print("  Duration = {d}ms\n", .{@intToFloat(f64, part1_time) / 1e6});
    std.debug.print("Part 2:\n", .{});
    std.debug.print("  Result   = {d}\n", .{answer2});
    std.debug.print("  Duration = {d}ms\n", .{@intToFloat(f64, part2_time) / 1e6});
}
