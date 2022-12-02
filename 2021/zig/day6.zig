const std = @import("std");
const fs = std.fs;
const process = std.process;
const ArrayList = std.ArrayList;
const Timer = std.time.Timer;

var a: *std.mem.Allocator = undefined;

fn iterate(entries: *ArrayList(u8), iterations: u32) u64 {
    var itr = iterations;
    var state = [_]u64{0} ** 9;

    for (entries.items) |entry| {
        state[entry] += 1;
    }

    while (itr > 0) {
        const n: u64 = state[0];
        std.mem.rotate(u64, state[0..], 1);
        state[6] += n;
        itr -= 1;
    }

    var sum: u64 = 0;
    for (state) |n| {
        sum += n;
    }
    return sum;
}

fn part1(input: *ArrayList(u8)) u64 {
    return iterate(input, 80);
}

fn part2(input: *ArrayList(u8)) u64 {
    return iterate(input, 256);
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    a = &arena.allocator;

    var args = process.args();
    _ = args.skip(); // Skip executable name

    const input_path_rel = try (args.next(a) orelse {
        std.debug.print("USAGE: day5 INPUT\n", .{});
        return error.InvalidArgs;
    });
    const input_path = try fs.path.resolve(a, &[_][]const u8{input_path_rel});

    const file = try fs.openFileAbsolute(input_path, .{});
    defer file.close();
    const reader = file.reader();

    var buf: [1024]u8 = undefined;
    var input = ArrayList(u8).init(a);
    defer input.deinit();
    const entries = try reader.readUntilDelimiterOrEof(&buf, '\n');
    var itr = std.mem.split(u8, entries.?, ",");
    while (itr.next()) |entry| {
        try input.append(try std.fmt.parseInt(u8, entry, 10));
    }

    var timer = try Timer.start();
    const answer1 = part1(&input);
    const part1_time = timer.lap();
    const answer2 = part2(&input);
    const part2_time = timer.lap();

    std.debug.print("Part 1:\n", .{});
    std.debug.print("  Result   = {d}\n", .{answer1});
    std.debug.print("  Duration = {d}ms\n", .{@intToFloat(f64, part1_time) / 1e6});
    std.debug.print("Part 2:\n", .{});
    std.debug.print("  Result   = {d}\n", .{answer2});
    std.debug.print("  Duration = {d}ms\n", .{@intToFloat(f64, part2_time) / 1e6});
}
