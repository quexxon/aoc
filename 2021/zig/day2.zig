const std = @import("std");
const fs = std.fs;
const process = std.process;
const ArrayList = std.ArrayList;
const Timer = std.time.Timer;

var a: *std.mem.Allocator = undefined;

const ParseError = error{InvalidInput};

const Direction = enum {
    forward,
    up,
    down,

    pub fn parse(str: []const u8) ParseError!Direction {
        if (std.mem.eql(u8, str, "forward")) return Direction.forward;
        if (std.mem.eql(u8, str, "up")) return Direction.up;
        if (std.mem.eql(u8, str, "down")) return Direction.down;
        return ParseError.InvalidInput;
    }
};

const Bearing = struct {
    direction: Direction,
    distance: u32,

    pub fn init(direction: Direction, distance: u32) Bearing {
        return Bearing{
            .direction = direction,
            .distance = distance,
        };
    }

    pub fn parse(str: []const u8) !Bearing {
        var itr = std.mem.split(u8, str, " ");
        const direction = try Direction.parse(itr.next().?);
        const distance = try std.fmt.parseInt(u32, itr.next().?, 10);
        return Bearing.init(direction, distance);
    }
};

fn part1(input: *ArrayList(Bearing)) u32 {
    var horiz_pos: u32 = 0;
    var depth: u32 = 0;

    for (input.items) |bearing| {
        switch (bearing.direction) {
            Direction.forward => horiz_pos += bearing.distance,
            Direction.up => depth -= bearing.distance,
            Direction.down => depth += bearing.distance,
        }
    }

    return horiz_pos * depth;
}

fn part2(input: *ArrayList(Bearing)) u32 {
    var horiz_pos: u32 = 0;
    var depth: u32 = 0;
    var aim: i32 = 0;

    for (input.items) |bearing| {
        switch (bearing.direction) {
            Direction.forward => {
                horiz_pos += bearing.distance;
                depth = @intCast(u32, @intCast(i32, depth) + @intCast(i32, bearing.distance) * aim);
            },
            Direction.up => aim -= @intCast(i32, bearing.distance),
            Direction.down => aim += @intCast(i32, bearing.distance),
        }
    }

    return horiz_pos * depth;
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
    var input = ArrayList(Bearing).init(a);
    defer input.deinit();
    while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        try input.append(try Bearing.parse(line));
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
