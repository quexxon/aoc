const std = @import("std");
const fs = std.fs;
const process = std.process;
const ArrayList = std.ArrayList;
const Timer = std.time.Timer;

var a: *std.mem.Allocator = undefined;

const Point = struct {
    x: u16,
    y: u16,

    pub fn init(x: u16, y: u16) Point {
        return Point{
            .x = x,
            .y = y,
        };
    }

    pub fn parse(str: []const u8) !Point {
        var itr = std.mem.split(u8, str, ",");
        const x = try std.fmt.parseInt(u16, itr.next().?, 10);
        const y = try std.fmt.parseInt(u16, itr.next().?, 10);
        return Point.init(x, y);
    }
};

const Line = struct {
    start: Point,
    end: Point,

    pub fn init(start: Point, end: Point) Line {
        return Line{
            .start = start,
            .end = end,
        };
    }

    pub fn parse(str: []const u8) !Line {
        var itr = std.mem.split(u8, str, " -> ");
        const start = try Point.parse(itr.next().?);
        const end = try Point.parse(itr.next().?);
        return Line.init(start, end);
    }
};

fn part1(input: *ArrayList(Line)) !u32 {
    var visited = std.AutoHashMap(Point, u32).init(a);
    defer visited.deinit();
    var point: Point = undefined;
    var count: u32 = undefined;
    for (input.items) |line| {
        if (line.start.x == line.end.x or line.start.y == line.end.y) {
            var x = line.start.x;
            var y = line.start.y;
            while (x != line.end.x or y != line.end.y) {
                point = Point.init(x, y);
                count = visited.get(point) orelse 0;
                try visited.put(point, count + 1);
                if (x < line.end.x) x += 1 else if (x > line.end.x) x -= 1;
                if (y < line.end.y) y += 1 else if (y > line.end.y) y -= 1;
            }
            point = Point.init(x, y);
            count = visited.get(point) orelse 0;
            try visited.put(point, count + 1);
        }
    }

    var sum: u32 = 0;
    var itr = visited.iterator();
    while (itr.next()) |entry| {
        if (entry.value_ptr.* > 1) sum += 1;
    }

    return sum;
}

fn part2(input: *ArrayList(Line)) !u32 {
    var visited = std.AutoHashMap(Point, u32).init(a);
    defer visited.deinit();
    var point: Point = undefined;
    var count: u32 = undefined;
    for (input.items) |line| {
        var x = line.start.x;
        var y = line.start.y;
        while (x != line.end.x or y != line.end.y) {
            point = Point.init(x, y);
            count = visited.get(point) orelse 0;
            try visited.put(point, count + 1);
            if (x < line.end.x) x += 1 else if (x > line.end.x) x -= 1;
            if (y < line.end.y) y += 1 else if (y > line.end.y) y -= 1;
        }
        point = Point.init(x, y);
        count = visited.get(point) orelse 0;
        try visited.put(point, count + 1);
    }

    var sum: u32 = 0;
    var itr = visited.iterator();
    while (itr.next()) |entry| {
        if (entry.value_ptr.* > 1) sum += 1;
    }

    return sum;
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
    var input = ArrayList(Line).init(a);
    defer input.deinit();
    while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        try input.append(try Line.parse(line));
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
