using System.Diagnostics;

if (args.Length == 0)
{
    throw new Exception("USAGE: aoc INPUT_DIR");
}

string inputDir = args[0];

Stopwatch stopwatch = new Stopwatch();
Day01 day01 = new Day01(Path.Join(inputDir, "01.txt"));
stopwatch.Start();
uint result = day01.Part1();
stopwatch.Stop();
Console.WriteLine(
    "Day 1:\n  Part 1:\n    Result: {0}\n    Elapsed: {1}ms",
    result,
    stopwatch.Elapsed.TotalMilliseconds
);
stopwatch.Start();
result = day01.Part2();
stopwatch.Stop();
Console.WriteLine(
    "  Part 2:\n    Result: {0}\n    Elapsed: {1}ms\n",
    result,
    stopwatch.Elapsed.TotalMilliseconds
);
