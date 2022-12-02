import * as Path from "https://deno.land/std@0.116.0/path/mod.ts";
import { exec, sleep } from "./util.ts";

function part1(input: number[][]): number {
  const entries = input[0].sort((x, y) => x - y);
  const median = entries[Math.floor(entries.length / 2)];
  return entries.reduce((sum, n) => sum + Math.abs(median - n), 0);
}

function part2(input: number[][]): number {
  const entries = input[0].sort((x, y) => x - y);
  const median = entries[Math.floor(entries.length / 2)];
  const mean = Math.round(entries.reduce((x, y) => x + y) / entries.length);
  const candidates = [
    ...new Set(
      entries.filter((n) => Math.abs(mean - n) < Math.abs(median - mean)),
    ),
  ];
  return Math.min(
    ...candidates.map((target) =>
      entries.reduce((sum, n) => {
        const distance = Math.abs(target - n);
        return sum + ((distance * (1 + distance)) / 2);
      }, 0)
    ),
  );
}

if (Deno.args.length !== 1) {
  throw new Error("Usage: day7 INPUT");
}

sleep(0).then(() => {
  return exec({
    inputPath: Path.resolve(Deno.args[0]),
    parseLine: (line) => line.split(",").map((n) => parseInt(n)),
    part1,
    part2,
  });
}).catch((err) => console.error(err));
