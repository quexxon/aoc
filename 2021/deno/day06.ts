import * as Path from "https://deno.land/std@0.116.0/path/mod.ts";
import { exec, sleep } from "./util.ts";

function iterate(entries: number[], iterations: number): number {
  const state: number[] = [0, 0, 0, 0, 0, 0, 0, 0, 0];

  for (const entry of entries) {
    state[entry]++;
  }

  while (iterations > 0) {
    const n = state.shift();
    state.push(n as number);
    state[6] += n as number;
    iterations--;
  }

  return state.reduce((x, y) => x + y);
}

function part1(input: number[][]): number {
  return iterate(input[0], 80);
}

function part2(input: number[][]): number {
  return iterate(input[0], 256);
}

if (Deno.args.length !== 1) {
  throw new Error("Usage: day6 INPUT");
}

sleep(0).then(() => {
  return exec({
    inputPath: Path.resolve(Deno.args[0]),
    parseLine: (line) => line.split(",").map((n) => parseInt(n)),
    part1,
    part2,
  });
}).catch((err) => console.error(err));
