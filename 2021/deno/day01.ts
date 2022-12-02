import * as Path from "https://deno.land/std@0.116.0/path/mod.ts";
import { exec } from "./util.ts";

const scriptDir = Path.dirname(Path.fromFileUrl(import.meta.url));
const inputPath = Path.join(scriptDir, "../inputs/01.txt");

function part1(input: number[]): number {
  let descent = 0;
  let previousDepth = input[0];

  for (const depth of input.slice(1)) {
    if (depth > previousDepth) descent++;
    previousDepth = depth;
  }

  return descent;
}

function part2(input: number[]): number {
  let descent = 0;
  const previousDepths: number[] = [];

  for (const [index, depth] of input.slice(0, 3).entries()) {
    previousDepths[index] = depth;
  }

  for (const depth of input.slice(3)) {
    const shared = previousDepths[2] + previousDepths[1];
    const prevGroup = shared + previousDepths[0];
    const group = shared + depth;
    if (group > prevGroup) descent++;
    previousDepths[0] = previousDepths[1];
    previousDepths[1] = previousDepths[2];
    previousDepths[2] = depth;
  }

  return descent;
}

exec({
  inputPath,
  parseLine: (line: string) => parseInt(line),
  part1,
  part2,
}).catch((err) => console.error(err));
