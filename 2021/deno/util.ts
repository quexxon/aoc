import { readLines } from "https://deno.land/std@0.116.0/io/mod.ts";

const PART_1 = "Part 1";
const PART_2 = "Part 2";

export async function exec<T, U, V>({
  inputPath,
  parseLine,
  part1,
  part2,
}: {
  inputPath: string;
  parseLine: (line: string) => PromiseLike<T> | T;
  part1: (input: T[]) => Promise<U> | U;
  part2: (input: T[]) => Promise<V> | V;
}) {
  const input = [];
  const file = await Deno.open(inputPath);
  for await (const line of readLines(file)) {
    input.push(await parseLine(line));
  }
  Deno.close(file.rid);
  const results = new Map();
  performance.mark(PART_1);
  results.set(PART_1, await part1(input));
  performance.measure(PART_1, PART_1);
  performance.mark(PART_2);
  results.set(PART_2, await part2(input));
  performance.measure(PART_2, PART_2);
  const measures = performance.getEntriesByType("measure");
  for (const measure of measures) {
    console.log(measure.name + ":");
    console.log(`  Result   = ${results.get(measure.name)}`);
    console.log(`  Duration = ${measure.duration}ms`);
  }
}

export async function forEachLine(
  filePath: string,
  fn: (line: string) => void,
) {
  const file = await Deno.open(filePath);
  for await (const line of readLines(file)) {
    fn(line);
  }
  Deno.close(file.rid);
}

export function sleep(secs: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, secs * 1000));
}
