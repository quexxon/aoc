import * as Path from "https://deno.land/std@0.116.0/path/mod.ts";
import { exec } from "./util.ts";

const scriptDir = Path.dirname(Path.fromFileUrl(import.meta.url));
const inputPath = Path.join(scriptDir, "../inputs/03.txt");

function part1(input: string[]): number {
  let width = 0;
  let lines = 0;
  const bitFreq: number[] = [];

  for (const line of input) {
    width = line.length;
    lines++;
    for (let i = 0; i < width; i++) {
      bitFreq[i] = (bitFreq[i] ?? 0) + parseInt(line[i]);
    }
  }

  let gamma = 0;
  for (const [index, freq] of bitFreq.entries()) {
    if (freq > (lines / 2)) {
      gamma += 1 << (width - 1 - index);
    }
  }

  const delta = gamma ^ (2 ** width - 1);

  return gamma * delta;
}

function part2(input: string[]): number {
  let bit = 0;
  let oxyBitFreq = 0;
  let co2BitFreq = 0;
  let oxyCandidates: string[] = [];
  let co2Candidates: string[] = [];

  for (const line of input) {
    oxyCandidates.push(line);
    co2Candidates.push(line);
    oxyBitFreq += parseInt(line[bit]);
  }
  co2BitFreq = oxyBitFreq;

  do {
    if (oxyCandidates.length > 1) {
      const mostFreq = oxyBitFreq >= (oxyCandidates.length / 2) ? "1" : "0";
      oxyCandidates = oxyCandidates.filter((line) => line[bit] === mostFreq);
    }

    if (co2Candidates.length > 1) {
      const leastFreq = co2BitFreq >= (co2Candidates.length / 2) ? "0" : "1";
      co2Candidates = co2Candidates.filter((line) => line[bit] === leastFreq);
    }

    bit++;
    oxyBitFreq = co2BitFreq = 0;
    for (const candidate of oxyCandidates) {
      oxyBitFreq += parseInt(candidate[bit]);
    }
    for (const candidate of co2Candidates) {
      co2BitFreq += parseInt(candidate[bit]);
    }
  } while (oxyCandidates.length > 1 || co2Candidates.length > 1);

  const oxy = parseInt(oxyCandidates[0], 2);
  const co2 = parseInt(co2Candidates[0], 2);

  return oxy * co2;
}

exec({
  inputPath,
  parseLine(line) {
    return line.trim();
  },
  part1,
  part2,
});
