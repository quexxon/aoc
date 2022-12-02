import * as Path from "https://deno.land/std@0.116.0/path/mod.ts";
import { exec, sleep } from "./util.ts";

interface Input {
  signals: Signal[];
  outputs: Signal[];
}

interface Signal {
  length: number;
  value: number;
}

const CHAR_TABLE = new Map([
  ["a", 0b1],
  ["b", 0b10],
  ["c", 0b100],
  ["d", 0b1000],
  ["e", 0b10000],
  ["f", 0b100000],
  ["g", 0b1000000],
]);

enum Segment {
  Top = 0b1,
  UpperRight = 0b10,
  LowerRight = 0b100,
  Bottom = 0b1000,
  LowerLeft = 0b10000,
  UpperLeft = 0b100000,
  Middle = 0b1000000,
}

function solve(input: Input): number {
  const digitToSignal: Map<number, number> = new Map();
  const signalToDigit: Map<number, number> = new Map();
  const segments: Map<Segment, number> = new Map();

  // Set known digits
  const remaining: Map<number, Set<number>> = new Map();
  for (const signal of input.signals) {
    if (signal.length === 2) {
      digitToSignal.set(1, signal.value);
      signalToDigit.set(signal.value, 1);
    } else if (signal.length === 3) {
      digitToSignal.set(7, signal.value);
      signalToDigit.set(signal.value, 7);
    } else if (signal.length === 4) {
      digitToSignal.set(4, signal.value);
      signalToDigit.set(signal.value, 4);
    } else if (signal.length === 7) {
      digitToSignal.set(8, signal.value);
      signalToDigit.set(signal.value, 8);
    } else {
      let values = remaining.get(signal.length);
      if (values === undefined) {
        values = new Set();
        remaining.set(signal.length, values);
      }
      values.add(signal.value);
    }
  }

  segments.set(
    Segment.Top,
    (digitToSignal.get(7) as number) - (digitToSignal.get(1) as number),
  );
  const top = segments.get(Segment.Top) as number;

  // Solve for 3 - Middle, UpperLeft, Bottom
  const one = digitToSignal.get(1) as number;
  let candidates = remaining.get(5) as Set<number>;
  for (const candidate of candidates) {
    if ((candidate & one) === one) {
      digitToSignal.set(3, candidate);
      signalToDigit.set(candidate, 3);
      const ambiguous = (digitToSignal.get(4) as number) -
        (digitToSignal.get(1) as number);
      const middle = ambiguous & candidate;
      const upperLeft = ambiguous - middle;
      const bottom = candidate - one - top - middle;
      segments.set(Segment.Middle, middle);
      segments.set(Segment.UpperLeft, upperLeft);
      segments.set(Segment.Bottom, bottom);
      candidates.delete(candidate);
    }
  }
  const three = digitToSignal.get(3) as number;
  const middle = segments.get(Segment.Middle) as number;
  const upperLeft = segments.get(Segment.UpperLeft) as number;
  const bottom = segments.get(Segment.Bottom) as number;

  // Solve for 0 - LowerLeft
  candidates = remaining.get(6) as Set<number>;
  for (const candidate of candidates) {
    if ((candidate & middle) === 0) {
      digitToSignal.set(0, candidate);
      signalToDigit.set(candidate, 0);
      const lowerLeft = candidate - one - top - bottom - upperLeft;
      segments.set(Segment.LowerLeft, lowerLeft);
      candidates.delete(candidate);
    }
  }
  const lowerLeft = segments.get(Segment.LowerLeft) as number;

  // Solve for 6 - UpperRight, LowerRight
  const left = upperLeft + lowerLeft;
  for (const candidate of candidates) {
    if ((candidate & left) === left) {
      digitToSignal.set(6, candidate);
      signalToDigit.set(candidate, 6);
      const lowerRight = candidate - left - top - middle - bottom;
      const upperRight = one - lowerRight;
      segments.set(Segment.LowerRight, lowerRight);
      segments.set(Segment.UpperRight, upperRight);
      candidates.delete(candidate);
    }
  }
  const upperRight = segments.get(Segment.UpperRight) as number;
  const lowerRight = segments.get(Segment.LowerRight) as number;

  signalToDigit.set(top + upperRight + middle + lowerLeft + bottom, 2);
  signalToDigit.set(top + upperLeft + middle + lowerRight + bottom, 5);
  signalToDigit.set(three + upperLeft, 9);

  let output = "";
  for (const signal of input.outputs) {
    output += signalToDigit.get(signal.value) as number;
  }

  return parseInt(output);
}

function part1(input: Input[]): number {
  const lens = input.flatMap(({ outputs }) => outputs.map((o) => o.length));
  return lens.reduce(
    (sum, len) => [2, 3, 4, 7].includes(len) ? sum + 1 : sum,
    0,
  );
}

function part2(input: Input[]): number {
  return input.reduce((sum, input) => sum + solve(input), 0);
}

if (Deno.args.length !== 1) {
  throw new Error("Usage: day7 INPUT");
}

sleep(0).then(() => {
  return exec({
    inputPath: Path.resolve(Deno.args[0]),
    parseLine: (line) => {
      const [signals, outputs] = line.split(" | ").map((s) =>
        s.split(" ").map((digit) => {
          let value = 0;
          for (const char of digit) {
            const charValue = CHAR_TABLE.get(char);
            if (charValue === undefined) {
              throw new Error(`Unknown character: ${char}`);
            }
            value |= charValue;
          }
          return {
            length: digit.length,
            value,
          };
        })
      );
      return { signals, outputs };
    },
    part1,
    part2,
  });
}).catch((err) => console.error(err));
