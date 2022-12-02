import * as Path from "https://deno.land/std@0.116.0/path/mod.ts";
import { exec } from "./util.ts";

const scriptDir = Path.dirname(Path.fromFileUrl(import.meta.url));
const inputPath = Path.join(scriptDir, "../inputs/02.txt");

enum Direction {
  forward,
  up,
  down,
}

function parseDirection(direction: string): Direction {
  switch (direction) {
    case "forward":
      return Direction.forward;
    case "up":
      return Direction.up;
    case "down":
      return Direction.down;
    default:
      throw new Error(`Unknown direction: ${direction}`);
  }
}

interface Bearing {
  direction: Direction;
  distance: number;
}

function part1(bearings: Bearing[]): number {
  let horizontalPosition = 0;
  let depth = 0;

  for (const bearing of bearings) {
    switch (bearing.direction) {
      case Direction.forward:
        horizontalPosition += bearing.distance;
        break;
      case Direction.up:
        depth -= bearing.distance;
        break;
      case Direction.down:
        depth += bearing.distance;
        break;
    }
  }

  return horizontalPosition * depth;
}

function part2(bearings: Bearing[]): number {
  let horizontalPosition = 0;
  let depth = 0;
  let aim = 0;

  for (const bearing of bearings) {
    switch (bearing.direction) {
      case Direction.forward:
        horizontalPosition += bearing.distance;
        depth += bearing.distance * aim;
        break;
      case Direction.up:
        aim -= bearing.distance;
        break;
      case Direction.down:
        aim += bearing.distance;
        break;
    }
  }

  return horizontalPosition * depth;
}

exec({
  inputPath,
  parseLine(line): Bearing {
    const [rawDirection, rawDistance] = line.split(/\s+/);

    return {
      direction: parseDirection(rawDirection),
      distance: parseInt(rawDistance),
    };
  },
  part1,
  part2,
});
