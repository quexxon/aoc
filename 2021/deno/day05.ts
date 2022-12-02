import * as Path from "https://deno.land/std@0.116.0/path/mod.ts";
import { exec, sleep } from "./util.ts";

const scriptDir = Path.dirname(Path.fromFileUrl(import.meta.url));
const inputPath = Path.join(scriptDir, "../inputs/05.txt");

interface Point {
  x: number;
  y: number;
}

function parsePoint(str: string): Point {
  const [x, y] = str.split(",").map((n) => parseInt(n));
  return { x, y };
}

interface Line {
  start: Point;
  end: Point;
}

function parseLine(str: string): Line {
  const [start, end] = str.split(" -> ").map(parsePoint);
  return { start, end };
}

function part1(lines: Line[]): number {
  const visitedPoints = new Map();
  let point: number;
  let count: number;
  for (const { start, end } of lines) {
    if (start.x === end.x || start.y === end.y) {
      let x = start.x;
      let y = start.y;
      while (x !== end.x || y !== end.y) {
        point = (x << 16) + y;
        count = visitedPoints.get(point) ?? 0;
        visitedPoints.set(point, count + 1);
        if (x < end.x) {
          x++;
        } else if (x > end.x) {
          x--;
        }
        if (y < end.y) {
          y++;
        } else if (y > end.y) {
          y--;
        }
      }
      point = (x << 16) + y;
      count = visitedPoints.get(point) ?? 0;
      visitedPoints.set(point, count + 1);
    }
  }

  let sum = 0;
  for (const freq of visitedPoints.values()) {
    if (freq > 1) sum++;
  }

  return sum;
}

function part2(lines: Line[]): number {
  const visitedPoints = new Map();
  let point: number;
  let count: number;
  for (const { start, end } of lines) {
    let x = start.x;
    let y = start.y;
    while (x !== end.x || y !== end.y) {
      point = (x << 16) + y;
      count = visitedPoints.get(point) ?? 0;
      visitedPoints.set(point, count + 1);
      if (x < end.x) {
        x++;
      } else if (x > end.x) {
        x--;
      }
      if (y < end.y) {
        y++;
      } else if (y > end.y) {
        y--;
      }
    }
    point = (x << 16) + y;
    count = visitedPoints.get(point) ?? 0;
    visitedPoints.set(point, count + 1);
  }

  let sum = 0;
  for (const freq of visitedPoints.values()) {
    if (freq > 1) sum++;
  }

  return sum;
}

sleep(0).then(() => {
  return exec({
    inputPath,
    parseLine,
    part1,
    part2,
  });
}).catch((err) => console.error(err));
