import * as Path from "https://deno.land/std@0.116.0/path/mod.ts";
import { exec, sleep } from "./util.ts";

type Plane = number[][];
type Point = number;

const Point = {
  create(x: number, y: number, z: number): Point {
    return (x << 16) + (y << 8) + z;
  },
  x(point: Point): number {
    return point >> 16;
  },
  y(point: Point): number {
    return (point & 0xFF00) >> 8;
  },
  z(point: Point): number {
    return point & 0xFF;
  },
};

function findLowPoints(plane: Plane): Point[] {
  const lowPoints = [];
  for (let y = 0; y < plane.length; y++) {
    for (let x = 0; x < plane[y].length; x++) {
      const z = plane[y][x];
      const n = (plane[y - 1] ?? [])[x];
      const e = plane[y][x + 1];
      const s = (plane[y + 1] ?? [])[x];
      const w = plane[y][x - 1];
      if (n !== undefined && z >= n) continue;
      if (e !== undefined && z >= e) continue;
      if (s !== undefined && z >= s) continue;
      if (w !== undefined && z >= w) continue;
      lowPoints.push(Point.create(x, y, z));
    }
  }
  return lowPoints;
}

function calculateBasin(plane: Plane, point: Point): Set<Point> {
  const perimeter = new Set([point]);
  const basin = new Set([point]);

  while (perimeter.size > 0) {
    for (const point of perimeter) {
      perimeter.delete(point);
      const x = Point.x(point);
      const y = Point.y(point);
      const n = (plane[y - 1] ?? [])[x];
      const e = plane[y][x + 1];
      const s = (plane[y + 1] ?? [])[x];
      const w = plane[y][x - 1];
      if (n !== undefined) {
        const p = Point.create(x, y - 1, n);
        if (!basin.has(p) && n !== 9) {
          perimeter.add(p);
          basin.add(p);
        }
      }
      if (e !== undefined) {
        const p = Point.create(x + 1, y, e);
        if (!basin.has(p) && e !== 9) {
          perimeter.add(p);
          basin.add(p);
        }
      }
      if (s !== undefined) {
        const p = Point.create(x, y + 1, s);
        if (!basin.has(p) && s !== 9) {
          perimeter.add(p);
          basin.add(p);
        }
      }
      if (w !== undefined) {
        const p = Point.create(x - 1, y, w);
        if (!basin.has(p) && w !== 9) {
          perimeter.add(p);
          basin.add(p);
        }
      }
    }
  }

  return basin;
}

function part1(plane: Plane): number {
  const lowPoints = findLowPoints(plane);
  return lowPoints.reduce((sum, p) => sum + Point.z(p) + 1, 0);
}

function part2(plane: Plane): number {
  const lowPoints = findLowPoints(plane);
  const basins = lowPoints.map((point) => calculateBasin(plane, point));
  basins.sort((x, y) => x.size - y.size);
  return basins.slice(-3).reduce((product, basin) => product * basin.size, 1);
}

if (Deno.args.length !== 1) {
  throw new Error("Usage: day9 INPUT");
}

sleep(0).then(() => {
  return exec({
    inputPath: Path.resolve(Deno.args[0]),
    parseLine: (line) => Array.from(line).map((str) => parseInt(str)),
    part1,
    part2,
  });
}).catch((err) => console.error(err));
