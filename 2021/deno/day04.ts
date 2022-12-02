import * as Path from "https://deno.land/std@0.116.0/path/mod.ts";
import { exec, sleep } from "./util.ts";

const ROW_SIZE = 5;

const BINGO_TABLE = [
  0b11111,
  0b11111 << 5,
  0b11111 << 5 * 2,
  0b11111 << 5 * 3,
  0b11111 << 5 * 4,
  0b10000_10000_10000_10000_10000,
  0b01000_01000_01000_01000_01000,
  0b00100_00100_00100_00100_00100,
  0b00010_00010_00010_00010_00010,
  0b00001_00001_00001_00001_00001,
];

function isBingo(board: Board): boolean {
  for (const BINGO of BINGO_TABLE) {
    if ((board.population & BINGO) === BINGO) {
      return true;
    }
  }

  return false;
}

function scoreBoard(board: Board): number {
  let sum = 0;
  for (const [number, { x, y }] of board.board) {
    if ((board.population & (1 << (x + y * ROW_SIZE))) === 0) {
      sum += number;
    }
  }
  return sum;
}

interface Position {
  x: number;
  y: number;
}

interface Board {
  board: Map<number, Position>;
  population: number;
}

function part1(input: string[]): number {
  const [rawNumbers, ...rawBoards] = input.join("\n").split("\n\n");

  const numbers = rawNumbers.split(",").map((s) => parseInt(s));
  const boards: Board[] = [];

  for (const rawBoard of rawBoards) {
    const board = new Map();
    for (const [y, row] of rawBoard.trim().split("\n").entries()) {
      for (const [x, number] of row.trim().split(/\s+/).entries()) {
        board.set(parseInt(number), { x, y });
      }
    }
    boards.push({ board, population: 0 });
  }

  for (const number of numbers) {
    for (const board of boards) {
      const position = board.board.get(number);
      if (position !== undefined) {
        board.population += 1 << (position.x + position.y * ROW_SIZE);
        if (isBingo(board)) {
          return number * scoreBoard(board);
        }
      }
    }
  }

  throw new Error("Unreachable");
}

function part2(input: string[]): number {
  const [rawNumbers, ...rawBoards] = input.join("\n").split("\n\n");

  const numbers = rawNumbers.split(",").map((s) => parseInt(s));
  let boards: Board[] = [];

  for (const rawBoard of rawBoards) {
    const board = new Map();
    for (const [y, row] of rawBoard.trim().split("\n").entries()) {
      for (const [x, number] of row.trim().split(/\s+/).entries()) {
        board.set(parseInt(number), { x, y });
      }
    }
    boards.push({ board, population: 0 });
  }

  for (const number of numbers) {
    for (const board of boards) {
      const position = board.board.get(number);
      if (position !== undefined) {
        board.population += 1 << (position.x + position.y * ROW_SIZE);
      }
    }
    if (boards.length === 1) {
      const board = boards[0];
      return number * scoreBoard(board);
    }
    boards = boards.filter((board) => !isBingo(board));
  }

  throw new Error("Unreachable");
}

if (Deno.args.length !== 1) {
  throw new Error("Usage: day10 INPUT");
}

sleep(0).then(() => {
  return exec({
    inputPath: Path.resolve(Deno.args[0]),
    parseLine: (line) => line,
    part1,
    part2,
  });
}).catch((err) => console.error(err));
