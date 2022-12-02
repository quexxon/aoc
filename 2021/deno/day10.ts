import * as Path from "https://deno.land/std@0.116.0/path/mod.ts";
import { exec, sleep } from "./util.ts";

const OPENING_DELIM = ["(", "[", "{", "<"];
const WRONG_DELIM: Record<string, number> = {
  ")": 3,
  "]": 57,
  "}": 1197,
  ">": 25137,
};
const MISSING_DELIM: Record<string, number> = {
  ")": 1,
  "]": 2,
  "}": 3,
  ">": 4,
};

class ClosingDelimiterError extends Error {
  char: string;
  position: number;

  constructor(message: string, char: string, position: number) {
    super(message);
    this.char = char;
    this.position = position;
    this.name = this.constructor.name;
  }
}

class MissingDelimiterError extends Error {
  char: string;
  position: number;

  constructor(message: string, char: string, position: number) {
    super(message);
    this.char = char;
    this.position = position;
    this.name = this.constructor.name;
  }
}

type ParenGroup = {
  kind: "(";
  children: AST[];
};

type SquareGroup = {
  kind: "[";
  children: AST[];
};

type BraceGroup = {
  kind: "{";
  children: AST[];
};

type AngleGroup = {
  kind: "<";
  children: AST[];
};

type AST =
  | ParenGroup
  | SquareGroup
  | BraceGroup
  | AngleGroup;

class Parser {
  #position = 0;
  #input: string;
  pendingDelimiters: string[] = [];

  constructor(input: string) {
    this.#input = input;
  }

  parse(): AST[] {
    const result = [];
    while (!this.isEnd()) {
      result.push(this.group());
      this.advance();
    }
    return result;
  }

  group(): AST {
    let grp: AST;
    let delimiter: string;

    switch (this.currentChar()) {
      case "(":
        grp = { kind: "(", children: [] };
        delimiter = ")";
        break;
      case "[":
        grp = { kind: "[", children: [] };
        delimiter = "]";
        break;
      case "{":
        grp = { kind: "{", children: [] };
        delimiter = "}";
        break;
      case "<":
        grp = { kind: "<", children: [] };
        delimiter = ">";
        break;
      default:
        throw new ClosingDelimiterError(
          `Unexpected char: ${this.currentChar()}`,
          this.currentChar(),
          this.#position,
        );
    }

    this.pendingDelimiters.push(delimiter);
    this.advance();

    while (OPENING_DELIM.includes(this.currentChar())) {
      grp.children.push(this.group());
      this.advance();
    }

    if (this.isEnd()) {
      throw new MissingDelimiterError(
        "Unexpected EOL",
        delimiter,
        this.#position,
      );
    }

    if (this.currentChar() !== delimiter) {
      throw new ClosingDelimiterError(
        `Unexpected char: ${this.currentChar()}`,
        this.currentChar(),
        this.#position,
      );
    }

    this.pendingDelimiters.pop();

    return grp;
  }

  currentChar(): string {
    return this.#input[this.#position];
  }

  advance(): void {
    this.#position++;
  }

  isEnd(): boolean {
    return this.#input[this.#position] === undefined;
  }
}

function part1(input: string[]): number {
  let score = 0;

  for (const line of input) {
    try {
      const parser = new Parser(line);
      parser.parse();
    } catch (error) {
      if (error instanceof ClosingDelimiterError) {
        score += WRONG_DELIM[error.char] ?? 0;
      }
    }
  }

  return score;
}

function part2(input: string[]): number {
  const scores = [];

  for (const line of input) {
    const parser = new Parser(line);
    try {
      parser.parse();
    } catch (error) {
      if (error instanceof MissingDelimiterError) {
        scores.push(
          parser.pendingDelimiters.reverse().reduce((score, char) => {
            return score * 5 + MISSING_DELIM[char];
          }, 0),
        );
      }
    }
  }

  return scores.sort((x, y) => x - y)[Math.floor(scores.length / 2)];
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
