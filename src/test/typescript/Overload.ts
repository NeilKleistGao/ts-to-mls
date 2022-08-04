function f(x: number): number;
function f(x: string): string;

function f(x) {
  if (typeof x == "number") return x + 42;
  else return "->" + x;
}

class M {
  foo(x: number): number;
  foo(x: string): string;

  foo(x) {
    return x;
  }
}

function app(f: (x: number) => void, x: number): void;
function app(f: (x: string) => void, x: string): void;

function app(f, x): void {
  f(x)
}

function create(x: number): () => number;
function create(x: boolean): () => boolean;

function create(x) {
  return function() { return x; }
}

function g0(x: string[]): string;
function g0(x: object[]): object;

function g0(x) {
  return x[0];
}

function db(x: number): number[];
function db(x: object): object[];

function db(x) {
  return [x, x];
}

class N {}

function id(x: M): M;
function id(x: N): N;

function id(x) { return x; }

id({foo: function(x: boolean): void {}})