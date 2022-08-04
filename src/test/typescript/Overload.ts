function f(x: number): number;
function f(x: string): string;

function f(x) {
  if (typeof x == "number") return x + 42;
  else return "->" + x;
}

function app(f: (x: number) => void, x: number): void;
function app(f: (x: string) => void, x: string): void;

function app(f, x): void {
  f(x)
}