function f(x: number): number;
function f(x: string): string;

function f(x) {
  if (typeof x == "number") return x + 42;
  else return "->" + x;
}