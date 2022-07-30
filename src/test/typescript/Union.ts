function getString(x: string | number | boolean): string {
    return x.toString()
}

function test(x: boolean): (string | number) {
    if (x) return "foo";
    else return 42;
}

function run(f: ((x: number) => number) | ((x: number) => string)): any {
    return f(42);
}