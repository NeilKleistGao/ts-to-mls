function key(x: [string, boolean]): string {
    return x[0];
}

function value(x: [string, boolean]): boolean {
    return x[1];
}

function third(x: [number, number, number]): number {
    return x[2];
}

function vec2(x: number, y: number): [number, number] {
    return [x, y];
}

function twoFunctions(ff: [(x: number) => number, (x: number) => number], x: number): number {
    return ff[0](x) + ff[1](x);
}

function s(flag: boolean): [string | number, number | boolean] {
    if (flag) {
        return ["abc", 12];
    }
    else {
        return [24, false];
    }
}

function ex<T, U>(x: T, y: U): [T, U, T & U] {
    return [x, y , <T & U>{}];
}