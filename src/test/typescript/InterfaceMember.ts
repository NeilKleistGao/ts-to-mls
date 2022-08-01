interface IFoo {
    a: string
    b: (x: number) => number
    c: () => boolean
    d: (x: string) => void
}

interface II<T extends number> {
    test: (x: T) => number
}

function create() {
    return {v: 0};
}

function get(x: {t: string}): string {
    return x.t;
}