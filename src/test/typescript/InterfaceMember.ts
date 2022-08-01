interface IFoo {
    a: string
    b: (x: number) => number
    c: () => boolean
    d: (x: string) => void
}

interface II<T extends number> {
    test: (x: T) => number
}