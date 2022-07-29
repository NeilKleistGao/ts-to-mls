function inc<T extends number>(x: T) {
    return x + 1
}

class CC<T extends string> {
    constructor() {}

    print(s: T) { console.log(s) }
}