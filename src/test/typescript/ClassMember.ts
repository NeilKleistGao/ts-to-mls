class Student {
    name: string

    constructor() {}

    getID() { return 114514; }
    addScore(sub: string, score: number) {}
    isFriend(other: Student) { return true; }
}

class Foo<T extends Student> {
    constructor() {}

    bar(x: T) {}
}