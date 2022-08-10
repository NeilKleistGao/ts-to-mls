namespace N1 {
  export function f(x) {
    return 42;
  }

  function ff(y) {
    return 42;
  }

  export class C {
    f() {}
  }

  interface I {
    f: () => number
  }

  export namespace N2 {
    /**
     * @debug
     */
    export function fff(x: boolean) {
      return 42;
    }
  }
}

namespace AA {
  export function f(x) {
    return "42";
  }

  export class C {
    f() {}
  }

  export interface I {
    f: () => number
  }

  export namespace N2 {}
}

/**
  * @debug
  */
function f1(x: N1.C): N1.C {
  return x;
}

/**
  * @debug
  */
function f2(x: AA.C): AA.C {
  return x;
}