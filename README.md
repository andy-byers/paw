# paw

> NOTE: Paw is under active development and is not ready for production use.
> See the [roadmap](#roadmap) to get an idea of where things are going.
> Also see [known issues](#known-issues) for a list of known problems that will eventually be fixed.

A cute little scripting language

Paw is a high-level, statically-typed, embeddable scripting language.
It is designed to run on a virtual machine written in C.

## Features
+ No dependencies
+ Static strong typing
+ Bidirectional type checking
+ Block expressions
+ Module system
+ Exhaustive pattern matching and sum types
+ Traits (interfaces checked at compile time)
+ Generics and generic bounds
+ Container literals (`[T]` and `[K: V]`)

## Examples

### Hello world
```paw
pub fn main() {
    print("Hello, world!\n");
}
```

### FizzBuzz
```paw
pub fn main() {
    // Create a closure. The type of "n" is inferred as "int" and the 
    // return type as "str".
    let fizzbuzz = |n| {
        if n % 15 == 0 { 
            "FizzBuzz" 
        } else if n % 3 == 0 { 
            "Fizz"
        } else if n % 5 == 0 { 
            "Buzz" 
        } else { 
            n.to_string() 
        }
    };

    // Call the closure for each integer 1 to 100, exclusive.
    for i in 1..100 {
        print(fizzbuzz(i) + "\n");
    }
}
```

### Sum types
```paw
pub enum Expr {
    Zero,
    Succ(Expr),
    Add(Expr, Expr)
}

pub fn eval(e: Expr) -> int {
    // match expressions must be exhaustive
    match e {
        Expr::Zero => 0,
        Expr::Succ(x) => eval(x) + 1,
        Expr::Add(x, y) => eval(x) + eval(y),
    }
}

pub fn three() -> int {
    let zero = Expr::Zero;
    let one = Expr::Succ(zero);
    let two = Expr::Add(one, one);
    eval(Expr::Add(one, two))
}
```

### Generics
```paw

// type aliases can accept type arguments
type VecList2<T> = [(T, T)];

fn map2<X, Y>(f: fn(X, X) -> Y, xs: VecList2<X>) -> [Y] {
    let ys = [];
    // destructuring is supported in "for" loops and "let" declarations
    for (a, b) in xs {
        ys.push(f(a, b));
    }
    ys
}

pub fn main() {
    let data = [
        (1, 2),
        (2, 3),
        (3, 4),
        (4, 5),
        (5, 6),
    ];

    let data = map2(|x: int, y| x + y, data);

    let total = 0;
    for value in data {
        total = total + value;
    }

    print(total.to_string() + "\n"); // 35
}

```

### Traits

```paw
pub trait Get<T> {
    fn get(self) -> T;
}

struct Inner<X>: Get<X> {
    pub value: X,

    pub fn get(self) -> X {
        self.value
    }
}

struct Outer<X: Get<Y>, Y>: Get<Y> {
    pub value: X,

    pub fn get(self) -> Y {
        self.value.get()
    }
}

fn get<X: Get<Y>, Y>(x: X) -> Y {
    x.get()
}

pub fn main() {
    let inner = Inner{value: 123};
    let outer = Outer{value: inner};
    let value = get(outer);

    print(value.to_string() + "\n"); // 123
}
```

## Operators

|Precedence|Operator   |Description                                   |Associativity|
|:---------|:----------|:---------------------------------------------|:------------|
|14        |`() [] . ?`|Call, Subscript, Member access, Question mark |Left         |
|13        |`! - ~ #`  |Not, Negate, Bitwise not, length              |Right        |
|12        |`as`       |Cast                                          |Left         |
|11        |`* / %`    |Multiply, Divide, Modulus                     |Left         |
|10        |`+ -`      |Add, Subtract                                 |Left         |
|9         |`<< >>`    |Shift left, Shift right                       |Left         |
|8         |`&`        |Bitwise and                                   |Left         |
|7         |`^`        |Bitwise xor                                   |Left         |
|6         |<code>&#124;</code>|Bitwise or                            |Left         |
|5         |`< <= > >=`|Relational comparisons                        |Left         |
|4         |`== !=`    |Equality comparisons                          |Left         |
|3         |`&&`       |And                                           |Left         |
|2         |<code>&#124;&#124;</code>|Or                              |Left         |
|1         |`=`        |Assignment                                    |Right        |

## Roadmap
+ [x] static, strong typing
+ [x] special syntax for builtin containers (`[T]` and `[K: V]`)
+ [x] type inference for polymorphic `fn` and `struct`
+ [x] sum types/discriminated unions (`enum`)
+ [x] product types (tuple)
+ [x] custom garbage collector (using Boehm GC for now)
+ [x] methods
+ [x] module system and `use` keyword
+ [x] type inference for polymorphic `enum`
+ [x] exhaustive pattern matching (`match` construct)
+ [x] more featureful `use` declarations: `use mod::*`, `use mod::specific_symbol`, `use mod as alias`, etc.
+ [x] generic constraints/bounds
+ [x] constant folding, constant propagation
+ [x] traits (more like Swift protocols, maybe needs a different name)
+ [x] integrate traits into stdlib (iterators, hash map keys, etc.)
+ [x] `let` bindings/destructuring
+ [ ] error handling (`try` needs to be an operator, or we need something like a 'parameter pack' for generics to implement the `try` function)
+ [ ] function inlining
+ [ ] refactor user-provided allocation interface to allow heap expansion

## Known problems
+ Generic bounds should not be allowed on type aliases
+ The C API has pretty much 0 type safety
    + It may be necessary to reduce the scope of the C API somewhat
+ Pointer tracking (test only) feature is broken on MSVC
    + Might indicate a problem somewhere in the library
    + Need a machine that can run Windows for debugging
+ Methods on primitives are unable to modify "self"
    + Results in "int::incremented(self) -> int" hack in prelude (would be nicer as "int::increment(self)")
    + Need to use a pointer to "self" in this case, but paw has no concept of pointers right now
