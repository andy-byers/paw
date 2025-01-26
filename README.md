# paw

> NOTE: Paw is under active development and is not ready for production use.
> See the [roadmap](#roadmap) to get an idea of where things are going.
> Also see [known issues](#known-issues) for a list of known problems that will eventually be fixed.

A cute little scripting language

Paw is a high-level, statically-typed, embeddable scripting language.
It is designed to run on a virtual machine written in C.

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
struct Pair<First, Second> {
    first: First,
    second: Second,
    pub name: str,
}

impl<X, Y> Pair<X, Y> {
    pub fn new(first: X, second: Y) -> Self {
        // shorthand for "Self{first: first, second: second}"
        Self{first, second, name: "public field"}
    }
}

impl<T> Pair<T, T> {
    pub fn swap(self) {
        let temp = self.first;
        self.first = self.second;
        self.second = temp;
    }
}

impl Pair<int, int> {
    pub fn scale(self, n: int) {
        self.first = self.first * n;
        self.second = self.second * n;
    }
}

type SwappablePair<Ty> = Pair<Ty, Ty>;

pub fn swap<Ty>(p: SwappablePair<Ty>) {
    p.swap();    
}

pub fn main() {
    let p = SwappablePair::new(1, 23);

    // transform "(1, 23)" into "(230, 10)"
    p.scale(10);
    swap(p);

    // fields marked "pub" can be set outside of methods/associated
    // functions defined on Pair
    p.name = "MyPair";
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
+ [x] methods using `impl` blocks
+ [x] module system and `use` keyword
+ [x] type inference for polymorphic `enum`
+ [x] exhaustive pattern matching (`match` construct)
+ [ ] traits
+ [ ] for loops: use `Iterate<T>` trait
+ [ ] error handling (`try` needs to be an operator, or we need something like a 'parameter pack' for generics to implement the `try` function)
+ [ ] `let` bindings/destructuring
+ [x] more featureful `use` declarations: `use mod::*`, `use mod::specific_symbol`, `use mod as alias`, etc.
+ [ ] generic constraints/bounds
+ [ ] constant folding, constant propagation, maybe inlining
+ [ ] refactor user-provided allocation interface to allow heap expansion

## Known problems
+ The C API has pretty much 0 type safety
    + It may be necessary to reduce the scope of the C API somewhat
