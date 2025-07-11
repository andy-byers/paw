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
+ Unboxed objects using "inline" keyword
+ Container literals (`[T]` and `[K: V]`)

## Examples

### Hello world
```paw
pub fn main() {
    println("Hello, world!");
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
            n.to_str() 
        }
    };

    // Call the closure for each integer 1 to 100, exclusive.
    for i in 1..100 {
        println("fizzbuzz(\{i}) = \{fizzbuzz(i)}");
    }
}
```

### Containers
```paw
pub fn main() {
    let list = []; // [int]

    // add a single element to the end
    list.push(1); // [1]

    // concatenate with another list
    list += [2, 3, 4]; // [1, 2, 3, 4]

    // lists (and "str") can be indexed with range objects
    let suffix = list[-2..]; // [3, 4]
    let prefix = list[0..3]; // [1, 2, 3]


    let map = [:]; // [char: int]

    // add a few key-value pairs
    map['a'] = 1;
    map['b'] = 2;
    map['c'] = 3;

    match map.get('a') {
        Some(v) => println("map['a'] = \{v}"),
        None => panic("not found"),
    }

    assert(map.get_or('d', 4) == 4);
}
```

### Sum types
Note that "inline" cannot be used on a recursive type as this would cause resulting objects to have a size of infinity (see [value types](#value-types)).
```paw
pub enum Expr {
    Zero,
    Succ(Expr),
    Add(Expr, Expr)
}

// import variants into the global value namespace
use Expr::*;

pub fn eval(e: Expr) -> int {
    // match expressions must be exhaustive
    match e {
        Zero => 0,
        Succ(x) => eval(x) + 1,
        Add(x, y) => eval(x) + eval(y),
    }
}

pub fn three() -> int {
    let zero = Zero;
    let one = Succ(zero);
    let two = Add(one, one);
    eval(Add(one, two))
}
```

### Generics
Paw supports parametric polymorphism, a.k.a. generic type parameters.

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

    println("total = \{total}"); // total = 35
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

    println("value = \{value}"); // value = 123
}
```

### Value types
Structures and enumerations have reference semantics by default.
The `inline` keyword can be used to give a type value semantics.
Primitives (`int`, `float`, etc.) and tuples are always value types.
Inline types can be used to reduce memory consumption in programs containing many small objects.
They can also be used to implement "newtype" wrappers with no additional runtime overhead.
```paw
inline struct Data<T> {
    pub value: T,
}

pub fn main() {
    // "data" consists of exactly 3 integers stored directly in the activation frame
    let data = Data{value: Data{value: (1, (2, 3))}};

    // all fields are copied: "copy" is independent from "data"
    let copy = data;
}
```

## Error handling
Paw uses `Result<T, E>` to express a recoverable error, e.g. "no such file or directory".
Runtime panics are issued for unrecoverable errors, e.g. "out of memory", an assertion failure, or an out-of-bounds element access.
Panics cannot be caught inside Paw.
A panic always stops execution at the location of the panic and causes the VM entrpoint function to return with an error.
A panic can also be caused by calling the `panic` builtin function.

## Operators

|Precedence|Operator                 |Description                                  |Associativity|
|:---------|:------------------------|:--------------------------------------------|:------------|
|14        |`() [] . ?`              |Call, Subscript, Member access, Question mark|Left         |
|13        |`! - ~ #`                |Not, Negate, Bitwise not, length             |Right        |
|12        |`as`                     |Cast                                         |Left         |
|11        |`* / %`                  |Multiply, Divide, Modulus                    |Left         |
|10        |`+ -`                    |Add, Subtract                                |Left         |
|9         |`<< >>`                  |Shift left, Shift right                      |Left         |
|8         |`&`                      |Bitwise and                                  |Left         |
|7         |`^`                      |Bitwise xor                                  |Left         |
|6         |<code>&#124;</code>      |Bitwise or                                   |Left         |
|5         |`< <= > >=`              |Relational comparisons                       |Left         |
|4         |`== !=`                  |Equality comparisons                         |Left         |
|3         |`&&`                     |And                                          |Left         |
|2         |<code>&#124;&#124;</code>|Or                                           |Left         |
|1         |`= op=`                  |Assignment, operator assignment              |Right        |

## Roadmap
+ [x] fix list/str slice operation (should use `Range`, `RangeTo`, etc.)
+ [ ] use `mut` to indicate mutability and make immutable the default for local variables
+ [ ] decide on and implement either RAII or "defer" for cleaning up resources
+ [ ] overflow checks for `paw_Int` operations during constant folding and inside VM
+ [ ] function inlining
+ [ ] refactor user-provided allocation interface to allow heap expansion

## Known problems
+ These need to be converted into issues, along with some TODO comments scattered throughout the codebase...
+ Paw requires that "int" be at least 32 bits (probably fine in practice)
+ Need to make sure functions/closures with a return type annotation of "!" diverge unconditionally 
    + See TODO comment in `test_error.c` `test_divergence` function
    + Consider returns to be jumps to a special block, possibly after setting the return variable
    + Unconditionally-diverging functions should not have any writes to this variable
+ Generic bounds should not be allowed on type aliases
+ Pointer tracking (test only) feature is broken on MSVC
    + Might indicate a problem somewhere in the library
    + Need a machine that can run Windows for debugging
+ Instructions to satisfy some of the runtime constraints are injected during code generation
    + This makes them invisible to the constant propagation pass, leading to less efficient byte code
    + For example, function calls require the callable followed by the arguments on top of the stack
        + The moves to put everything in place are injected during codegen
        + If they were made explicit in an earlier pass, then the instructions that compute them could be made to write directly into the proper registers, avoiding the moves altogether in many cases
    + Probably need some sort of register hints to accomplish this
+ Need a lower-level CFG-based IR (LIR) to use for register allocation and codegen
    + Convert the `scalarize`/`ssa` pass into `lower_mir`, which will output LIR in SSA form
    + Perform constant propagation on the LIR, monomorphization doesn't need to change
    + LIR will contain `GETFIELD`, `SETELEMENT`, etc. instructions, which are represented by places in the MIR
    + Each LIR register will represent a single Paw value (`Value` structure in C), while MIR registers can be multiple values wide
    + This representation is needed due to the attempt to unbox composite values, it is a bit painful to operate on the MIR
    + This is somewhat low-priority, since the MIR will work for unboxed values, it's just not quite as nice to work with
