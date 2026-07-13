# paw

> NOTE: Paw is under active development and is not ready for production use.
> See the [roadmap](#roadmap) to get an idea of where things are going.
> Also see [known issues](#known-issues) for a list of known problems that will eventually be fixed.
> Finally, this README needs to be updated (e.g. dynamic containers were repalced in the core language by arrays and slices, we are likely violating move semantics in some examples, etc.)

A general-purpose programming language

Paw is a statically-typed, ahead-of-time compiled, general-purpose programming language.

## Features
+ Static strong typing
+ AOT compiled using LLVM
+ Bidirectional type checking
+ Destructive move semantics
+ Manual memory management, RAII
+ Module system
+ Exhaustive pattern matching and sum types
+ Traits (interfaces checked at compile time)
+ Generics and generic bounds
+ Core language never uses dynamic memory
+ Standard library is optional

## Examples

### Hello world
```paw
use io;

pub fn main() {
    io::println("Hello, world!");
}
```

### FizzBuzz
```paw
use io;

pub fn main() -> Result<(), mem::OutOfMemory> {
    // Create a closure. The type of "n" is inferred as "int64" and the 
    // return type as "String".
    let fizzbuzz = |n| {
        if n % 15 == 0 { 
            "FizzBuzz".to_string() 
        } else if n % 3 == 0 {
            "Fizz".to_string()
        } else if n % 5 == 0 {
            "Buzz".to_string() 
        } else { 
            n.to_string() 
        }
    };

    // Call the closure for each integer 1 to 100, exclusive.
    for i in 1..100 {
        io::println(f"fizzbuzz(\{i}) = \{fizzbuzz(i)?}"?);
    }
}
```

### Containers
```paw
use hashmap;
use io;
use list;
use mem;

fn example() -> Result<(), mem::OutOfMemory> {
    let list = list::List::new();

    // add a single element to the end
    list.push(1)?;

    let map = hashmap::HashMap::new();

    // add a few key-value pairs
    map.insert('a', "(A)")?;
    map.insert('b', "(B)")?;
    map.insert('c', "(C)")?;

    match map.get('a') {
        Some(v) => io::println(v),
        None => panic("not found"),
    }

    map.remove('a');
}
```

### Data types
Paw provides 2 mechanisms for creating custom data types: `struct` and `enum`.
Structures (structs) are nominal composite types created using the `struct` keyword. 
Enumerations (enums) are sum types (tagged unions) created using the `enum` keyword. 
Enums are described greater detail in [sum types](#sum-types).
In both cases, the datatype definition specifies only the data layout of the type.
Methods and associated functions can be attached using an [`impl` block](#impl-blocks).
```paw
struct Statistic {
    pub name: String, // accessible from anywhere
    value: float32, // only accessible from within the same module
}
```

### Sum types
Sum types in Paw consist of tagged unions created by an `enum` definition.
Conceptually, an enum is an object that takes the value of one of its variants depending on the value of the discriminant field.
In the example below, an instance of `Expr` must contain space for an integer large enough to distinguish 3 variants, as well as space for the largest of the possible variants (`Expr::Add` here).
```paw
pub enum Expr {
    Zero,
    Succ(*Expr),
    Add(*Expr, *Expr)
}

impl Copy for Expr {}

// import variants into the global value namespace
use Expr::*;

pub fn eval(e: Expr) -> int32 {
    // match expressions must be exhaustive
    match e {
        Zero => 0,
        Succ(*x) => eval(x) + 1,
        Add(*x, *y) => eval(x) + eval(y),
    }
}

pub fn three() -> int32 {
    let zero = Zero;
    let one = Succ(&zero);
    let two = Add(&one, &one);
    eval(Add(&one, &two))
}
```

### Implementations
Methods and associated functions can be defined on a data type using an `impl` block.


### Generics
Paw supports parametric polymorphism, a.k.a. generic type parameters.

```paw
use list::List;

// type aliases can accept type arguments
type PairSlice<T> = [](T, T);

fn map2<X, Y>(f: fn(X, X) -> Y, xs: PairSlice<X>) -> List<Y> {
    let ys = List::new();
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

    let data = map2(|x: int16, y| x + y, data.to_slice());

    let total = 0;
    for value in data {
        total = total + value;
    }

    io::println(f"total = \{total}".unwrap()); // total = 35
}
```

### Traits

```paw
pub trait Get<T> {
    fn get(self) -> T;
}

struct Inner<X> {
    pub value: X,
}

impl<X> Get<X> for Inner<X> {
    fn get(self) -> X {
        self.value
    }
}

struct Outer<X: Get<Y>, Y> {
    pub value: X,
}

impl<X: Get<Y>, Y> Get<Y> for Outer<X, Y> {
    fn get(self) -> Y {
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

    io::println(f"value = \{value}".unwrap()); // value = 123
}
```

### Pointers
```paw
pub fn increment(value: *uint64) {
    *value += 1;
}

pub fn main() {
    let value = 0;
    increment(&value);
    assert(value == 1);
}
```

## Error handling
Paw uses `Result<T, E>` to express a recoverable error, e.g. "no such file or directory".
Runtime panics are issued for unrecoverable errors, e.g. "out of memory", an assertion failure, or an out-of-bounds element access.
Panics cannot be caught.
A panic always stops execution at the location of the panic and exits the process with a nonzero status code.
A panic can also be caused by calling the `panic` builtin function.

## Operators

|Precedence|Operator                 |Description                                  |Associativity|
|:---------|:------------------------|:--------------------------------------------|:------------|
|14        |`() [] . ?`              |Call, Subscript, Member access, Question mark|Left         |
|13        |`! - ~`                  |Not, Negate, Bitwise not                     |Right        |
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
+ [ ] add `mut` keyword
+ [x] associated types (needed especially for iterator ergonomics)
+ [x] minimal const generics (no type expressions involving const generics)
+ [x] reimplement capturing closures
+ [ ] Implement higher RAII (Vale lang concept) to have compiler-enforced resource "closing" (may require per-field drop flags) using `SuppressDrop` trait or similar. May require supporting negative trait bounds and having `T: !SuppressDrop` be the default for generics.
+ [ ] `#[must_use]` or similar annotation on type declarations
+ [ ] allow linking in a custom "panic handler" for platforms where the default panic handler doesn't make sense (no OS to return back to, nowhere for error messages to go, etc.)
+ [ ] prevent duplicate methods across compatible inherent impl blocks
+ [x] make sure to complain when generic params not mentioned on context of impl block. i.e. `impl<T> Trait<T> for Type {...}` is an error if `Type` has generic parameters.
+ [ ] add overflow checks for `paw_Int` operations during constant folding and codegen
+ [ ] remove dependency on clang (as a linker driver) and invoke linker manually
+ [ ] report multiple errors per invocation of compiler, emit warnings

## Known problems
+ These need to be converted into issues, along with some TODO comments scattered throughout the codebase...
+ In some cases, the compiler can't determine the type of a method without knowing the type of some arguments (happens with `IndexRange` trait)
+ Associated types on supertraits don't work properly
+ Need to add a check to make sure there are not extraneous trait bounds on trait methods (only checking for unsatisfied trait obligations, i.e. the check needs to be performed the "other way" as well)
+ Generic type parameters on type aliases can't be constrained with trait bounds
+ Type aliases need check for cycles. Also, compiler crashes when it encounters type alias in RHS that hasn't been defined yet.
+ Edge cases exist related to impl blocks and traits
    + The following should be allowed `T: Trait<int32> + Trait<uint32>`, that is, having 2 trait bounds on a single type with the same `DeclId`
+ Need to make sure functions/closures with a return type annotation of "!" diverge unconditionally 
    + See TODO comment in `test_error.c` `test_divergence` function
