# paw

> NOTE: Paw is under active development and is not ready for production use.
> See the [roadmap](#roadmap) to get an idea of where things are going.
> Also see [known issues](#known-issues) for a list of known problems that will eventually be fixed.

A general-purpose programming language

Paw is a high-level, statically-typed, ahead-of-time compiled, general-purpose programming language.
Currently, the frontend is written in C and the LLVM interface in C++.

## Features
+ Static strong typing
+ AOT compiled using LLVM
+ Bidirectional type checking
+ Block expressions
+ Module system
+ Exhaustive pattern matching and sum types
+ Traits (interfaces checked at compile time)
+ Generics and generic bounds
+ Unboxed objects using "inline" keyword
+ "inout" function parameters
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
    list ++= [2, 3, 4]; // [1, 2, 3, 4]


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

### Data types
Paw provides 2 mechanisms for creating custom data types: `struct` and `enum`.
Structures (structs) are nominal composite types created using the `struct` keyword. 
Enumerations (enums) are sum types (tagged unions) created using the `enum` keyword. 
Enums are described greater detail in [sum types](#sum-types).
In both cases, the datatype definition specifies only the data layout of the type.
Methods and associated functions can be attached using an [`impl` block](#impl-blocks).
```paw
struct Statistic {
    pub name: str, // accessible from anywhere
    value: float, // only accessible from a method
}
```

### Sum types
Sum types in Paw consist of tagged unions created by an `enum` definition.
Conceptually, an enum is an object that takes the value of one of its variants depending on the value of the discriminant field.
In the example below, an instance of `Expr` must contain space for an integer large enough to distinguish 3 variants, as well as space for the largest of the possible variants (`Expr::Add` here).
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

### Implementations
Methods and associated functions can be defined on a data type using an `impl` block.


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

    println("value = \{value}"); // value = 123
}
```

### Inout parameters
Function arguments are normally passed by value in Paw.
This behavior can be changed by using an inout parameter.
An parameter is specified inout by writing a `&` before its name.
In the example below, accesses to the `value` variable are made through a reference.
Note, however, that the reference is never allowed to escape, as this would allow memory to be accessed outside of its lifetime.
```paw
pub fn increment(&value: int) {
    value += 1;
}

pub fn main() {
    let value = 0;
    increment(value);
    assert(value == 1);
}
```

### Value types
Structures and enumerations have reference semantics by default.
The `inline` keyword can be used to give a type value semantics.
Primitives (`int`, `float`, etc.) and tuples are always value types.
Inline types can be used to reduce memory consumption in programs containing many small objects.
They can also be used to implement "newtype" wrappers with no additional runtime overhead.
Note that `inline` cannot be used on a recursive type as this would cause resulting objects to have a size of infinity.
```paw
inline struct Data<T> {
    pub value: T,
}

impl<T> Data<T> {
    // Value types can be modified using inout parameters.
    pub fn swap(&self, &rhs: Data<T>) {
        let temp = self.value;
        self.value = rhs.value;
        rhs.value = temp;
    }
}

pub fn main() {
    // "data" consists of exactly 3 integers stored on the stack or in registers
    let data = Data{value: Data{value: (1, (2, 3))}};

    // all fields are copied: "copy" is independent from "data"
    let copy = data;
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
+ [ ] ensure mangling produces a unique name
+ [ ] prevent duplicate methods across compatible inherent impl blocks
+ [ ] support operators # and [] on slices
+ [ ] make slice a builtin type (better syntax, allow creation with `container[range]` syntax, use mutability of container to determine slice mutablilty, etc.)
+ [ ] make a note about "gotcha" situations involving slices (For example, if the container a slice is referencing is modified while the slice is live, it is possible for the container to be reallocated, leaving the slice pointing to the old allocation. The GC will keep the old allocation alive via an internal pointer, so while this won't cause a "use after free", it is likely to cause unexpected behavior.)
+ [x] parameterize methods and associated functions on the type parameters of their containing impl blocks, as well as their own type parameters, then get rid of IrSignature::self member
+ [x] consider collecting generic bounds into predicate lists to be validated after the main unification routine
+ [ ] add check to make sure implemented trait methods are compatible with trait declarations
+ [ ] make sure to complain when generic params not mentioned on context of impl block. i.e. `impl<T> Trait<T> for Type {...}` is an error if `Type` has generic parameters.
+ [ ] consider using `mut` to indicate mutability and make immutable the default for locals and arguments
+ [ ] consider implementing either RAII or "defer" for cleaning up resources
+ [ ] add overflow checks for `paw_Int` operations during constant folding and codegen

## Known problems
+ These need to be converted into issues, along with some TODO comments scattered throughout the codebase...
+ Edge cases exist related to impl blocks and traits
+ Fix concatenation-assignment operator
+ Need to keep track of source-to-source mappings that result from IR transformations (e.g. when ForExpr AST node is lowered into a Loop + Match)
+ Don't throw errors in 'lex.c'. Return a token of type `TK_ERROR` and let the parser handle it. Allows for more sensible error messages.
+ Need to make sure functions/closures with a return type annotation of "!" diverge unconditionally 
    + See TODO comment in `test_error.c` `test_divergence` function
+ Need to prevent inout parameters from binding to container elements
    + If the container is modified, the pointer will point to freed memory
    + This rule must be applied transitively, e.g. modify(list[0].field) should not be allowed if list[0] is an inline type
+ Remove dependency on clang (as a linker driver) and invoke linker manually
