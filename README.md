# paw

> NOTE: Paw is under active development and is not ready for production use.
> See the [roadmap](#roadmap) to get an idea of where things are going.
> Also see [known issues](#known-issues) for a list of known problems that will eventually be fixed.

> This branch is being used to integrate a new register allocator, and to convert the MIR to SSA form.
> The new register allocator performs a linear scan over the live intervals generated in a previous step.

## Timeline
+ [ ] For loops 
    + The Lua-style for loops aren't working so well with the new register allocator, possibly transform into a while loop.
+ [ ] Upvalues
    + Each "version" of a variable captured in a closure must be placed in the same physical register
    + The closure needs a stable address for all of its upvalues, until they are closed
    + Even if there is a lifetime hole, this register cannot be reused, otherwise the closure will see the wrong value
    + Currently, the runtime expects upvalues to be in order on the stack
    + Idea 1: Extend the lifetime of the first "version" to cover all other versions, and fill in lifetime holes
        + Not sure how to keep registers in order
    + Idea 2: Put all captured variables in a special section, right after the arguments
        + Wastes registers, but is simpler and variables stay in order

A cute little scripting language

Paw is a high-level, statically-typed, embeddable scripting language.

## Language Overview

### Comments
Paw supports both line- and block-style comments.
Nesting is not supported in block-style comments.
```paw
// line comment

/* block
   comment */
```

### Modules
In Paw, toplevel declarations are called 'items'.
Items can be accessed from anywhere within the module in which they are defined.
Items can be marked `pub` to indicate public visibility, which allows access from the outside (C or other Paw modules).
Otherwise, items are considered private to the containing module.
Items are resolved at compile-time, meaning missing definition errors cannot occur at runtime.
Only named functions, abstract data type (ADT) definitions, `impl` blocks, and compile-time constants may appear at the toplevel.

Modules that are intended to run as scripts under the bundled Paw interpreter `paw` should define an entrypoint function called `main` with signature `pub fn main(argv: [str]) -> int`.
`paw` (the builtin Paw interpreter) will look for `main` in the module's exported symbols and call it with arguments forwarded from the commandline.
When `main` is finished, its return value is passed back to the process that invoked `paw`.

### Types
Paw is statically-typed, meaning all types must be known at compile-time.
Paw is also strongly typed, meaning implicit conversions are not allowed.

The following example demonstrates creation of the basic value types.
Composite types are discussed in [tuple](#tuple), [structure](#structure), etc., below.
```paw
// initializer is validated against the type annotation
let b: bool = true;
let i: int = 123;
let f: float = 10.0e-1;
let s: str = 'abc';
let f: fn() -> int = || 123;

// type is inferred from the initializer
let b = false;
let i = 40 + 2;
let f = 1.0 * 2;
let s = 'de' + 'f';
let F = |x: int| x * 2;

// explicit type conversion operator
let b = 1 as bool;
let i = 2 + 3.4 as int;
```

The previous example showed examples of a simple kind of type inference, where the type of a variable is inferred from an initializer expression (the expression to the right of the `=`).
Paw supports a more general kind of type inference for containers and closures, where the type of each 'unknown' must be inferred before the declaration in question goes out of scope.
The main caveat here is that Paw does not yet have support for 'generic bounds', so we can't handle, for example, a closure like `|a, b| a + b` where the operator `+` imposes a bound on both `a` and `b`, restricting the types they can be filled in with.
For example:
```paw
let f = |a| a; // fn(?0) -> ?0
f(123); // infer ?0 = int
f(42);

let v = []; // [?1]
v.push('a'); // infer ?1 = str
let s: str = v[0];
```

### Variables
Any variable referenced in the runtime must first be declared, either locally or in an enclosing function (see [closures](#closures)).
Otherwise, a "name error" is raised (see the section on [error handling](#error-handling) below).
Local variables can be shadowed and 'rebound', and a global item may be shadowed by a local.
```paw
let x: int = 0;

// rebind 'x' to a float (type is inferred from initializer)
let x = 6.63e-34;
```

### Scope
Paw uses lexical scoping: variables declared in a given block can only be referenced from within that block, or one of its subblocks.
A block begins when a `{` token is encountered, and ends when the matching `}` is found.
Many language constructs use blocks to create their own scope, like functions, structures, for loops, etc. 
Explicit scoping blocks are also supported.
```paw
{
    let x = 42;
} // 'x' goes out of scope here

// Block expressions: last "expression statement" without a semicolon is the result of
// the block expression. This applies for all block constructs, including: if-else,
// match, and loops. The type of a loop is always "()".
let outer = {
    let first = 123;
    let second = 456;
    first + second
} + 789;
```

### Functions
Functions are first-class in Paw, which means they are treated like any other Paw value.
They can be stored in variables, or passed as parameters to higher-order functions.
Note that named functions can only be defined at the toplevel in Paw.
[Closures](#closures), however, may be nested arbitrarily.
```paw
fn fib(n: int) -> int {
    if n < 2 { return n; }
    fib(n - 2) + fib(n - 1)
}
```

### Closures
```paw
fn make_fib(n: int) -> fn() -> int {
    // captures 'n'
    || fib(n)
}
```

### Structures
```paw
struct Object {
    a: int,
    b: str,
}

// all fields must be initialized
let o = Object{b: 'two', a: 1};

// unit structs are written without curly braces
struct Unit;
let u = Unit;
```

### Enumerations
```paw
enum Choices {
    First,
    Second(int),
}

// unit variants are written without parenthesis
let c = Choices::First;
let c = Choices::Second(123);
```

### Control flow
Paw supports many common types of control flow.
```paw
// 'if-else' expression:
let x = if i == 0 {
    'first'
} else if i == 1 {
    'second'
} else {
    'third'
};

// Null chaining operator: return immediately (with None/Err) if the operand is None/Err 
// (must appear in a function that returns Option<T>/Result<T, E>), otherwise, unwraps
// the Option/Result
fn maybe() -> Option<int> {
    let i = maybe_none()?;
    fallible(i)
}

// 'break'/'continue' (must appear in a loop):
break;
continue;

// Numeric 'for' loop:
for i = 0, 10, 2 { // start, end[, step]
    
}

// 'while' loop:
let i = 0;
while i < 10 {
    i = i + 1;
}
```

### Pattern matching
```
pub enum Num {
    Zero,
    Succ(Num),
    Add(Num, Num),
}

pub fn eval(num: Num) -> int {
    // it is an error if the match is not exhaustive
    match num {
        Num::Zero => 0,
        Num::Succ(x) => eval(x) + 1,
        Num::Add(x, y) => eval(x) + eval(y),
    }
}

pub fn describe(target: Option<(int, Num)>) -> str {
    match target {
        Option::None => "a",
        Option::Some(
            (0, Num::Zero)
            | (1, Num::Succ(Num::Zero))
            | (2, Num::Succ(Num::Succ(Num::Zero)))
        ) => "b",
        Option::Some((x, y)) => {
            if x == eval(y) {
                return "c";
            }
            "d"
        },
    }
}
```

### Strings
```paw
let s = 'Hello, world!';
assert(s.starts_with('Hello'));
assert(s.ends_with('world!'));
assert(s[:5].ends_with('Hello'));
assert(s[-6:].starts_with('world!'));
assert(1 == s.find('ello'));
assert(-1 == s.find('Cello'));

let a = s.split(',');
assert(s == ','.join(a));
```

### Generics
Paw supports basic parametric polymorphism.
Variables with generic types must be treated generically, that is, they can only be assigned to other variables of the same type, passed to functions expecting a generic parameter, or stored in a container.
This allows each template to be type checked a single time, rather than once for each unique instantiation, and makes it easier to generate meaningful error messages.
```paw
fn map<A, B>(f: fn(A) -> B, list: [A]) -> [B] {
    let result = [];
    for a in list {
        result.push(f(a));
    }
    result
}

// infer A = float, B = int
let list = map(|f: float| f as int, [0.5, 1.5, 2.5]);
assert(list == [0, 1, 2]);

// struct template
struct Object<S, T> {
    pub a: S,
    pub b: T,
}

impl<A, B> Object<A, B> {
    // methods on all instances of Object...

    // methods without 'self', called associated functions, can be called
    // without an instance
    pub fn get_constant() -> int { 42 }
}

impl<T> Object<T, T> {
    // methods for when '.a' and '.b' have the same type...

    pub fn swap(self) {
        let t = self.a;
        self.a = self.b;
        self.b = t;
    }
}

impl Object<int, str> {
    // methods for a specific type of Object...

    pub fn equals(self, a: int, b: str) -> bool {
        a == self.a && b == self.b
    }
}

// explicit instantiation requires 'turbofish' ('::<>')
let o = Object::<float, float>{
    a: 0.99,
    b: 1.23,
};
o.swap();

// type inference is supported
let o = Object{
    a: 123,
    b: 'abc',
};

// field and method access using '.'
let a = o.a + 1;
let b = o.b + 'two';
let c = o.equals(a, b);
// o.swap not available: S != T

// call the associated function
let xyz = Object::<int, str>::get_constant();
```

### Tuples
```paw
let unit = ();
let singleton = (42,);
let pair = (true, 'abc');
let triplet = (1.0, 'two', 3);

let a = singleton.0;
let b = pair.1;
let c = triplet.2;
```

### Lists
```paw
let empty: [int] = [];

// infer T = str
let empty = [];
empty.push('a'); 

let list = [
    [[1, 2, 3], [0]],
    [[4, 5, 6], [1]], 
    [[7, 8, 9], [2]],
]

// slice syntax is supported:
let start_of_list = list[:1];
let middle_of_list = list[1:-1];
let end_of_list = list[-1:];
```

### Maps
```paw
let empty: [int: str] = [:];

// infer K = int, V = str
let empty = [:];
empty[0] = 'abc';

let map = [1: 'a', 2: 'b'];
map[3] = 42;
map.erase(1);

assert(m == [2: 'b']);

// prints 'default'
print(m.get_or(1, 'default'));
```

### Error handling
```paw
fn divide_by_0(n: int) {
    n = n / 0;
}
let status = try(divide_by_0, 42);
assert(status != 0);
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
+ [ ] error handling (`try` needs to be an operator, or we need something like a 'parameter pack' for generics to implement the `try` function)
+ [ ] `let` bindings/destructuring
+ [ ] more featureful `use` declarations: `use mod::*`, `use mod::specific_symbol`, `use mod as alias`, etc.
+ [ ] generic constraints/bounds
+ [ ] constant folding pass, maybe inlining
+ [ ] refactor user-provided allocation interface to allow heap expansion

## Known problems
+ The C API has pretty much 0 type safety
    + It may be necessary to reduce the scope of the C API somewhat
+ Compiler will allow functions that don't return a value in all code paths
    + Use MIR to ensure a value is returned in all paths
+ Pattern matching:
    + Should be an expression, not a statement (it was easier to make it a statement initially)
    + Produces a large number of local variables, may of which are only used once
        + Causes register exhaustion very quickly
        + Probably need to analyze live ranges and get rid of locals when possible
