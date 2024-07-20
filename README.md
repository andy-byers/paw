# paw

> NOTE: Paw is now statically typed!
> Some features will not work for a while (builtin functions, metamethods, C API).
> Pretty much everything is subject to change.
> See the [roadmap](#roadmap) to get an idea of where things are going.

An unobtrusive scripting language

Paw is a high-level, statically-typed programming language and runtime designed for embedding.

## Syntax Overview

### Comments
paw supports both line- and block-style comments.
Nesting is not allowed in block-style comments.
```
// line comment

/* block
   comment */
```

### Variables
Any variable referenced in the runtime must first be declared.
Otherwise, a "name error" is raised (see the section on [error handling](#error-handling) below).
Global variables are intoduced with the `global` keyword, and locals with `let`,
Both globals and locals can only be referenced where they are visible (see [scope](#scope)), but globals can only be declared at the module level.
Local variables can be shadowed and 'rebound', but globals cannot.
A global can be shadowed by a local, however.
Locals can also be captured in a function body (see [functions](#functions)).
```
// initializer (' = 0') is required
let x: int = 0

// rebind 'x' to a float (type is inferred from RHS)
let x = 6.63e-34 

// create a global string named 'g' (value may be changed by the C API)
global g: string = "hello, world!"
```

### Types
Paw is statically-typed, meaning all types must be known at compile-time.
Also note that Paw is strongly typed, meaning implicit conversion are not allowed.
Conversion operators are provided to convert between primitive types explicitly.
The following example demonstrates creation of the basic value types.
Composite types are discussed in [tuple](#tuple), [structure](#structure), etc., below.
```
// initializer is validated against the type annotation
let b: bool = true
let i: int = 123
let f: float = 10.0e-1 
let s: string = 'abc'
let v: [int] = [1, 2, 3]
let m: [string: int] = ['a': 1, 'b': 2]
let f: fn() -> int = some_function

// type is inferred from the initializer
let b = false
let i = 40 + 2
let f = 1.0 * 2
let s = 'de' + 'f'
let v = ['a', 'b', 'c']
let m = [1: 1, 2: 2]
let F = some_other_function

let b = 1 as bool
let i = 2 + 3.4 as int

struct Object {
    value: int
}
// all fields must be initialized
let obj = Object{value: 42}
```

### Scope
Paw uses lexical scoping, meaning variables declared in a given block can only be referenced from within that block, or one of its subblocks.
A block begins when a '{' token is encountered, and ends when a matching '}' is found.
Many language constructs use blocks to create their own scope, like functions, structures, for loops, etc. 
Explicit scoping blocks are also supported.
```
{
    let x = 42
} // 'x' goes out of scope here
```

### Functions
Functions are first-class in paw, which means they are treated like any other paw value.
They can be stored in variables, or passed as parameters to higher-order functions.
```
fn fib(n: int) -> int {
    if n < 2 {
        return n
    }
    return fib(n - 2) + fib(n - 1)
}
fib(10)
```

### Structures
```
struct Object {
    a: int
    b: string
}

// all fields must be initialized
let o = Object{
    a: 1,
    b: 'two',
}
```

### Enumerations
```
enum Choices {
    First,
    Second(int),
}

let c = Choices::Second(123)
```

### Control flow
paw supports some common types of control flow.
```
// 'if-else' statement:
if i == 0 {

} else if i == 1 {

} else {

}

// Null chaining operator: return immediately (with None/Err) if the operand is None/Err 
// (must appear in a function that returns Option<T>/Result<T, E>), otherwise, unwraps
// the Option/Result
let v = maybe_none()?.field

// 'break'/'continue' (must appear in a loop):
break
continue

// Numeric 'for' loop:
for i in 0, 10, 2 { // start, end[, step]
    
}

// 'while' loop:
let i = 0
while i < 10 {
    i = i + 1
}

// 'do...while' loop:
let i = 10
do {
    i = i - 1
} while i > 0
```

### Strings
```
let s = 'Hello, world!'
assert(s.starts_with('Hello'))
assert(s.ends_with('world!'))
assert(s[:5].ends_with('Hello'))
assert(s[-6:].starts_with('world!'))
assert(1 == s.find('ello'))
assert(-1 == s.find('Cello'))

let a = s.split(',')
assert(s == ','.join(a))
```

### Generics
Paw supports basic parametric polymorphism.
Variables with generic types must be treated generically, that is, they can only be assigned to other variables of the same type, passed to functions expecting a generic parameter, or stored in a container.
This allows each template to be type checked a single time, rather than once for each unique instantiation, and makes it easier to generate meaningful error messages.
```
fn map<A, B>(f: fn(A) -> B, vec: [A]) -> [B] {
    let result: [B] = []
    for a in vec {
        result.push(f(a))
    }
    return result
}
fn float2int(value: float) -> int {
    return int(value)
}

// infer A = float, B = int
let vec = map(float2int, [0.5, 1.5, 2.5])
assert(vec == [0, 1, 2])

// struct template
struct Object<S, T> {
    a: S
    b: T
}

// explicit instantiation uses 'turbofish'
let o = Object::<bool, float>{
    a: false,
    b: 1.23,
}

let o = Object{
    a: 123, // infer S = int
    b: 'abc', // infer T = string
}
// field access using '.'
let a = o.a + 1
let b = o.b + 'two'
```

### Tuples
```
let unit = ()
let singleton = (42,)
let pair = (true, 'abc')
let triplet = (1.0, 'two', 3)

let a = singleton.0
let b = pair.1
let c = triplet.2
```

### Vectors
```
let empty: [int] = []

let empty = []
empty.push('a')

let vec = [
    [[1, 2, 3], [0]],
    [[4, 5, 6], [1]], 
    [[7, 8, 9], [2]],
]

// infer T = int
let vec = [1, 2, 3] 
assert(vec[:1] == [1])
assert(vec[1:-1] == [2])
assert(vec[-1:] == [3])
```

### Maps
```
let empty: [int: string] = [:]

let empty = [:]
empty[0] = 'abc'

// infer K = int, V = string
let map = [1: 'a', 2: 'b'] 
map[3] = 42
map.erase(1)

assert(m == [2: 'b'])

// prints 'default'
print(m.get_or(1, 'default'))
```

### Error handling
```
fn divide_by_0(n: int) {
    n = n / 0
}
let status = try(divide_by_0, 42)
assert(status != 0)
```

## Operators

|Precedence|Operator      |Description                                   |Associativity|
|:---------|:-------------|:---------------------------------------------|:------------|
|14        |`() [] . ?`   |Call, Subscript, Member access, Question mark |Left         |
|13        |`! - ~ #`     |Not, Negate, Bitwise not, length              |Right        |
|12        |`as`          |Cast                                          |Left         |
|11        |`* / %`       |Multiply, Divide, Modulus                     |Left         |
|10        |`+ -`         |Add, Subtract                                 |Left         |
|9         |`<< >>`       |Shift left, Shift right                       |Left         |
|8         |`&`           |Bitwise and                                   |Left         |
|7         |`^`           |Bitwise xor                                   |Left         |
|6         |<code>&#124;</code>|Bitwise or                               |Left         |
|5         |`in < <= > >=`|Inclusion, Relational comparisons             |Left         |
|4         |`== !=`       |Equality comparisons                          |Left         |
|3         |`&&`          |And                                           |Left         |
|2         |<code>&#124;&#124;</code>|Or                                 |Left         |
|1         |`=`           |Assignment                                    |Right        |

## Roadmap
+ [x] static typing
+ [x] special-cased builtin containers (`[T]` and `[K: V]`)
+ [x] type inference for structure templates (including builtin containers)
+ [ ] type inference for enumeration templates
+ [ ] pattern matching (`switch` construct)
+ [ ] pattern matching (`if let`, `let` bindings)
+ [ ] constness (`var` vs `let`)
+ [ ] split `string` into `str` and `String`, where `str` is a byte array and `String` is always valid UTF-8
+ [x] sum types/discriminated unions (`enum`)
+ [x] product types (tuple)
+ [ ] custom garbage collector (using Boehm GC for now)
+ [ ] methods
+ [ ] metamethods
+ [ ] generic constraints/bounds
+ [ ] associated types on `struct`s (`A::B`)
+ [ ] existential types

## Known problems
+ Compiler will allow functions that don't return a value in all code paths
    + Likely requires a CFG and some data flow analysis: it would be very difficult to get right otherwise
+ It isn't possible to create an empty vector or map of a specific known type without creating a temporary: `let vec: [int] = []`
    + Could use Swift syntax, or something similar:
```
let empty_vec = [int]()
let empty_map = [string: int]()
```

## References
+ [Lua](https://www.lua.org/)
+ [MicroPython](https://micropython.org/)
+ [Crafting Interpreters](https://craftinginterpreters.com/)

