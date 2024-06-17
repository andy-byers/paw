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
Locals can also be captured in a function or method body (see [functions](#functions)).
```
// initializer (' = 0') is required
let x: int = 0

// rebind 'x' to a float (type is inferred from RHS)
let x = 6.63e-34 

// create a global string named 'g' (value may be changed by the C API)
global g: string = "hello, world!"
```

### Types
paw is statically-typed, meaning that types are bound to variables, not values.
The type of each variable must be known at compile time.
paw supports type inference on variable definitions.
The following example demonstrates creation of the basic value types.

```
// initializer is validated against the type annotation
let b: bool = true
let i: int = 123
let f: float = 10.0e-1 
let v: Vec[int] = Vec[int]{1, 2, 3}
let m: Map[string, int] = Map[string, int]{'a': 1, 'b': 2}
let f: fn() -> int = some_function

// supports type inference
let b = false
let i = 40 + 2
let f = 1.0 * 2
let a = Vec{'a', 'b', 'c'}
let m = Map{1: 1, 2: 2}
let f = some_other_function

struct Object {
    value: int
    times2(a: int) -> int {
        return a * 2
    }
}
let instance = Object()      // Object
let method = instance.times2 // fn(Object, int) -> int
```

### Scope
paw implements lexical scoping, meaning variables declared in a given block can only be referenced from within that block, or one of its subblocks.
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
Functions can be stored in variables, or passed as parameters to compose higher-order functions.
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
    method() {
        return self.value
    }
}

// create an instance
let o = Object{}
```

### Control flow
paw supports some common types of control flow.
```
// 'if-else' statement:
if i == 0 {

} else if i == 1 {

} else {

}

// Conditional (ternary) expressions:
let v = cond ?? 'then' :: 'else'

// Null chaining operator: return immediately (with None/Err) if the operand is None/Err 
// (must appear in a function that returns Option[T]/Result[T, E])
let v = maybe_none()?.field?

// 'break'/'continue' (must appear in a loop):
break
continue

// Numeric 'for' loop:
for i in 0, 10, 2 { // start, end, step
    
}

// Iterator 'for' loop: allows iterating over arrays and maps. If a struct implements
// both '__getitem' and '__len', then instances of that struct can be used in an
// iterator 'for' loop.
for v in iterable {

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
```
// function template
fn fib[T](n: T) -> T {
    if n < 2 {
        return n
    }
    return fib(n - 2) + fib(n - 1)
}

fib[int](10)

// A template has no value representation. 'func' must be explicitly 
// instantiated before it is stored in a variable.
let fib_i = fib[int]
fib_i(10)

fib(10) // infer T = int

// struct template
struct Cls[S, T] {
    a: S
    b: T
    f(s: S, t: T) -> T {
        self.a = self.a + s
        return self.b + t
    }
    // method template
    g[U](u: U) -> U {
        return u
    }
}

let c = Cls{
    a: 123, // infer S = int
    b: 'abc', // infer T = string
}
let g_i = c.g[int]
g_i(42)
c.g(123)
```

### Vectors
TODO: implement as struct template Vec[T]
```
let a = Vec{1, 2, 3} // infer T = int
assert(a[:1] == Vec {1})
assert(a[1:-1] == Vec {2})
assert(a[-1:] == Vec {3})
```

### Maps
TODO: implement as struct template Map[K, V]
```
let m = Map{1: 'a', 2: 'b'} // infer K = int, V = string
m[3] = 42
m.erase(1)

// prints 'default'
print(m.get(1, 'default'))
```

### Error handling
```
fn divide_by_0(n: int) -> int {
    return n / 0
}
let status = try(divide_by_0, 42)
assert(status != 0)
```

## Operators

|Precedence|Operator      |Description                                   |Associativity|
|:---------|:-------------|:---------------------------------------------|:------------|
|15        |`() [] . ?`   |Call, Subscript, Member access, Question mark |Left         |
|14        |`! - ~`       |Not, Negate, Bitwise not                      |Right        |
|13        |`* / %`       |Multiply, Divide, Modulus                     |Left         |
|12        |`+ -`         |Add, Subtract                                 |Left         |
|11        |`++`          |Concatenate                                   |Left         | 
|10        |`<< >>`       |Shift left, Shift right                       |Left         |
|9         |`&`           |Bitwise and                                   |Left         |
|8         |`^`           |Bitwise xor                                   |Left         |
|7         |<code>&#124;</code>|Bitwise or                               |Left         |
|6         |`in < <= > >=`|Inclusion, Relational comparisons             |Left         |
|5         |`== !=`       |Equality comparisons                          |Left         |
|4         |`&&`          |And                                           |Left         |
|3         |<code>&#124;&#124;</code>|Or                                 |Left         |
|2         |`??::`        |Conditional                                   |Right        |
|1         |`=`           |Assignment                                    |Right        |

## Roadmap
+ [x] static typing
+ [ ] pattern matching (`match` construct)
+ [ ] pattern matching (`let` bindings)
+ [ ] sum types/discriminated unions (`enum`)
+ [ ] product types (tuple)
+ [ ] nested templates
+ [ ] custom garbage collector
+ [ ] associated types on `struct`s (`A::B`)
+ [ ] metamethods
+ [ ] existential types

## References
+ [Lua](https://www.lua.org/)
+ [MicroPython](https://micropython.org/)
+ [Crafting Interpreters](https://craftinginterpreters.com/)

