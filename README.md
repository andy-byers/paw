# paw

> NOTE: This branch is being used to implement static typing and templates.
> It is likely to be very broken for a while.
> So far, the compiler has been rewritten to work in multiple passes. 
> Pass 1 constructs an AST (lexical + syntax analysis).
> Pass 2 is semantic analysis, where types are checked and symbol tables built.
> Template are also intantiated during pass 2.
> Pass 3 generates code (backend).

An unobtrusive scripting language

paw is a high-level, imperative, statically-typed programming language intended for embedding into larger projects.

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
let v: Vec[int] = Vec[int] {1, 2, 3}
let m: Map[string, int] = Map[string, int] {'a': 1, 'b': 2}
let f: fn() -> int = some_function

// supports type inference
let b = false
let i = 40 + 2
let f = 1.0 * 2
let a = Vec {'a', 'b', 'c'}
let m = Map {1: 1, 2: 2}
let f = some_other_function

class Class {
    value: int
    times2(a: int) -> int {
        return a * 2
    }
}
let instance = Class()       // Class
let method = instance.method // Class.method(int): int
```

### Scope
paw implements lexical scoping, meaning variables declared in a given block can only be referenced from within that block, or one of its subblocks.
A block begins when a '{' token is encountered, and ends when a matching '}' is found.
Many language constructs use blocks to create their own scope, like functions, classes, for loops, etc. 
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

### Classes
```
class Superclass {
    value: int
    __init(value: int) {
        // 'self' is an implicit parameter
        self.value = value
    }
}

class Class: Superclass {
    // metamethod for initialization: called when 'Class(x)' is encountered
    __init(value: int) {
        // call Superclass.__init(self)
        super.__init(value) 
    }

    // normal class method
    method() {
        return self.value
    }
}

// create an instance
let c = Class()
```

### Metamethods
Metamethods are how paw implements operator overloading.
A metamethod is a function bound to an instance object that is called when the object is used in a specific operation.
Metamethod names are always prefixed with 2 underscores, i.e. `__add`.

Many binary operators have a 'reverse' metamethod that is called when the receiver is not the first operand.
For example, `__radd`, the reverse metamethod for `__add` is called to evaluate the expression `1 + x`, where `x` is an instance.
It is an error is `x` does not have the `__radd` metamethod.

Relational comparisons are handled similarly to binary operators.
In the expression `1 < x`, we cannot call `__lt`, since `x` is not on the left-hand side.
Instead we attempt `x.__ge(1)` and negate the result.

|Operation|Metamethod|Reverse metamethod|
|:-------:|:--------:|:----------------:|
|-|`__null`|-|
|`string()`|`__string`|-|
|`int()`|`__int`|-|
|`float()`|`__float`|-|
|`bool()`|`__bool`|-|
|`array()`|`__array`|-|
|`map()`|`__map`|-|
|`o()`|`__call`|-|
|`o.a`|`__getattr`|-|
|`o.a = v`|`__setattr`|-|
|`o[i]`|`__getitem`|-|
|`o[i] = v`|`__setitem`|-|
|`o[i:j]`|`__getslice`|-|
|`o[i:j] = v`|`__setslice`|-|
|`==`|`__eq`|-|
|`<`|`__lt`|`__gt`|
|`<=`|`__le`|`__ge`|
|`>`|`__gt`|`__lt`|
|`>=`|`__ge`|`__le`|
|`in`|`__contains`|-|
|`#`|`__len`|-|
|`-`|`__neg`|-|
|`!`|`__not`|-|
|`~`|`__bnot`|-|
|`+`|`__add`|`__radd`|
|`-`|`__sub`|`__rsub`|
|`*`|`__mul`|`__rmul`|
|`/`|`__div`|`__rdiv`|
|`%`|`__mod`|`__rmod`|
|`^`|`__bxor`|`__rbxor`|
|`&`|`__band`|`__rband`|
|<code>&#124;</code>|`__bor`|`__rbor`|
|`<<`|`__shl`|`__rshl`|
|`>>`|`__shr`|`__rshr`|
```

class Class {
    value: int

    __init(value: int) {
        self.value = value
    }

    // metamethod for '+': called when 'Class(lhs) + rhs' is encountered
    __add(rhs: int) {
        let value = self.value ++ rhs
        return Class(value)
    }

    // reverse metamethod for '+': called when 'lhs + Class(rhs)' is encountered
    __radd(lhs: int): Class {
        let value = lhs ++ self.value
        return Class(value)
    }

    // metamethod for '=='
    __eq(rhs: int): Class {
        return self.value == rhs
    }

    // metamethod for controlling null chaining and null coalescing operators
    __null(): Class {
        // Return null if this object is semantically null, nonnull otherwise.
        // If null is returned, then the expression 'x?' will return 'x' from
        // the enclosing function (not null), and 'x ?: 123' will evaluate to
        // '123'. If nonnull is returned, then both 'x?' and 'x ?: 123' will 
        // evaluate to this function's return value (not 'x').
        return self.value < 0 ?? null :: self
    }
}

// Instances of 'Class' can be equality-compared with, and added to, integers.
assert(3 == Class(1) + 2)
assert(3 == 1 + Class(2))
```

### Control flow
paw supports many common types of control flow.
```
// 'if-else' statement:
if i == 0 {

} else if i == 1 {

} else {

}

// Conditional (ternary) expressions:
let v = cond ?? 'then' :: 'else'

// Null chaining operator: return immediately (with null) if the operand is null 
// A paw module is actually considered a function, so '?' can exist at the top
// level. 
let v = maybe_null()?.field?

// Null coalescing operator: evaluates to the first operand if it is nonnull, the
// second operand otherwise. The second expression is not evaluated if the first
// operand is null.
let v = a ?: b

// 'break'/'continue' (must appear in a loop):
break
continue

// Numeric 'for' loop:
for i in 0, 10, 2 { // start, end, step
    
}

// Iterator 'for' loop: allows iterating over arrays and maps. If a class implements
// both '__getitem' and '__len', then instances of that class can be used in an
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
// instantiated before it is stored in a variable (there are no 
// arguments from which to infer the type parameters).
let fib_i = fib[int]
fib_i(10)

fib(10) // infer T = int

// class template
class Cls[S, T] {
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

let c = Cls {
    a: 123, // infer S = int
    b: 'abc', // infer T = string
}
let g_i = c.g[int]
g_i(42)
c.g(123)
```

### Vectors
TODO: implement as class template Vec[T]
```
let a = Vec {1, 2, 3} // infer T = int
assert(a[:1] == Vec {1})
assert(a[1:-1] == Vec {2})
assert(a[-1:] == Vec {3})
```

### Maps
TODO: implement as class template Map[K, V]
```
let m = Map {1: 'a', 2: 'b'} // infer K = int, V = string
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
|16        |`() [] . ?`   |Call, Subscript, Member access, Question mark |Left         |
|15        |`! - ~`       |Not, Negate, Bitwise not                      |Right        |
|14        |`* / %`       |Multiply, Divide, Modulus                     |Left         |
|13        |`+ -`         |Add, Subtract                                 |Left         |
|12        |`++`          |Concatenate                                   |Left         | 
|11        |`<< >>`       |Shift left, Shift right                       |Left         |
|10        |`&`           |Bitwise and                                   |Left         |
|9         |`^`           |Bitwise xor                                   |Left         |
|8         |<code>&#124;</code>|Bitwise or                               |Left         |
|7         |`in < <= > >=`|Inclusion, Relational comparisons             |Left         |
|6         |`== !=`       |Equality comparisons                          |Left         |
|5         |`&&`          |And                                           |Left         |
|4         |<code>&#124;&#124;</code>|Or                                 |Left         |
|3         |`?:`          |Coalesce                                      |Left         |
|2         |`??::`        |Conditional                                   |Right        |
|1         |`=`           |Assignment                                    |Right        |

## TODO
+ Known defects that need to be fixed!
    + The compiler has difficulty distinguishing between instances of a `struct` and the `struct` itself
        + Code like `let a = B.c` may cause problems
+ Test bad syntax, and get the fuzzer to work again
+ Write a new garbage collector...
    + First, get the project to work with a third party GC
    + I feel like it will be painful to try and maintain bookkeeping info about what `Value`s are pointers
    + Doing so would be required if we want the GC to know what objects it should try to collect
    + The other option, and one I feel would be more feasible, would be to keep track of what regions of memory are in use.
    + When the GC is scanning, it can check to see if a value looks like a pointer (a numeric value that seemingly references some place in an allocated block)
    + If so, that block can be marked (this allows false positives, but never false negatives, and false positives should only cause memory to not be freed when it actually can be)
    + This seems to be what general-purpose GCs do, and also some GCs for statically-typed languages, like Go
    + Seems to assume that heap allocations come from the high end of the address space, which is usually the case, but is definitely not guaranteed
+ Error on missing return type
+ Implement the rest of the builtin types: tuple and enum (maybe array), and the builtin classes: Vector, etc. 
+ When values are guaranteed to stay within the paw runtime (not exposed to C), we can elide some allocations by reserving more than 1 stack slot
    + For example, a tuple `(int, float)` can just be 2 slots, 1 for an `int` and the other for a `float`
    + The compiler builds an array of locals as it performs the codegen pass, and each local variable description can store how many slots it occupies, and maybe its starting slot number
    + The tuple is still treated like a single object, from the user's point-of-view, and code like `t[1]` is translated to an `OP_GETLOCAL` from the proper stack slot
    + Allows an `Option[int]` that doesn't result in a heap allocation (`Option[object]` uses an unused pointer bit for its discriminator)
+ `?` and `?:` should work on `Some(...)` and `None` variants of `Option` enumerator
+ Allow some type params to be specified explicitly and some to be inferred in a given call or composite literal
+ Clean up the compiler code
+ Redesign the C API
+ Documentation
+ Make it fast!

## References
+ [Lua](https://www.lua.org/)
+ [MicroPython](https://micropython.org/)
+ [Crafting Interpreters](https://craftinginterpreters.com/)

