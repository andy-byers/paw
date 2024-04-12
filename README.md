# paw

An unobtrusive scripting language

paw is a high-level, imperative, statically-typed programming language intended for embedding into larger projects.

## Goals
+ **Correctness**: This is the most-important goal, by far.
A language that returns incorrect results can't be very useful, so of course, paw should be implemented correctly.
The language should be designed for human readability, and eliminate syntax foot-guns where possible (for example, an `if` statement without '{}' followed by 2 indented lines will not guard the second line).
paw code should never invoke undefined behavior (UB), and the C interface should be carefully documented (since, of course, it is possible to have UB there).
+ **Performance**: paw should be (relatively) fast, possibly to the detriment of portability, but never at the expense of correctness.
Being dynamically-typed and hosted, paw will never achieve the same performance as C, the statically-typed host language.
This makes interoperating with C particularly important, since it is likely users will want to call C functions to perform computationally-intensive work.
+ **Ergonomics**: paw should be easy to use, and easy to learn.

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
// short for 'let x: int = null'
let x: int

// rebind 'x' to a float (type is inferred from RHS)
let x = 6.63e-34 

// create a global string named 'g' (value may be changed by the C API)
global g: string = "hello, world!"
```

### Types
paw is statically-typed, meaning that all types must be known at compile time.
The following example demonstrates creation of the basic value types.

```
// variables without an initializer (right-hand side) are set to `null`
let b: bool
let i: int

// initializer is validated against the type annotation
let f: float = 10.0e-1 
let a: [int] = [1, 2, 3]
let m: string[int] = {'a': 1, 'b': 2}
let f: fn(): int = fn(): int {return 42}

// supports type inference on variable definitions
let b = false
let i = 40 + 2
let f = 1.0 * 2
let a = ['a', 'b', 'c']
let m = {1: 1, 2: 2}
let f = fn(): float {return 42.0}

class Class {
    value: int
    times2(a: int): int {
        return a * 2
    }
}
let instance = Class()       // Class
let method = instance.method // Class.method(int): int
```

### Scope
paw implements lexical scoping, meaning variables declared in a given block can only be referenced from within that block, or one of its subblocks.
A block begins when a '{' token is encountered that is not the start of a map literal, and ends when a matching '}' is found.
Many language constructs use blocks to create their own scope, like functions, classes, for loops, etc. 
paw also provides raw blocks for exerting finer control over variable lifetimes.
```
{
    let x = 42
} // 'x' goes out of scope here
```

### Functions
Functions are first-class in paw, which means they are treated like any other paw value.
Functions can be stored in variables, or passed as parameters to compose higher-order functions.
```
fn fib(n: int) {
    if n < 2 {
        return n
    }
    return fib(n - 2) + fib(n - 1)
}
fib(10)

// Anonymous functions:
let add = fn(a: str, b: str) {
    return a + b
}
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

### Arrays
```
// inferred as array<int>
let a = [1, 2, 3]
assert(a[:1] == [1])
assert(a[1:-1] == [2])
assert(a[-1:] == [3])
```

### Maps
```
// inferred as map<int, string>
let m = {1: 'a', 2: 'b'}
m[3] = 42
m.erase(1)

// prints 'default'
print(m.get(1, 'default'))
```

### Error handling
```
fn divide_by_0(n: int): int {
    return n / 0
}
let status = try(divide_by_0, 42)
assert(status != 0)
```

## Operators

|Precedence|Operator      |Description                               |Associativity|
|:---------|:-------------|:-----------------------------------------|:------------|
|16        |`() [] . ?`   |Call, Subscript, Member access, Null chain|Left         |
|15        |`! - ~`       |Not, Negate, Bitwise not                  |Right        |
|14        |`* / // %`    |Multiply, Divide, Integer divide, Modulus |Left         |
|13        |`+ -`         |Add, Subtract                             |Left         |
|12        |`++`          |Concatenate                               |Left         | 
|11        |`<< >>`       |Shift left, Shift right                   |Left         |
|10        |`&`           |Bitwise and                               |Left         |
|9         |`^`           |Bitwise xor                               |Left         |
|8         |<code>&#124;</code>|Bitwise or                           |Left         |
|7         |`in < <= > >=`|Inclusion, Relational comparisons         |Left         |
|6         |`== !=`       |Equality comparisons                      |Left         |
|5         |`&&`          |And                                       |Left         |
|4         |<code>&#124;&#124;</code>|Or                             |Left         |
|3         |`?:`          |Null coalesce                             |Left         |
|2         |`??::`        |Conditional                               |Right        |
|1         |`=`           |Assignment                                |Right        |

## TODO
+ Add a few things to the C API:
    + Better way to call builtin functions and methods on builtin types
    + Better API for arrays: `paw_*_item` will throw an error if the index is out of bounds
+ For loops won't work with bigint right now.
+ Finish designing things first...
    + Language features:
        + `**` (pow) operator
        + Slicing syntax
        + Spread operator, used in call expressions, assignments/let statements, and array literals
        + Multi-return/let/assign with Lua semantics?
        + Concurrency: fibers, coroutines?
+ Documentation
+ Make it fast!

## References
+ [Lua](https://www.lua.org/)
+ [MicroPython](https://micropython.org/)
+ [Crafting Interpreters](https://craftinginterpreters.com/)

