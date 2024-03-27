# paw

An unobtrusive scripting language

paw is a high-level, imperative, dynamically-typed programming language intended for embedding into larger projects.

paw is heavily inspired by the Lua project.
This can be seen in the language syntax, as well as the design of the VM.
Like the Lua VM, paw's VM is reentrant, meaning a C function called from paw can call other paw functions.
paw also restricts the language grammer to eliminate the need for semicolons (most-notably: assignments are **not** expressions).
Additionally, paw uses a quick single-pass compiler and is easy to embed.

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
Block comments do not support nesting.
```
-- line comment

-* block
   comment *-
```

### Variables
Any variable referenced in the runtime must first be declared: either by a `let` statement, or with the C API.
Otherwise, a "name error" is raised (see the section on [error handling](#error-handling) below).
Variables declared at the module level are considered global variables, otherwise, they are locals.
Global variables can be referenced from anywhere in the program, while locals can only be referenced where they are visible (see [scope](#scope)).
Locals can be captured in a function or class definition (see [functions](#functions)).
```
let x       -- short for 'let x = null'
let x = 123 -- rebind 'x' to 123
```

### Types
paw is dynamically-typed, meaning that variable types are determined at runtime.
A variable initialized with a value of one type may have a value of any other type assigned to it, at any point during execution.
paw tries to make reasoning about programs a bit easier by throwing errors when incompatible types are used in an operation, rather than performing implicit conversions.
For example, attempting a bitwise operation on a float will always result in a type error.
Binary arithmetic operators, other than `/` and `//`, can be used on mixed numeric types (e.g. float and integer), and will result in a float if one of the operands is a float, and an integer otherwise.

Every paw value contains 2 fields: a type tag and a value (essentially a tagged union).
NaN boxing is used to pack both of these fields into 8 bytes of memory.
The following example demonstrates creation of the basic value types.
```
let null_ = null
let boolean = true
let integer = 0x123
let float_ = 10.0e-1
let array = [1, 2, 3]
let map = {'a': 1, 2: 'b'}
let function = fn() {return 42}

class Class {
    method(self) {}
}
let instance = Class()
let method = instance.method
```

### Scope
paw implements lexical scoping, meaning variables declared in a given block can only be referenced from within that block, or one of its subblocks.
A block begins when a '{' token is encountered that is not the start of a map literal, and ends when a matching '}' is found.
Many language constructs use blocks to create their own scope, like functions, classes, for loops, etc. 
paw also provides raw blocks for exerting finer control over variable lifetimes.
```
{
    let x = 42
} -- 'x' goes out of scope here
```

### Functions
Functions are first-class in paw, which means they are treated like any other paw value.
Functions can be stored in variables, or passed as parameters to compose higher-order functions.
```
fn fib(n) {
    if n < 2 {
        return n
    }
    return fib(n - 2) + fib(n - 1)
}
fib(10)

-- Anonymous functions:
let add = fn(a, b) {
    return a + b
}
```

### Classes
```
class Superclass {
    __init(value) {
        -- 'self' is an implicit parameter
        self.value = value
    }
}

class Class: Superclass {
    -- metamethod for initialization: called when 'Class(x)' is encountered
    __init(value) {
        -- call Superclass.__init(self)
        super.__init(value) 
    }

    -- normal class method
    method() {
        return self.value
    }

    -- metamethod for '+': called when 'Class(lhs) + rhs' is encountered
    __add(rhs) {
        let value = self.value ++ rhs
        return Class(value)
    }

    -- reverse metamethod for '+': called when 'lhs + Class(rhs)' is encountered
    __radd(lhs) {
        let value = lhs ++ self.value
        return Class(value)
    }

    -- metamethod for '=='
    __eq(rhs) {
        return self.value == rhs
    }
}

-- Instances of 'Class' can be equality-compared with, and added to, numeric
-- values.
assert(3 == Class(1) + 2)
assert(3 == 1 + Class(2))
```

### Control flow
paw supports many common types of control flow.
```
-- 'if-else' statement:
if i == 0 {

} else if i == 1 {

} else {

}

-- Conditional (ternary) expressions:
let v = cond ?? 'then' :: 'else'

-- Null chaining operator: return immediately (with null) if the operand is null 
-- A paw module is actually considered a function, so '?' can exist at the top
-- level.
let v = maybe_null()?.field?

-- Null coalescing operator: evaluates to the first operand if it is nonnull, the
-- second operand otherwise. The second expression is not evaluated if the first
-- operand is null.
let v = a ?: b

-- 'break'/'continue' (must appear in a loop):
break
continue

-- Numeric 'for' loop:
for i in 0,10,2 { -- start,end,step
    
}

-- Iterator 'for' loop: allows iterating over arrays and maps. If a class implements
-- both '__getattr' and '__len', then instances of that class can be used in an
-- iterator 'for' loop.
for v in iterable {

}

-- 'while' loop:
let i = 0
while i < 10 {
    i = i + 1
}

-- 'do...while' loop:
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
```

### Arrays
```
let a = [1, 2, 3]
assert(a[:1] == [1])
assert(a[1:-1] == [2])
assert(a[-1:] == [3])
```

### Maps
```

```

### Error handling
```
fn divide_by_0(n) {
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

