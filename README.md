# paw

> **NOTE:** This branch is being used add static typing to paw.
> The idea is to require type annotations on function parameters, and allow type inference on variables and return types.
> All types must be known at compile time, and the type system must be sound.
> All types are nullable, so the statement `let a: String` will compile, and `a` will be set to `null`. 
> That allows the `?` and `?:` operators to continue being used.
> Furthermore, `null` is not a real type anymore, so neither `let x: null`, nor `let x = null`, will compile.
> Unfortunately, static typing will make certain features unfeasible, like `load()`, and other features will need extra consideration, like variadic function parameters.
> It would also require implementing generics, since we would need them for containers.
> Eiter that, or we could have 'pseudo generics' (not sure what it's actually called) that only work for builtin containers, like `let array: [Integer] = []`, or `let map: String[UserDefinedType] = {}`.
> The new syntax will look like
```
let i: int = 1
let i = 1 -- inferred

fn f(a: int, b: int): int {
    return a + b
}

-- Field types are fixed at compile time, and fields cannot be added or removed 
class A {
    field: int
    another_field: str
    method(a: int, b: float): float {
        -- Compiler should generate a float addition, or just validate?
        return self.field + a + b
    }

    __add(y: int) {
        return A(self.field + y)
    }

    -- Overloading on parameter type would be nice for metamethods. 
    __add(y: A) {
        return A(self.field + y.field)
    }
}

-- New global variable syntax, 'let' always defines a local
-- All globals must be declared by the script. May set values from the C
-- API (only after load, and value types are fixed/values cannot be added or removed). 
global a = A() + 123

-- Fancy sum types and pattern matching would also be cool
-- Just match on type, arity (tuples), maybe keys, and unpack variables.
enum E {
    Variant,
    AnotherVariant,
    VariantWithData(String),
}

let e = E.VariantWithData('hello')
return match e {
    Variant => 'a'
    AnotherVariant => 'b'
    VariantWithData(x) => x
}
```
> Plan: first, write code that builds an AST and uses it to generate the code.
> Add support for type annotations, but don't have them do anything yet.
> Make sure we are generating the same code as before.
> Implement the type checking pass.
> Work on the VM: we should not have to deal with type errors anymore, so many things can be simplified/optimized
> Fix the API: it will have to change a decent amount to accomodate types
> Notes:
> Don't actually need to use NaN boxing anymore.
> We may be able to encode the value type in the opcode instead, being careful with the C API boundary.
> This would be great, because then paw could be somewhat more conforming to standard C (pointer stuff for NaN boxing is not portable)!
> Would need to keep type info for anything accessible from C.
> User-defined structs need to be registered before a module using one is compiled.
> We can check to make sure a struct instance has the right metamethods for the operations it is used in.

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
Nesting is not allowed in block-style comments.
```
-- line comment

-* block
   comment *-
```

### Variables
Any variable referenced in the runtime must first be declared.
Otherwise, a "name error" is raised (see the section on [error handling](#error-handling) below).
Global variables are intoduced with the `global` keyword, and locals with `let`,
Global variables can be referenced from anywhere in the program, while locals can only be referenced where they are visible (see [scope](#scope)).
Locals can be captured in a function or class definition (see [functions](#functions)).
```
-- short for 'let x: int = null'
let x: int

-- rebind 'x' to a float (type is inferred)
let x = 6.63e-34 
```

### Types
paw is dynamically-typed, meaning that variable types are determined at runtime.
Every paw value contains 2 fields: a type tag and a value (essentially a tagged union).
NaN boxing is used to pack both of these fields into 8 bytes of memory.
The following example demonstrates creation of the basic value types.
```
let _0 = null
let b = true
let i = 0x123
let f = 10.0e-1
let a = [1, 2, 3]
let m = {'a': 1, 2: 'b'}
let f = fn() {return 42}

class Class {
    times2(a: int): int {
        return a * 2
    }
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
fn fib(n: int) {
    if n < 2 {
        return n
    }
    return fib(n - 2) + fib(n - 1)
}
fib(10)

-- Anonymous functions:
let add = fn(a: str, b: str) {
    return a + b
}
```

### Classes
```
class Superclass {
    value: int
    __init(value: int) {
        -- 'self' is an implicit parameter
        self.value = value
    }
}

class Class: Superclass {
    -- metamethod for initialization: called when 'Class(x)' is encountered
    __init(value: int) {
        -- call Superclass.__init(self)
        super.__init(value) 
    }

    -- normal class method
    method() {
        return self.value
    }
}

-- create an instance
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
|`str`|`__str`|-|
|`int`|`__int`|-|
|`float`|`__float`|-|
|`bool`|`__bool`|-|
|`array`|`__array`|-|
|`map`|`__map`|-|
|`f()`|`__call`|-|
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
|`//`|`__idiv`|`__ridiv`|
|`%`|`__mod`|`__rmod`|
|`++`|`__concat`|`__rconcat`|
|`^`|`__bxor`|`__rbxor`|
|`&`|`__band`|`__rband`|
|<code>&#124;</code>|`__bor`|`__rbor`|
|`<<`|`__shl`|`__rshl`|
|`>>`|`__shr`|`__rshr`|
```

class Class {
    __init(value: int) {
        self.value = value
    }

    -- metamethod for '+': called when 'Class(lhs) + rhs' is encountered
    __add(rhs: int) {
        let value = self.value ++ rhs
        return Class(value)
    }

    -- reverse metamethod for '+': called when 'lhs + Class(rhs)' is encountered
    __radd(lhs: int) {
        let value = lhs ++ self.value
        return Class(value)
    }

    -- metamethod for '=='
    __eq(rhs: int) {
        return self.value == rhs
    }

    -- metamethod for controlling null chaining and null coalescing operators
    __null() {
        -- Return null if this object is semantically null, nonnull otherwise.
        -- If null is returned, then the expression 'x?' will return 'x' from
        -- the enclosing function (not null), and 'x ?: 123' will evaluate to
        -- '123'. If nonnull is returned, then both 'x?' and 'x ?: 123' will 
        -- evaluate to this function's return value (not 'x').
        return self.value < 0 ?? null :: self
    }
}

-- Instances of 'Class' can be equality-compared with, and added to, integers.
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
for i in 0, 10, 2 { -- start, end, step
    
}

-- Iterator 'for' loop: allows iterating over arrays and maps. If a class implements
-- both '__getitem' and '__len', then instances of that class can be used in an
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
assert(1 == s.find('ello'))
assert(-1 == s.find('Cello'))

let a = s.split(',')
assert(s == ','.join(a))
```

### Arrays
```
-- inferred as array<int>
let a = [1, 2, 3]
assert(a[:1] == [1])
assert(a[1:-1] == [2])
assert(a[-1:] == [3])
```

### Maps
```
-- inferred as map<int, string>
let m = {1: 'a', 2: 'b'}
m[3] = 42
m.erase(1)

-- prints 'default'
print(m.get(1, 'default'))
```

### Error handling
```
fn divide_by_0(n: int) {
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

