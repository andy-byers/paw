# paw

An embeddable scripting language

paw is heavily inspired by the Lua project.
This can be seen in the scripting language syntax, as well as the design of the virtual machine (VM).
Like the Lua VM, paw's VM is reentrant, meaning a C function called from paw can call other paw functions.
paw also restricts the language grammer to eliminate the need for semicolons (most-notably: assignments are **not** expressions).
Additionally, paw uses a quick single-pass compiler.

## Goals
+ No undefined behavior in paw:
    + Overflowing signed integers become multi-precision integers
    + Container bounds are always checked
    + Limit implementation defined behavior wherever possible

The goal of the paw project is to create a nice, modern scripting language that can be easily embedded into C or C++ projects.
While there are many such languages already, I thought it would be fun to create my own.
The project was bootstrapped by following [Crafting Interpreters](https://craftinginterpreters.com/), and looking at Lua source code.
So far we have mostly everything implemented from part 2 of the book (the stack-based VM part), as well as an array type, a map type (unordered), bigints, simple operator overloading (metamethods, with reverse versions), `break`, `continue`, `for i = start,end,step` loops, `for x in container` loops, builtin functions, and more!
Currently, the code is pretty messy and unstable: it has been changing quite a bit during prototyping.
Right now, I'm working on stabilizing the design, and ironing out a few more language features (slice syntax, variable unpacking, spread operator, etc.).
Once that happens, this readme will get some attention.
See the [TODO](#todo) section for a list of things that need to be worked on.

The syntax is inspired by the Lua language: semicolons are optional and whitespace is not significant.

The 2 most important rules to keep in mind are:
1. paw statements are not arbitrary expressions, like they are in many languages. 
In paw, an 'expression statement' must be either a function call or an assignment.
This rule forbids a lot of ambiguous syntax, but is a bit restrictive for metamethods ('~x' may have side-effects, but cannot appear by itself on a line).
2. If a `return` statement is present in a given function, then it must be the last statement in its containing block. 
Lua enforces this rule for `break` statements as well, but paw does not (Lua doesn't have `continue`). 
Any code after such statements is by-definition dead code anyway, but it can be useful to insert `break` and `continue` statements in various places during debugging.

## Syntax Overview
```
-- Comments start with '--'. Only line comments are supported.

-- Variable declaration/definition. It is an error to reference a variable that
-- hasn't been declared using a 'let' statement. Variables declared at the module
-- level are considered global variables, otherwise they are locals. 
let x       -- Short for 'let x = null'
let x = 123 -- Rebind 'x' to 123

-- Scoping block. paw has lexical scoping, meaning variables declared in a given
-- block can only be referenced from within that block, or one of its subblocks. 
{
    let x = 42 -- Shadows 'x' from earlier
} -- 'x' goes out of scope here


-- Functions:
fn example(a, b, c) {
    -- 'return' must be the last statement in the block
    return a + b + c
}

-- Anonymous functions:
let example = fn(a) {
    return a
}

-- Classes:
class Example: Superclass {
    -- Metamethod for initialization: called when 'Example(x)' is encountered
    __init(v) {
        self.v = v
    }

    -- Normal class method
    method() {
        return self.v
    }

    -- Metamethod for '+': called when 'Example(x) + y' is encountered
    __add(y) {
        let v = self.v ++ y
        return Example(v)
    }

    -- Reverse metamethod for '+': called when 'x + Example(y)' is encountered
    __radd(x) {
        let v = x ++ self.v
        return Example(v)
    }
}

-- 'if-else' statement:
if i == 0 {

} else if i == 1 {

} else {

}

-- Conditional (ternary) expressions:
let v = cond ?? 'then' : 'else'

-- Null chaining operator: return immediately (with null) if the operand itself 
-- is null. A paw module is actually considered a function, so '?' can exist at
-- the top level.
let v = maybe_null()?.field?

-- Null coalescing operator: evaluates to 'a' if the 'a' is nonnull, 'b' otherwise.
-- 'b' is not evaluated if 'a' is nonnull.
let v = a ?: b

-- 'break'/'continue' (must appear in a loop):
break
continue

-- Numeric 'for' loop:
for i in 0,10,2 { -- start,end,step
    
}

-- Iterator 'for' loop:
for v in iterable {

}

-- 'while' loop:
let i = 0
while i < 10 {

}

-- 'do...while' loop:
let i = 0
do {

} while i

-- Example:

-- Variables declared at module scope are registered as globals.
let genfib = fn() {
    -- 'memo' is a local in this anonymous function. It is captured
    -- in the 'fib' closure. Note that anonymous functions can also
    -- capture variables.
    let memo = [0, 1]
    fn fib(n) {
        if n < #memo { -- '#' is the 'length' operator
            return memo[n]
        }
        let f = fib(n - 2) + fib(n - 1)
        memo.push(f)
        return f
    }
    return fib
}

let f = genfib()
assert(f(0) == 0)
assert(f(1) == 1)
assert(f(2) == 1)
assert(f(3) == 2)
assert(f(4) == 3)
```

## Operators

|Precedence|Operator      |Description                               |Associativity|
|:---------|:-------------|:-----------------------------------------|:------------|
|16        |`() [] . ?`   |Call, Subscript, Member access, Null chain|Left         |
|15        |`! - ~`       |Not, Negate, Bitwise not                  |Right        |
|14        |`* / // %`    |Multiply, divide, integer divide, modulus |Left         |
|13        |`+ -`         |Add, Subtract                             |Left         |
|12        |`++`          |Concatenate                               |Left         | 
|11        |`<< >>`       |Shift left, shift right                   |Left         |
|10        |`&`           |Bitwise and                               |Left         |
|9         |`^`           |Bitwise xor                               |Left         |
|8         |`|`           |Bitwise or                                |Left         |
|7         |`in < <= > >=`|Inclusion, relational comparisons         |Left         |
|6         |`== !=`       |Equality comparisons                      |Left         |
|5         |`&&`          |And                                       |Left         |
|4         |`||`          |Or                                        |Left         |
|3         |`?:`          |Null coalesce                             |Left         |
|2         |`??::`        |Conditional                               |Right        |
|1         |`=`           |Assignment                                |Right        |

## TODO
+ Add a few things to the C API:
    + Better way to call builtin functions and methods on builtin types
    + Better API for arrays: `paw_*_item` will throw an error if the index is out of bounds
    + Put a few functions in paw.h that perform operations on paw values (`paw_arith`, `paw_bitwise`, `paw_compare`, which can just call the runtime routines)
+ Need some sort of restriction on when methods can be added to a class
The compiler will get confused if 'self' or 'super' is used outside of a class body.
+ 'for i = x,y,z' loops: should we prevent 'x,y,z' from being floats?
For loops using floats often produce unexpected results: it's probably better to use an int loop counter and just multiply by a float constant as needed
Also, for loops won't work with bigint right now.
+ Finish designing things first...
    + Language features:
        + `**` (pow) operator
        + Slicing syntax
        + Spread operator, used in call expressions, assignments/let statements, and array literals
        + Multi-return/let/assign with Lua semantics?
        + Concurrency: fibers, coroutines?
+ Get the fuzzer (which fuzzes source files) to work
+ Documentation
+ Big integer math needs work (need to convert back to `paw_Int` when possible)
+ Make it fast!
