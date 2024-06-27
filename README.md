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
paw is statically-typed, meaning all types must be known at compile-time.
paw supports type inference on variable definitions and function template calls.
The following example demonstrates creation of the basic value types.

```
// initializer is validated against the type annotation
let b: bool = true
let i: int = 123
let f: float = 10.0e-1 
let v: Vec[int] = Vec[int]{1, 2, 3}
let m: Map[string, int] = Map[string, int]{'a': 1, 'b': 2}
let f: fn() -> int = some_function

// type is inferred from the initializer
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
paw uses lexical scoping, meaning variables declared in a given block can only be referenced from within that block, or one of its subblocks.
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
Paw supports basic parametric polymorphism.
Variables with generic types must be treated generically, that is, they can only be assigned to other variables of the same type, passed to functions expecting a generic parameter, or stored in a container.
This allows each template to be type checked a single time, rather than once for each unique instantiation, and makes it easier to generate meaningful error messages.
```
fn mapval[A, B](f: fn(A) -> B, a: A) -> B {
    return f(a)
}
fn float2int(value: float) -> int {
    return int(value)
}

// infer A = float, B = int
let i = mapval(float2int, 1.5)
assert(i == 1)

// struct template
struct Object[S, T] {
    a: S
    b: T
}

let c = Object{
    a: 123, // infer S = int
    b: 'abc', // infer T = string
}
let a = c.a + 1
let b = c.b + 'two'
```

### Vectors
```
let v = Vector{1, 2, 3} // infer T = int
assert(v[:1] == Vector{1})
assert(v[1:-1] == Vector{2})
assert(v[-1:] == Vector{3})
```

### Maps
```
let m = Map{1: 'a', 2: 'b'} // infer K = int, V = string
m[3] = 42
m.erase(1)

assert(m == Map{2: 'b'})

// prints 'default'
print(m.get(1, 'default'))
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
+ [ ] builtin containers
+ [ ] pattern matching (`match` construct)
+ [ ] pattern matching (`let` bindings)
+ [ ] sum types/discriminated unions (`enum`)
+ [ ] product types (tuple)
+ [ ] generic constraints/bounds
+ [ ] custom garbage collector
+ [ ] associated types on `struct`s (`A::B`)
+ [ ] struct methods
+ [ ] metamethods
+ [ ] existential types

## Pattern matching notes

### Match construct
Identifiers in a match guard create new variable bindings, given that they don't refer to exiting **types** (variables are shadowed).
Each identifier can be bound at most 1 time per guard (`E::X(a, b, a)` is not allowed).
A match construct eventually becomes a series of comparisons and jumps, similar to an if-else chain.
```
struct S {v: float}
enum D {A, B(string)}
enum E {X, Y(int), Z(S), W(D)}
let e = select_variant()
let v = match e {
    E::X => 0,                           
    E::Y(1) => 1,                        
    E::Y(i) => i,                        
    E::Z(S{v}) if v < 0.0 => 4,          
    E::Z(S{v: renamed}) => int(renamed), 
    E::W(D::A) => 6,                     
    E::W(D::B(s)) if #s > 1 => #s,
    E::W(d) => 8,
    _ => 9,
}
```

### Desugared code
```
let v = while 1 {
    if disc(e) == 0 {
        break 0
    }
    if disc(e) == 1 {
        if e.0 == 1 {
            break 1
        }
    }
    if disc(e) == 1 {
        let i = e.0
        break i
    }
    if disc(e) == 2 {
        let v = e.0.v
        if v < 0.0 {
            break 4
        }
    }
    if disc(e) == 2 {
        let renamed = e.0.v
        break int(renamed)
    }
    if disc(e) == 3 {
        if disc(e.0) == 0 {
            break 6
        }
    }
    if disc(e) == 3 {
        if disc(e.0) == 1 {
            let s = e.0.0
            if #s > 1 {
                break #s
            }
        }
    }
    if disc(e) == 3 {
        let d = e.0
        break 8
    }
    // TODO: exhaustiveness check
    break 9
}
```

### Desugared code with merged cases
Cases can be moved around, but cannot 'cross' another case selecting the same variant, otherwise the semantics of the program might be changed.
```
let v = while 1 {
    if disc(e) == 0 {
        break 0
    }
    if disc(e) == 1 {
        if e.0 == 1 {
            break 1
        } else {
            let i = e.0
            break i
        }
    }
    if disc(e) == 2 {
        { // careful about scope. although, it may not matter too much if we have already resolved variable accesses
            let v = e.0.v
            if v < 0.0 {
                break 4
            }
        }
        let renamed = e.0.v
        break int(renamed)
    }
    if disc(e) == 3 {
        if disc(e.0) == 0 {
            break 6
        } else if disc(e.0) == 1 {
            let s = e.0.0
            if #s > 1 {
                break #s
            }
        } else {
            break 8
        }
    }
    // TODO: exhaustiveness check
    break 9
}
```


### Possible opcodes
```
  OP_COPY
  OP_MATCHVARIANT(0)
  OP_JUMPFALSE('next1') // patch locally
  OP_POP(1)
  OP_PUSHCONST(0)
  OP_TRANSIT(1)
  OP_JUMP('out') // save until the end, in label list
next1:
  OP_COPY
  OP_MATCHVARIANT(1)
  OP_JUMPF('next2')
  OP_POP(1)
  OP_GETFIELD(0)
  OP_PUSHCONST(1)
  OP_BINARY('==')
  OP_JUMPF('next2')
  OP_POP(1)
  OP_PUSHCONST(1)
  OP_TRANSIT(1)
  OP_JUMP('out')
next2:
  ...
out:
```

## References
+ [Lua](https://www.lua.org/)
+ [MicroPython](https://micropython.org/)
+ [Crafting Interpreters](https://craftinginterpreters.com/)

