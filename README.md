# paw

> NOTE: Paw is under active development and is not ready for production use.
> See the [roadmap](#roadmap) to get an idea of where things are going.
> Also see [known issues](#known-issues) for a list of known problems that will eventually be fixed.

An expressive scripting language

Paw is a high-level scripting language and runtime designed for embedding.
Paw features static strong typing under a sound type system, generics, and bidirectional type checking.
Paw is an interpreted language that runs on a stack-based virtual machine written in C.

## Language Overview

### Comments
Paw supports both line- and block-style comments.
Nesting is not supported in block-style comments.
```
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
```
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
The main caveat here is that Paw does not yet have support for 'generic bounds', so we can't handle, for example, a closure like `|a, b| a + b` where the operator `+` imposes a bound on both `a` and `b`, restricting the types they can be instantiated with.
For example:
```
let f = |a| a; // fn(?0) -> ?0
f(123); // infer ?0 = int
f(42);

let v = []; // [?1]
v.push('a'); // infer ?1 = str
let s: str = v[0];
```

### Variables
Any variable referenced in the runtime must first be declared (all variables are locals, see [Modules](#modules) above).
Otherwise, a "name error" is raised (see the section on [error handling](#error-handling) below).
Local variables can be shadowed and 'rebound', and a global item may be shadowed by a local.
Locals can also be captured in the body of a closure (see [closures](#closures)).
```
// initializer (' = 0') is required
let x: int = 0;

// rebind 'x' to a float (type is inferred from initializer)
let x = 6.63e-34;
```

### Scope
Paw uses lexical scoping: variables declared in a given block can only be referenced from within that block, or one of its subblocks.
A block begins when a `{` token is encountered, and ends when the matching `}` is found.
Many language constructs use blocks to create their own scope, like functions, structures, for loops, etc. 
Explicit scoping blocks are also supported.
```
{
    let x = 42;
} // 'x' goes out of scope here
```

### Functions
Functions are first-class in Paw, which means they are treated like any other Paw value.
They can be stored in variables, or passed as parameters to higher-order functions.
Note that named functions can only be defined at the toplevel in Paw.
[Closures](#closures), however, may be nested arbitrarily.
```
fn fib(n: int) -> int {
    if n < 2 {
        return n;
    }
    return fib(n - 2) + fib(n - 1);
}
```

### Closures
```
fn make_fib(n: int) -> fn() -> int {
    // captures 'n'
    return || fib(n);
}
```

### Structures
```
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
```
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
```
// 'if-else' statement:
if i == 0 {

} else if i == 1 {

} else {

}

// Null chaining operator: return immediately (with None/Err) if the operand is None/Err 
// (must appear in a function that returns Option<T>/Result<T, E>), otherwise, unwraps
// the Option/Result
fn maybe() -> Option<int> {
    let i = maybe_none()?;
    return fallible(i);
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

// 'do...while' loop:
let i = 10;
do {
    i = i - 1;
} while i > 0;
```

### Strings
```
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
```
fn map<A, B>(f: fn(A) -> B, list: [A]) -> [B] {
    let result = [];
    for a in list {
        result.push(f(a));
    }
    return result;
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
}

impl<T> Object<T, T> {
    // methods for when '.a' and '.b' have the same type...

    // For example:
    pub fn swap(self) {
        let t = self.a;
        self.a = self.b;
        self.b = t;
    }
}

impl Object<int, str> {
    // methods for this specific type of Object...

    // For example:
    pub fn equals(self, a: int, b: str) -> bool {
        return a == self.a && b == self.b;
    }

    // methods without 'self', called associated functions, can be called
    // without an instance
    pub fn get_constant() -> int {
        return 42;
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
    a: 123, // infer S = int
    b: 'abc', // infer T = str
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
```
let unit = ();
let singleton = (42,);
let pair = (true, 'abc');
let triplet = (1.0, 'two', 3);

let a = singleton.0;
let b = pair.1;
let c = triplet.2;
```

### Lists
```
let empty: [int] = [];

// infer T = str
let empty = [];
empty.push('a'); 

let list = [
    [[1, 2, 3], [0]],
    [[4, 5, 6], [1]], 
    [[7, 8, 9], [2]],
]

let list = [1, 2, 3];
assert(list[:1] == [1]);
assert(list[1:-1] == [2]);
assert(list[-1:] == [3]);
```

### Maps
```
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
```
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
+ [x] special-cased builtin containers (`[T]` and `[K: V]`)
+ [x] type inference for polymorphic `fn` and `struct`
+ [x] sum types/discriminated unions (`enum`)
+ [x] product types (tuple)
+ [x] custom garbage collector (using Boehm GC for now)
+ [x] methods using `impl` blocks
+ [x] module system and `use` keyword
+ [ ] error handling (`try` needs to be an operator, or we need something like a 'parameter pack' for generics to implement the `try` function)
+ [ ] pattern matching (`switch` construct)
+ [ ] pattern matching (`if let`, `let` bindings)
+ [ ] type inference for polymorphic `enum`
+ [ ] more featureful `use` declarations: `use mod::*`, `use mod::specific_symbol`, `use mod as alias`, etc.
+ [ ] generic constraints/bounds
+ [ ] compiler optimization passes
+ [ ] refactor user-provided allocation interface to allow heap expansion

## Known problems
+ The C API has pretty much 0 type safety
+ Compiler will allow functions that don't return a value in all code paths
    + Likely requires a CFG and some data flow analysis: it would be very difficult to get right otherwise
+ Selector on nested tuple breaks the lexer (looks like a float: `t.0.1`)
    + Could resolve with an extra lexing pass, or use index expression (`t[x][y]`, where `x` and `y` are compile-time constant expressions)
    + Similarly, we can't write things like `1.to_string()`: we require parenthesis around the `1`
+ Need to prevent situations where 2 methods with the same name are accessible from the same type
    + Complicated by the fact that impl blocks can either target a polymorphic ADT, or an instantiation thereof
    + In the first case, methods are available to all instantiations of the ADT, while in the second case they are available only to that particular instance.
    + The second case allows us to specialize the body of a method for each specific type of instance.
    + Could rework the code that checks if a method can be called on a given type: just check to see if a method exists already before registering/resolving it
    + Probably don't want to resolve/instantiate things while searching, which is what the code currently does
+ May create multiple HIR decl. nodes for builtin container instantiations
    + Builtin containers (`[T]` and `[K: V]`) are ADTs that support type inference on their generic parameters after creation (normally, type params are inferred from the initializer)
    + Might as well support inferring all ADT type parameters after creation, since we have to do it for builtin containers anyway
    + Could fix by not ever 'instantiating' ADTs: just record instance type args and use the base decl. plus an instance's type args to do some substitution to get the instance's field types
        + Needs to happen when dealing with 'selector' expressions and composite literals, but that's about it
    + Currently, performing a separate pass to "cannonicalize" types
+ Compiler complains about missing methods in certain cases (when only called inside another polymorphic function; see `fix_list` functions scattered throughout the tests)
    + Could fix by iterating over matching impl blocks and instantiating them when a polymorphic ADT is instantiated
+ Need to implement type inference on "projections", which appear in associated methods called on polymorphic types and polymorphic enumerations

## References
+ [Lua](https://www.lua.org/)
+ [MicroPython](https://micropython.org/)
+ [Crafting Interpreters](https://craftinginterpreters.com/)

