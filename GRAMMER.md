# Paw language grammer (EBNF)
**TODO: get rid of requirement that "break" | "return" | "continue" is the last statement in the block**
**      just don't emit unreachable code**
**      use a tool to validate this EBNF...**

## Module
```ebnf
Module = "mod" name {Item} .
```

## Items
An item defined in a given module can be accessed from anywhere in the module.
Items must be placed at the toplevel, and are fixed at compile time.
```ebnf
Item     = ["pub"] ItemDecl .
ItemDecl = ConstDecl | FunctionDecl | StructDecl | 
           EnumDecl | TypeDecl .
```

### Functions
```ebnf
FunctionDecl = "fn" Function .
Function     = name [Generics] FuncHead Block .
FuncHead     = "(" [{Field ","} Field] ")" ["->" Type] .
Field        = name ":" Type .
```

### Structures
```ebnf
StructDecl = "struct" name [Generics] [StructBody] .
StructBody = "{" {Field ","} Field [","] "}" .
```

### Enumerations
```ebnf
EnumDecl = "enum" name [Generics] EnumBody .
EnumBody = "{" [{Variant ","} Variant] "}" .
Variant  = name [Payload] .
Payload  = "(" {Type ","} Type [","] ")" .
```

## Statements
```ebnf
Stmt     = ExprStmt | WhileStmt | DoWhileStmt |
           ForStmt | IfElse | Declaration | Block .
Block    = "{" {Stmt ";"} "}" .
ExprStmt = Expr .
```

### Control flow
```ebnf
IfElse      = "if" Expr Block [{"else" IfElse} | "else" Block] .
WhileStmt   = "while" Expr Block .
DoWhileStmt = "do" Block "while" Expr .
ForStmt     = ForIn | ForNum .
ForIn       = "for" name "in" Expr Block .
ForNum      = "for" name "=" Expr "," Expr ["," Expr] Block .
```

## Pattern matching
```ebnf
MatchExpr   = "match" Expr MatchBody .
MatchBody   = "{" {MatchArm ","} MatchArm "}" .
MatchClause = Pattern "=>" MatchArm .
MatchArm    = Expr | Block .
```

## Paths
```ebnf
Path    = {Segment "::"} Segment .
Segment = name [TypeArgs] .
```

## Patterns
```ebnf
Pattern = LiteralPat | TuplePat | StructPat | 
          VariantPat | PathPat | RangePat .
LiteralPat = StrPat | IntPat | BoolPat .
RangePat = Pattern RangeSep Pattern .
RangeSep = ".." | "..=" .
PatList = {Pattern ","} Pattern [","] .
TuplePat = "(" [{Pattern ","} Pattern "," [Pattern]] ")".
VariantPat = Path "(" PatList ")" .
StructPat = Path "{" PatList "}" .
PathPat = Path .
```

## Declarations
```ebnf
Declaration = VarDecl | TypeDecl .
VarDecl     = "let" name [":" Type] "=" Expr .
TypeDecl    = "type" name [Generics] "=" Type .
Generics    = "<" {name ","} name [","] ">" .
```

## Operators
```ebnf
BinOp = "+" | "-" | "*" | "/" |
        "%" | "&" | "^" | "|" |
        "<" | "<=" | ">" | ">=" | 
        "==" | "!=" | "&&" | "||" .
UnOp  = "-" | "~" | "!" | "#" .
```

## Expressions
```ebnf
Expr        = PrimaryExpr | Expr BinOp Expr | UnOp Expr . 
PrimaryExpr = Operand | Call | "(" Expr ")" .
Call        = PrimaryExpr "(" [ExprList] ")" .
Index       = PrimaryExpr "[" ExprList "]" .
Selector    = PrimaryExpr "." name .
ExprList    = {Expr ","} Expr .
```

## Types
```ebnf
Type       = NamedType | TypeLit .
TypeLit    = FuncType | ListType | MapType | 
             TupleType | NamedType .
FuncType   = "fn" "(" [TypeList] ")" ["->" Type] .
TypeList   = {Type ","} Type [","] .
TypeArgs   = "::" "<" TypeList ">" .
NamedType  = name [TypeArgs] .
ListType = "[" Type "]" .
MapType    = "[" Type ":" Type "]" .
TupleType  = "(" [{Type ","} Type "," [Type]] ")".
```

## Operands
```ebnf
Operand  = Path | Index | Selector | Literal .
Literal  = BasicLit | CompositeLit .
BasicLit = int_lit | bool_lit | float_lit | string_lit .
```

### Composite literals
Note that the unit type is just a 0-tuple (an tuple with 0 elements).
A 1-tuple must have a trailing `,` to distinguish it from a parenthesized expression.
Enumerator literals are not listed here: they are created using constructor syntax, which is indistinguishable from calling a function with the same name as the variant.
A constructor function takes the enumerator's fields as arguments and returns an instance of the enumeration.
```ebnf
CompositeLit = TupleLit | ListLit | MapLit | StructLit .
TupleLit     = "(" [{Expr ","} Expr "," [Expr]] ")".
ListLit    = "[" [ExprList [","]] "]" .
MapLit       = "[" ":" "]" | "[" [MapElems [","]] "]" .
MapElems     = MapElem {"," MapElem} [","] .
MapElem      = Expr ":" Expr .
StructLit    = Path ["{" [StructFields [","]] "}"] .
StructFields = StructField {"," StructField} [","] .
StructField  = name ":" Expr .
```

### Integer literals
```ebnf
int_lit        = decimal_lit | binary_lit | octal_lit | hex_lit .
decimal_lit    = "0" | ("1" … "9") [decimal_digits] .
binary_lit     = "0" ("b" | "B") binary_digits .
octal_lit      = "0" ("o" | "O") octal_digits .
hex_lit        = "0" ("x" | "X") hex_digits .
```

### Float literals
```ebnf
float_lit        = decimal_digits "." [decimal_digits] [decimal_exponent] |
                   decimal_digits decimal_exponent |
                   "." decimal_digits [decimal_exponent] .
decimal_exponent = ("e" | "E") ["+" | "-"] decimal_digits .
```

## Miscellaneous
```ebnf
name           = letter {letter | decimal_digit} .
letter         = "A" … "Z" | "a" … "z" | "_" .
decimal_digit  = "0" … "9" .
binary_digit   = "0" | "1" .
octal_digit    = "0" … "7" .
hex_digit      = "0" … "9" | "A" … "F" | "a" … "f" .
decimal_digits = decimal_digit {decimal_digit} .
binary_digits  = binary_digit {binary_digit} .
octal_digits   = octal_digit {octal_digit} .
hex_digits     = hex_digit {hex_digit} .
```

## Imports
TODO: support something like this for modules
mod Mod
pub fn a() {...}
pub struct B {...}
pub enum C {...}

Import statement:
(a) import Mod
(b) import Mod::a
(c) import Mod::{a, B}
(d) import Mod::a as x
(e) import Mod::{a as x, B as Y}
(f) import Mod::*

Symbols added to public/local items:
(a) Mod::a, Mod::B, Mod::C
(b) Mod::a
(c) Mod::a, Mod::B
(d) x
(e) x, Y
(d) a, B, C

