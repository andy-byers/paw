# Paw language grammer (EBNF)
**TODO: get rid of requirement that "break" | "return" | "continue" is the last statement in the block**
**      just don't emit unreachable code**
**      use a tool to validate this...**

## Statements
```ebnf
Stmt     ::= ExprStmt | WhileStmt | DoWhileStmt |
             ForStmt | IfElse | Declaration | 
             Block .
Chunk    ::= {Stmt [";"]} [LastStmt [";"]] .
Block    ::= "{" Chunk "}" .
LastStmt ::= "return" [Expr] | "continue" | "break" .
ExprStmt ::= Operand "=" Expr | Call | Match .
```

### Control flow
```ebnf
IfElse      ::= "if" Expr Block [{"else" IfElse} | "else" Block] .
WhileStmt   ::= "while" Expr Block .
DoWhileStmt ::= "do" Block "while" Expr .
ForStmt     ::= ForIn | ForNum .
ForIn       ::= "for" name "in" Expr Block .
ForNum      ::= "for" name "=" Expr "," Expr ["," Expr] Block .
```

## Pattern matching
```ebnf
MatchExpr   ::= "match" Expr MatchBody .
MatchBody   ::= "{" {MatchArm ","} MatchArm "}" .
MatchClause ::= Pattern "=>" MatchArm .
MatchArm    ::= Expr | Block .
```

## Paths
TODO: 'Suffixed expressions' can be split up into a path, possibly followed by a '.', '?', '(', or '{'.
TODO: The suffix after the path cannot contain '::', since any of the aformentioned tokens resolve to a value, as opposed to a type
```ebnf
Path    ::= {Segment "::"} Segment
Segment ::= name [TypeArgs]
```

## Patterns
```ebnf
Pattern ::= LiteralPat | TuplePat | StructPat | 
            VariantPat | PathPat | RangePat .
LiteralPat ::= StrPat | IntPat | BoolPat .
RangePat ::= Pattern RangeSep Pattern .
RangeSep ::= ".." | "..=" .
PatList ::= {Pattern ","} Pattern [","] .
TuplePat ::= "(" PatList ")" .
VariantPat ::= Path "(" PatList ")"
StructFieldPat ::= 
StructPat ::= Path "{" PatList "}"
PathPat ::= Path
```

## Declarations
```ebnf
Declaration ::= VarDecl | FunctionDecl | StructDecl | 
                EnumDecl | TypeDecl .
VarDecl     ::= "let" name [":" Type] "=" Expr .
TypeDecl    ::= "type" name [Generics] "=" Type .
Generics    ::= "[" {name ","} name "]" .
```

### Functions
```ebnf
FunctionDecl ::= "fn" Function .
Function     ::= name [Generics] FuncHead Block .
FuncHead    ::= "(" [{Field ","} Field] ")" ["->" Type] .
Field        ::= name ":" Type .
```

### Structures
```ebnf
StructDecl ::= "struct" name [Generics] StructBody .
StructBody ::= "{" {Field [";"]} "}" .
```

### Enumerators
```ebnf
EnumDecl ::= "enum" name [Generics] EnumBody .
EnumBody ::= "{" [{Variant ","} Variant] "}" .
Variant  ::= Path [Payload] .
Payload  ::= "(" {Type ","} Type ")" .
```

## Operators
```ebnf
BinOp ::= "+" | "-" | "*" | "/" |
          "%" | "&" | "^" | "|" |
          "<" | "<=" | ">" | ">=" | 
          "==" | "!=" | "&&" | "||" .
UnOp  ::= "-" | "~" | "!" | "#" .
```

## Expressions
```ebnf
Expr        ::= PrimaryExpr | Expr BinOp Expr | UnOp Expr . 
PrimaryExpr ::= Operand | Call | Literal | "(" Expr ")" .
Call        ::= PrimaryExpr "(" [ExprList] ")" .
Operand     ::= name | Index | Selector .
Index       ::= PrimaryExpr "[" ExprList "]" .
Access      ::= PrimaryExpr "::" name .
Selector    ::= PrimaryExpr "." name .
ExprList    ::= {Expr ","} Expr .
```

## Types
```ebnf
Type       ::= NamedType | TypeLit .
TypeLit    ::= FuncType | VectorType | MapType | 
               TupleType | NamedType .
FuncType   ::= "fn" "(" [TypeList] ")" ["->" Type] .
TypeList   ::= {Type ","} Type
TypeArgs   ::= "[" TypeList "]" .
NamedType  ::= name [TypeArgs] .
VectorType ::= "[" Type "]" .
MapType    ::= "[" Type ":" Type "]" .
TupleType  ::= "(" [{Type ","} Type "," [Type]] ")".
```

## Operands
```ebnf
Operand  ::= Path | Literal .
Literal  ::= BasicLit | CompositeLit .
BasicLit ::= int_lit | bool_lit | float_lit | string_lit .
```

### Composite literals
Note that the unit type is just a 0-tuple (an tuple with 0 elements).
A 1-tuple must have a trailing `,` to distinguish it from a parenthesized expression.
```ebnf
CompositeLit ::= VectorLit | MapLit | TupleLit | StructLit | VariantLit .
VectorLit    ::= "[" [ExprList [","]] "]" .
MapLit       ::= "[" ":" "]" | "[" [ItemList [","]] "]" .
TupleLit     ::= "(" [{Expr ","} Expr "," [Expr]] ")".
StructLit    ::= Path "{" [ItemList [","]] "}" .
VariantLit   ::= Path ["(" {Expr ","} Expr ")"] .
ItemList     ::= KeyedItem {"," KeyedItem} [","] .
KeyedItem    ::= [Key ":"] Expr .
Key          ::= name | Expr .
```

### Integer literals
```ebnf
int_lit        ::= decimal_lit | binary_lit | octal_lit | hex_lit .
decimal_lit    ::= "0" | ("1" … "9") [decimal_digits] .
binary_lit     ::= "0" ("b" | "B") binary_digits .
octal_lit      ::= "0" ("o" | "O") octal_digits .
hex_lit        ::= "0" ("x" | "X") hex_digits .
```

### Float literals
```ebnf
float_lit        := decimal_digits "." [decimal_digits] [decimal_exponent] |
                    decimal_digits decimal_exponent |
                    "." decimal_digits [decimal_exponent] .
decimal_exponent := ("e" | "E") ["+" | "-"] decimal_digits .
```

## Miscellaneous
```ebnf
name           ::= letter {letter | decimal_digit} .
letter         ::= "A" … "Z" | "a" … "z" | "_" .
decimal_digit  ::= "0" … "9" .
binary_digit   ::= "0" | "1" .
octal_digit    ::= "0" … "7" .
hex_digit      ::= "0" … "9" | "A" … "F" | "a" … "f" .
decimal_digits ::= decimal_digit {decimal_digit} .
binary_digits  ::= binary_digit {binary_digit} .
octal_digits   ::= octal_digit {octal_digit} .
hex_digits     ::= hex_digit {hex_digit} .
```

