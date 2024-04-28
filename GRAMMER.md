# Paw language grammer (EBNF)
**TODO: get rid of requirement that "break" | "return" | "continue" is the last statement in the block**
**      just don't emit unreachable code**
**      use a tool to validate this...**

## Statements
```
Stmt     ::= ExprStmt | WhileLoop | DoWhileLoop |
             ForLoop | IfElse | Declaration | 
             Block .
Chunk    ::= {Stmt [";"]} [LastStmt [";"]] .
Block    ::= "{" Chunk "}" .
LastStmt ::= "return" [Expr] | "continue" | "break" .
ExprStmt ::= Operand "=" Expr | Call | Match .
```

### Control flow
```
IfElse      ::= "if" Expr Block [{"else" IfElse} | "else" Block] .
WhileLoop   ::= "while" Expr Block .
DoWhileLoop ::= "do" Block "while" Expr .
ForLoop     ::= ForIn | ForNum .
ForIn       ::= "for" name "in" Expr Block .
ForNum      ::= "for" name "=" Expr "," Expr ["," Expr] Block .
Match       ::= "match" Expr MatchBody .
MatchBody   ::= "{" {MatchArm ","} MatchArm "}" .
MatchClause ::= Expr "=>" MatchArm .
MatchArm    ::= Expr | Block .
```

## Declarations
```
Declaration ::= VarDecl | FunctionDecl |
                ClassDecl | EnumDecl | TypeDecl .
VarDecl     ::= "let" name [":" Type] "=" Expr .
TypeDecl    ::= "type" name [TypeParam] "=" Type .
TypeParam   ::= "[" {name ","} name "]" .
```

### Functions
```
FunctionDecl ::= "fn" Function .
Function     ::= name [TypeParam] FuncType Block .
FuncType    ::= "(" [{Field ","} Field] ")" ["->" Type] .
Field        ::= name ":" Type .
```

### Classes
```
ClassDecl ::= "class" ClassType .
ClassType ::= name [TypeParam] ClassBody .
ClassBody ::= "{" {Attribute [";"]} "}" .
Attribute ::= Method | Field .
Method    ::= ["static"] Function .
```

### Enumerators
```
EnumDecl ::= "enum" name EnumBody .
EnumBody ::= "{" [{Variant ","} Variant] "}" .
Variant  ::= name [Payload] .
Payload  ::= "(" {Type ","} Type ")" .
```

## Operators
```
BinOp ::= "+" | "-" | "*" | "/" |
          "%" | "&" | "^" | "|" |
          "<" | "<=" | ">" | ">=" | 
          "==" | "!=" | "&&" | "||" .
UnOp  ::= "-" | "~" | "!" | "#" .
```

## Expressions
```
Expr        ::= PrimaryExpr | Expr BinOp Expr | UnOp Expr . 
PrimaryExpr ::= Operand | Call | Literal | "(" Expr ")" .
Call        ::= PrimaryExpr "(" [ExprList] ")" .
Operand     ::= name | Index | Selector .
Index       ::= PrimaryExpr "[" ExprList "]" .
Selector    ::= PrimaryExpr "." name .
ExprList    ::= {Expr ","} Expr .
```

## Types
```
Type      ::= name [TypeArgs] | TypeLit .
TypeLit   ::= FuncType | ArrayType | TupleType .
FuncType  ::= "fn" "(" [TypeList] ")" ["->" Type] .
TypeList  ::= {Type ","} Type
TypeArgs  ::= "[" TypeList "]" .
NamedType ::= name [TypeArgs] .
ArrayType ::= "[" Type ";" int_lit "]" .
TupleType ::= "(" [Type "," [Type]] ")" . 
```

## Operands
```
Operand  ::= Literal | name [TypeArgs] .
Literal  ::= BasicLit | CompositeLit .
BasicLit ::= int_lit | bool_lit | float_lit | string_lit .
```

### Composite literals
Note that the unit type is just a 0-tuple (an tuple with 0 elements).
A 1-tuple must have a trailing `,` to distinguish it from a parenthesized expression.
```
CompositeLit ::= ClassLit | ArrayLit | TupleLit | VariantLit .
ClassLit     ::= NamedType "{" [ItemList [","]] "}" .
ArrayLit     ::= "[" [ExprList [","]] "]" .
TupleLit     ::= "(" [Expr "," [Expr [","]]] ")" .
VariantLit   ::= name ["(" {Expr ","} Expr ")"] .
ItemList     ::= KeyedItem {"," KeyedItem} [","] .
KeyedItem    ::= [Key ":"] Expr .
Key          ::= name | Expr .
```

### Integer literals
```
int_lit        ::= decimal_lit | binary_lit | octal_lit | hex_lit .
decimal_lit    ::= "0" | ("1" … "9") [decimal_digits] .
binary_lit     ::= "0" ("b" | "B") binary_digits .
octal_lit      ::= "0" ("o" | "O") octal_digits .
hex_lit        ::= "0" ("x" | "X") hex_digits .
```

### Float literals
```
float_lit        := decimal_digits "." [decimal_digits] [decimal_exponent] |
                    decimal_digits decimal_exponent |
                    "." decimal_digits [decimal_exponent] .
decimal_exponent := ("e" | "E") ["+" | "-"] decimal_digits .
```

## Miscellaneous
```
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

