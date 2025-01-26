# Paw language grammer (EBNF)

## Items
An item defined in a given module can be accessed from anywhere in the module.
Items must be placed at the toplevel, and are fixed at compile time.
```ebnf
Item     = ["pub"] ItemDecl .
ItemDecl = ConstDecl | FunctionDecl | StructDecl | 
           EnumDecl | TypeDecl | UseDecl | ImplDecl .
```

### Functions
```ebnf
FunctionDecl = "fn" Function .
Function     = name [Generics] Signature Block .
FParameters  = {FParam ","} FParam [","] .
Signature    = "(" [FParameters] ")" ["->" Type] .
FParam       = name ":" Type .
```

### Structures
```ebnf
StructDecl = "struct" name [Generics] (StructBody | ";") .
StructBody = "{" {Field ","} Field [","] "}" .
Field      = ["pub"] name ":" Type .
```

### Enumerations
```ebnf
EnumDecl = "enum" name [Generics] (EnumBody | ";") .
EnumBody = "{" [{Variant ","} Variant] "}" .
Variant  = name ["(" TypeList ")"] .
```

### Implementations
```ebnf
ImplDecl = "impl" [Generics] name [Generics] ImplBody .
ImplBody = ["pub"]  .
```

## Statements
```ebnf
Stmt     = ExprStmt | DeclStmt .
ExprStmt = Expr .
DeclStmt = Declaration .
```

## Pattern matching
```ebnf
MatchExpr   = "match" Expr MatchBody .
MatchBody   = "{" {MatchClause ","} MatchClause "}" .
MatchClause = Pattern "=>" Expr .
```

## Paths
```ebnf
Path    = {Segment "::"} Segment .
Segment = name [TypeArgs] .
```

## Patterns
```ebnf
Pattern    = LiteralPat | TuplePat | StructPat | 
             VariantPat | PathPat | RangePat .
LiteralPat = StrPat | IntPat | BoolPat .
RangePat   = Pattern RangeSep Pattern .
RangeSep   = ".." | "..=" .
PatList    = {Pattern ","} Pattern [","] .
TuplePat   = "(" [{Pattern ","} Pattern "," [Pattern]] ")".
VariantPat = Path "(" PatList ")" .
StructPat  = Path "{" PatList "}" .
PathPat    = Path .
```

## Declarations
```ebnf
Declaration = UseDecl | VarDecl | TypeDecl .
UseDecl     = "use" name ["::" name] as name
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

## Expressions
```ebnf
Expr        = PrimaryExpr | Expr BinOp Expr | UnOp Expr | Closure |
              Block | IfElse | ForLoop | WhileLoop . 
PrimaryExpr = Operand | Call | "(" Expr ")" .
Call        = PrimaryExpr "(" [ExprList] ")" .
Index       = PrimaryExpr "[" Expr [":" Expr] "]" .
Selector    = PrimaryExpr "." name .
ExprList    = {Expr ","} Expr .
Block       = "{" {Stmt ";"} [Expr] "}" .
Closure     = "|" [CParameters] "|" (("->" Type Block) | Expr) .
CParameters = {CParam ","} CParam [","] .
CParam      = name [":" Type] .
```

### Control flow
```ebnf
IfElse    = "if" Expr Block [{"else" IfElse} | "else" Block] .
WhileLoop = "while" Expr Block .
ForLoop   = "for" name "in" Expr Block .
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
ListLit      = "[" [ExprList [","]] "]" .
MapLit       = "[" ":" "]" | "[" [MapElems [","]] "]" .
MapElems     = MapElem {"," MapElem} [","] .
MapElem      = Expr ":" Expr .
StructLit    = Path ["{" [StructFields [","]] "}"] .
StructFields = StructField {"," StructField} [","] .
StructField  = name ":" Expr .
```

### Integer literals
```ebnf
int_lit     = decimal_lit | binary_lit | octal_lit | hex_lit .
decimal_lit = "0" | ("1" … "9") [decimal_digits] .
binary_lit  = "0" ("b" | "B") binary_digits .
octal_lit   = "0" ("o" | "O") octal_digits .
hex_lit     = "0" ("x" | "X") hex_digits .
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
