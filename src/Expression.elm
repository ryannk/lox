module Expression exposing (..)

-- expression → literal
--            | unary
--            | binary
--            | grouping ;
-- literal    → NUMBER | STRING | "true" | "false" | "nil" ;
-- grouping   → "(" expression ")" ;
-- unary      → ( "-" | "!" ) expression ;
-- binary     → expression operator expression ;
-- operator   → "==" | "!=" | "<" | "<=" | ">" | ">="
--            | "+"  | "-"  | "*" | "/" ;


type Expression
    = Literal Literal
    | Binary Expression Operator Expression
    | Unary
    | Grouping Expression


type Literal
    = Number Float
    | String String
    | True
    | False
    | Nil


type Unary
    = Negative Expression
    | Not Expression


type Operator
    = BangEqual
    | EqualEqual
    | Greater
    | GreaterEqual
    | Less
    | LessEqual
    | Minus
    | Plus
    | Slash
    | Star
