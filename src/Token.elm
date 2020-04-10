module Token exposing (..)

import Parser exposing (Parser)


type alias Name =
    String


type Token
    = LeftParen
    | RightParen
    | LEFT_BRACE
    | RIGHT_BRACE
    | COMMA
    | DOT
    | MINUS
    | PLUS
    | SEMICOLON
    | SLASH
    | STAR
    | BANG
    | BANG_EQUAL
    | EQUAL
    | EQUAL_EQUAL
    | GREATER
    | GREATER_EQUAL
    | LESS
    | LESS_EQUAL
    | IDENTIFIER Name
    | STRING String
    | NUMBER Float
    | AND
    | CLASS
    | ELSE
    | FALSE
    | FUN
    | FOR
    | IF
    | NIL
    | OR
    | PRINT
    | RETURN
    | SUPER
    | THIS
    | TRUE
    | VAR
    | WHILE
    | EOF


scan : String -> Result (List Parser.DeadEnd) (List Token)
scan source =
    Parser.run parser source


parser : Parser (List Token)
parser =
    Parser.succeed [ EOF ]
