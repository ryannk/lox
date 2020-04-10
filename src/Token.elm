module Token exposing (..)

import Parser exposing ((|.), (|=), Parser, float, keyword, map, oneOf, succeed, symbol)


type alias Name =
    String


type Token
    = LEFT_PAREN
    | RIGHT_PAREN
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
    Parser.loop [] loopHelper


loopHelper : List Token -> Parser (Parser.Step (List Token) (List Token))
loopHelper tokens =
    oneOf
        [ succeed (\token -> Parser.Loop (token :: tokens))
            |= singleCharacterParser
        , succeed (Parser.Done (List.reverse (EOF :: tokens)))
            |. Parser.end
        , Parser.problem "unexpected character"
        ]


singleCharacterParser : Parser Token
singleCharacterParser =
    oneOf
        [ symbol "{" |> Parser.map (\_ -> LEFT_BRACE)
        , symbol "}" |> Parser.map (\_ -> RIGHT_BRACE)
        , symbol "(" |> Parser.map (\_ -> LEFT_PAREN)
        , symbol ")" |> Parser.map (\_ -> RIGHT_PAREN)
        , symbol "," |> Parser.map (\_ -> COMMA)
        , symbol "." |> Parser.map (\_ -> DOT)
        , symbol "-" |> Parser.map (\_ -> MINUS)
        , symbol "+" |> Parser.map (\_ -> PLUS)
        , symbol ";" |> Parser.map (\_ -> SEMICOLON)
        , symbol "*" |> Parser.map (\_ -> STAR)
        ]
