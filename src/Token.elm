module Token exposing (..)

import Parser exposing ((|.), (|=), Parser, float, keyword, map, oneOf, succeed, symbol)


type alias Name =
    String


type Token
    = LeftParen
    | RightParen
    | LeftBrace
    | RightBrace
    | Comma
    | Dot
    | Minus
    | Plus
    | Semicolon
    | Slash
    | Star
    | Bang
    | BangEqual
    | Equal
    | EqualEqual
    | Greater
    | GreaterEqual
    | Less
    | LessEqual
    | Identifier Name
    | String String
    | Number Float
    | And
    | Class
    | Else
    | False
    | Fun
    | For
    | If
    | Nil
    | Or
    | Print
    | Return
    | Super
    | This
    | True
    | Var
    | While
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
            |= oneOf
                [ singleCharacterParser
                , reservedWordParser
                ]
        , succeed (Parser.Done (List.reverse (EOF :: tokens)))
            |. Parser.end
        , Parser.problem "unexpected character"
        ]


singleCharacterParser : Parser Token
singleCharacterParser =
    oneOf
        [ symbol "{" |> Parser.map (\_ -> LeftBrace)
        , symbol "}" |> Parser.map (\_ -> RightBrace)
        , symbol "(" |> Parser.map (\_ -> LeftParen)
        , symbol ")" |> Parser.map (\_ -> RightParen)
        , symbol "," |> Parser.map (\_ -> Comma)
        , symbol "." |> Parser.map (\_ -> Dot)
        , symbol "-" |> Parser.map (\_ -> Minus)
        , symbol "+" |> Parser.map (\_ -> Plus)
        , symbol ";" |> Parser.map (\_ -> Semicolon)
        , symbol "*" |> Parser.map (\_ -> Star)
        ]


reservedWordParser : Parser Token
reservedWordParser =
    oneOf
        [ keyword "and" |> Parser.map (\_ -> And)
        , keyword "class" |> Parser.map (\_ -> Class)
        , keyword "else" |> Parser.map (\_ -> Else)
        , keyword "false" |> Parser.map (\_ -> False)
        , keyword "for" |> Parser.map (\_ -> For)
        , keyword "fun" |> Parser.map (\_ -> Fun)
        , keyword "if" |> Parser.map (\_ -> If)
        , keyword "nil" |> Parser.map (\_ -> Nil)
        , keyword "or" |> Parser.map (\_ -> Or)
        , keyword "print" |> Parser.map (\_ -> Print)
        , keyword "return" |> Parser.map (\_ -> Return)
        , keyword "super" |> Parser.map (\_ -> Super)
        , keyword "this" |> Parser.map (\_ -> This)
        , keyword "true" |> Parser.map (\_ -> True)
        , keyword "var" |> Parser.map (\_ -> Var)
        , keyword "while" |> Parser.map (\_ -> While)
        ]
