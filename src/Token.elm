module Token exposing (Token(..), scan)

import Parser exposing (..)


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
        [ succeed (Parser.Loop tokens)
            |. oneOf
                [ commentParser
                , chompIf (\c -> c == ' ' || c == '\n' || c == '\u{000D}' || c == '\t')
                ]
        , succeed (\token -> Parser.Loop (token :: tokens))
            |= oneOf
                [ numberParser
                , stringLiteralParser
                , symbolParser
                , reservedWordParser
                , identifierParser
                ]
        , succeed (Parser.Done (List.reverse (EOF :: tokens)))
            |. Parser.end
        ]


commentParser : Parser ()
commentParser =
    lineComment "//"


symbolParser : Parser Token
symbolParser =
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
        , symbol "!=" |> Parser.map (\_ -> BangEqual)
        , symbol "!" |> Parser.map (\_ -> Bang)
        , symbol "/" |> Parser.map (\_ -> Slash)
        , symbol "==" |> Parser.map (\_ -> EqualEqual)
        , symbol "=" |> Parser.map (\_ -> Equal)
        , symbol "<=" |> Parser.map (\_ -> LessEqual)
        , symbol "<" |> Parser.map (\_ -> Less)
        , symbol ">=" |> Parser.map (\_ -> GreaterEqual)
        , symbol ">" |> Parser.map (\_ -> Greater)
        ]


identifierParser : Parser Token
identifierParser =
    succeed Identifier
        |= alphaParser


alphaParser : Parser String
alphaParser =
    Parser.getChompedString <|
        succeed ()
            |. chompIf (\c -> Char.isAlpha c || c == '_')
            |. chompWhile (\c -> Char.isAlphaNum c || c == '_')


numberParser : Parser Token
numberParser =
    succeed Number
        |= numberLiteralParser


numberLiteralParser : Parser Float
numberLiteralParser =
    let
        failIfNotFloat : String -> Parser Float
        failIfNotFloat str =
            case String.toFloat str of
                Just float ->
                    succeed float

                Nothing ->
                    problem "not a float!"
    in
    oneOf
        [ symbol "." |> andThen (\_ -> problem "Cannot start with .")
        , backtrackable int |> map toFloat
        , (succeed ()
            |. chompWhile Char.isDigit
            |. chompIf ((==) '.')
            |. chompIf Char.isDigit
            |. chompWhile Char.isDigit
          )
            |> getChompedString
            |> andThen failIfNotFloat
        ]


stringLiteralParser : Parser Token
stringLiteralParser =
    (succeed ()
        |. chompIf ((==) '"')
        |. loop () validStringCharacter
    )
        |> getChompedString
        |> map (\str -> String.dropLeft 1 str |> String.dropRight 1)
        |> map String


validStringCharacter : () -> Parser (Step () ())
validStringCharacter _ =
    oneOf
        [ succeed (Loop ())
            |. chompIf ((==) '\\')
            |. chompIf ((==) '"')
        , succeed (Loop ())
            |. chompIf ((/=) '"')
        , succeed (Done ())
            |. chompIf ((==) '"')
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
