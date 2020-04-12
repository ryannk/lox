module TokenTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Token exposing (..)


suite : Test
suite =
    describe "Token"
        [ describe "Token.scan"
            [ test "handle empty file" <|
                \_ ->
                    ""
                        |> Token.scan
                        |> Expect.equal (Ok [ Token.EOF ])
            , test "Parentheses" <|
                \_ ->
                    "()"
                        |> Token.scan
                        |> Expect.equal (Ok [ Token.LeftParen, Token.RightParen, Token.EOF ])
            , test "Parentheses with keyworks" <|
                \_ ->
                    "(var;)"
                        |> Token.scan
                        |> Expect.equal (Ok [ Token.LeftParen, Token.Var, Token.Semicolon, Token.RightParen, Token.EOF ])
            , test "orchid not or" <|
                \_ ->
                    "orchid"
                        |> Token.scan
                        |> Expect.equal (Ok [ Token.Identifier "orchid", Token.EOF ])
            , test "number with . in middle" <|
                \_ ->
                    "2.5"
                        |> Token.scan
                        |> Expect.equal (Ok [ Token.Number 2.5, Token.EOF ])
            , test "number starting with ." <|
                \_ ->
                    ".5"
                        |> Token.scan
                        |> Expect.err
            , test "full int" <|
                \_ ->
                    "200"
                        |> Token.scan
                        |> Expect.equal (Ok [ Token.Number 200, Token.EOF ])
            , test "full int trailing ." <|
                \_ ->
                    "200."
                        |> Token.scan
                        |> Expect.err
            , test "strings require a closing double quote" <|
                \_ ->
                    "\"Hello"
                        |> Token.scan
                        |> Expect.err
            , test "strings work without escaped quotes" <|
                \_ ->
                    "\"Hello\""
                        |> Token.scan
                        |> Expect.ok [ Token.String "\"Hello\"", Token.EOF ]
            , test "strings work with escaped quotes" <|
                \_ ->
                    "\"Hello, \\\"Kelch\\\"!\""
                        |> Token.scan
                        |> Expect.ok [ Token.String "\"Hello, \\\"Kelch\\\"!\"", Token.EOF ]
            , test "error" <|
                \_ ->
                    "$%$ chicket"
                        |> Token.scan
                        |> Expect.err
            ]
        ]
