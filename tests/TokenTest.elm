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
                        |> Expect.equal (Ok [ Token.LEFT_PAREN, Token.RIGHT_PAREN, Token.EOF ])
            , test "error" <|
                \_ ->
                    "$%$ chicket"
                        |> Token.scan
                        |> Expect.ok
            ]
        ]
