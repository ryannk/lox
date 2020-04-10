port module Ports exposing (tokenize)

import Token


port tokens : String -> Cmd msg


tokenize : String -> Cmd msg
tokenize source =
    source
        |> Token.scan
        |> Debug.toString
        |> tokens
