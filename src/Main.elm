module Main exposing (main)

import Ports


main =
    Platform.worker
        { init = init
        , update = \msg model -> ( model, Cmd.none )
        , subscriptions = \model -> Sub.none
        }


init : String -> ( (), Cmd mgs )
init source =
    ( (), Ports.tokenize source )
