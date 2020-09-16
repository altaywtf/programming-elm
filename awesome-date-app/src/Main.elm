module Main exposing (main)

import App exposing (Flags, Model, Msg, init, update, view)
import Browser
import Html


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
