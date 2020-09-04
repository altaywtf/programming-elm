module Main exposing (main)

import Html exposing (text)


sayHello : String -> String
sayHello name =
    "Hello, " ++ name ++ "."


main =
    text (sayHello "Elm")
