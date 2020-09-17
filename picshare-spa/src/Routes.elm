module Routes exposing (Route(..), match)

import Url exposing (Url)
import Url.Parser as Parser exposing (Parser)


type Route
    = Home
    | Account


routes : Parser (Route -> a) a
routes =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map Account (Parser.s "account")
        ]


match : Url -> Maybe Route
match url =
    Parser.parse routes url
