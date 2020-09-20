module Routes exposing (Route(..), href, match)

import Html
import Html.Attributes
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)


type Route
    = Home
    | Account
    | UserFeed String


routes : Parser (Route -> a) a
routes =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map Account (Parser.s "account")
        , Parser.map UserFeed (Parser.s "user" </> Parser.string </> Parser.s "feed")
        ]


match : Url -> Maybe Route
match url =
    Parser.parse routes url


navigateToUrl : Route -> String
navigateToUrl route =
    case route of
        Home ->
            "/"

        Account ->
            "/account"

        UserFeed username ->
            "/user/" ++ username ++ "/feed"


href : Route -> Html.Attribute msg
href route =
    Html.Attributes.href (navigateToUrl route)
